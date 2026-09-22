if (!exists("ai_report_ui", mode = "function")) source("R/ai.R")
if (!exists("algorithm_title_ui", mode = "function")) source("R/algorithm_tutorials.R")

market_date_range <- function(from, to) {
  from <- suppressWarnings(as.Date(from)); to <- suppressWarnings(as.Date(to))
  if (length(from) != 1L || is.na(from) || length(to) != 1L || is.na(to)) {
    stop("请选择有效的开始和结束日期。", call. = FALSE)
  }
  if (from > to) stop("开始日期不能晚于结束日期。", call. = FALSE)
  if (to > Sys.Date()) stop("结束日期不能晚于今天。", call. = FALSE)
  list(from = from, to = to)
}

market_symbol <- function(value, label = "代码") {
  value <- toupper(trimws(if (is.null(value) || !length(value)) "" else as.character(value)[1]))
  if (!nzchar(value)) stop(paste0("请输入", label, "。"), call. = FALSE)
  if (!grepl("^[A-Z0-9^.=:_-]{1,40}$", value)) stop(paste0(label, "格式无效。"), call. = FALSE)
  value
}

online_dataset_name <- function(source, symbol, from, to) {
  source_label <- if (identical(source, "fred")) "FRED" else "Yahoo"
  paste0(source_label, " · ", market_symbol(symbol), " · ", format(as.Date(from)), "_", format(as.Date(to)))
}

register_online_dataset <- function(add_dataset, value, label) {
  if (!is.function(add_dataset)) return(NULL)
  added <- add_dataset(stats::setNames(list(value), label))
  if (!length(added) || is.na(added[1]) || !nzchar(added[1])) {
    stop("在线数据已下载，但未能加入数据集列表。", call. = FALSE)
  }
  as.character(added[1])
}

quantmod_frame <- function(value, symbol, source = c("yahoo", "fred")) {
  source <- match.arg(source)
  if (is.null(value) || !NROW(value) || !NCOL(value)) stop("数据源没有返回观测值。", call. = FALSE)
  dates <- suppressWarnings(as.Date(zoo::index(value)))
  values <- as.data.frame(value, stringsAsFactors = FALSE)
  if (all(is.na(dates))) stop("返回数据中没有可识别的日期。", call. = FALSE)
  if (source == "yahoo") {
    suffix <- sub("^.*\\.", "", names(values))
    labels <- c(Open = "开盘价", High = "最高价", Low = "最低价", Close = "收盘价", Volume = "成交量", Adjusted = "复权收盘价")
    names(values) <- unname(ifelse(suffix %in% names(labels), labels[suffix], suffix))
  } else names(values) <- symbol
  result <- data.frame(日期 = dates, values, check.names = FALSE)
  result <- result[!is.na(result[[1]]), , drop = FALSE]
  row.names(result) <- NULL
  result
}

download_yahoo_data <- function(symbol, from, to, get_symbols = quantmod::getSymbols) {
  symbol <- market_symbol(symbol, "Yahoo 代码")
  dates <- market_date_range(from, to)
  value <- get_symbols(symbol, src = "yahoo", from = dates$from, to = dates$to,
    auto.assign = FALSE, warnings = FALSE)
  quantmod_frame(value, symbol, "yahoo")
}

parse_fred_observations <- function(payload, series_id) {
  observations <- payload$observations
  if (is.null(observations) || !NROW(observations)) stop("FRED 没有返回观测值。", call. = FALSE)
  dates <- suppressWarnings(as.Date(observations$date))
  values <- suppressWarnings(as.numeric(observations$value))
  result <- data.frame(日期 = dates, values, check.names = FALSE)
  names(result)[2] <- series_id
  result <- result[!is.na(result[[1]]), , drop = FALSE]
  row.names(result) <- NULL
  if (!nrow(result)) stop("FRED 返回数据中没有可识别的观测值。", call. = FALSE)
  result
}

download_fred_data <- function(series_id, from, to, api_key, perform = httr2::req_perform) {
  series_id <- market_symbol(series_id, "FRED 序列 ID")
  dates <- market_date_range(from, to)
  api_key <- trimws(if (is.null(api_key) || !length(api_key)) "" else as.character(api_key)[1])
  if (!grepl("^[a-z0-9]{32}$", api_key)) stop("请输入 32 位小写字母或数字组成的 FRED API Key。", call. = FALSE)
  request <- httr2::request("https://api.stlouisfed.org/fred/series/observations") |>
    httr2::req_url_query(series_id = series_id, api_key = api_key, file_type = "json",
      observation_start = format(dates$from), observation_end = format(dates$to)) |>
    httr2::req_timeout(60)
  response <- perform(request)
  payload <- httr2::resp_body_json(response, simplifyVector = TRUE)
  parse_fred_observations(payload, series_id)
}

ts_number <- function(x) {
  if (!is.finite(x)) return("无法计算")
  format(signif(x, 4), trim = TRUE, scientific = abs(x) > 0 && abs(x) < 0.001)
}

ts_pvalue <- function(p) {
  if (!is.finite(p)) "无法计算" else if (p < 0.001) "< 0.001" else paste0("= ", formatC(p, digits = 3, format = "f"))
}

parse_time_axis <- function(x) {
  if (inherits(x, "Date")) return(list(value = x, type = "date"))
  if (inherits(x, "POSIXt")) return(list(value = as.POSIXct(x), type = "datetime"))
  if (is.numeric(x)) return(list(value = as.numeric(x), type = "numeric"))
  raw <- trimws(as.character(x))
  raw[raw == ""] <- NA_character_
  numeric_value <- suppressWarnings(as.numeric(raw))
  if (all(is.na(raw) | !is.na(numeric_value))) return(list(value = numeric_value, type = "numeric"))
  parsed <- rep(as.Date(NA), length(raw))
  formats <- c("%Y-%m-%d", "%Y/%m/%d", "%Y%m%d", "%d/%m/%Y", "%m/%d/%Y", "%Y-%m", "%Y/%m")
  for (format in formats) {
    missing <- is.na(parsed) & !is.na(raw)
    parsed[missing] <- suppressWarnings(as.Date(raw[missing], format = format))
  }
  if (!any(!is.na(parsed))) stop("时间字段无法识别。请使用日期、日期时间或数值序号。", call. = FALSE)
  list(value = parsed, type = "date")
}

prepare_time_series <- function(data, time, value, transform = "level") {
  time <- as.integer(time); value <- as.integer(value)
  valid_column <- function(i) length(i) == 1 && !is.na(i) && i %in% seq_along(data)
  if (!valid_column(time)) stop("请选择有效时间字段。", call. = FALSE)
  if (!valid_column(value) || !is.numeric(data[[value]])) stop("请选择有效数值字段。", call. = FALSE)
  if (time == value) stop("时间字段和数值字段不能相同。", call. = FALSE)
  parsed <- parse_time_axis(data[[time]])
  time_value <- parsed$value
  numeric_value <- data[[value]]
  valid <- !is.na(time_value) & !is.na(numeric_value) & is.finite(numeric_value)
  if (is.numeric(time_value)) valid <- valid & is.finite(time_value)
  excluded <- sum(!valid)
  time_value <- time_value[valid]
  numeric_value <- numeric_value[valid]
  if (length(numeric_value) < 3) stop("有效时间序列数据不足 3 行。", call. = FALSE)
  order_index <- order(time_value)
  time_value <- time_value[order_index]
  numeric_value <- numeric_value[order_index]
  duplicate_rows <- length(time_value) - length(unique(time_value))
  if (duplicate_rows) {
    groups <- match(time_value, unique(time_value))
    numeric_value <- as.numeric(tapply(numeric_value, groups, mean))
    time_value <- time_value[!duplicated(time_value)]
  }
  if (length(numeric_value) < 3) stop("合并重复时间后不足 3 个时间点。", call. = FALSE)
  transform_labels <- c(level = "原始值", difference = "一阶差分", percent = "百分比变化（%）", log_return = "对数收益率（%）")
  if (!transform %in% names(transform_labels)) stop("请选择有效的序列变换。", call. = FALSE)
  transformed_time <- time_value
  transformed_value <- numeric_value
  if (transform == "difference") {
    transformed_time <- time_value[-1]
    transformed_value <- diff(numeric_value)
  } else if (transform == "percent") {
    transformed_time <- time_value[-1]
    transformed_value <- 100 * (numeric_value[-1] / numeric_value[-length(numeric_value)] - 1)
  } else if (transform == "log_return") {
    if (any(numeric_value <= 0)) stop("对数收益率要求所有有效原始值均大于 0。", call. = FALSE)
    transformed_time <- time_value[-1]
    transformed_value <- 100 * diff(log(numeric_value))
  }
  finite <- is.finite(transformed_value)
  transform_excluded <- sum(!finite)
  d <- data.frame(.time = transformed_time[finite], .value = transformed_value[finite])
  if (nrow(d) < 3) stop("变换后有效时间点不足 3 个。", call. = FALSE)
  gaps <- diff(as.numeric(d$.time))
  interval <- if (length(gaps)) stats::median(gaps) else NA_real_
  regular <- length(gaps) < 2 || all(abs(gaps - interval) <= sqrt(.Machine$double.eps) * max(1, abs(interval)))
  index <- seq_len(nrow(d))
  trend_model <- stats::lm(d$.value ~ index)
  trend_table <- suppressWarnings(summary(trend_model))$coefficients
  acf1 <- if (nrow(d) > 1 && stats::sd(d$.value) > 0) as.numeric(stats::acf(d$.value, plot = FALSE, lag.max = 1)$acf[2]) else NA_real_
  report <- c(
    "时间序列分析摘要",
    paste0("时间字段：", names(data)[time], "；数值字段：", names(data)[value], "；分析序列：", transform_labels[[transform]], "。"),
    sprintf("原始数据 %d 行；因时间无效、数值缺失或非有限而排除 %d 行；合并 %d 行重复时间；变换后另排除 %d 个非有限结果；最终分析 %d 个时间点。",
      nrow(data), excluded, duplicate_rows, transform_excluded, nrow(d)),
    paste0("时间范围：", format(min(d$.time)), " 至 ", format(max(d$.time)), "。记录已按时间升序排列；同一时间的多个数值按均值合并。"),
    paste0("描述统计：均值 = ", ts_number(mean(d$.value)), "；标准差 = ", ts_number(stats::sd(d$.value)),
      "；最小值 = ", ts_number(min(d$.value)), "；最大值 = ", ts_number(max(d$.value)), "。"),
    paste0("线性时间趋势（以观测顺序为单位）：斜率 = ", ts_number(coef(trend_model)[2]), "；p ", ts_pvalue(trend_table[2, 4]),
      "。该结果是描述性趋势；序列相关会使常规 lm 标准误和 p 值失真。"),
    paste0("一阶样本自相关 ACF(1) = ", ts_number(acf1), "。ACF 的滞后单位是相邻观测，而不是固定日历天数。"),
    if (regular) paste0("时间间隔规则，典型间隔为 ", ts_number(interval), "。") else
      paste0("时间间隔不完全规则，中位间隔为 ", ts_number(interval), "。移动平均、ACF 和季节分解均按观测顺序计算；金融交易日数据中的周末缺口不会自动补齐。"),
    "趋势、季节性和自相关描述统计结构，不自动建立因果关系，也不等同于样本外预测。季节分解要求用户给出的周期与数据频率具有实际含义。"
  )
  list(data = d, report = paste(report, collapse = "\n\n"), time_name = names(data)[time],
    value_name = names(data)[value], transform = transform, transform_label = transform_labels[[transform]],
    excluded = excluded, duplicate_rows = duplicate_rows, transform_excluded = transform_excluded,
    regular = regular, interval = interval, trend_model = trend_model, acf1 = acf1,
    analysis_summary = ai_analysis_summary(data.frame(value = d$.value), names(data)[value]))
}

garch_arch_lm_test <- function(standardized_residuals, lag = 10L) {
  z <- as.numeric(standardized_residuals)
  z <- z[is.finite(z)]
  lag <- as.integer(lag)
  if (!is.finite(lag) || lag < 1L) stop("ARCH-LM 检验滞后阶数至少为 1。", call. = FALSE)
  lag <- min(lag, max(1L, floor(length(z) / 5L)))
  if (length(z) <= lag + 2L) return(list(lag = lag, statistic = NA_real_, p_value = NA_real_))
  embedded <- embed(z^2, lag + 1L)
  fit <- stats::lm.fit(cbind(1, embedded[, -1L, drop = FALSE]), embedded[, 1L])
  total <- sum((embedded[, 1L] - mean(embedded[, 1L]))^2)
  r_squared <- if (total > 0) 1 - sum(fit$residuals^2) / total else 0
  statistic <- nrow(embedded) * max(0, r_squared)
  list(lag = lag, statistic = statistic, p_value = stats::pchisq(statistic, df = lag, lower.tail = FALSE))
}

fit_garch_analysis <- function(time_series_result, garch_order = 1L, arch_order = 1L,
                               demean = TRUE, diagnostic_lag = 10L) {
  if (!requireNamespace("tseries", quietly = TRUE)) {
    stop("缺少 tseries 包，请重新点击 Run App 让程序自动安装。", call. = FALSE)
  }
  garch_order <- as.integer(garch_order); arch_order <- as.integer(arch_order)
  diagnostic_lag <- as.integer(diagnostic_lag)
  if (length(garch_order) != 1L || is.na(garch_order) || !is.finite(garch_order) || garch_order < 0L || garch_order > 5L) stop("GARCH 阶数 p 应在 0～5 之间。", call. = FALSE)
  if (length(arch_order) != 1L || is.na(arch_order) || !is.finite(arch_order) || arch_order < 1L || arch_order > 5L) stop("ARCH 阶数 q 应在 1～5 之间。", call. = FALSE)
  if (length(diagnostic_lag) != 1L || is.na(diagnostic_lag) || !is.finite(diagnostic_lag) || diagnostic_lag < 1L || diagnostic_lag > 100L) stop("诊断滞后阶数应在 1～100 之间。", call. = FALSE)
  values <- as.numeric(time_series_result$data$.value)
  minimum <- max(40L, 10L * (garch_order + arch_order + 1L))
  if (length(values) < minimum) stop(paste0("GARCH(", garch_order, ",", arch_order,
    ") 至少需要 ", minimum, " 个有效时间点。"), call. = FALSE)
  if (!all(is.finite(values)) || stats::sd(values) <= 0) stop("序列必须包含足够的有限变化值才能拟合 GARCH。", call. = FALSE)
  center <- if (isTRUE(demean)) mean(values) else 0
  model_values <- values - center
  warnings <- character()
  fit <- withCallingHandlers(
    tseries::garch(model_values, order = c(garch_order, arch_order),
      series = time_series_result$value_name,
      control = tseries::garch.control(trace = FALSE, maxiter = 500)),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  coefficients <- stats::coef(fit)
  covariance <- tryCatch(stats::vcov(fit), error = function(e) matrix(NA_real_, length(coefficients), length(coefficients)))
  standard_error <- if (all(dim(covariance) == c(length(coefficients), length(coefficients)))) {
    sqrt(pmax(0, diag(covariance)))
  } else rep(NA_real_, length(coefficients))
  z_value <- coefficients / standard_error
  p_value <- 2 * stats::pnorm(abs(z_value), lower.tail = FALSE)
  type <- ifelse(names(coefficients) == "a0", "方差常数 ω",
    ifelse(grepl("^a", names(coefficients)), "ARCH 冲击项 α", "GARCH 波动项 β"))
  coefficient_table <- data.frame(参数 = names(coefficients), 含义 = type, 估计值 = as.numeric(coefficients),
    标准误 = standard_error, z值 = z_value, p值 = p_value, check.names = FALSE, row.names = NULL)
  persistence <- sum(coefficients[names(coefficients) != "a0"])
  stable <- all(coefficients >= 0) && is.finite(persistence) && persistence < 1
  unconditional_variance <- if (stable) coefficients[["a0"]] / (1 - persistence) else NA_real_
  sigma <- as.numeric(stats::fitted(fit)[, 1L])
  standardized <- as.numeric(stats::residuals(fit))
  finite_residual <- standardized[is.finite(standardized)]
  diag_lag <- min(max(1L, diagnostic_lag), max(1L, floor(length(finite_residual) / 5L)))
  box_residual <- stats::Box.test(finite_residual, lag = diag_lag, type = "Ljung-Box")
  box_squared <- stats::Box.test(finite_residual^2, lag = diag_lag, type = "Ljung-Box")
  arch_lm <- garch_arch_lm_test(finite_residual, diag_lag)
  jarque <- tryCatch(tseries::jarque.bera.test(finite_residual), error = function(e) NULL)
  log_likelihood <- as.numeric(stats::logLik(fit)); parameter_count <- length(coefficients)
  information <- list(log_likelihood = log_likelihood,
    aic = -2 * log_likelihood + 2 * parameter_count,
    bic = -2 * log_likelihood + log(length(model_values)) * parameter_count,
    negative_log_likelihood_without_constant = fit$n.likeli)
  prediction <- tryCatch(stats::predict(fit, newdata = model_values, genuine = TRUE), error = function(e) NULL)
  next_sigma <- if (is.null(prediction)) NA_real_ else as.numeric(tail(prediction[, 1L], 1L))
  diagnostics <- data.frame(
    检验 = c("标准化残差 Ljung-Box", "标准化残差平方 Ljung-Box", "ARCH-LM", "Jarque-Bera 正态性"),
    滞后阶数 = c(diag_lag, diag_lag, arch_lm$lag, NA_integer_),
    统计量 = c(unname(box_residual$statistic), unname(box_squared$statistic), arch_lm$statistic,
      if (is.null(jarque)) NA_real_ else unname(jarque$statistic)),
    p值 = c(box_residual$p.value, box_squared$p.value, arch_lm$p_value,
      if (is.null(jarque)) NA_real_ else jarque$p.value), check.names = FALSE)
  model_label <- paste0("GARCH(", garch_order, ",", arch_order, ")")
  report <- c(
    paste0(model_label, " 条件波动率分析"),
    paste0("分析序列：", time_series_result$value_name, " · ", time_series_result$transform_label,
      "；有效时间点：", length(values), "；", if (isTRUE(demean)) paste0("已减去样本均值 ", ts_number(center)) else "未去均值", "。"),
    paste0("模型设定：p = ", garch_order, " 表示条件方差滞后项 β 的阶数；q = ", arch_order,
      " 表示历史平方冲击 α 的阶数；采用条件正态准最大似然估计。"),
    paste0("波动持续性 Σα+Σβ = ", ts_number(persistence), "。",
      if (stable) paste0("系数非负且持续性小于 1；模型给出的无条件方差为 ", ts_number(unconditional_variance),
        "，无条件标准差为 ", ts_number(sqrt(unconditional_variance)), "。") else
        "系数非负性或 Σα+Σβ<1 的平稳性条件未满足，无条件方差不可作稳定的长期解释。"),
    paste0("对数似然 = ", ts_number(information$log_likelihood), "；AIC = ", ts_number(information$aic),
      "；BIC = ", ts_number(information$bic), "。AIC/BIC 只适合比较同一分析序列、同一均值处理下的候选模型。"),
    paste0("下一期条件标准差预测 = ", ts_number(next_sigma), "（单位与当前分析序列一致）。"),
    paste0("残差诊断（滞后 ", diag_lag, "）：标准化残差 Ljung-Box p ", ts_pvalue(box_residual$p.value),
      "；平方标准化残差 Ljung-Box p ", ts_pvalue(box_squared$p.value),
      "；ARCH-LM p ", ts_pvalue(arch_lm$p_value), "；Jarque-Bera p ",
      ts_pvalue(if (is.null(jarque)) NA_real_ else jarque$p.value), "。"),
    "平方标准化残差仍有显著自相关或 ARCH-LM 显著，通常表示当前阶数尚未充分解释波动聚集；Jarque-Bera 显著表示条件正态分布对尾部或偏度的描述可能不足。",
    if (time_series_result$transform == "level") "当前拟合使用原始水平值。金融价格通常应先转换为百分比变化或对数收益率；直接对非平稳价格水平拟合 GARCH 可能产生误导。" else
      "GARCH 描述的是条件方差随时间的变化，不代表收益方向，也不建立因果关系。",
    if (length(warnings)) paste0("拟合警告：", paste(unique(warnings), collapse = "；"), "。") else "优化器未返回警告。"
  )
  list(model = fit, order = c(garch = garch_order, arch = arch_order), demean = isTRUE(demean), center = center,
    coefficients = coefficient_table, persistence = persistence, stable = stable,
    unconditional_variance = unconditional_variance, information = information, diagnostics = diagnostics,
    conditional_sigma = sigma, standardized_residuals = standardized, next_sigma = next_sigma,
    warnings = unique(warnings), report = paste(report, collapse = "\n\n"))
}

build_garch_volatility_plot <- function(time_series_result) {
  req <- time_series_result$garch
  if (is.null(req)) stop("请先启用并运行 GARCH 分析。", call. = FALSE)
  d <- data.frame(.time = time_series_result$data$.time,
    .absolute = abs(time_series_result$data$.value - req$center),
    .sigma = req$conditional_sigma)
  ggplot2::ggplot(d, ggplot2::aes(.time)) +
    ggplot2::geom_line(ggplot2::aes(y = .absolute, colour = "绝对波动"), linewidth = 0.45, alpha = 0.48, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = .sigma, colour = "GARCH 条件标准差"), linewidth = 0.9, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = c("绝对波动" = "#94a3b8", "GARCH 条件标准差" = "#dc2626"), name = NULL) +
    ts_plot_theme() + ggplot2::labs(title = paste0("GARCH(", req$order[["garch"]], ",", req$order[["arch"]], ") 条件波动率"),
      subtitle = "红线为条件标准差；灰线为去均值后序列的绝对值", x = time_series_result$time_name,
      y = paste0(time_series_result$transform_label, "的波动"))
}

build_garch_diagnostic_plot <- function(time_series_result, max_lag = 30L) {
  garch <- time_series_result$garch
  if (is.null(garch)) stop("请先启用并运行 GARCH 分析。", call. = FALSE)
  z <- garch$standardized_residuals; z <- z[is.finite(z)]
  max_lag <- min(max(1L, as.integer(max_lag)), length(z) - 1L)
  acf_z <- as.numeric(stats::acf(z, plot = FALSE, lag.max = max_lag)$acf)[-1L]
  acf_sq <- as.numeric(stats::acf(z^2, plot = FALSE, lag.max = max_lag)$acf)[-1L]
  d <- data.frame(滞后 = rep(seq_len(max_lag), 2L), 自相关 = c(acf_z, acf_sq),
    序列 = factor(rep(c("标准化残差", "标准化残差平方"), each = max_lag),
      levels = c("标准化残差", "标准化残差平方")))
  bound <- stats::qnorm(.975) / sqrt(length(z))
  ggplot2::ggplot(d, ggplot2::aes(滞后, 自相关)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#64748b") +
    ggplot2::geom_hline(yintercept = c(-bound, bound), colour = "#dc2626", linetype = "dashed") +
    ggplot2::geom_segment(ggplot2::aes(xend = 滞后, y = 0, yend = 自相关), colour = "#2563eb", linewidth = .55) +
    ggplot2::geom_point(colour = "#2563eb", size = 1.7) + ggplot2::facet_grid(序列 ~ .) + ts_plot_theme() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(title = "GARCH 标准化残差诊断", subtitle = "平方残差中不应继续存在明显的波动聚集", x = "滞后阶数", y = "ACF")
}

forecast_accuracy <- function(actual, predicted, training) {
  actual <- as.numeric(actual); predicted <- as.numeric(predicted); error <- actual - predicted
  nonzero <- is.finite(actual) & actual != 0 & is.finite(error)
  smape_denominator <- abs(actual) + abs(predicted)
  scale <- mean(abs(diff(as.numeric(training))), na.rm = TRUE)
  data.frame(指标 = c("MAE", "RMSE", "MAPE", "sMAPE", "MASE"),
    数值 = c(mean(abs(error), na.rm = TRUE), sqrt(mean(error^2, na.rm = TRUE)),
      if (any(nonzero)) mean(abs(error[nonzero] / actual[nonzero])) * 100 else NA_real_,
      if (any(smape_denominator > 0)) mean(200 * abs(error[smape_denominator > 0]) / smape_denominator[smape_denominator > 0]) else NA_real_,
      if (is.finite(scale) && scale > 0) mean(abs(error), na.rm = TRUE) / scale else NA_real_),
    单位 = c("原序列单位", "原序列单位", "%", "%", "相对朴素一步预测"), check.names = FALSE)
}

forecast_arima_fit <- function(y, order, seasonal_order = c(0L, 0L, 0L), frequency = 1L) {
  frequency <- max(1L, as.integer(frequency)); series <- stats::ts(as.numeric(y), frequency = frequency)
  fit <- suppressWarnings(stats::arima(series, order = as.integer(order),
    seasonal = list(order = as.integer(seasonal_order), period = frequency), method = "ML"))
  list(type = "arima", fit = fit, order = as.integer(order), seasonal_order = as.integer(seasonal_order),
    frequency = frequency,
    label = paste0("ARIMA(", paste(order, collapse = ","), ")",
      if (any(seasonal_order != 0L)) paste0("(", paste(seasonal_order, collapse = ","), ")[", frequency, "]") else ""))
}

forecast_auto_arima <- function(y, max_order = 2L, seasonal = FALSE, frequency = 12L) {
  max_order <- max(1L, min(3L, as.integer(max_order)))
  d_candidates <- if (length(y) >= 12L && abs(stats::acf(y, plot = FALSE, lag.max = 1)$acf[2]) > .8) c(1L, 0L, 2L) else c(0L, 1L)
  pq <- expand.grid(p = 0:max_order, q = 0:max_order)
  pq <- pq[pq$p + pq$q <= max_order, , drop = FALSE]
  seasonal_orders <- list(c(0L, 0L, 0L))
  if (isTRUE(seasonal) && frequency >= 2L && length(y) >= 2L * frequency + 5L) {
    seasonal_orders <- list(c(0L, 0L, 0L), c(1L, 0L, 0L), c(0L, 0L, 1L),
      c(0L, 1L, 1L), c(1L, 1L, 0L))
  }
  best <- NULL; candidates <- list()
  for (d in d_candidates) for (i in seq_len(nrow(pq))) for (seasonal_order in seasonal_orders) {
    order <- c(pq$p[i], d, pq$q[i])
    candidate <- tryCatch(forecast_arima_fit(y, order, seasonal_order, frequency), error = function(e) NULL)
    if (is.null(candidate) || !is.finite(candidate$fit$aic)) next
    candidate$aic <- candidate$fit$aic
    candidates[[length(candidates) + 1L]] <- data.frame(模型 = candidate$label, AIC = candidate$aic)
    if (is.null(best) || candidate$aic < best$aic) best <- candidate
  }
  if (is.null(best)) stop("自动 ARIMA 未找到可稳定拟合的候选模型，请尝试手动阶数或 ETS。", call. = FALSE)
  best$candidates <- if (length(candidates)) {
    table <- do.call(rbind, candidates); utils::head(table[order(table$AIC), , drop = FALSE], 20L)
  } else data.frame()
  best
}

forecast_ets_candidates <- function(y, frequency = 1L, seasonal = FALSE) {
  series <- stats::ts(as.numeric(y), frequency = max(1L, as.integer(frequency)))
  specifications <- list(
    list(label = "ETS：简单指数平滑", beta = FALSE, gamma = FALSE, seasonal = "additive"),
    list(label = "ETS：Holt 线性趋势", beta = NULL, gamma = FALSE, seasonal = "additive")
  )
  if (isTRUE(seasonal) && frequency >= 2L && length(y) >= 2L * frequency + 2L) {
    specifications <- c(specifications, list(
      list(label = "ETS：加法季节", beta = NULL, gamma = NULL, seasonal = "additive"),
      if (all(y > 0)) list(label = "ETS：乘法季节", beta = NULL, gamma = NULL, seasonal = "multiplicative") else NULL
    ))
  }
  Filter(Negate(is.null), lapply(specifications, function(specification) {
    fit <- tryCatch(stats::HoltWinters(series, beta = specification$beta, gamma = specification$gamma,
      seasonal = specification$seasonal), error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    list(type = "ets", fit = fit, label = specification$label, specification = specification,
      frequency = max(1L, as.integer(frequency)))
  }))
}

forecast_predict_model <- function(model, horizon) {
  horizon <- as.integer(horizon)
  if (identical(model$type, "arima")) {
    prediction <- stats::predict(model$fit, n.ahead = horizon, se.fit = TRUE)
    center <- as.numeric(prediction$pred); se <- as.numeric(prediction$se)
    return(data.frame(预测值 = center,
      下限80 = center - stats::qnorm(.90) * se, 上限80 = center + stats::qnorm(.90) * se,
      下限95 = center - stats::qnorm(.975) * se, 上限95 = center + stats::qnorm(.975) * se,
      check.names = FALSE))
  }
  prediction80 <- stats::predict(model$fit, n.ahead = horizon, prediction.interval = TRUE, level = .80)
  prediction95 <- stats::predict(model$fit, n.ahead = horizon, prediction.interval = TRUE, level = .95)
  data.frame(预测值 = as.numeric(prediction95[, "fit"]),
    下限80 = as.numeric(prediction80[, "lwr"]), 上限80 = as.numeric(prediction80[, "upr"]),
    下限95 = as.numeric(prediction95[, "lwr"]), 上限95 = as.numeric(prediction95[, "upr"]), check.names = FALSE)
}

forecast_refit_model <- function(model, y) {
  if (identical(model$type, "arima")) return(forecast_arima_fit(y, model$order, model$seasonal_order, model$frequency))
  specification <- model$specification; series <- stats::ts(as.numeric(y), frequency = model$frequency)
  fit <- stats::HoltWinters(series, beta = specification$beta, gamma = specification$gamma,
    seasonal = specification$seasonal)
  list(type = "ets", fit = fit, label = model$label, specification = specification, frequency = model$frequency)
}

forecast_future_time <- function(time, horizon, step = "auto") {
  horizon <- as.integer(horizon); last <- tail(time, 1L)
  numeric_time <- as.numeric(time); typical <- if (length(time) > 1L) stats::median(diff(numeric_time)) else 1
  if (!is.finite(typical) || typical <= 0) typical <- 1
  if (inherits(time, "Date")) {
    resolved <- step
    if (identical(step, "auto")) resolved <- if (typical >= 360 && typical <= 370) "year" else if (typical >= 80 && typical <= 100) "quarter" else if (typical >= 27 && typical <= 32) "month" else "observed"
    if (resolved == "business_day") {
      output <- as.Date(character()); candidate <- last
      while (length(output) < horizon) {
        candidate <- candidate + 1L
        if (!as.POSIXlt(candidate)$wday %in% c(0L, 6L)) output <- c(output, candidate)
      }
      return(as.Date(output, origin = "1970-01-01"))
    }
    by <- switch(resolved, day = "day", week = "week", month = "month", quarter = "3 months", year = "year", NULL)
    if (!is.null(by)) return(seq.Date(last, by = by, length.out = horizon + 1L)[-1L])
    return(as.Date(as.numeric(last) + typical * seq_len(horizon), origin = "1970-01-01"))
  }
  if (inherits(time, "POSIXt")) return(as.POSIXct(as.numeric(last) + typical * seq_len(horizon), origin = "1970-01-01", tz = attr(time, "tzone") %||% "UTC"))
  as.numeric(last) + typical * seq_len(horizon)
}

fit_time_series_forecast <- function(time_series_result, method = "auto", horizon = 12L,
                                     validation_n = 20L, seasonal = FALSE, frequency = 12L,
                                     max_arima_order = 2L, manual_order = c(1L, 1L, 1L),
                                     manual_seasonal_order = c(0L, 0L, 0L), time_step = "auto") {
  values <- as.numeric(time_series_result$data$.value); n <- length(values)
  horizon <- as.integer(horizon); validation_n <- as.integer(validation_n); frequency <- as.integer(frequency)
  if (!method %in% c("auto", "arima", "ets", "manual_arima")) stop("请选择有效的预测模型。", call. = FALSE)
  if (length(horizon) != 1L || is.na(horizon) || horizon < 1L || horizon > 500L) stop("预测期数应在 1～500 之间。", call. = FALSE)
  if (length(validation_n) != 1L || is.na(validation_n) || validation_n < 3L || validation_n > 500L) stop("验证期数应在 3～500 之间。", call. = FALSE)
  if (n < max(30L, validation_n + 15L)) stop("用于预测的有效时间点过少；至少需要验证期数再加 15 个训练时间点，且总数不少于 30。", call. = FALSE)
  if (isTRUE(seasonal) && (frequency < 2L || frequency > 10000L)) stop("季节周期应在 2～10000 之间。", call. = FALSE)
  train <- values[seq_len(n - validation_n)]; validation <- tail(values, validation_n)
  candidates <- list(); comparison <- list()
  if (method %in% c("auto", "arima")) {
    arima_model <- forecast_auto_arima(train, max_arima_order, seasonal, frequency)
    prediction <- tryCatch(forecast_predict_model(arima_model, validation_n)$预测值, error = function(e) rep(NA_real_, validation_n))
    metric <- forecast_accuracy(validation, prediction, train)
    arima_model$validation_rmse <- metric$数值[metric$指标 == "RMSE"]
    candidates$arima <- arima_model
    comparison[[length(comparison) + 1L]] <- data.frame(模型 = arima_model$label, 验证RMSE = arima_model$validation_rmse)
  }
  if (method %in% c("auto", "ets")) {
    ets_models <- forecast_ets_candidates(train, frequency, seasonal)
    for (candidate in ets_models) {
      prediction <- tryCatch(forecast_predict_model(candidate, validation_n)$预测值, error = function(e) rep(NA_real_, validation_n))
      metric <- forecast_accuracy(validation, prediction, train)
      candidate$validation_rmse <- metric$数值[metric$指标 == "RMSE"]
      comparison[[length(comparison) + 1L]] <- data.frame(模型 = candidate$label, 验证RMSE = candidate$validation_rmse)
      if (is.null(candidates$ets) || candidate$validation_rmse < candidates$ets$validation_rmse) candidates$ets <- candidate
    }
  }
  if (identical(method, "manual_arima")) {
    order <- as.integer(manual_order); seasonal_order <- if (isTRUE(seasonal)) as.integer(manual_seasonal_order) else c(0L, 0L, 0L)
    if (length(order) != 3L || anyNA(order) || any(order < 0L) || any(order > 5L) ||
        length(seasonal_order) != 3L || anyNA(seasonal_order) || any(seasonal_order < 0L) || any(seasonal_order > 2L)) {
      stop("手动 ARIMA 阶数无效：p、d、q 应为 0～5，季节阶数应为 0～2。", call. = FALSE)
    }
    candidates$manual <- forecast_arima_fit(train, order, seasonal_order, if (isTRUE(seasonal)) frequency else 1L)
    prediction <- forecast_predict_model(candidates$manual, validation_n)$预测值
    candidates$manual$validation_rmse <- forecast_accuracy(validation, prediction, train)$数值[2]
    comparison[[1L]] <- data.frame(模型 = candidates$manual$label, 验证RMSE = candidates$manual$validation_rmse)
  }
  available <- Filter(function(x) !is.null(x) && is.finite(x$validation_rmse), candidates)
  if (!length(available)) stop("候选预测模型均未能产生有效验证预测，请调整季节周期、阶数或数据变换。", call. = FALSE)
  selected <- available[[which.min(vapply(available, function(x) x$validation_rmse, numeric(1)))]]
  validation_prediction <- forecast_predict_model(selected, validation_n)$预测值
  metrics <- forecast_accuracy(validation, validation_prediction, train)
  final_model <- forecast_refit_model(selected, values)
  future <- forecast_predict_model(final_model, horizon)
  future$时间 <- forecast_future_time(time_series_result$data$.time, horizon, time_step)
  future$期数 <- seq_len(horizon)
  future <- future[, c("期数", "时间", "预测值", "下限80", "上限80", "下限95", "上限95")]
  validation_table <- data.frame(时间 = tail(time_series_result$data$.time, validation_n), 实际值 = validation,
    验证预测 = validation_prediction, 残差 = validation - validation_prediction, check.names = FALSE)
  residual <- if (identical(final_model$type, "arima")) as.numeric(stats::residuals(final_model$fit)) else {
    fitted_values <- as.numeric(stats::fitted(final_model$fit)[, 1L]); tail(values, length(fitted_values)) - fitted_values
  }
  residual <- residual[is.finite(residual)]
  diagnostic_lag <- min(10L, max(1L, floor(length(residual) / 5L)))
  residual_test <- stats::Box.test(residual, lag = diagnostic_lag, type = "Ljung-Box")
  comparison_table <- do.call(rbind, comparison); comparison_table <- comparison_table[order(comparison_table$验证RMSE), , drop = FALSE]
  model_information <- if (identical(final_model$type, "arima")) list(
    aic = final_model$fit$aic, bic = stats::BIC(final_model$fit), log_likelihood = as.numeric(stats::logLik(final_model$fit)),
    coefficients = stats::coef(final_model$fit)) else list(
    smoothing_parameters = final_model$fit$alpha %||% NA_real_, beta = final_model$fit$beta %||% NA_real_,
    gamma = final_model$fit$gamma %||% NA_real_, in_sample_sse = final_model$fit$SSE)
  parameter_table <- if (identical(final_model$type, "arima")) {
    data.frame(参数 = names(stats::coef(final_model$fit)), 估计值 = as.numeric(stats::coef(final_model$fit)), check.names = FALSE)
  } else data.frame(参数 = c("alpha", "beta", "gamma", "样本内 SSE"),
    估计值 = c(final_model$fit$alpha %||% NA_real_, final_model$fit$beta %||% NA_real_,
      final_model$fit$gamma %||% NA_real_, final_model$fit$SSE), check.names = FALSE)
  report <- c("时间序列预测报告",
    paste0("预测对象：", time_series_result$value_name, " · ", time_series_result$transform_label,
      "；模型：", final_model$label, "；未来预测 ", horizon, " 期。"),
    paste0("模型使用最后 ", validation_n, " 期作时间顺序验证，其余 ", length(train), " 期用于候选模型拟合；选定模型后使用全部 ", n, " 期重新拟合。"),
    paste0("验证集：MAE = ", ts_number(metrics$数值[1]), "；RMSE = ", ts_number(metrics$数值[2]),
      "；MAPE = ", ts_number(metrics$数值[3]), "%；sMAPE = ", ts_number(metrics$数值[4]),
      "%；MASE = ", ts_number(metrics$数值[5]), "。"),
    paste0("最终模型残差 Ljung-Box（滞后 ", diagnostic_lag, "）p ", ts_pvalue(residual_test$p.value),
      "。显著结果提示残差仍含可预测的线性相关结构。"),
    "预测区间同时给出 80% 和 95% 范围。区间反映模型及其误差假设下未来观测的不确定性；ARIMA 区间未计入参数估计的不确定性。",
    "验证集同时参与了自动候选模型比较，因此验证指标可能略偏乐观。重要应用应进一步使用滚动时间窗验证，并在新数据到达后持续检查误差。",
    if (time_series_result$transform %in% c("percent", "log_return", "difference"))
      "当前预测对应变换后的序列，并不是自动还原后的价格或水平值。" else
      "当前预测直接对应所选数值字段的水平值。趋势变化、结构突变和异常事件可能使未来偏离历史模式。"
  )
  list(model = final_model, selected_label = final_model$label, method = method, horizon = horizon,
    validation_n = validation_n, seasonal = isTRUE(seasonal), frequency = frequency,
    metrics = metrics, comparison = comparison_table, validation = validation_table, future = future,
    model_information = model_information, parameters = parameter_table,
    residual_diagnostic = data.frame(检验 = "残差 Ljung-Box", 滞后阶数 = diagnostic_lag,
      统计量 = unname(residual_test$statistic), p值 = residual_test$p.value, check.names = FALSE),
    residual_distribution = ai_numeric_distribution(residual), report = paste(report, collapse = "\n\n"))
}

build_forecast_plot <- function(time_series_result) {
  forecast <- time_series_result$forecast
  if (is.null(forecast)) stop("请先启用并运行预测。", call. = FALSE)
  history <- time_series_result$data
  future <- forecast$future; validation <- forecast$validation
  ggplot2::ggplot() +
    ggplot2::geom_line(data = history, ggplot2::aes(.time, .value, colour = "历史数据"), linewidth = .65) +
    ggplot2::geom_ribbon(data = future, ggplot2::aes(x = 时间, ymin = 下限95, ymax = 上限95, fill = "95% 预测区间"), alpha = .16) +
    ggplot2::geom_ribbon(data = future, ggplot2::aes(x = 时间, ymin = 下限80, ymax = 上限80, fill = "80% 预测区间"), alpha = .26) +
    ggplot2::geom_line(data = future, ggplot2::aes(时间, 预测值, colour = "未来预测"), linewidth = .95) +
    ggplot2::geom_line(data = validation, ggplot2::aes(时间, 验证预测, colour = "验证预测"), linewidth = .75, linetype = "dashed") +
    ggplot2::scale_colour_manual(values = c("历史数据" = "#2563eb", "验证预测" = "#d97706", "未来预测" = "#dc2626"), name = NULL) +
    ggplot2::scale_fill_manual(values = c("95% 预测区间" = "#93c5fd", "80% 预测区间" = "#60a5fa"), name = NULL) +
    ts_plot_theme() + ggplot2::labs(title = paste0(forecast$selected_label, " · 未来 ", forecast$horizon, " 期预测"),
      subtitle = "虚线为留出验证期预测；阴影为未来预测区间", x = time_series_result$time_name, y = time_series_result$transform_label)
}

ts_plot_theme <- function() {
  family <- switch(Sys.info()[["sysname"]], Darwin = "Arial Unicode MS", Windows = "Microsoft YaHei", "sans")
  ggplot2::theme_minimal(base_size = 13, base_family = family) +
    ggplot2::theme(plot.title.position = "plot", legend.position = "bottom")
}

build_time_series_plot <- function(result, moving_average = TRUE, window = 5, trend = "none") {
  d <- result$data
  window <- as.integer(window)
  p <- ggplot2::ggplot(d, ggplot2::aes(.time, .value)) +
    ggplot2::geom_line(ggplot2::aes(colour = "序列"), linewidth = 0.65, alpha = 0.85) +
    ts_plot_theme()
  colours <- c("序列" = "#2563eb", "移动平均" = "#d97706", "线性趋势" = "#dc2626", "LOESS 趋势" = "#059669")
  if (isTRUE(moving_average)) {
    if (is.na(window) || window < 2 || window > 200) stop("移动平均窗口应在 2～200 之间。", call. = FALSE)
    if (window > nrow(d)) stop("移动平均窗口不能大于有效时间点数量。", call. = FALSE)
    d$.moving <- as.numeric(stats::filter(d$.value, rep(1 / window, window), sides = 1))
    p <- p + ggplot2::geom_line(data = d, ggplot2::aes(.time, .moving, colour = "移动平均"), linewidth = 1, na.rm = TRUE)
  }
  if (identical(trend, "linear")) {
    d$.trend <- stats::fitted(stats::lm(d$.value ~ seq_len(nrow(d))))
    p <- p + ggplot2::geom_line(data = d, ggplot2::aes(.time, .trend, colour = "线性趋势"), linewidth = 1)
  } else if (identical(trend, "loess")) {
    if (nrow(d) < 10) stop("LOESS 趋势至少需要 10 个有效时间点。", call. = FALSE)
    fit <- stats::loess(d$.value ~ seq_len(nrow(d)), span = 0.3)
    d$.trend <- stats::predict(fit)
    p <- p + ggplot2::geom_line(data = d, ggplot2::aes(.time, .trend, colour = "LOESS 趋势"), linewidth = 1)
  } else if (!identical(trend, "none")) stop("请选择有效趋势线。", call. = FALSE)
  p + ggplot2::scale_colour_manual(values = colours, name = NULL) +
    ggplot2::labs(title = paste0(result$value_name, " · ", result$transform_label),
      subtitle = "按时间升序；移动平均为向后滚动窗口", x = result$time_name, y = result$transform_label)
}

build_time_series_acf_plot <- function(result, max_lag = 30) {
  n <- nrow(result$data)
  max_lag <- as.integer(max_lag)
  if (is.na(max_lag) || max_lag < 1) stop("最大滞后阶数至少为 1。", call. = FALSE)
  max_lag <- min(max_lag, n - 1)
  if (stats::sd(result$data$.value) == 0) stop("序列没有变化，无法计算自相关。", call. = FALSE)
  values <- stats::acf(result$data$.value, plot = FALSE, lag.max = max_lag)$acf
  d <- data.frame(.lag = seq.int(0, length(values) - 1), .acf = as.numeric(values))
  bound <- stats::qnorm(0.975) / sqrt(n)
  ggplot2::ggplot(d, ggplot2::aes(.lag, .acf)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#64748b") +
    ggplot2::geom_hline(yintercept = c(-bound, bound), linetype = "dashed", colour = "#dc2626") +
    ggplot2::geom_segment(ggplot2::aes(xend = .lag, y = 0, yend = .acf), colour = "#2563eb", linewidth = 0.65) +
    ggplot2::geom_point(colour = "#2563eb", size = 2) + ts_plot_theme() +
    ggplot2::labs(title = "自相关函数（ACF）", subtitle = "红色虚线为白噪声近似 95% 界限；未调整多重比较",
      x = "滞后阶数（相邻观测）", y = "自相关系数")
}

build_time_series_decomposition_plot <- function(result, frequency) {
  frequency <- as.integer(frequency)
  if (is.na(frequency) || frequency < 2 || frequency > 10000) stop("季节周期应为 2～10000 的整数。", call. = FALSE)
  if (nrow(result$data) < 2 * frequency + 1) stop("STL 分解至少需要两个完整周期以上的数据，请降低周期或增加数据。", call. = FALSE)
  fit <- stats::stl(stats::ts(result$data$.value, frequency = frequency), s.window = "periodic", robust = TRUE)
  components <- fit$time.series
  labels <- c("原始序列", "趋势", "季节项", "余项")
  d <- data.frame(.time = rep(result$data$.time, 4),
    .component = factor(rep(labels, each = nrow(result$data)), levels = labels),
    .value = c(result$data$.value, components[, "trend"], components[, "seasonal"], components[, "remainder"]))
  ggplot2::ggplot(d, ggplot2::aes(.time, .value)) + ggplot2::geom_line(colour = "#2563eb", linewidth = 0.55) +
    ggplot2::facet_grid(.component ~ ., scales = "free_y") + ts_plot_theme() +
    ggplot2::theme(legend.position = "none", strip.text.y = ggplot2::element_text(angle = 0)) +
    ggplot2::labs(title = paste0("STL 季节分解（周期 = ", frequency, "）"),
      subtitle = "周期表示每个完整季节循环包含的观测数", x = result$time_name, y = NULL)
}

timeseries_ui <- function(id) {
  ns <- NS(id)
  tagList(
    algorithm_title_ui(ns, "时间序列分析"),
    p("选择时间字段和数值字段。程序会按时间排序，并将重复时间的数值按均值合并。"),
    conditionalPanel(sprintf("input['%s'] %% 2 === 1", ns("tutorial_toggle")),
      algorithm_tutorial_ui(ns("tutorial"), "timeseries")),
    tags$details(class = "ts-source-card",
      tags$summary(icon("cloud-arrow-down"), tags$span("在线下载金融与 FRED 数据")),
      tags$div(class = "ts-source-body",
        p(class = "ts-source-hint", "Yahoo Finance 适合股票、ETF 和指数；FRED 适合利率、GDP、CPI 等经济序列。下载后会加入顶部数据集列表。"),
        selectInput(ns("download_source"), "数据源", c("Yahoo Finance（quantmod）" = "yahoo", "FRED 经济数据" = "fred")),
        conditionalPanel(paste0("input['", ns("download_source"), "'] === 'yahoo'"),
          textInput(ns("yahoo_symbol"), "Yahoo 代码", "AAPL", placeholder = "例如 AAPL、0700.HK、^GSPC")),
        conditionalPanel(paste0("input['", ns("download_source"), "'] === 'fred'"),
          textInput(ns("fred_series"), "FRED 序列 ID", "GDP", placeholder = "例如 GDP、CPIAUCSL、DGS10"),
          passwordInput(ns("fred_api_key"), "FRED API Key", placeholder = "32 位小写字母或数字"),
          p(class = "ts-source-hint", "FRED 要求每位用户使用自己的 API Key。Key 只保存在当前会话内，不会写入文件。"),
          p(class = "ts-source-hint", "使用 FRED 下载即表示同意 ",
            tags$a("FRED API 使用条款", href = "https://fred.stlouisfed.org/docs/api/terms_of_use.html", target = "_blank", rel = "noopener noreferrer"), "。")),
        dateRangeInput(ns("download_dates"), "日期范围", start = Sys.Date() - 365 * 5, end = Sys.Date(),
          min = as.Date("1900-01-01"), max = Sys.Date(), format = "yyyy-mm-dd", separator = " 至 "),
        tags$div(class = "ts-source-actions",
          actionButton(ns("download_data"), "下载并使用", icon = icon("download"), class = "btn-primary"),
          downloadButton(ns("download_csv"), "下载原始 CSV")),
        tags$div(class = "ts-source-status", textOutput(ns("download_status")))
      )
    ),
    tags$div(class = "ts-active-source", textOutput(ns("active_source"))),
    fluidRow(column(5, selectInput(ns("time"), "时间字段", NULL)),
      column(5, selectInput(ns("value"), "数值字段", NULL)),
      column(2, selectInput(ns("transform"), "序列变换", c("原始值" = "level", "一阶差分" = "difference", "百分比变化" = "percent", "对数收益率" = "log_return")))),
    fluidRow(column(3, checkboxInput(ns("moving_average"), "显示移动平均", TRUE)),
      column(3, numericInput(ns("window"), "移动平均窗口", 5, min = 2, max = 200)),
      column(3, selectInput(ns("trend"), "趋势线", c("不显示" = "none", "线性趋势" = "linear", "LOESS 趋势" = "loess"))),
      column(3, numericInput(ns("max_lag"), "ACF 最大滞后", 30, min = 1, max = 500))),
    fluidRow(column(3, checkboxInput(ns("decompose"), "生成 STL 季节分解", FALSE)),
      column(3, conditionalPanel(paste0("input['", ns("decompose"), "']"),
        numericInput(ns("frequency"), "季节周期（观测数）", 12, min = 2, max = 10000)))),
    tags$details(class = "ts-source-card",
      tags$summary(icon("wave-square"), tags$span("GARCH 条件波动率")),
      tags$div(class = "ts-source-body",
        checkboxInput(ns("garch_enabled"), "同时拟合 GARCH 模型", FALSE),
        conditionalPanel(paste0("input['", ns("garch_enabled"), "']"),
          p(class = "ts-source-hint", "GARCH 适合研究波动聚集。金融价格通常应先在上方选择“百分比变化”或“对数收益率”。"),
          fluidRow(
            column(3, numericInput(ns("garch_p"), "GARCH 阶数 p（β）", 1, min = 0, max = 5, step = 1)),
            column(3, numericInput(ns("garch_q"), "ARCH 阶数 q（α）", 1, min = 1, max = 5, step = 1)),
            column(3, numericInput(ns("garch_diagnostic_lag"), "诊断滞后阶数", 10, min = 1, max = 100, step = 1)),
            column(3, checkboxInput(ns("garch_demean"), "拟合前减去均值", TRUE))
          ),
          helpText("模型采用条件正态准最大似然估计。p 是条件方差滞后项阶数，q 是历史平方冲击阶数；常用起点为 GARCH(1,1)。")
        )
      )
    ),
    tags$details(class = "ts-source-card",
      tags$summary(icon("chart-line"), tags$span("时间序列预测")),
      tags$div(class = "ts-source-body",
        checkboxInput(ns("forecast_enabled"), "生成未来预测", FALSE),
        conditionalPanel(paste0("input['", ns("forecast_enabled"), "']"),
          fluidRow(
            column(4, selectInput(ns("forecast_method"), "预测方法", c(
              "自动比较 ARIMA 与 ETS（推荐）" = "auto", "自动 ARIMA" = "arima",
              "ETS 指数平滑" = "ets", "手动 ARIMA" = "manual_arima"))),
            column(2, numericInput(ns("forecast_horizon"), "未来期数", 12, min = 1, max = 500, step = 1)),
            column(2, numericInput(ns("forecast_validation"), "留出验证期数", 20, min = 3, max = 500, step = 1)),
            column(4, selectInput(ns("forecast_time_step"), "未来时间间隔", c(
              "自动判断" = "auto", "沿用典型观测间隔" = "observed", "工作日" = "business_day",
              "日" = "day", "周" = "week", "月" = "month", "季度" = "quarter", "年" = "year")))
          ),
          fluidRow(
            column(3, checkboxInput(ns("forecast_seasonal"), "包含季节结构", FALSE)),
            column(3, conditionalPanel(paste0("input['", ns("forecast_seasonal"), "']"),
              numericInput(ns("forecast_frequency"), "季节周期", 12, min = 2, max = 10000, step = 1))),
            column(3, conditionalPanel(paste0("input['", ns("forecast_method"), "'] === 'auto' || input['", ns("forecast_method"), "'] === 'arima'"),
              numericInput(ns("forecast_max_order"), "自动 AR/MA 最大总阶数", 2, min = 1, max = 3, step = 1)))
          ),
          conditionalPanel(paste0("input['", ns("forecast_method"), "'] === 'manual_arima'"),
            h4("手动 ARIMA 阶数"),
            fluidRow(
              column(2, numericInput(ns("forecast_p"), "p", 1, min = 0, max = 5)),
              column(2, numericInput(ns("forecast_d"), "d", 1, min = 0, max = 5)),
              column(2, numericInput(ns("forecast_q"), "q", 1, min = 0, max = 5)),
              column(2, numericInput(ns("forecast_P"), "季节 P", 0, min = 0, max = 2)),
              column(2, numericInput(ns("forecast_D"), "季节 D", 0, min = 0, max = 2)),
              column(2, numericInput(ns("forecast_Q"), "季节 Q", 0, min = 0, max = 2))
            )),
          p(class = "ts-source-hint", "模型按时间顺序保留最后若干期验证，并报告 MAE、RMSE、MAPE、sMAPE 和 MASE。预测图显示验证预测、未来点预测以及 80%/95% 预测区间。")
        )
      )
    ),
    ai_parameter_ui(ns("ai_params")),
    helpText("示例：月度数据周期常为 12，季度数据为 4；交易日周周期可尝试 5。周期必须结合数据含义确定。"),
    actionButton(ns("run"), "运行时间序列分析", class = "btn-primary"),
    tags$div(style = "margin:12px 0", textOutput(ns("status"))),
    tabsetPanel(
      tabPanel("分析摘要", tags$div(style = "white-space:pre-wrap;line-height:1.9", textOutput(ns("report")))),
      tabPanel("AI 增强解读", ai_report_ui(ns("ai_report"))),
      tabPanel("序列图（ggplot2）", ggplot_editor_ui(ns("series_editor"), height = "500px")),
      tabPanel("自相关图（ggplot2）", ggplot_editor_ui(ns("acf_editor"), height = "500px")),
      tabPanel("季节分解（ggplot2）", ggplot_editor_ui(ns("decomposition_editor"), height = "760px")),
      tabPanel("GARCH 波动率",
        tags$div(style = "white-space:pre-wrap;line-height:1.85", textOutput(ns("garch_report"))),
        DT::DTOutput(ns("garch_coefficients")),
        ggplot_editor_ui(ns("garch_volatility_editor"), height = "520px")),
      tabPanel("GARCH 诊断", DT::DTOutput(ns("garch_diagnostics")),
        ggplot_editor_ui(ns("garch_diagnostic_editor"), height = "620px")),
      tabPanel("未来预测",
        tags$div(style = "white-space:pre-wrap;line-height:1.85", textOutput(ns("forecast_report"))),
        fluidRow(column(6, h4("验证指标"), DT::DTOutput(ns("forecast_metrics"))),
          column(6, h4("候选模型比较"), DT::DTOutput(ns("forecast_comparison")))),
        h4("最终模型参数"), DT::DTOutput(ns("forecast_parameters")),
        ggplot_editor_ui(ns("forecast_editor"), height = "600px")),
      tabPanel("预测数据", DT::DTOutput(ns("forecast_table")), downloadButton(ns("download_forecast"), "下载预测 CSV")),
      tabPanel("分析数据", DT::DTOutput(ns("table")))
    )
  )
}

timeseries_server <- function(id, data, directory = reactive(getwd()), ai_config = reactive(list()), add_dataset = NULL) {
  moduleServer(id, function(input, output, session) {
    algorithm_tutorial_server("tutorial", "timeseries")
    algorithm_tutorial_toggle_server(input, session)
    result <- reactiveVal(NULL)
    status <- reactiveVal("选择时间和数值字段后，点击“运行时间序列分析”。")
    downloaded_data <- reactiveVal(NULL)
    download_cache <- reactiveVal(NULL)
    downloaded_label <- reactiveVal(NULL)
    download_status <- reactiveVal("在线数据会作为新数据集加入顶部列表，不会覆盖已有数据。")
    analysis_data <- reactive({
      if (!is.null(downloaded_data())) downloaded_data() else data()
    })
    output$download_status <- renderText(download_status())
    output$active_source <- renderText({
      if (!is.null(downloaded_data())) paste0("当前数据源：", downloaded_label(), "（", nrow(downloaded_data()), " 行）")
      else if (!is.null(data())) paste0("当前数据源：已导入数据（", nrow(data()), " 行）")
      else "当前数据源：尚未导入或下载数据"
    })
    observeEvent(input$download_data, {
      download_status("正在下载，请稍候……")
      tryCatch({
        req(length(input$download_dates) == 2L)
        source <- input$download_source
        value <- withProgress(message = "正在获取在线数据……", value = 0.5, {
          if (identical(source, "fred")) {
            download_fred_data(input$fred_series, input$download_dates[1], input$download_dates[2], input$fred_api_key)
          } else download_yahoo_data(input$yahoo_symbol, input$download_dates[1], input$download_dates[2])
        })
        if (!nrow(value)) stop("所选日期范围没有可用数据。", call. = FALSE)
        symbol <- if (identical(source, "fred")) market_symbol(input$fred_series, "FRED 序列 ID") else market_symbol(input$yahoo_symbol, "Yahoo 代码")
        label <- online_dataset_name(source, symbol, input$download_dates[1], input$download_dates[2])
        download_cache(value); downloaded_label(label)
        if (is.function(add_dataset)) {
          label <- register_online_dataset(add_dataset, value, label)
          downloaded_data(NULL)
          downloaded_label(label)
          download_status(paste0("下载完成：已将“", label, "”加入顶部数据集并自动切换，", nrow(value), " 行 × ", ncol(value), " 列。"))
        } else {
          downloaded_data(value)
          download_status(paste0("下载完成：", label, "，", nrow(value), " 行 × ", ncol(value), " 列。已切换为当前分析数据。"))
        }
      }, error = function(e) {
        key <- if (is.null(input$fred_api_key)) "" else input$fred_api_key
        message <- conditionMessage(e)
        if (nzchar(key)) message <- gsub(key, "***", message, fixed = TRUE)
        if (inherits(e, "shiny.silent.error")) message <- "请完整填写代码和日期。"
        download_status(paste0("下载失败：", message)); showNotification(message, type = "error", duration = 10)
      })
    })
    output$download_csv <- downloadHandler(
      filename = function() paste0("easyr-", if (is.null(downloaded_label())) "time-series" else gsub("[^A-Za-z0-9_-]+", "-", downloaded_label()), ".csv"),
      content = function(file) {
        req(download_cache())
        con <- file(file, open = "wb"); on.exit(close(con))
        writeBin(charToRaw("\ufeff"), con)
        lines <- capture.output(write.csv(download_cache(), row.names = FALSE, na = ""))
        writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n"))), con)
      }
    )
    ai_params <- ai_parameter_server("ai_params", ai_config, analysis_data, "时间序列分析",
      reactive(list(transform = input$transform, moving_average = input$moving_average, window = input$window,
        trend = input$trend, max_lag = input$max_lag, decompose = input$decompose, frequency = input$frequency,
        garch_enabled = input$garch_enabled, garch_p = input$garch_p, garch_q = input$garch_q,
        garch_diagnostic_lag = input$garch_diagnostic_lag, garch_demean = input$garch_demean,
        forecast_enabled = input$forecast_enabled, forecast_method = input$forecast_method,
        forecast_horizon = input$forecast_horizon, forecast_validation = input$forecast_validation,
        forecast_seasonal = input$forecast_seasonal, forecast_frequency = input$forecast_frequency)),
      list(transform = c("level", "difference", "percent", "log_return"), moving_average = "true 或 false",
        window = "2 到 200", trend = c("none", "linear", "loess"), max_lag = "1 到 500",
        decompose = "true 或 false", frequency = "2 到 10000；必须符合观测频率",
        garch_enabled = "true 或 false", garch_p = "0 到 5", garch_q = "1 到 5",
        garch_diagnostic_lag = "1 到 100", garch_demean = "true 或 false",
        forecast_enabled = "true 或 false", forecast_method = c("auto", "arima", "ets", "manual_arima"),
        forecast_horizon = "1 到 500", forecast_validation = "3 到 500",
        forecast_seasonal = "true 或 false", forecast_frequency = "2 到 10000"))
    observeEvent(ai_params$apply(), {
      p <- ai_params$proposal()$parameters; if (is.null(p)) return()
      if (!is.null(p$transform) && p$transform %in% c("level", "difference", "percent", "log_return")) updateSelectInput(session, "transform", selected = p$transform)
      if (!is.null(p$moving_average)) updateCheckboxInput(session, "moving_average", value = isTRUE(as.logical(p$moving_average)))
      if (!is.null(p$window)) updateNumericInput(session, "window", value = max(2L, min(200L, as.integer(p$window))))
      if (!is.null(p$trend) && p$trend %in% c("none", "linear", "loess")) updateSelectInput(session, "trend", selected = p$trend)
      if (!is.null(p$max_lag)) updateNumericInput(session, "max_lag", value = max(1L, min(500L, as.integer(p$max_lag))))
      if (!is.null(p$decompose)) updateCheckboxInput(session, "decompose", value = isTRUE(as.logical(p$decompose)))
      if (!is.null(p$frequency)) updateNumericInput(session, "frequency", value = max(2L, min(10000L, as.integer(p$frequency))))
      if (!is.null(p$garch_enabled)) updateCheckboxInput(session, "garch_enabled", value = isTRUE(as.logical(p$garch_enabled)))
      if (!is.null(p$garch_p)) updateNumericInput(session, "garch_p", value = max(0L, min(5L, as.integer(p$garch_p))))
      if (!is.null(p$garch_q)) updateNumericInput(session, "garch_q", value = max(1L, min(5L, as.integer(p$garch_q))))
      if (!is.null(p$garch_diagnostic_lag)) updateNumericInput(session, "garch_diagnostic_lag", value = max(1L, min(100L, as.integer(p$garch_diagnostic_lag))))
      if (!is.null(p$garch_demean)) updateCheckboxInput(session, "garch_demean", value = isTRUE(as.logical(p$garch_demean)))
      if (!is.null(p$forecast_enabled)) updateCheckboxInput(session, "forecast_enabled", value = isTRUE(as.logical(p$forecast_enabled)))
      if (!is.null(p$forecast_method) && p$forecast_method %in% c("auto", "arima", "ets", "manual_arima")) updateSelectInput(session, "forecast_method", selected = p$forecast_method)
      if (!is.null(p$forecast_horizon)) updateNumericInput(session, "forecast_horizon", value = max(1L, min(500L, as.integer(p$forecast_horizon))))
      if (!is.null(p$forecast_validation)) updateNumericInput(session, "forecast_validation", value = max(3L, min(500L, as.integer(p$forecast_validation))))
      if (!is.null(p$forecast_seasonal)) updateCheckboxInput(session, "forecast_seasonal", value = isTRUE(as.logical(p$forecast_seasonal)))
      if (!is.null(p$forecast_frequency)) updateNumericInput(session, "forecast_frequency", value = max(2L, min(10000L, as.integer(p$forecast_frequency))))
      showNotification("AI 建议参数已填入；请检查后点击运行。", type = "message")
    }, ignoreInit = TRUE)
    observeEvent(analysis_data(), {
      d <- analysis_data()
      choices <- setNames(as.character(seq_along(d)), paste0(seq_along(d), ". ", names(d)))
      preferred <- which(vapply(d, function(x) inherits(x, c("Date", "POSIXt")), logical(1)))
      fallback <- if (length(preferred)) as.character(preferred[1]) else unname(head(choices, 1))
      selected <- if (length(input$time) == 1 && input$time %in% unname(choices)) input$time else fallback
      updateSelectInput(session, "time", choices = choices, selected = selected)
    })
    observeEvent(list(analysis_data(), input$time), {
      d <- analysis_data()
      numeric <- setdiff(which(vapply(d, is.numeric, logical(1))), as.integer(input$time))
      labels <- if (length(numeric)) paste0(numeric, ". ", names(d)[numeric]) else character()
      choices <- setNames(as.character(numeric), labels)
      preferred <- if (!is.null(downloaded_data()) && "复权收盘价" %in% names(d)) as.character(match("复权收盘价", names(d))) else NULL
      selected <- if (length(preferred) && preferred %in% unname(choices)) preferred else if (length(input$value) == 1 && input$value %in% unname(choices)) input$value else unname(head(choices, 1))
      updateSelectInput(session, "value", choices = choices, selected = selected)
    })
    observe({
      result(NULL); status("数据或字段选择已更新，请点击“运行时间序列分析”。")
      analysis_data(); input$time; input$value; input$transform; input$garch_enabled
      input$garch_p; input$garch_q; input$garch_diagnostic_lag; input$garch_demean
      input$forecast_enabled; input$forecast_method; input$forecast_horizon; input$forecast_validation
      input$forecast_seasonal; input$forecast_frequency; input$forecast_max_order; input$forecast_time_step
      input$forecast_p; input$forecast_d; input$forecast_q; input$forecast_P; input$forecast_D; input$forecast_Q
    }, priority = 100)
    observeEvent(input$run, {
      result(NULL)
      tryCatch({
        prepared <- prepare_time_series(analysis_data(), input$time, input$value, input$transform)
        if (isTRUE(input$garch_enabled)) {
          prepared$garch <- fit_garch_analysis(prepared, input$garch_p, input$garch_q,
            isTRUE(input$garch_demean), input$garch_diagnostic_lag)
        }
        if (isTRUE(input$forecast_enabled)) {
          prepared$forecast <- fit_time_series_forecast(prepared,
            method = input$forecast_method %||% "auto", horizon = input$forecast_horizon,
            validation_n = input$forecast_validation, seasonal = isTRUE(input$forecast_seasonal),
            frequency = input$forecast_frequency %||% 12L, max_arima_order = input$forecast_max_order %||% 2L,
            manual_order = c(input$forecast_p %||% 1L, input$forecast_d %||% 1L, input$forecast_q %||% 1L),
            manual_seasonal_order = c(input$forecast_P %||% 0L, input$forecast_D %||% 0L, input$forecast_Q %||% 0L),
            time_step = input$forecast_time_step %||% "auto")
        }
        result(prepared)
        additions <- c(if (!is.null(prepared$garch)) paste0("GARCH(", prepared$garch$order[["garch"]], ",", prepared$garch$order[["arch"]], ")"),
          if (!is.null(prepared$forecast)) paste0(prepared$forecast$selected_label, " 未来 ", prepared$forecast$horizon, " 期预测"))
        status(sprintf("分析完成：使用 %d 个时间点%s。", nrow(prepared$data),
          if (!length(additions)) "" else paste0("；已生成 ", paste(additions, collapse = " 和 "))))
      }, error = function(e) {
        message <- if (inherits(e, "shiny.silent.error")) "请先导入数据并选择字段。" else conditionMessage(e)
        status(paste("未能分析：", message)); showNotification(message, type = "error", duration = 10)
      })
    })
    series_plot <- reactive({
      req(result())
      tryCatch(build_time_series_plot(result(), isTRUE(input$moving_average), input$window, input$trend),
        error = function(e) validate(need(FALSE, conditionMessage(e))))
    })
    output$status <- renderText(status())
    output$report <- renderText({ req(result()); result()$report })
    ai_report_server("ai_report", ai_config, "时间序列分析",
      reactive({
        if (is.null(result())) return("")
        paste(c(result()$report,
          if (!is.null(result()$garch)) result()$garch$report,
          if (!is.null(result()$forecast)) result()$forecast$report), collapse = "\n\n---\n\n")
      }),
      reactive(if (is.null(result())) NULL else ai_model_context("时间序列分析", result())), analysis_data)
    acf_plot <- reactive({
      req(result())
      tryCatch(build_time_series_acf_plot(result(), input$max_lag),
        error = function(e) validate(need(FALSE, conditionMessage(e))))
    })
    decomposition_plot <- reactive({
      req(result()); validate(need(isTRUE(input$decompose), "勾选“生成 STL 季节分解”后查看。"))
      tryCatch(build_time_series_decomposition_plot(result(), input$frequency),
        error = function(e) validate(need(FALSE, conditionMessage(e))))
    })
    garch_volatility_plot <- reactive({
      req(result()); validate(need(!is.null(result()$garch), "请勾选“同时拟合 GARCH 模型”并重新运行。"))
      build_garch_volatility_plot(result())
    })
    garch_diagnostic_plot <- reactive({
      req(result()); validate(need(!is.null(result()$garch), "请勾选“同时拟合 GARCH 模型”并重新运行。"))
      build_garch_diagnostic_plot(result(), input$max_lag)
    })
    forecast_plot <- reactive({
      req(result()); validate(need(!is.null(result()$forecast), "请勾选“生成未来预测”并重新运行。"))
      build_forecast_plot(result())
    })
    ggplot_editor_server("series_editor", series_plot, directory, "easyr-time-series")
    ggplot_editor_server("acf_editor", acf_plot, directory, "easyr-time-series-acf")
    ggplot_editor_server("decomposition_editor", decomposition_plot, directory, "easyr-time-series-stl")
    ggplot_editor_server("garch_volatility_editor", garch_volatility_plot, directory, "easyr-garch-volatility")
    ggplot_editor_server("garch_diagnostic_editor", garch_diagnostic_plot, directory, "easyr-garch-diagnostics")
    ggplot_editor_server("forecast_editor", forecast_plot, directory, "easyr-time-series-forecast")
    output$garch_report <- renderText({
      req(result()); validate(need(!is.null(result()$garch), "请勾选“同时拟合 GARCH 模型”并重新运行。"))
      result()$garch$report
    })
    output$garch_coefficients <- DT::renderDT({
      req(result()); validate(need(!is.null(result()$garch), "请先运行 GARCH 分析。"))
      DT::formatSignif(DT::datatable(result()$garch$coefficients, rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)), columns = 3:6, digits = 5)
    })
    output$garch_diagnostics <- DT::renderDT({
      req(result()); validate(need(!is.null(result()$garch), "请先运行 GARCH 分析。"))
      DT::formatSignif(DT::datatable(result()$garch$diagnostics, rownames = FALSE,
        options = list(dom = "t", scrollX = TRUE)), columns = 3:4, digits = 5)
    })
    output$forecast_report <- renderText({
      req(result()); validate(need(!is.null(result()$forecast), "请勾选“生成未来预测”并重新运行。"))
      result()$forecast$report
    })
    output$forecast_metrics <- DT::renderDT({
      req(result()); validate(need(!is.null(result()$forecast), "请先运行预测。"))
      DT::formatSignif(DT::datatable(result()$forecast$metrics, rownames = FALSE,
        options = list(dom = "t", scrollX = TRUE)), columns = "数值", digits = 5)
    })
    output$forecast_comparison <- DT::renderDT({
      req(result()); validate(need(!is.null(result()$forecast), "请先运行预测。"))
      DT::formatSignif(DT::datatable(result()$forecast$comparison, rownames = FALSE,
        options = list(pageLength = 8, scrollX = TRUE)), columns = "验证RMSE", digits = 5)
    })
    output$forecast_parameters <- DT::renderDT({
      req(result()); validate(need(!is.null(result()$forecast), "请先运行预测。"))
      DT::formatSignif(DT::datatable(result()$forecast$parameters, rownames = FALSE,
        options = list(dom = "t", scrollX = TRUE)), columns = "估计值", digits = 6)
    })
    output$forecast_table <- DT::renderDT({
      req(result()); validate(need(!is.null(result()$forecast), "请先运行预测。"))
      DT::formatSignif(DT::datatable(result()$forecast$future, rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE)), columns = c("预测值", "下限80", "上限80", "下限95", "上限95"), digits = 6)
    })
    output$download_forecast <- downloadHandler(
      filename = function() paste0("easyr-time-series-forecast-", Sys.Date(), ".csv"),
      content = function(file) { req(result(), result()$forecast); write.csv(result()$forecast$future, file, row.names = FALSE, fileEncoding = "UTF-8") }
    )
    output$table <- DT::renderDT({
      req(result())
      display <- result()$data; names(display) <- c(result()$time_name, result()$transform_label)
      DT::datatable(display, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
    })
    reactive(result())
  })
}
