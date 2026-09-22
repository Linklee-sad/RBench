if (!exists("ai_report_ui", mode = "function")) source("R/ai.R")
if (!exists("algorithm_title_ui", mode = "function")) source("R/algorithm_tutorials.R")
if (!exists("regression_evaluation", mode = "function")) source("R/model_evaluation.R")

advanced_number <- function(x) {
  if (!length(x) || !is.finite(x)) return("无法计算")
  format(signif(x, 4), trim = TRUE, scientific = abs(x) > 0 && abs(x) < .001)
}

advanced_soft_threshold <- function(value, penalty) sign(value) * pmax(abs(value) - penalty, 0)

advanced_elastic_fit <- function(x, y, lambda, alpha = 1, max_iter = 600L, tolerance = 1e-7) {
  x <- as.matrix(x); y <- as.numeric(y); lambda <- as.numeric(lambda); alpha <- as.numeric(alpha)
  if (!nrow(x) || !ncol(x) || length(y) != nrow(x)) stop("高级回归输入数据无效。", call. = FALSE)
  if (!is.finite(lambda) || lambda < 0 || !is.finite(alpha) || alpha < 0 || alpha > 1) stop("惩罚参数无效。", call. = FALSE)
  center <- colMeans(x); scale <- apply(x, 2, stats::sd)
  scale[!is.finite(scale) | scale <= sqrt(.Machine$double.eps)] <- 1
  xs <- sweep(sweep(x, 2, center, "-"), 2, scale, "/")
  y_center <- mean(y); yc <- y - y_center; n <- nrow(xs); beta <- numeric(ncol(xs))
  column_norm <- colSums(xs^2) / n; residual <- yc
  for (iteration in seq_len(as.integer(max_iter))) {
    old <- beta
    for (j in seq_along(beta)) {
      partial <- residual + xs[, j] * beta[j]
      score <- sum(xs[, j] * partial) / n
      updated <- advanced_soft_threshold(score, lambda * alpha) /
        (column_norm[j] + lambda * (1 - alpha))
      residual <- partial - xs[, j] * updated
      beta[j] <- updated
    }
    if (max(abs(beta - old)) < tolerance) break
  }
  coefficients <- beta / scale
  intercept <- y_center - sum(center * coefficients)
  list(intercept = intercept, coefficients = coefficients, standardized = beta,
    center = center, scale = scale, lambda = lambda, alpha = alpha,
    iterations = iteration, converged = iteration < max_iter)
}

predict_advanced_elastic <- function(model, x) {
  as.numeric(model$intercept + as.matrix(x) %*% model$coefficients)
}

advanced_feature_labels <- function(names, predictors) {
  output <- names
  for (j in rev(seq_along(predictors))) {
    output <- gsub(paste0("(^|:|\\*)x", j, "(?=($|[^0-9]))"), paste0("\\1", predictors[j]), output, perl = TRUE)
  }
  gsub("__pow", "^", output, fixed = TRUE)
}

prepare_advanced_regression_data <- function(data, outcome, predictors, degree = 1,
                                             interactions = FALSE, max_features = 300L) {
  outcome <- as.integer(outcome); predictors <- unique(as.integer(predictors)); degree <- as.integer(degree)
  if (length(outcome) != 1L || is.na(outcome) || !outcome %in% seq_along(data)) stop("请选择一个数值目标字段。", call. = FALSE)
  if (!length(predictors) || anyNA(predictors) || !all(predictors %in% seq_along(data))) stop("请至少选择一个预测字段。", call. = FALSE)
  if (outcome %in% predictors) stop("目标字段不能同时作为预测字段。", call. = FALSE)
  if (!degree %in% 1:3) stop("多项式次数应为 1、2 或 3。", call. = FALSE)
  selected <- data[, c(outcome, predictors), drop = FALSE]
  valid <- stats::complete.cases(selected)
  for (x in selected) if (is.numeric(x)) valid <- valid & is.finite(x)
  selected <- selected[valid, , drop = FALSE]
  if (nrow(selected) < 30L) stop("有效数据不足 30 行，无法可靠进行训练、交叉验证和测试。", call. = FALSE)
  y <- selected[[1]]
  if (!is.numeric(y) || length(unique(y)) < 2L) stop("高级回归要求目标字段为有变化的数值。", call. = FALSE)
  xdata <- selected[-1]; labels <- names(data)[predictors]; keep <- logical(length(xdata)); removed <- character()
  for (j in seq_along(xdata)) {
    x <- xdata[[j]]; label <- labels[j]
    if (inherits(x, "Date") || inherits(x, "POSIXt")) x <- as.numeric(x)
    if (is.character(x) || is.logical(x)) x <- factor(as.character(x))
    if (is.factor(x)) { x <- droplevels(x); if (nlevels(x) > 30L) stop(paste0("预测字段“", label, "”超过 30 类，请先合并类别。"), call. = FALSE) }
    if (!(is.numeric(x) || is.factor(x))) stop(paste0("预测字段“", label, "”的数据类型不受支持。"), call. = FALSE)
    if (length(unique(x)) < 2L) removed <- c(removed, label) else { keep[j] <- TRUE; xdata[[j]] <- x }
  }
  if (!any(keep)) stop("所选预测字段都没有变化。", call. = FALSE)
  xdata <- xdata[, keep, drop = FALSE]; labels <- labels[keep]; names(xdata) <- paste0("x", seq_along(xdata))
  base <- stats::model.matrix(~ . - 1, data = xdata)
  varying <- apply(base, 2, function(column) stats::sd(column) > sqrt(.Machine$double.eps))
  base <- base[, varying, drop = FALSE]
  features <- base
  if (degree >= 2L) {
    continuous <- which(apply(base, 2, function(column) length(unique(column)) > 2L))
    for (power in 2:degree) if (length(continuous)) {
      extra <- base[, continuous, drop = FALSE]^power
      colnames(extra) <- paste0(colnames(base)[continuous], "__pow", power)
      features <- cbind(features, extra)
    }
  }
  if (isTRUE(interactions) && ncol(base) >= 2L) {
    pairs <- utils::combn(seq_len(ncol(base)), 2)
    interaction_matrix <- vapply(seq_len(ncol(pairs)), function(i) base[, pairs[1, i]] * base[, pairs[2, i]], numeric(nrow(base)))
    if (is.null(dim(interaction_matrix))) interaction_matrix <- matrix(interaction_matrix, ncol = 1)
    colnames(interaction_matrix) <- paste0(colnames(base)[pairs[1, ]], ":", colnames(base)[pairs[2, ]])
    varying_interactions <- apply(interaction_matrix, 2, function(column) stats::sd(column) > sqrt(.Machine$double.eps))
    features <- cbind(features, interaction_matrix[, varying_interactions, drop = FALSE])
  }
  if (ncol(features) > max_features) stop(paste0("展开后产生 ", ncol(features), " 个特征，超过上限 ", max_features, "。请减少字段、关闭交互项或降低次数。"), call. = FALSE)
  colnames(features) <- make.unique(colnames(features))
  list(x = features, y = as.numeric(y), outcome = names(data)[outcome], predictors = labels,
    feature_labels = advanced_feature_labels(colnames(features), labels), degree = degree,
    interactions = isTRUE(interactions), used = nrow(selected), excluded = sum(!valid), removed = removed,
    valid_rows = which(valid), analysis_summary = ai_analysis_summary(data.frame(.outcome = y, features, check.names = FALSE), c(names(data)[outcome], colnames(features))))
}

advanced_lambda_grid <- function(x, y, alpha, count = 36L) {
  xs <- scale(x); yc <- y - mean(y); gradient <- max(abs(crossprod(xs, yc) / nrow(xs)))
  upper <- if (alpha > 0) gradient / max(alpha, .05) else max(gradient * 10, 1)
  upper <- max(upper, 1e-4)
  exp(seq(log(upper), log(upper * 1e-4), length.out = as.integer(count)))
}

advanced_cv <- function(x, y, lambdas, alpha, folds = 5L, seed = 2026, progress = NULL) {
  folds <- as.integer(folds); if (!folds %in% c(5L, 10L) || length(y) < folds * 3L) stop("交叉验证折数与样本量不匹配。", call. = FALSE)
  set.seed(as.integer(seed)); fold_id <- sample(rep(seq_len(folds), length.out = length(y)))
  errors <- matrix(NA_real_, nrow = length(lambdas), ncol = folds)
  for (fold in seq_len(folds)) {
    train <- fold_id != fold; valid <- !train
    for (i in seq_along(lambdas)) {
      model <- advanced_elastic_fit(x[train, , drop = FALSE], y[train], lambdas[i], alpha)
      prediction <- predict_advanced_elastic(model, x[valid, , drop = FALSE])
      errors[i, fold] <- sqrt(mean((y[valid] - prediction)^2))
    }
    if (is.function(progress)) progress(fold / folds, fold)
  }
  means <- rowMeans(errors); standard_errors <- apply(errors, 1, stats::sd) / sqrt(folds)
  data.frame(lambda = lambdas, mean_rmse = means, se_rmse = standard_errors,
    low = means - standard_errors, high = means + standard_errors, check.names = FALSE)
}

fit_advanced_regression <- function(data, outcome, predictors, method = "lasso", degree = 1,
                                    interactions = FALSE, folds = 5, selection = "min",
                                    train_ratio = .8, seed = 2026) {
  if (!method %in% c("ridge", "lasso", "elastic")) stop("请选择有效的正则化方法。", call. = FALSE)
  if (!selection %in% c("min", "1se")) stop("请选择有效的 λ 规则。", call. = FALSE)
  alpha <- c(ridge = 0, lasso = 1, elastic = .5)[[method]]
  prepared <- prepare_advanced_regression_data(data, outcome, predictors, degree, interactions)
  train_ratio <- as.numeric(train_ratio); seed <- as.integer(seed)
  if (!is.finite(train_ratio) || train_ratio < .5 || train_ratio > .9) stop("训练集比例应在 50% 到 90% 之间。", call. = FALSE)
  set.seed(seed); train_n <- max(20L, min(length(prepared$y) - 5L, floor(length(prepared$y) * train_ratio)))
  train_index <- sort(sample(seq_along(prepared$y), train_n)); test_index <- setdiff(seq_along(prepared$y), train_index)
  x_train <- prepared$x[train_index, , drop = FALSE]; y_train <- prepared$y[train_index]
  lambdas <- advanced_lambda_grid(x_train, y_train, alpha)
  cv <- advanced_cv(x_train, y_train, lambdas, alpha, folds, seed)
  minimum <- which.min(cv$mean_rmse)
  selected_index <- minimum
  if (selection == "1se") {
    eligible <- which(cv$mean_rmse <= cv$mean_rmse[minimum] + cv$se_rmse[minimum])
    selected_index <- min(eligible)
  }
  selected_lambda <- cv$lambda[selected_index]
  model <- advanced_elastic_fit(x_train, y_train, selected_lambda, alpha)
  predicted <- predict_advanced_elastic(model, prepared$x[test_index, , drop = FALSE]); actual <- prepared$y[test_index]
  evaluation <- regression_evaluation(actual, predicted)
  coefficients <- data.frame(字段 = c("截距", prepared$feature_labels),
    系数 = c(model$intercept, model$coefficients), 入选 = c(TRUE, abs(model$coefficients) > 1e-10), check.names = FALSE)
  path_indices <- unique(round(seq(1, length(lambdas), length.out = min(28L, length(lambdas)))))
  path <- do.call(rbind, lapply(path_indices, function(i) {
    fitted <- advanced_elastic_fit(x_train, y_train, lambdas[i], alpha)
    data.frame(lambda = lambdas[i], 字段 = prepared$feature_labels, 系数 = fitted$coefficients, check.names = FALSE)
  }))
  predictions <- data.frame(原始行号 = prepared$valid_rows[test_index], 实际值 = actual,
    预测值 = predicted, 残差 = actual - predicted, check.names = FALSE)
  method_label <- c(ridge = "岭回归", lasso = "Lasso", elastic = "Elastic Net")[[method]]
  selected_count <- sum(abs(model$coefficients) > 1e-10)
  report <- c("高级回归：专业模型解读", "一、模型设定",
    paste0("方法：", method_label, "；目标字段：", prepared$outcome, "；α = ", alpha, "。"),
    paste0("原始预测字段：", paste(prepared$predictors, collapse = "、"), "。多项式次数：", degree, "；交互项：", if (interactions) "包含" else "不包含", "。"),
    sprintf("使用 %d 行完整数据；训练集 %d 行，测试集 %d 行；排除 %d 行。展开后共有 %d 个候选特征。", prepared$used, length(train_index), length(test_index), prepared$excluded, ncol(prepared$x)),
    "二、交叉验证与惩罚强度",
    paste0("使用 ", folds, " 折交叉验证，并按 ", if (selection == "min") "最低平均 RMSE" else "1-SE 简化规则", "选择 λ = ", advanced_number(selected_lambda), "。"),
    paste0("最终保留 ", selected_count, " 个非零特征。岭回归通常不会把系数压到严格的 0；Lasso 与 Elastic Net 可以产生变量筛选效果。"),
    "三、独立测试集表现",
    paste0("RMSE = ", advanced_number(evaluation$summary$数值[evaluation$summary$指标 == "RMSE"]),
      "，MAE = ", advanced_number(evaluation$summary$数值[evaluation$summary$指标 == "MAE"]),
      "，R² = ", advanced_number(evaluation$summary$数值[evaluation$summary$指标 == "R²"]), "。"),
    "四、解释与边界",
    "正则化系数在标准化后的优化过程中估计，页面表格已换算回原始特征尺度。多项式和交互项的存在会改变主效应含义；主效应系数表示其他相关项为 0 时的局部关系。",
    "岭回归适合缓解共线性，Lasso适合产生稀疏模型，Elastic Net兼顾两者。变量被筛掉不表示它与结果没有关系，相关变量可能相互替代。",
    "当前流程在训练集内部选择 λ，再用独立测试集评估。结果仍依赖样本代表性、特征构造和随机划分，不自动具有因果含义。时间序列、重复测量或分组数据需要专门的验证方式。")
  list(model = model, task = "regression", method = method, method_label = method_label,
    outcome = prepared$outcome, predictors = prepared$predictors, metrics = evaluation$summary,
    coefficients = coefficients, cv = cv, path = path, predictions = predictions,
    report = paste(report, collapse = "\n\n"), used = prepared$used, excluded = prepared$excluded,
    train_n = length(train_index), test_n = length(test_index), actual = actual, predicted = predicted,
    lambda = selected_lambda, lambda_index = selected_index, alpha = alpha, selected_count = selected_count,
    analysis_summary = prepared$analysis_summary)
}

build_advanced_cv_plot <- function(result) {
  d <- result$cv
  ggplot2::ggplot(d, ggplot2::aes(lambda, mean_rmse)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = low, ymax = high), fill = "#bfdbfe", alpha = .55) +
    ggplot2::geom_line(colour = "#2563eb", linewidth = 1) +
    ggplot2::geom_vline(xintercept = result$lambda, linetype = "dashed", colour = "#d97706") +
    ggplot2::scale_x_log10() + model_evaluation_theme() +
    ggplot2::labs(title = paste0(result$method_label, "：交叉验证选择 λ"), subtitle = "橙色虚线为最终选择；阴影为平均 RMSE ± 1 标准误", x = "λ（对数刻度）", y = "交叉验证 RMSE")
}

build_advanced_path_plot <- function(result) {
  d <- result$path
  importance <- aggregate(abs(系数) ~ 字段, d, max); names(importance)[2] <- "importance"
  keep <- head(importance$字段[order(importance$importance, decreasing = TRUE)], 20)
  d <- d[d$字段 %in% keep, , drop = FALSE]
  ggplot2::ggplot(d, ggplot2::aes(lambda, 系数, colour = 字段, group = 字段)) +
    ggplot2::geom_line(linewidth = .85) + ggplot2::geom_vline(xintercept = result$lambda, linetype = "dashed", colour = "#172b4d") +
    ggplot2::scale_x_log10() + model_evaluation_theme() +
    ggplot2::labs(title = paste0(result$method_label, "：系数路径"), subtitle = "最多显示路径变化最大的 20 个特征；虚线为最终 λ", x = "λ（对数刻度）", y = "系数", colour = "特征")
}

build_advanced_prediction_plot <- function(result) {
  d <- data.frame(actual = result$actual, predicted = result$predicted)
  ggplot2::ggplot(d, ggplot2::aes(actual, predicted)) + ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "#d97706") +
    ggplot2::geom_point(colour = "#2563eb", alpha = .72, size = 2.5) + ggplot2::coord_equal() + model_evaluation_theme() +
    ggplot2::labs(title = paste0(result$method_label, "：测试集实际值与预测值"), x = "实际值", y = "预测值")
}

advanced_regression_ui <- function(id) {
  ns <- NS(id)
  tagList(
    algorithm_title_ui(ns, "高级回归"), p("使用多项式、交互项和正则化处理非线性、共线性与变量筛选，并用训练集内部交叉验证选择惩罚强度。"),
    conditionalPanel(sprintf("input['%s'] %% 2 === 1", ns("tutorial_toggle")), algorithm_tutorial_ui(ns("tutorial"), "advanced_regression")),
    fluidRow(column(5, selectInput(ns("outcome"), "数值目标字段", NULL)), column(7, selectizeInput(ns("predictors"), "预测字段（可多选）", NULL, multiple = TRUE))),
    fluidRow(column(3, selectInput(ns("method"), "正则化方法", c("Lasso（变量筛选）" = "lasso", "岭回归（缓解共线性）" = "ridge", "Elastic Net（折中）" = "elastic"))),
      column(3, selectInput(ns("degree"), "多项式次数", c("1 次（仅线性项）" = 1, "2 次" = 2, "3 次" = 3))),
      column(3, checkboxInput(ns("interactions"), "加入两两交互项", FALSE)),
      column(3, selectInput(ns("folds"), "训练集内部交叉验证", c("5 折" = 5, "10 折" = 10)))),
    fluidRow(column(4, selectInput(ns("selection"), "λ 选择规则", c("最低 RMSE" = "min", "1-SE：更简单稳定" = "1se"))),
      column(4, sliderInput(ns("train_ratio"), "训练集比例", .5, .9, .8, step = .05)), column(4, numericInput(ns("seed"), "随机种子", 2026, min = 1))),
    ai_parameter_ui(ns("ai_params")),
    helpText("展开后的特征最多 300 个。交互项和高次项可能快速增加特征数量；建议先从 1 次、无交互项开始。"),
    actionButton(ns("run"), "运行高级回归", class = "btn-primary"), tags$div(style = "margin:12px 0", textOutput(ns("status"))),
    tabsetPanel(
      tabPanel("专业解读", tags$div(style = "white-space:pre-wrap;line-height:1.9", textOutput(ns("report")))),
      tabPanel("AI 增强解读", ai_report_ui(ns("ai_report"))),
      tabPanel("测试集评估", DT::DTOutput(ns("metrics")), ggplot_editor_ui(ns("prediction_editor"), height = "480px")),
      tabPanel("交叉验证", ggplot_editor_ui(ns("cv_editor"), height = "500px"), DT::DTOutput(ns("cv_table"))),
      tabPanel("系数与筛选", DT::DTOutput(ns("coefficients")), ggplot_editor_ui(ns("path_editor"), height = "520px")),
      tabPanel("测试集预测", DT::DTOutput(ns("predictions")))
    ), hr(), downloadButton(ns("download_report"), "下载高级回归报告 TXT"), downloadButton(ns("download_predictions"), "下载测试集预测 CSV"),
    actionButton(ns("save_report"), "保存高级回归报告到项目文件夹"), textOutput(ns("saved")))
}

advanced_regression_server <- function(id, data, directory = reactive(getwd()), ai_config = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    algorithm_tutorial_server("tutorial", "advanced_regression"); algorithm_tutorial_toggle_server(input, session)
    result <- reactiveVal(NULL); status <- reactiveVal("选择字段后，点击“运行高级回归”。"); saved <- reactiveVal("")
    ai_params <- ai_parameter_server("ai_params", ai_config, data, "高级回归",
      reactive(list(method = input$method, degree = input$degree, interactions = input$interactions, folds = input$folds, selection = input$selection, train_ratio = input$train_ratio, seed = input$seed)),
      list(method = c("lasso", "ridge", "elastic"), degree = "1 到 3", interactions = "true 或 false", folds = c(5, 10), selection = c("min", "1se"), train_ratio = "0.5 到 0.9", seed = "正整数"))
    observeEvent(ai_params$apply(), {
      p <- ai_params$proposal()$parameters; if (is.null(p)) return()
      if (!is.null(p$method) && p$method %in% c("lasso", "ridge", "elastic")) updateSelectInput(session, "method", selected = p$method)
      if (!is.null(p$degree)) updateSelectInput(session, "degree", selected = as.character(max(1L, min(3L, as.integer(p$degree)))))
      if (!is.null(p$interactions)) updateCheckboxInput(session, "interactions", value = isTRUE(as.logical(p$interactions)))
      if (!is.null(p$folds)) updateSelectInput(session, "folds", selected = if (as.integer(p$folds) >= 8) "10" else "5")
      if (!is.null(p$selection) && p$selection %in% c("min", "1se")) updateSelectInput(session, "selection", selected = p$selection)
      if (!is.null(p$train_ratio)) updateSliderInput(session, "train_ratio", value = max(.5, min(.9, as.numeric(p$train_ratio))))
      if (!is.null(p$seed)) updateNumericInput(session, "seed", value = max(1L, as.integer(p$seed)))
    }, ignoreInit = TRUE)
    observeEvent(data(), {
      d <- data(); eligible <- which(vapply(d, is.numeric, logical(1))); labels <- if (length(eligible)) paste0(eligible, ". ", names(d)[eligible]) else character(); choices <- setNames(as.character(eligible), labels)
      selected <- if (length(input$outcome) == 1L && input$outcome %in% unname(choices)) input$outcome else unname(head(choices, 1)); updateSelectInput(session, "outcome", choices = choices, selected = selected)
    })
    observeEvent(list(data(), input$outcome), {
      d <- data(); eligible <- setdiff(seq_along(d), as.integer(input$outcome)); labels <- if (length(eligible)) paste0(eligible, ". ", names(d)[eligible]) else character(); choices <- setNames(as.character(eligible), labels)
      updateSelectizeInput(session, "predictors", choices = choices, selected = intersect(input$predictors, unname(choices)))
    })
    observe({ result(NULL); status("数据、字段或设置已更新，请重新运行高级回归。"); data(); input$outcome; input$predictors; input$method; input$degree; input$interactions; input$folds; input$selection; input$train_ratio; input$seed }, priority = 100)
    observeEvent(input$run, {
      result(NULL)
      tryCatch({
        fitted <- withProgress(message = "正在交叉验证并拟合高级回归", value = 0, {
          prepared <- prepare_advanced_regression_data(data(), input$outcome, input$predictors, input$degree, input$interactions)
          incProgress(.08, detail = paste0("已生成 ", ncol(prepared$x), " 个特征"))
          fit_advanced_regression(data(), input$outcome, input$predictors, input$method, input$degree, input$interactions, input$folds, input$selection, input$train_ratio, input$seed)
        })
        result(fitted); status(paste0("建模完成：λ = ", advanced_number(fitted$lambda), "；非零特征 ", fitted$selected_count, " 个；测试集 ", fitted$test_n, " 行。"))
      }, error = function(e) { message <- if (inherits(e, "shiny.silent.error")) "请先导入数据并选择字段。" else conditionMessage(e); status(paste("未能建模：", message)); showNotification(message, type = "error", duration = 10) })
    })
    output$status <- renderText(status()); output$report <- renderText({ req(result()); result()$report })
    ai_report_server("ai_report", ai_config, "高级回归", reactive(if (is.null(result())) "" else result()$report), reactive(if (is.null(result())) NULL else ai_model_context("高级回归", result())), data)
    output$metrics <- DT::renderDT({ req(result()); DT::datatable(result()$metrics, rownames = FALSE, options = list(dom = "t")) |> DT::formatRound("数值", 4) })
    output$coefficients <- DT::renderDT({ req(result()); DT::datatable(result()$coefficients, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE)) |> DT::formatSignif("系数", 5) })
    output$cv_table <- DT::renderDT({ req(result()); DT::datatable(result()$cv, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE)) |> DT::formatSignif(names(result()$cv), 5) })
    output$predictions <- DT::renderDT({ req(result()); DT::datatable(result()$predictions, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) })
    prediction_plot <- reactive({ req(result()); build_advanced_prediction_plot(result()) }); cv_plot <- reactive({ req(result()); build_advanced_cv_plot(result()) }); path_plot <- reactive({ req(result()); build_advanced_path_plot(result()) })
    ggplot_editor_server("prediction_editor", prediction_plot, directory, "easyr-advanced-regression-prediction")
    ggplot_editor_server("cv_editor", cv_plot, directory, "easyr-advanced-regression-cv")
    ggplot_editor_server("path_editor", path_plot, directory, "easyr-advanced-regression-path")
    write_report <- function(file) { req(result()); writeLines(enc2utf8(result()$report), file, useBytes = TRUE) }
    write_predictions <- function(file) { req(result()); con <- file(file, "wb"); on.exit(close(con)); writeBin(charToRaw("\ufeff"), con); lines <- capture.output(write.csv(result()$predictions, row.names = FALSE, na = "")); writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n"))), con) }
    output$download_report <- downloadHandler(filename = function() paste0("easyr-advanced-regression-", Sys.Date(), ".txt"), content = write_report)
    output$download_predictions <- downloadHandler(filename = function() paste0("easyr-advanced-regression-predictions-", Sys.Date(), ".csv"), content = write_predictions)
    output$saved <- renderText(saved()); observeEvent(input$save_report, { req(result()); tryCatch({ path <- save_to_workdir(directory(), "easyr-advanced-regression", ".txt", write_report); saved(paste("上次保存：", path)) }, error = function(e) showNotification(conditionMessage(e), type = "error")) })
    result
  })
}
