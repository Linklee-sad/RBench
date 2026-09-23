plot_defaults <- function() list(
  bins = 30, mean_line = FALSE, median_line = FALSE, smooth = FALSE,
  ribbon = TRUE, points = FALSE, facet = FALSE, horizontal = FALSE,
  corr_labels = TRUE, corr_method = "pearson",
  title = "", x_label = "", y_label = "", z_label = "", theme = "minimal",
  color = "#3b82f6", palette = "D", font_size = 12, alpha = 0.65,
  point_size = 2.5, line_width = 1, width = 10, height = 6, dpi = 150
)

plotting_font_family <- function() {
  switch(Sys.info()[["sysname"]], Darwin = "Arial Unicode MS", Windows = "Microsoft YaHei", "sans")
}

function_plot_allowed <- c(
  "+", "-", "*", "/", "^", "%%", "%/%", "(",
  "sin", "cos", "tan", "asin", "acos", "atan", "sinh", "cosh", "tanh",
  "exp", "log", "log10", "log2", "sqrt", "abs", "floor", "ceiling",
  "round", "signif", "pnorm", "dnorm", "pt", "dt", "plogis", "dlogis"
)

validate_function_expression <- function(node, variables = "x") {
  if (is.numeric(node) || is.integer(node)) return(invisible(TRUE))
  if (is.symbol(node)) {
    if (!as.character(node) %in% c(variables, "pi", "Inf", "NaN")) {
      stop(sprintf("不支持符号“%s”；变量只能写 %s。", as.character(node), paste(variables, collapse = "、")), call. = FALSE)
    }
    return(invisible(TRUE))
  }
  if (!is.call(node)) stop("表达式只能包含数值、x 和受支持的数学函数。", call. = FALSE)
  operator <- as.character(node[[1]])
  if (length(operator) != 1L || !operator %in% function_plot_allowed) {
    stop(sprintf("不支持函数或运算“%s”。", paste(operator, collapse = "")), call. = FALSE)
  }
  for (argument in as.list(node)[-1]) validate_function_expression(argument, variables)
  invisible(TRUE)
}

function_plot_environment <- function(values, variable = "x") {
  bindings <- list(values, base::pi, Inf, NaN)
  names(bindings) <- c(variable, "pi", "Inf", "NaN")
  stats_functions <- c("pnorm", "dnorm", "pt", "dt", "plogis", "dlogis")
  for (name in function_plot_allowed) {
    bindings[[name]] <- if (name %in% stats_functions) getExportedValue("stats", name) else get(name, envir = baseenv(), inherits = FALSE)
  }
  list2env(bindings, parent = emptyenv())
}

parse_function_lines <- function(text, max_functions = 6L, variable = "x") {
  lines <- trimws(unlist(strsplit(as.character(text)[1], "\\r?\\n", perl = TRUE)))
  lines <- lines[nzchar(lines)]
  if (!length(lines)) stop("请至少输入一个函数，例如 sin(x)。", call. = FALSE)
  if (length(lines) > max_functions) stop(sprintf("一次最多绘制 %d 个函数。", max_functions), call. = FALSE)
  lapply(seq_along(lines), function(i) {
    line <- lines[i]; label <- line; expression_text <- line
    parts <- regmatches(line, regexec("^([^=]+?)\\s*=\\s*(.+)$", line, perl = TRUE))[[1]]
    if (length(parts) == 3L && grepl("^[[:alnum:]_ .()\\x{4e00}-\\x{9fff}-]+$", trimws(parts[2]), perl = TRUE)) {
      label <- trimws(parts[2]); expression_text <- trimws(parts[3])
    }
    expression <- tryCatch(parse(text = expression_text, keep.source = FALSE),
      error = function(e) stop(sprintf("第 %d 行无法解析：%s", i, conditionMessage(e)), call. = FALSE))
    if (length(expression) != 1L) stop(sprintf("第 %d 行只能包含一个表达式。", i), call. = FALSE)
    tryCatch(validate_function_expression(expression[[1]], variable),
      error = function(e) stop(sprintf("第 %d 行：%s", i, conditionMessage(e)), call. = FALSE))
    list(label = label, text = expression_text, expression = expression[[1]])
  })
}

function_plot_roots <- function(x, y) {
  valid <- is.finite(x) & is.finite(y); x <- x[valid]; y <- y[valid]
  if (length(x) < 2L) return(numeric())
  scale <- stats::median(abs(y), na.rm = TRUE)
  if (!is.finite(scale) || scale == 0) scale <- max(abs(y), na.rm = TRUE)
  if (!is.finite(scale) || scale == 0) scale <- 1
  exact <- x[abs(y) <= max(1e-10, scale * 1e-7)]
  pairs <- which(y[-length(y)] * y[-1] < 0 & pmax(abs(y[-length(y)]), abs(y[-1])) <= max(1, scale * 20))
  crossed <- if (length(pairs)) x[pairs] - y[pairs] * (x[pairs + 1] - x[pairs]) / (y[pairs + 1] - y[pairs]) else numeric()
  middle <- if (length(y) >= 3L) which(abs(y[2:(length(y) - 1)]) <=
    pmin(abs(y[1:(length(y) - 2)]), abs(y[3:length(y)]))) + 1L else integer()
  tangent <- vapply(middle, function(i) {
    fit <- stats::lm(y[c(i - 1L, i, i + 1L)] ~ stats::poly(x[c(i - 1L, i, i + 1L)], 2, raw = TRUE))
    coefficients <- stats::coef(fit)
    if (length(coefficients) != 3L || !is.finite(coefficients[3]) || abs(coefficients[3]) < .Machine$double.eps) return(NA_real_)
    vertex <- -coefficients[2] / (2 * coefficients[3])
    value <- coefficients[1] + coefficients[2] * vertex + coefficients[3] * vertex^2
    if (vertex >= x[i - 1L] && vertex <= x[i + 1L] && abs(value) <= max(1e-9, scale * 1e-7)) vertex else NA_real_
  }, numeric(1))
  roots <- sort(c(exact, crossed, tangent[is.finite(tangent)]))
  if (!length(roots)) return(roots)
  spacing <- max(diff(range(x)), 1) / max(length(x), 2)
  roots[c(TRUE, diff(roots) > spacing * 1.5)]
}

build_function_plot_specs <- function(specifications, points = 1000, options = list(),
    show_axes = TRUE, show_grid = TRUE, mark_roots = FALSE) {
  o <- modifyList(plot_defaults(), options)
  points <- suppressWarnings(as.integer(points)[1])
  if (!is.finite(points) || points < 100L || points > 5000L) stop("绘图点数应在 100～5000 之间。", call. = FALSE)
  if (!length(specifications)) stop("请至少添加一个函数。", call. = FALSE)
  labels <- vapply(specifications, function(item) trimws(as.character(item$label)[1]), character(1))
  if (any(!nzchar(labels))) stop("每个函数都需要名称。", call. = FALSE)
  if (anyDuplicated(labels)) stop("函数名称不能重复。", call. = FALSE)
  series <- lapply(seq_along(specifications), function(i) {
    item <- specifications[[i]]
    parametric <- identical(item$type, "parametric")
    lower <- suppressWarnings(as.numeric(if (parametric) item$t_min else item$x_min)[1])
    upper <- suppressWarnings(as.numeric(if (parametric) item$t_max else item$x_max)[1])
    if (!is.finite(lower) || !is.finite(upper) || lower >= upper) {
      stop(sprintf("曲线“%s”的%s必须是两个有限数值，且起点小于终点。", labels[i], if (parametric) "参数范围" else "定义域"), call. = FALSE)
    }
    parameter <- seq(lower, upper, length.out = points)
    evaluate <- function(expression, coordinate) {
      value <- tryCatch(eval(expression, envir = function_plot_environment(parameter, if (parametric) "t" else "x")),
        error = function(e) stop(sprintf("曲线“%s”的%s计算失败：%s", labels[i], coordinate, conditionMessage(e)), call. = FALSE))
      if (!is.numeric(value)) stop(sprintf("曲线“%s”的%s没有返回数值。", labels[i], coordinate), call. = FALSE)
      if (length(value) == 1L) value <- rep(value, length(parameter))
      if (length(value) != length(parameter)) stop(sprintf("曲线“%s”的%s返回长度不正确。", labels[i], coordinate), call. = FALSE)
      as.numeric(value)
    }
    if (parametric) {
      x <- evaluate(item$x_expression, "x(t)")
      y <- evaluate(item$y_expression, "y(t)")
    } else {
      x <- parameter
      y <- evaluate(item$expression, "f(x)")
    }
    valid <- is.finite(x) & is.finite(y); x[!valid] <- NA_real_; y[!valid] <- NA_real_
    y <- as.numeric(y); y[!is.finite(y)] <- NA_real_
    if (sum(valid) < 2L) stop(sprintf("曲线“%s”在当前范围内没有足够的有限值。", labels[i]), call. = FALSE)
    data.frame(.x = x, .y = y, .function = labels[i], .domain = sprintf("%s ∈ [%s, %s]",
      if (parametric) "t" else "x", format(lower), format(upper)), stringsAsFactors = FALSE)
  })
  d <- do.call(rbind, series)
  d$.function <- factor(d$.function, levels = labels)
  series_colours <- vapply(specifications, function(item) {
    if (!is.null(item$color) && length(item$color) == 1L && nzchar(item$color)) as.character(item$color) else NA_character_
  }, character(1))
  p <- ggplot2::ggplot(d, ggplot2::aes(.x, .y, group = .function))
  if (length(specifications) > 1L) {
    p <- p + ggplot2::geom_path(ggplot2::aes(colour = .function), linewidth = o$line_width, alpha = o$alpha, na.rm = TRUE)
    p <- if (all(!is.na(series_colours))) p + ggplot2::scale_colour_manual(values = stats::setNames(series_colours, labels), name = "函数") else
      p + ggplot2::scale_colour_viridis_d(option = o$palette, name = "函数")
  } else {
    colour <- if (!is.na(series_colours[1])) series_colours[1] else o$color
    p <- p + ggplot2::geom_path(colour = colour, linewidth = o$line_width, alpha = o$alpha, na.rm = TRUE)
  }
  if (isTRUE(show_axes)) {
    if (min(d$.x) <= 0 && max(d$.x) >= 0) p <- p + ggplot2::geom_vline(xintercept = 0, colour = "#64748b", linewidth = 0.45)
    p <- p + ggplot2::geom_hline(yintercept = 0, colour = "#64748b", linewidth = 0.45)
  }
  roots_by_function <- lapply(split(d, d$.function, drop = TRUE), function(item) function_plot_roots(item$.x, item$.y))
  root_data <- do.call(rbind, lapply(names(roots_by_function), function(name) {
    roots <- roots_by_function[[name]]
    if (!length(roots)) return(NULL)
    data.frame(.x = head(roots, 100), .y = 0, .function = name)
  }))
  if (isTRUE(mark_roots) && !is.null(root_data) && nrow(root_data)) {
    p <- p + ggplot2::geom_point(data = root_data, ggplot2::aes(.x, .y), inherit.aes = FALSE,
      colour = "#dc2626", fill = "white", shape = 21, stroke = 1, size = max(2.5, o$point_size))
  }
  theme <- switch(o$theme, classic = ggplot2::theme_classic, bw = ggplot2::theme_bw, ggplot2::theme_minimal)
  label <- function(custom, fallback) if (length(custom) == 1L && nzchar(trimws(custom))) custom else fallback
  p <- p + theme(base_size = o$font_size, base_family = plotting_font_family()) +
    ggplot2::theme(legend.position = "bottom", plot.title.position = "plot",
      panel.grid = if (isTRUE(show_grid)) ggplot2::element_line() else ggplot2::element_blank()) +
    ggplot2::labs(title = label(o$title, "函数图像"), x = label(o$x_label, "x"), y = label(o$y_label, "f(x)"))
  if (any(vapply(specifications, function(item) identical(item$type, "parametric"), logical(1)))) p <- p + ggplot2::coord_equal()
  if (!isTRUE(show_axes)) p <- p + ggplot2::theme(axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), axis.title = ggplot2::element_blank())
  stats <- do.call(rbind, lapply(split(d, d$.function, drop = TRUE), function(item) {
    values <- item$.y[is.finite(item$.y)]; roots <- roots_by_function[[as.character(item$.function[1])]]
    data.frame(函数 = as.character(item$.function[1]), 定义域 = item$.domain[1],
      有限点数 = length(values), 最小值 = min(values), 最大值 = max(values),
      零点 = if (length(roots)) paste(format(signif(head(roots, 8), 6), trim = TRUE), collapse = ", ") else "未发现", check.names = FALSE)
  }))
  nonfinite <- sum(!is.finite(d$.y))
  notes <- sprintf("每个函数使用 %d 个采样点，共绘制 %d 个函数；%d 个非有限结果已断开显示。",
    points, length(specifications), nonfinite)
  if (isTRUE(mark_roots)) notes <- paste(notes, "红色圆点是根据采样点估算的零点。")
  list(plot = p, notes = notes, stats = stats, used = nrow(d) - nonfinite, excluded = nonfinite)
}

build_function_plot <- function(expressions, x_min = -10, x_max = 10, points = 1000,
    options = list(), show_axes = TRUE, show_grid = TRUE, mark_roots = FALSE) {
  specifications <- parse_function_lines(expressions)
  specifications <- lapply(specifications, function(item) {
    item$x_min <- x_min; item$x_max <- x_max; item
  })
  build_function_plot_specs(specifications, points, options, show_axes, show_grid, mark_roots)
}

build_correlation_plot <- function(data, variables, options = list()) {
  o <- modifyList(plot_defaults(), options)
  variables <- unique(suppressWarnings(as.integer(variables)))
  variables <- variables[!is.na(variables) & variables %in% seq_along(data)]
  if (length(variables) < 2L) stop("相关性热图至少需要选择两个数值字段。", call. = FALSE)
  if (!all(vapply(data[variables], is.numeric, logical(1)))) stop("相关性热图只能使用数值字段。", call. = FALSE)
  method <- as.character(o$corr_method)[1]
  if (!method %in% c("pearson", "spearman")) stop("请选择 Pearson 或 Spearman 相关系数。", call. = FALSE)
  values <- as.data.frame(lapply(data[variables], function(x) { x[!is.finite(x)] <- NA_real_; x }), check.names = FALSE)
  names(values) <- names(data)[variables]
  usable <- vapply(values, function(x) sum(!is.na(x)) >= 2L && length(unique(x[!is.na(x)])) >= 2L, logical(1))
  if (sum(usable) < 2L) stop("至少需要两个包含足够有效变化值的数值字段。", call. = FALSE)
  values <- values[usable]
  matrix <- suppressWarnings(stats::cor(values, use = "pairwise.complete.obs", method = method))
  long <- as.data.frame(as.table(matrix), stringsAsFactors = FALSE)
  names(long) <- c("字段一", "字段二", "相关系数")
  levels <- colnames(matrix)
  long$字段一 <- factor(long$字段一, levels = levels)
  long$字段二 <- factor(long$字段二, levels = rev(levels))
  p <- ggplot2::ggplot(long, ggplot2::aes(字段一, 字段二, fill = 相关系数)) +
    ggplot2::geom_tile(colour = "white", linewidth = max(0.2, o$line_width / 2)) +
    ggplot2::scale_fill_gradient2(low = "#2563eb", mid = "#ffffff", high = "#dc2626",
      midpoint = 0, limits = c(-1, 1), na.value = "#e5e7eb", name = "相关系数") +
    ggplot2::coord_equal()
  if (isTRUE(o$corr_labels)) {
    labels <- ifelse(is.finite(long$相关系数), sprintf("%.2f", long$相关系数), "NA")
    p <- p + ggplot2::geom_text(ggplot2::aes(label = labels), size = max(2.5, o$font_size / 4.2))
  }
  theme <- switch(o$theme, classic = ggplot2::theme_classic, bw = ggplot2::theme_bw, ggplot2::theme_minimal)
  custom_title <- if (length(o$title) == 1L && nzchar(trimws(o$title))) o$title else
    paste0(if (method == "pearson") "Pearson" else "Spearman", " 相关性热图")
  p <- p + theme(base_size = o$font_size, base_family = plotting_font_family()) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1), plot.title.position = "plot") +
    ggplot2::labs(title = custom_title, x = if (nzchar(trimws(o$x_label))) o$x_label else NULL,
      y = if (nzchar(trimws(o$y_label))) o$y_label else NULL)
  table <- data.frame(字段 = rownames(matrix), round(matrix, 4), check.names = FALSE)
  list(plot = p,
    notes = sprintf("使用 %d 个数值字段计算 %s 相关系数；每一对字段使用其共同有效观测。灰色表示无法计算。",
      ncol(matrix), if (method == "pearson") "Pearson" else "Spearman"),
    stats = table, used = nrow(data), excluded = 0L)
}

plot_no_group_value <- "0"

groupable_columns <- function(data, exclude = integer(), max_groups = 20) {
  candidates <- setdiff(seq_along(data), as.integer(exclude))
  candidates[vapply(candidates, function(i) {
    x <- data[[i]]
    if (!is.atomic(x) || !is.null(dim(x))) return(FALSE)
    valid <- !is.na(x)
    if (is.numeric(x)) valid <- valid & is.finite(x)
    groups <- length(unique(x[valid]))
    groups >= 2 && groups <= max_groups
  }, logical(1))]
}

build_plotly_3d <- function(data, x, y, z, group = NULL, options = list(), max_points = 50000) {
  o <- modifyList(plot_defaults(), options)
  axes <- as.integer(c(x, y, z))
  if (length(axes) != 3 || anyNA(axes) || !all(axes %in% seq_along(data))) {
    stop("请选择三个有效的数值字段作为 X、Y、Z 轴。", call. = FALSE)
  }
  if (length(unique(axes)) != 3) stop("X、Y、Z 轴需要选择三个不同的字段。", call. = FALSE)
  if (!all(vapply(data[axes], is.numeric, logical(1)))) stop("3D 散点图的三个坐标轴都必须是数值字段。", call. = FALSE)
  if (!nrow(data)) stop("没有可绘制的数据，请调整清洗选项。", call. = FALSE)
  grouped <- length(group) == 1 && !is.na(group) && nzchar(group)
  if (grouped) {
    group <- as.integer(group)
    if (is.na(group) || !group %in% seq_along(data)) stop("请选择有效分组字段。", call. = FALSE)
  }
  valid <- Reduce(`&`, lapply(data[axes], is.finite))
  if (grouped) {
    valid <- valid & !is.na(data[[group]])
    if (is.numeric(data[[group]])) valid <- valid & is.finite(data[[group]])
  }
  d <- data.frame(.x = data[[axes[1]]], .y = data[[axes[2]]], .z = data[[axes[3]]], check.names = FALSE)
  d$.group <- if (grouped) as.character(data[[group]]) else "全部"
  d <- d[valid, , drop = FALSE]
  if (!nrow(d)) stop("三个坐标轴没有可配对的有效数据。", call. = FALSE)
  d$.group <- factor(d$.group)
  if (grouped && nlevels(d$.group) > 20) stop("分组超过 20 类，请选择类别较少的字段。", call. = FALSE)
  used <- nrow(d)
  max_points <- as.integer(max_points)
  if (!is.finite(max_points) || max_points < 100) max_points <- 50000L
  if (used > max_points) d <- d[unique(round(seq(1, used, length.out = max_points))), , drop = FALSE]
  escaped_names <- htmltools::htmlEscape(names(data)[axes])
  number <- function(value) htmltools::htmlEscape(format(signif(value, 6), trim = TRUE, scientific = FALSE))
  d$.hover <- paste0(escaped_names[1], "：", number(d$.x), "<br>",
    escaped_names[2], "：", number(d$.y), "<br>", escaped_names[3], "：", number(d$.z),
    if (grouped) paste0("<br>", htmltools::htmlEscape(names(data)[group]), "：", htmltools::htmlEscape(as.character(d$.group))) else "")
  marker <- list(size = max(2, o$point_size * 1.6), opacity = o$alpha,
    line = list(width = o$line_width, color = "rgba(255,255,255,0.45)"))
  if (grouped) {
    palette_name <- switch(o$palette, C = "Plasma", A = "Inferno", "Viridis")
    colours <- grDevices::hcl.colors(max(3, nlevels(d$.group)), palette = palette_name)
    p <- plotly::plot_ly(d, x = ~.x, y = ~.y, z = ~.z, color = ~.group, colors = colours,
      type = "scatter3d", mode = "markers", marker = marker, text = ~.hover, hoverinfo = "text")
  } else {
    marker$color <- o$color
    p <- plotly::plot_ly(d, x = ~.x, y = ~.y, z = ~.z, type = "scatter3d", mode = "markers",
      marker = marker, text = ~.hover, hoverinfo = "text", showlegend = FALSE)
  }
  label <- function(custom, fallback) if (length(custom) == 1 && nzchar(trimws(custom))) custom else fallback
  title <- label(o$title, paste(names(data)[axes], collapse = " × "))
  p <- plotly::layout(p, title = list(text = htmltools::htmlEscape(title)),
    scene = list(
      xaxis = list(title = list(text = htmltools::htmlEscape(label(o$x_label, names(data)[axes[1]])))),
      yaxis = list(title = list(text = htmltools::htmlEscape(label(o$y_label, names(data)[axes[2]])))),
      zaxis = list(title = list(text = htmltools::htmlEscape(label(o$z_label, names(data)[axes[3]]))))),
    legend = list(title = list(text = if (grouped) htmltools::htmlEscape(names(data)[group]) else "")))
  p <- plotly::config(p, displaylogo = FALSE, responsive = TRUE,
    toImageButtonOptions = list(format = "png", filename = "easyr-3d-chart"))
  excluded <- nrow(data) - used
  notes <- sprintf("使用 %d 行，排除 %d 行缺失或非有限数据；当前显示 %d 个点。可拖动旋转、滚轮缩放，工具栏相机按钮可保存 PNG。",
    used, excluded, nrow(d))
  if (used > nrow(d)) notes <- paste0(notes, " 为保证浏览器流畅，已均匀抽取最多 ", max_points, " 个点显示。")
  list(plot = p, notes = notes, stats = NULL, used = used, excluded = excluded, plotted = nrow(d))
}

build_ggplot <- function(data, kind, x, y = NULL, group = NULL, options = list()) {
  o <- modifyList(plot_defaults(), options)
  if (!kind %in% c("hist", "density", "scatter", "line", "qq", "ecdf", "bar", "box", "violin")) stop("请选择有效图形。", call. = FALSE)
  column <- function(i) length(i) == 1 && !is.na(i) && i %in% seq_along(data)
  x <- as.integer(x)
  if (!column(x)) stop("请选择绘图字段。", call. = FALSE)
  if (!nrow(data)) stop("没有可绘制的数据，请调整清洗选项。", call. = FALSE)
  grouped <- kind != "bar" && length(group) == 1 && !is.na(group) && nzchar(group)
  if (grouped) {
    group <- as.integer(group)
    if (!column(group)) stop("请选择有效分组字段。", call. = FALSE)
  }
  numeric_x <- !kind %in% c("bar", "line")
  if (numeric_x && !is.numeric(data[[x]])) stop("此图形需要数值字段。", call. = FALSE)
  if (kind == "line" && !(is.numeric(data[[x]]) || inherits(data[[x]], c("Date", "POSIXct", "POSIXlt")))) {
    stop("折线图横轴需要数值、日期或日期时间字段。", call. = FALSE)
  }
  d <- data.frame(.x = data[[x]], check.names = FALSE)
  valid <- if (numeric_x) is.finite(d$.x) else !is.na(d$.x)
  if (kind %in% c("scatter", "line")) {
    y <- as.integer(y)
    if (!column(y) || !is.numeric(data[[y]])) stop("散点图和折线图需要数值纵轴。", call. = FALSE)
    d$.y <- data[[y]]
    valid <- valid & is.finite(d$.y)
  }
  if (grouped) {
    valid <- valid & !is.na(data[[group]])
    if (is.numeric(data[[group]])) valid <- valid & is.finite(data[[group]])
    d$.group <- as.character(data[[group]])
  } else d$.group <- "全部"
  d <- d[valid, , drop = FALSE]
  if (!nrow(d)) stop("所选字段没有可配对的有效数据。", call. = FALSE)
  d$.group <- factor(d$.group)
  if (grouped && nlevels(d$.group) > 20) stop("分组超过 20 类，请选择类别较少的字段。数值分组会把不同数值视为不同类别。", call. = FALSE)
  if (isTRUE(o$facet) && grouped && nlevels(d$.group) > 12) stop("分面最多显示 12 组，请减少分组数量或关闭分面。", call. = FALSE)
  groups <- split(d, d$.group, drop = TRUE)
  if (kind %in% c("density", "violin") && any(vapply(groups, function(g) nrow(g) < 2 || length(unique(g$.x)) < 2, logical(1)))) {
    stop("密度图和小提琴图要求每组至少有两个不同的有效数值。", call. = FALSE)
  }
  label <- function(custom, fallback) if (nzchar(trimws(custom))) custom else fallback
  group_name <- if (grouped) names(data)[group] else NULL
  stats <- if (numeric_x) do.call(rbind, lapply(names(groups), function(name) {
    value <- groups[[name]]$.x
    data.frame(分组 = name, 有效数 = length(value), 均值 = mean(value), 中位数 = median(value), check.names = FALSE)
  })) else NULL
  notes <- sprintf("使用 %d 行，排除 %d 行缺失或非有限数据。", nrow(d), nrow(data) - nrow(d))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .x))
  x_axis <- names(data)[x]
  y_axis <- "频数"
  if (kind == "hist") {
    if (!is.finite(o$bins) || o$bins < 5 || o$bins > 100) stop("直方图分箱数应在 5～100 之间。", call. = FALSE)
    p <- if (grouped) p + ggplot2::geom_histogram(ggplot2::aes(fill = .group), bins = o$bins, position = "identity", alpha = o$alpha, colour = "white") else
      p + ggplot2::geom_histogram(bins = o$bins, fill = o$color, alpha = o$alpha, colour = "white")
    if (grouped && !isTRUE(o$facet)) notes <- paste(notes, "分组直方图透明叠加，使用相同分箱。")
  } else if (kind == "density") {
    y_axis <- "概率密度"
    p <- if (grouped) p + ggplot2::geom_density(ggplot2::aes(fill = .group, colour = .group), alpha = o$alpha / 2, linewidth = o$line_width) else
      p + ggplot2::geom_density(fill = o$color, colour = o$color, alpha = o$alpha / 2, linewidth = o$line_width)
    notes <- paste(notes, "每组密度曲线各自归一化，不能用曲线面积比较组样本量。")
  } else if (kind == "scatter") {
    y_axis <- names(data)[y]
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .x, y = .y))
    p <- if (grouped) p + ggplot2::geom_point(ggplot2::aes(colour = .group), alpha = o$alpha, size = o$point_size) else
      p + ggplot2::geom_point(colour = o$color, alpha = o$alpha, size = o$point_size)
    if (isTRUE(o$smooth)) {
      usable <- vapply(groups, function(g) nrow(g) >= 3 && length(unique(g$.x)) >= 2, logical(1))
      fit_data <- d[d$.group %in% names(groups)[usable], , drop = FALSE]
      if (nrow(fit_data)) {
        p <- if (grouped) p + ggplot2::geom_smooth(data = fit_data, ggplot2::aes(colour = .group, fill = .group), method = "lm", formula = y ~ x, se = isTRUE(o$ribbon), linewidth = o$line_width) else
          p + ggplot2::geom_smooth(data = fit_data, method = "lm", formula = y ~ x, se = isTRUE(o$ribbon), colour = "#d97706", fill = "#fbbf24", linewidth = o$line_width)
      }
      notes <- paste(notes, if (grouped) "回归线分别在每组拟合。" else "回归线使用简单线性回归。",
        if (isTRUE(o$ribbon)) "阴影为均值的 95% 置信带，不是预测区间。" else "",
        if (any(!usable)) "少于 3 行或横轴无变化的组不画回归线。" else "")
    }
  } else if (kind == "line") {
    y_axis <- names(data)[y]
    d <- d[order(d$.group, d$.x), , drop = FALSE]
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .x, y = .y, group = .group))
    p <- if (grouped) p + ggplot2::geom_line(ggplot2::aes(colour = .group), linewidth = o$line_width, alpha = o$alpha) else
      p + ggplot2::geom_line(colour = o$color, linewidth = o$line_width, alpha = o$alpha)
    if (isTRUE(o$points)) {
      p <- if (grouped) p + ggplot2::geom_point(ggplot2::aes(colour = .group), size = o$point_size, alpha = o$alpha) else
        p + ggplot2::geom_point(colour = o$color, size = o$point_size, alpha = o$alpha)
    }
    if (isTRUE(o$smooth)) {
      p <- if (grouped) p + ggplot2::geom_smooth(ggplot2::aes(colour = .group, fill = .group), method = "loess",
        formula = y ~ x, se = isTRUE(o$ribbon), linewidth = o$line_width) else
        p + ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = isTRUE(o$ribbon),
          colour = "#d97706", fill = "#fbbf24", linewidth = o$line_width)
      notes <- paste(notes, "趋势线使用 LOESS 局部平滑，仅用于观察趋势。")
    }
  } else if (kind == "qq") {
    x_axis <- "理论正态分位数"; y_axis <- names(data)[x]
    p <- ggplot2::ggplot(d, ggplot2::aes(sample = .x, group = .group))
    p <- if (grouped) p + ggplot2::stat_qq(ggplot2::aes(colour = .group), alpha = o$alpha, size = o$point_size) +
      ggplot2::stat_qq_line(ggplot2::aes(colour = .group), linewidth = o$line_width) else
      p + ggplot2::stat_qq(colour = o$color, alpha = o$alpha, size = o$point_size) +
        ggplot2::stat_qq_line(colour = "#d97706", linewidth = o$line_width)
    notes <- paste(notes, "点越接近参考直线，数据分布越接近正态分布；尾部偏离可提示偏态或厚尾。")
  } else if (kind == "ecdf") {
    y_axis <- "累计比例"
    p <- if (grouped) p + ggplot2::stat_ecdf(ggplot2::aes(colour = .group), linewidth = o$line_width, alpha = o$alpha) else
      p + ggplot2::stat_ecdf(colour = o$color, linewidth = o$line_width, alpha = o$alpha)
    p <- p + ggplot2::scale_y_continuous(labels = scales::label_percent())
    notes <- paste(notes, "曲线表示不超过横轴数值的观测比例，可用于比较各组分布的整体位置和离散程度。")
  } else if (kind == "bar") {
    counts <- sort(table(as.character(d$.x)), decreasing = TRUE)
    kept <- head(counts, 20)
    bar_data <- data.frame(.category = factor(names(kept), levels = names(kept)), .count = as.numeric(kept))
    p <- ggplot2::ggplot(bar_data, ggplot2::aes(.category, .count)) + ggplot2::geom_col(fill = o$color, alpha = o$alpha) +
      ggplot2::geom_text(ggplot2::aes(label = .count), vjust = -0.3, size = 3.5) + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12)))
    notes <- paste(notes, sprintf("显示 %d / %d 个类别，按频数排序；图中合计 %d 行。", length(kept), length(counts), sum(kept)))
    if (isTRUE(o$horizontal)) p <- p + ggplot2::coord_flip()
  } else {
    x_axis <- if (grouped) group_name else ""
    y_axis <- names(data)[x]
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .group, y = .x))
    if (kind == "box") {
      p <- if (grouped) p + ggplot2::geom_boxplot(ggplot2::aes(fill = .group), alpha = o$alpha, linewidth = o$line_width, outlier.shape = if (isTRUE(o$points)) NA else 19) else
        p + ggplot2::geom_boxplot(fill = o$color, alpha = o$alpha, linewidth = o$line_width, outlier.shape = if (isTRUE(o$points)) NA else 19)
    } else {
      p <- if (grouped) p + ggplot2::geom_violin(ggplot2::aes(fill = .group), alpha = o$alpha, linewidth = o$line_width, trim = FALSE) else
        p + ggplot2::geom_violin(fill = o$color, alpha = o$alpha, linewidth = o$line_width, trim = FALSE)
      p <- p + ggplot2::geom_boxplot(width = 0.12, fill = "white", outlier.shape = NA)
    }
    if (isTRUE(o$points)) p <- p + ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.12, height = 0, seed = 42), size = o$point_size, alpha = 0.45)
    if (isTRUE(o$horizontal)) p <- p + ggplot2::coord_flip()
  }
  if (kind %in% c("hist", "density") && (isTRUE(o$mean_line) || isTRUE(o$median_line))) {
    refs <- do.call(rbind, lapply(names(groups), function(name) {
      value <- groups[[name]]$.x
      rbind(if (isTRUE(o$mean_line)) data.frame(.group = name, .value = mean(value), .stat = "均值"),
        if (isTRUE(o$median_line)) data.frame(.group = name, .value = median(value), .stat = "中位数"))
    }))
    if (grouped) {
      p <- p + ggplot2::geom_vline(data = refs, ggplot2::aes(xintercept = .value, colour = .group, linetype = .stat), linewidth = o$line_width)
    } else {
      p <- p + ggplot2::geom_vline(data = refs, ggplot2::aes(xintercept = .value, colour = .stat, linetype = .stat), linewidth = o$line_width) +
        ggplot2::scale_colour_manual(values = c("均值" = "#dc2626", "中位数" = "#7c3aed"), name = "参考线")
    }
    p <- p + ggplot2::scale_linetype_manual(values = c("均值" = "dashed", "中位数" = "dotdash"), name = "参考线")
    notes <- paste(notes, if (grouped) "参考线按组计算，具体数值见下方表格。" else "参考线按当前有效数据计算，具体数值见下方表格。")
  }
  if (grouped) {
    p <- p + ggplot2::scale_fill_viridis_d(option = o$palette, name = group_name) + ggplot2::scale_colour_viridis_d(option = o$palette, name = group_name)
    if (isTRUE(o$facet) && kind %in% c("hist", "density", "scatter", "line", "qq", "ecdf")) p <- p + ggplot2::facet_wrap(ggplot2::vars(.group), ncol = 2)
  }
  theme <- switch(o$theme, classic = ggplot2::theme_classic, bw = ggplot2::theme_bw, ggplot2::theme_minimal)
  p <- p + theme(base_size = o$font_size, base_family = plotting_font_family()) + ggplot2::theme(legend.position = "bottom", plot.title.position = "plot") +
    ggplot2::labs(title = label(o$title, names(data)[x]), x = label(o$x_label, x_axis), y = label(o$y_label, y_axis))
  list(plot = p, notes = notes, stats = stats, used = nrow(d), excluded = nrow(data) - nrow(d))
}

analysis_ui <- function(id) {
  ns <- NS(id)
  condition <- function(kinds) paste(sprintf("input['%s'] === '%s'", ns("kind"), kinds), collapse = " || ")
  style_condition <- paste0("input['", ns("toggle_style"), "'] % 2 === 1")
  tagList(
    h3("ggplot2 绘图工作台"),
    fluidRow(
      column(4, selectInput(ns("kind"), "图形", c("直方图" = "hist", "密度图" = "density", "散点图" = "scatter",
        "折线趋势图" = "line", "Q-Q 正态检验图" = "qq", "经验累积分布图（ECDF）" = "ecdf",
        "相关性热图" = "correlation", "3D 散点图（可旋转）" = "scatter3d", "箱线图" = "box",
        "小提琴图" = "violin", "类别频数图" = "bar"))),
      column(4, conditionalPanel(sprintf("input['%s'] !== 'correlation'", ns("kind")),
        selectInput(ns("x"), "横轴 / 数值字段", NULL))),
      column(4, conditionalPanel(condition(c("scatter", "scatter3d", "line")), selectInput(ns("y"), "纵轴", NULL)))
    ),
    conditionalPanel(condition("correlation"),
      selectizeInput(ns("corr_variables"), "相关性字段（数值，可多选）", NULL, multiple = TRUE),
      fluidRow(column(6, selectInput(ns("corr_method"), "相关系数类型", c("Pearson 线性相关" = "pearson", "Spearman 秩相关" = "spearman"))),
        column(6, checkboxInput(ns("corr_labels"), "显示相关系数数值", TRUE)))),
    conditionalPanel(condition("scatter3d"), selectInput(ns("z"), "Z 轴（数值字段）", NULL)),
    conditionalPanel(sprintf("!['bar','correlation'].includes(input['%s'])", ns("kind")),
      selectInput(ns("group"), "分组字段（可选，仅显示 2～20 个类别的字段）",
        c("不分组" = plot_no_group_value))),
    conditionalPanel(condition(c("hist", "density")),
      fluidRow(column(4, checkboxInput(ns("mean_line"), "显示均值竖线", FALSE)),
        column(4, checkboxInput(ns("median_line"), "显示中位数竖线", FALSE)),
        column(4, conditionalPanel(condition("hist"), sliderInput(ns("bins"), "分箱数", min = 5, max = 100, value = 30))))),
    conditionalPanel(condition("scatter"), fluidRow(
      column(6, checkboxInput(ns("smooth"), "添加线性回归线", FALSE)),
      column(6, checkboxInput(ns("ribbon"), "显示 95% 均值置信带", TRUE)))),
    conditionalPanel(condition("line"), fluidRow(
      column(4, checkboxInput(ns("points"), "显示数据点", FALSE)),
      column(4, checkboxInput(ns("smooth"), "添加 LOESS 趋势线", FALSE)),
      column(4, checkboxInput(ns("ribbon"), "显示趋势置信带", TRUE)))),
    conditionalPanel(condition(c("hist", "density", "scatter", "line", "qq", "ecdf")), checkboxInput(ns("facet"), "按组分面显示（选择分组后生效）", FALSE)),
    conditionalPanel(condition(c("box", "violin")), checkboxInput(ns("points"), "叠加原始数据点", FALSE)),
    conditionalPanel(condition(c("box", "violin", "bar")), checkboxInput(ns("horizontal"), "横向显示", FALSE)),
    actionButton(ns("toggle_style"), "编辑图像", icon = icon("sliders-h")),
    conditionalPanel(style_condition, tags$div(class = "well", style = "margin-top:10px",
      textInput(ns("title"), "图表标题（留空使用字段名）"),
      fluidRow(column(4, textInput(ns("x_label"), "横轴标题（留空自动）")), column(4, textInput(ns("y_label"), "纵轴标题（留空自动）")),
        column(4, conditionalPanel(condition("scatter3d"), textInput(ns("z_label"), "Z 轴标题（留空自动）")))),
      fluidRow(column(4, selectInput(ns("theme"), "主题", c("简洁" = "minimal", "经典" = "classic", "黑白网格" = "bw"))),
        column(4, selectInput(ns("color"), "单组颜色", c("蓝色" = "#3b82f6", "绿色" = "#059669", "紫色" = "#7c3aed", "橙色" = "#ea580c"))),
        column(4, selectInput(ns("palette"), "分组配色", c("蓝绿黄" = "D", "紫红黄" = "C", "暗紫橙" = "A")))),
      fluidRow(column(3, sliderInput(ns("alpha"), "透明度", min = 0.1, max = 1, value = 0.65, step = 0.05)),
        column(3, sliderInput(ns("point_size"), "点大小", min = 1, max = 6, value = 2.5, step = 0.5)),
        column(3, sliderInput(ns("line_width"), "线宽", min = 0.2, max = 4, value = 1, step = 0.1)),
        column(3, numericInput(ns("font_size"), "字号", 12, min = 8, max = 24))),
      fluidRow(column(4, numericInput(ns("width"), "图片宽度（英寸）", 10, min = 4, max = 20)),
        column(4, numericInput(ns("height"), "图片高度（英寸）", 6, min = 3, max = 16)),
        column(4, selectInput(ns("dpi"), "导出 DPI", c(96, 150, 300), selected = 150))),
      actionButton(ns("reset_style"), "恢复默认绘图设置")
    )),
    conditionalPanel(sprintf("input['%s'] !== 'scatter3d'", ns("kind")), plotOutput(ns("plot"), height = "500px")),
    conditionalPanel(condition("scatter3d"), plotly::plotlyOutput(ns("plot3d"), height = "600px")),
    textOutput(ns("notes")),
    conditionalPanel(condition(c("hist", "density", "box", "violin", "correlation")), tableOutput(ns("group_stats"))),
    conditionalPanel(sprintf("input['%s'] !== 'scatter3d'", ns("kind")),
      downloadButton(ns("png"), "下载 PNG 图片"),
      actionButton(ns("save_png"), "保存 PNG 到项目文件夹"),
      tags$div(style = "overflow-wrap:anywhere", textOutput(ns("saved_png")))),
    tags$details(tags$summary("查看数值字段描述统计"), tableOutput(ns("summary")))
  )
}

analysis_server <- function(id, data, directory = reactive(getwd())) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$toggle_style, {
      label <- if (input$toggle_style %% 2 == 1) "收起图像编辑" else "编辑图像"
      updateActionButton(session, "toggle_style", label = label, icon = icon("sliders-h"))
    }, ignoreInit = TRUE)
    observeEvent(list(data(), input$kind), {
      d <- data()
      numeric <- which(vapply(d, is.numeric, logical(1)))
      eligible <- if (identical(input$kind, "bar")) seq_along(d) else if (identical(input$kind, "line"))
        which(vapply(d, function(x) is.numeric(x) || inherits(x, c("Date", "POSIXct", "POSIXlt")), logical(1))) else numeric
      labels <- if (length(eligible)) paste0(eligible, ". ", names(d)[eligible]) else character()
      choices <- setNames(as.character(eligible), labels)
      selected <- function(old, choices, fallback) if (length(old) == 1 && old %in% unname(choices)) old else fallback
      first <- unname(head(choices, 1))
      updateSelectInput(session, "x", choices = choices, selected = selected(input$x, choices, first))
      y_labels <- if (length(numeric)) paste0(numeric, ". ", names(d)[numeric]) else character()
      y_choices <- setNames(as.character(numeric), y_labels)
      second <- unname(if (length(y_choices) > 1) y_choices[2] else head(y_choices, 1))
      updateSelectInput(session, "y", choices = y_choices, selected = selected(input$y, y_choices, second))
      third <- unname(if (length(y_choices) > 2) y_choices[3] else tail(y_choices, 1))
      updateSelectInput(session, "z", choices = y_choices, selected = selected(input$z, y_choices, third))
      corr_selected <- intersect(input$corr_variables, unname(y_choices))
      if (length(corr_selected) < 2L) corr_selected <- unname(head(y_choices, min(6L, length(y_choices))))
      updateSelectizeInput(session, "corr_variables", choices = y_choices, selected = corr_selected)
    })
    observeEvent(list(data(), input$kind, input$x, input$y, input$z), {
      d <- data()
      current_kind <- if (length(input$kind) == 1L) input$kind else ""
      excluded <- as.integer(c(input$x,
        if (current_kind %in% c("scatter", "line")) input$y,
        if (identical(current_kind, "scatter3d")) c(input$y, input$z)))
      candidates <- groupable_columns(d, exclude = excluded)
      group_choices <- c("不分组" = plot_no_group_value)
      if (length(candidates)) {
        labels <- paste0(candidates, ". ", names(d)[candidates], "（", vapply(candidates, function(i) {
          x <- d[[i]]
          valid <- !is.na(x)
          if (is.numeric(x)) valid <- valid & is.finite(x)
          length(unique(x[valid]))
        }, integer(1)), " 类）")
        group_choices <- c(group_choices, setNames(as.character(candidates), labels))
      }
      current <- if (length(input$group) == 1 && input$group %in% unname(group_choices)) input$group else plot_no_group_value
      updateSelectInput(session, "group", choices = group_choices, selected = current)
    }, ignoreNULL = FALSE)
    active_group <- reactive({
      current_kind <- if (length(input$kind) == 1L) input$kind else ""
      if (current_kind %in% c("bar", "correlation", "function") || length(input$group) != 1 || is.na(input$group) ||
          !nzchar(input$group) || identical(input$group, plot_no_group_value)) return("")
      candidate <- suppressWarnings(as.integer(input$group))
      excluded <- as.integer(c(input$x,
        if (current_kind %in% c("scatter", "line")) input$y,
        if (identical(current_kind, "scatter3d")) c(input$y, input$z)))
      allowed <- groupable_columns(data(), exclude = excluded)
      if (length(candidate) == 1 && !is.na(candidate) && candidate %in% allowed) input$group else ""
    })
    settings <- reactive({
      defaults <- plot_defaults()
      for (name in names(defaults)) if (!is.null(input[[name]])) defaults[[name]] <- input[[name]]
      for (name in c("font_size", "width", "height", "dpi")) defaults[[name]] <- as.numeric(defaults[[name]])
      validate(need(length(defaults$font_size) == 1 && is.finite(defaults$font_size) && defaults$font_size >= 8 && defaults$font_size <= 24, "字号应在 8～24 之间。"))
      defaults
    })
    observeEvent(input$reset_style, {
      o <- plot_defaults()
      for (name in c("mean_line", "median_line", "smooth", "ribbon", "points", "facet", "horizontal", "corr_labels")) updateCheckboxInput(session, name, value = o[[name]])
      for (name in c("title", "x_label", "y_label", "z_label")) updateTextInput(session, name, value = o[[name]])
      for (name in c("theme", "color", "palette", "dpi", "corr_method")) updateSelectInput(session, name, selected = as.character(o[[name]]))
      for (name in c("bins", "alpha", "point_size", "line_width")) updateSliderInput(session, name, value = o[[name]])
      for (name in c("font_size", "width", "height")) updateNumericInput(session, name, value = o[[name]])
    })
    chart <- reactive({
      req(input$kind)
      if (identical(input$kind, "function")) {
        return(tryCatch(build_function_plot(input$function_expressions, input$function_x_min, input$function_x_max,
          input$function_points, settings(), input$function_axes, input$function_grid, input$function_roots),
          error = function(e) validate(need(FALSE, conditionMessage(e)))))
      }
      req(data())
      if (!identical(input$kind, "correlation")) req(input$x)
      tryCatch(if (identical(input$kind, "correlation")) {
        build_correlation_plot(data(), input$corr_variables, settings())
      } else if (identical(input$kind, "scatter3d")) {
        build_plotly_3d(data(), input$x, input$y, input$z, active_group(), settings())
      } else build_ggplot(data(), input$kind, input$x, input$y, active_group(), settings()),
        error = function(e) validate(need(FALSE, conditionMessage(e))))
    })
    output$plot <- renderPlot({ req(!identical(input$kind, "scatter3d")); print(chart()$plot) })
    output$plot3d <- plotly::renderPlotly({ req(identical(input$kind, "scatter3d")); chart()$plot })
    output$notes <- renderText(chart()$notes)
    output$group_stats <- renderTable(chart()$stats, digits = 3, striped = TRUE)
    output$summary <- renderTable({
      d <- data()
      indices <- which(vapply(d, is.numeric, logical(1)))
      validate(need(length(indices) > 0, "当前没有数值字段。"))
      do.call(rbind, lapply(indices, function(i) {
        x <- d[[i]]; x <- x[is.finite(x)]
        data.frame(字段 = names(d)[i], 有效数 = length(x), 均值 = if (length(x)) mean(x) else NA_real_,
          中位数 = if (length(x)) median(x) else NA_real_, 标准差 = if (length(x) > 1) sd(x) else NA_real_, check.names = FALSE)
      }))
    }, digits = 3, striped = TRUE)
    write_png <- function(file) {
      plot <- chart()$plot
      o <- settings()
      validate(need(is.finite(o$width) && o$width >= 4 && o$width <= 20 && is.finite(o$height) && o$height >= 3 && o$height <= 16 && o$dpi %in% c(96, 150, 300), "导出宽度应为 4～20 英寸，高度为 3～16 英寸，DPI 为 96、150 或 300。"))
      ggplot2::ggsave(file, plot = plot, device = "png", width = o$width, height = o$height, dpi = o$dpi, bg = "white")
    }
    output$png <- downloadHandler(filename = function() paste0("easyr-chart-", Sys.Date(), ".png"), content = write_png)
    saved_png <- reactiveVal("")
    output$saved_png <- renderText(saved_png())
    observeEvent(input$save_png, {
      if (!identical(input$kind, "function")) req(data())
      if (!input$kind %in% c("correlation", "function")) req(input$x)
      tryCatch({
        path <- save_to_workdir(directory(), "easyr-chart", ".png", write_png)
        saved_png(paste("上次保存：", path))
        showNotification("PNG 已保存到 EasyR 项目文件夹。", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })
  })
}
