source("setup.R")
library(shiny)
source("R/directory.R")
source("R/plotting.R")

ui_html <- as.character(analysis_ui("analysis"))
stopifnot(grepl("analysis-toggle_style", ui_html, fixed = TRUE),
  grepl("编辑图像", ui_html, fixed = TRUE),
  grepl("折线趋势图", ui_html, fixed = TRUE), grepl("相关性热图", ui_html, fixed = TRUE),
  !grepl("function_expressions", ui_html, fixed = TRUE),
  grepl("Q-Q 正态检验图", ui_html, fixed = TRUE), grepl("经验累积分布图", ui_html, fixed = TRUE),
  grepl(plot_no_group_value, ui_html, fixed = TRUE))

dirty <- data.frame(value = c(1, 2, 9, NA, Inf))
stopifnot(identical(groupable_columns(iris, exclude = 1), 5L))
stopifnot(identical(groupable_columns(iris, exclude = 2), 5L))
mixed_groups <- data.frame(continuous = 1:30, two_groups = rep(c(10, 20), 15),
  category = rep(letters[1:3], 10), constant = 1, missing = NA_character_)
stopifnot(identical(groupable_columns(mixed_groups), c(2L, 3L)))
three_d <- build_plotly_3d(iris, 1, 2, 3, 5, list(point_size = 3, alpha = 0.7))
stopifnot(inherits(three_d$plot, "plotly"), three_d$used == 150, three_d$plotted == 150,
  length(plotly::plotly_build(three_d$plot)$x$data) == 3)
large_3d <- build_plotly_3d(data.frame(x = 1:1000, y = 1001:2000, z = 2001:3000), 1, 2, 3, max_points = 200)
stopifnot(large_3d$used == 1000, large_3d$plotted == 200, grepl("均匀抽取", large_3d$notes))
hist <- build_ggplot(dirty, "hist", 1, options = list(mean_line = TRUE, median_line = TRUE))
built <- ggplot2::ggplot_build(hist$plot)
stopifnot(hist$used == 3, hist$excluded == 2)
stopifnot(isTRUE(all.equal(sort(built$data[[2]]$xintercept), sort(c(4, 2)))))
stopifnot(sum(built$data[[1]]$count) == 3)

grouped <- build_ggplot(iris, "hist", 1, group = 5, options = list(mean_line = TRUE, facet = TRUE))
built <- ggplot2::ggplot_build(grouped$plot)
stopifnot(nrow(built$layout$layout) == 3)
stopifnot(isTRUE(all.equal(sort(built$data[[2]]$xintercept), sort(as.numeric(tapply(iris$Sepal.Length, iris$Species, mean))))))
stopifnot(sum(built$data[[1]]$count) == 150)

for (kind in c("density", "scatter", "box", "violin", "bar")) {
  result <- build_ggplot(iris, kind, if (kind == "bar") 5 else 1, y = 2,
    group = if (kind == "bar") NULL else 5,
    options = list(smooth = TRUE, points = TRUE, horizontal = TRUE))
  stopifnot(inherits(result$plot, "ggplot"), length(ggplot2::ggplot_build(result$plot)$data) > 0)
}
trend_data <- data.frame(date = as.Date("2026-01-01") + 0:19, value = sin((1:20) / 3),
  group = rep(c("A", "B"), 10))
line <- build_ggplot(trend_data, "line", 1, 2, 3, list(points = TRUE, smooth = TRUE))
stopifnot(inherits(line$plot, "ggplot"), line$used == 20L, length(line$plot$layers) == 3L,
  grepl("LOESS", line$notes, fixed = TRUE))
qq <- build_ggplot(iris, "qq", 1, group = 5)
ecdf <- build_ggplot(iris, "ecdf", 1, group = 5, options = list(facet = TRUE))
stopifnot(length(qq$plot$layers) == 2L, grepl("正态分布", qq$notes),
  length(ecdf$plot$layers) == 1L, grepl("观测比例", ecdf$notes))
corr <- build_correlation_plot(iris, 1:4, list(corr_method = "spearman", corr_labels = TRUE))
stopifnot(inherits(corr$plot, "ggplot"), nrow(corr$stats) == 4L, length(corr$plot$layers) == 2L,
  grepl("Spearman", corr$notes), isTRUE(all.equal(corr$stats$Sepal.Length[1], 1)))
functions <- build_function_plot("正弦 = sin(x)\n抛物线 = x^2 - 1", -4, 4, 1001,
  options = list(line_width = 1.5), mark_roots = TRUE)
stopifnot(inherits(functions$plot, "ggplot"), nrow(functions$stats) == 2L,
  length(unique(functions$plot$data$.function)) == 2L, grepl("红色圆点", functions$notes),
  any(grepl("-1", functions$stats$零点, fixed = TRUE)), any(grepl("1", functions$stats$零点, fixed = TRUE)))
tangent_root <- build_function_plot("切点 = (x - 1)^2", -10, 10, 1000, mark_roots = TRUE)
stopifnot(grepl("1", tangent_root$stats$零点, fixed = TRUE))
few <- data.frame(x = 1:4, y = c(3, 5, 6, 7), g = c("a", "a", "b", "b"))
result <- build_ggplot(few, "scatter", 1, 2, 3, list(smooth = TRUE))
stopifnot(grepl("不画回归线", result$notes), length(result$plot$layers) == 1)
fails <- function(expr, pattern) {
  message <- tryCatch({ force(expr); "unexpected success" }, error = conditionMessage)
  stopifnot(grepl(pattern, message))
}
fails(build_ggplot(data.frame(x = c(1, 1)), "density", 1), "两个不同")
fails(build_ggplot(data.frame(x = c(NA_real_, Inf)), "hist", 1), "有效数据")
fails(build_ggplot(iris, "hist", 5), "数值字段")
fails(build_ggplot(data.frame(x = 1:30, g = 1:30), "hist", 1, group = 2), "超过 20")
fails(build_plotly_3d(iris, 1, 1, 3), "三个不同")
fails(build_correlation_plot(iris, 1), "至少需要选择两个")
fails(build_function_plot('system("echo unsafe")'), "不支持")
fails(build_function_plot("y <- x^2"), "不支持")
fails(build_function_plot("sqrt(x)", 2, -2), "起点小于终点")
names(dirty)[1] <- "均值 ` / 测试"
stopifnot(build_ggplot(dirty, "hist", 1)$plot$labels$x == names(dirty)[1])

test_directory <- tempfile("rmod-ggplot-")
dir.create(test_directory)
testServer(analysis_server, args = list(data = reactive(iris), directory = reactive(test_directory)), {
  session$setInputs(kind = "hist", x = "1", y = "2", group = "", mean_line = TRUE, median_line = TRUE,
    title = "Test histogram", width = 4, height = 3, dpi = "96")
  stopifnot(chart()$plot$labels$title == "Test histogram")
  stopifnot(length(chart()$plot$layers) == 2)
  session$setInputs(mean_line = FALSE, median_line = FALSE)
  stopifnot(length(chart()$plot$layers) == 1)
  # testServer does not apply browser-side select updates, so verify the server-side
  # guard directly: a stale/high-cardinality numeric grouping cannot break the plot.
  session$setInputs(group = "3")
  session$flushReact()
  stopifnot(identical(active_group(), ""), inherits(chart()$plot, "ggplot"))
  exported <- output$png
  con <- file(exported, "rb")
  readBin(con, "raw", 16)
  dimensions <- readBin(con, "integer", 2, size = 4, endian = "big")
  close(con)
  stopifnot(identical(dimensions, c(384L, 288L)))
  session$setInputs(group = "5", facet = TRUE, mean_line = TRUE)
  stopifnot(nrow(chart()$stats) == 3)
  session$setInputs(group = plot_no_group_value)
  stopifnot(identical(active_group(), ""), nrow(chart()$stats) == 1)
  session$setInputs(kind = "scatter3d", x = "1", y = "2", z = "3", group = "5")
  stopifnot(inherits(chart()$plot, "plotly"), chart()$used == 150, nzchar(output$plot3d),
    grepl("拖动旋转", output$notes))
  session$setInputs(kind = "hist", x = "1", group = "5")
  session$setInputs(save_png = 1)
  stopifnot(length(list.files(test_directory, pattern = "[.]png$")) == 1)
  session$setInputs(kind = "correlation", corr_variables = c("1", "2", "3", "4"),
    corr_method = "pearson", corr_labels = TRUE, group = plot_no_group_value)
  session$flushReact()
  stopifnot(inherits(chart()$plot, "ggplot"), nrow(chart()$stats) == 4L,
    grepl("Pearson", output$notes))
  session$setInputs(kind = "function", function_expressions = "sin(x)\n平方 = x^2", function_x_min = -3,
    function_x_max = 3, function_points = 501, function_axes = TRUE, function_grid = TRUE, function_roots = TRUE)
  session$flushReact()
  stopifnot(inherits(chart()$plot, "ggplot"), nrow(chart()$stats) == 2L, grepl("2 个函数", output$notes))
})
unlink(test_directory, recursive = TRUE)
cat("ggplot2 函数图像、常用统计图、Plotly 3D、趋势线、相关性、分组、参考线、分面与导出检查通过。\n")
