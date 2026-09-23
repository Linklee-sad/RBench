source("setup.R")
library(shiny)
source("R/directory.R")
source("R/plotting.R")
source("R/function_plotter.R")

rendered <- htmltools::renderTags(function_plotter_ui("functions"))$html
stopifnot(
  grepl("function-plotter-sidebar", rendered, fixed = TRUE),
  grepl("添加函数", rendered, fixed = TRUE),
  grepl("每个函数的绘图点数", rendered, fixed = TRUE),
  grepl("function_rows", rendered, fixed = TRUE),
  grepl("图像设置", rendered, fixed = TRUE),
  grepl("下载 PNG", rendered, fixed = TRUE),
  !grepl("函数数量", rendered, fixed = TRUE)
)

specifications <- list(
  modifyList(parse_function_lines("sin(x)")[[1]], list(label = "正弦", x_min = -10, x_max = 0)),
  modifyList(parse_function_lines("(x - 2)^2")[[1]], list(label = "抛物线", x_min = 0, x_max = 4))
)
result <- build_function_plot_specs(specifications, points = 501, mark_roots = TRUE)
stopifnot(
  inherits(result$plot, "ggplot"),
  nrow(result$stats) == 2L,
  identical(result$stats$定义域, c("x ∈ [-10, 0]", "x ∈ [0, 4]")),
  min(result$plot$data$.x[result$plot$data$.function == "正弦"]) == -10,
  max(result$plot$data$.x[result$plot$data$.function == "正弦"]) == 0,
  min(result$plot$data$.x[result$plot$data$.function == "抛物线"]) == 0,
  max(result$plot$data$.x[result$plot$data$.function == "抛物线"]) == 4,
  grepl("2", result$stats$零点[2], fixed = TRUE)
)
ellipse <- build_function_plot_specs(list(list(type = "parametric", label = "椭圆",
  x_expression = parse_function_lines("3*cos(t)", variable = "t")[[1]]$expression,
  y_expression = parse_function_lines("2*sin(t)", variable = "t")[[1]]$expression,
  t_min = 0, t_max = 2 * pi, color = "#6d597a")), points = 1001)
stopifnot(inherits(ellipse$plot, "ggplot"), identical(ellipse$plot$coordinates$ratio, 1),
  abs(max(ellipse$plot$data$.x) - 3) < 1e-6, abs(min(ellipse$plot$data$.x) + 3) < 1e-4,
  abs(max(ellipse$plot$data$.y) - 2) < 1e-6, grepl("t ∈", ellipse$stats$定义域, fixed = TRUE))

test_directory <- tempfile("easyr-function-plotter-")
dir.create(test_directory)
testServer(function_plotter_server, args = list(directory = reactive(test_directory)), {
  session$setInputs(points = 501, show_axes = TRUE, show_grid = TRUE, mark_roots = TRUE,
    expression_1 = "sin(x)", x_min_1 = -6, x_max_1 = 0,
    title = "不同定义域", x_label = "x", y_label = "f(x)", theme = "minimal", color = "#3b82f6",
    palette = "D", font_size = 12, alpha = 0.8, line_width = 1, width = 4, height = 3, dpi = "96")
  session$flushReact()
  stopifnot(identical(function_ids(), 1L))
  session$setInputs(add_function = 1)
  session$flushReact()
  stopifnot(identical(function_ids(), c(1L, 2L)))
  session$setInputs(expression_2 = "x^2", x_min_2 = 1, x_max_2 = 3)
  session$flushReact()
  stopifnot(inherits(chart()$plot, "ggplot"), chart()$plot$labels$title == "不同定义域",
    identical(chart()$stats$定义域, c("x ∈ [-6, 0]", "x ∈ [1, 3]")))
  exported <- output$png
  stopifnot(file.exists(exported), file.info(exported)$size > 0)
  session$setInputs(save_png = 1)
  stopifnot(length(list.files(test_directory, pattern = "[.]png$")) == 1L)
  session$setInputs(mode_1 = "parametric", x_expression_1 = "3*cos(t)", y_expression_1 = "2*sin(t)",
    t_min_1 = 0, t_max_1 = 2 * pi)
  session$flushReact()
  stopifnot(identical(chart()$plot$coordinates$ratio, 1), grepl("t ∈", chart()$stats$定义域[1], fixed = TRUE))
})
unlink(test_directory, recursive = TRUE)

cat("独立函数绘图入口、逐函数定义域、零点、样式和 PNG 导出检查通过。\n")
