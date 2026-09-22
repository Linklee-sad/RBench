source("setup.R")
library(shiny)
source("R/algorithm_tutorials.R")

keys <- c("regression", "logistic", "advanced_regression", "decision_tree", "svm", "pca", "kmeans", "timeseries")
expected_steps <- c(regression = 6L, logistic = 6L, advanced_regression = 6L, decision_tree = 6L, svm = 6L, pca = 6L, kmeans = 6L, timeseries = 7L)

for (key in keys) {
  lessons <- algorithm_tutorial_lessons(key)
  stopifnot(length(lessons) == expected_steps[[key]],
    all(vapply(lessons, length, integer(1)) == 4L),
    all(vapply(lessons, function(x) all(nzchar(unlist(x))), logical(1))))
  plots <- lapply(seq_along(lessons), function(step) algorithm_tutorial_plot(key, step))
  stopifnot(all(vapply(plots, inherits, logical(1), "ggplot")))
}

interactive_plots <- list(
  algorithm_tutorial_plot("regression", 2, list(sample_n = 80, slope = -2, noise = .5, outlier = TRUE), 1),
  algorithm_tutorial_plot("logistic", 5, list(sample_n = 160, effect = 2, threshold = .35), 1),
  algorithm_tutorial_plot("advanced_regression", 5, list(correlation = .9, lambda = .4, curve = 1.4), 1),
  algorithm_tutorial_plot("decision_tree", 4, list(maxdepth = 4, cp = .005, overlap = .4), 1),
  algorithm_tutorial_plot("svm", 4, list(shape = "curved", kernel = "radial", cost = 5, gamma = 2), 1),
  algorithm_tutorial_plot("pca", 5, list(correlation = -.7, scale_ratio = 4, standardize = FALSE), 1),
  algorithm_tutorial_plot("kmeans", 3, list(true_groups = 4, centers = 2, overlap = 1, standardize = TRUE), 1),
  algorithm_tutorial_plot("timeseries", 6, list(trend_strength = -.1, seasonal_strength = 9, noise = 3, horizon = 24), 1)
)
stopifnot(all(vapply(interactive_plots, inherits, logical(1), "ggplot")))

testServer(algorithm_tutorial_server, args = list(key = "regression"), {
  session$setInputs(sample_n = 40, slope = 1.5, noise = 2, outlier = FALSE)
  session$flushReact()
  stopifnot(step() == 1L, parameters()$sample_n == 40, inherits(tutorial_plot(), "ggplot"),
    nzchar(output$progress), nzchar(output$lesson), nzchar(output$plot), nzchar(output$stats))
  session$setInputs(`next` = 1); session$flushReact(); stopifnot(step() == 2L)
  session$setInputs(slope = -2, noise = .5, outlier = TRUE); session$flushReact()
  stopifnot(parameters()$slope == -2, parameters()$noise == .5, isTRUE(parameters()$outlier), inherits(tutorial_plot(), "ggplot"))
  session$setInputs(previous = 1); session$flushReact(); stopifnot(step() == 1L)
  session$setInputs(reset = 1); session$flushReact(); stopifnot(step() == 1L)
})

cat("线性回归、逻辑回归、高级回归、决策树、SVM、PCA、K-means 和时间序列交互参数、分步演示、示意图及手动翻页检查通过。\n")
