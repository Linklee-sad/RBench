source("setup.R")
library(shiny)
source("R/directory.R")
source("R/plot_editor.R")
source("R/model_evaluation.R")
source("R/algorithm_tutorials.R")
source("R/advanced_regression.R")

set.seed(12)
n <- 120
x1 <- stats::rnorm(n); x2 <- .88 * x1 + stats::rnorm(n, 0, .35)
group <- factor(sample(c("A", "B"), n, replace = TRUE))
y <- 2 + 1.7 * x1 - .8 * x2 + 1.2 * x1^2 + .7 * (group == "B") + stats::rnorm(n, 0, .6)
example <- data.frame(y, x1, x2, group, constant = 1)

prepared <- prepare_advanced_regression_data(example, 1, 2:4, degree = 2, interactions = TRUE)
stopifnot(nrow(prepared$x) == n, ncol(prepared$x) > 4, any(grepl("\\^2", prepared$feature_labels)), any(grepl(":", prepared$feature_labels)))

base_fit <- advanced_elastic_fit(as.matrix(example[c("x1", "x2")]), y, lambda = .1, alpha = 1)
prediction <- predict_advanced_elastic(base_fit, as.matrix(example[c("x1", "x2")]))
stopifnot(length(prediction) == n, all(is.finite(prediction)), length(base_fit$coefficients) == 2)

lasso <- fit_advanced_regression(example, 1, 2:4, "lasso", 2, TRUE, 5, "min", .8, 2026)
ridge <- fit_advanced_regression(example, 1, 2:4, "ridge", 1, FALSE, 5, "1se", .8, 2026)
stopifnot(lasso$task == "regression", lasso$method == "lasso", lasso$train_n + lasso$test_n == n,
  is.finite(lasso$lambda), nrow(lasso$cv) > 20, nrow(lasso$path) > 20,
  all(c("RMSE", "MAE", "R²") %in% lasso$metrics$指标), grepl("Lasso", lasso$report),
  ridge$method == "ridge", ridge$alpha == 0, ridge$selected_count > 0)

plots <- list(build_advanced_cv_plot(lasso), build_advanced_path_plot(lasso), build_advanced_prediction_plot(lasso))
stopifnot(all(vapply(plots, inherits, logical(1), "ggplot")))
test_directory <- tempfile("easyr-advanced-"); dir.create(test_directory)
plot_file <- file.path(test_directory, "cv.png"); ggplot2::ggsave(plot_file, plots[[1]], width = 8, height = 5, dpi = 96)
stopifnot(file.info(plot_file)$size > 1000)

removed <- prepare_advanced_regression_data(example, 1, c(2, 5), 1, FALSE)
stopifnot(identical(removed$predictors, "x1"), identical(removed$removed, "constant"))

testServer(advanced_regression_server, args = list(data = reactive(example), directory = reactive(test_directory)), {
  session$setInputs(outcome = "1", predictors = c("2", "3", "4"), method = "lasso", degree = "2",
    interactions = FALSE, folds = "5", selection = "min", train_ratio = .8, seed = 2026)
  session$setInputs(run = 1)
  stopifnot(!is.null(result()), result()$method == "lasso", grepl("建模完成", status()))
  report_file <- output$download_report; prediction_file <- output$download_predictions
  stopifnot(file.exists(report_file), file.exists(prediction_file), grepl("高级回归", paste(readLines(report_file, warn = FALSE), collapse = "")))
  session$setInputs(save_report = 1)
  stopifnot(length(list.files(test_directory, pattern = "easyr-advanced-regression.*[.]txt$")) == 1)
})

unlink(test_directory, recursive = TRUE)
cat("高级回归特征展开、岭回归、Lasso、交叉验证、系数路径、测试集评估和导出检查通过。\n")
