source("setup.R")
library(shiny)
source("R/directory.R")
source("R/plot_editor.R")
source("R/svm.R")

classification_data <- prepare_svm_data(iris, 5, 1:4, "auto")
stopifnot(classification_data$task == "classification", nrow(classification_data$x) == nrow(iris), ncol(classification_data$x) == 4)
classification_split <- svm_train_test_split(classification_data$y, 0.8, 2026, TRUE)
stopifnot(all(table(classification_data$y[classification_split$train]) > 0), all(table(classification_data$y[classification_split$test]) > 0))

classification <- fit_svm_analysis(iris, 5, 1:4, "auto", 0.8, "radial", 1, 0, 3, 0, 0.1, TRUE, 2026)
classification_repeat <- fit_svm_analysis(iris, 5, 1:4, "classification", 0.8, "radial", 1, 0, 3, 0, 0.1, TRUE, 2026)
stopifnot(classification$task == "classification", classification$train_n + classification$test_n == nrow(iris),
  nrow(classification$details) == 3, sum(classification$support$支持向量数) == nrow(classification$model$SV),
  classification$metrics$数值[1] >= 0.7,
  is.matrix(classification$probabilities), nrow(classification$probabilities) == classification$test_n,
  identical(as.character(classification$predicted), as.character(classification_repeat$predicted)),
  grepl("支持向量", classification$report), grepl("参数含义", classification$report))

regression <- fit_svm_analysis(mtcars, 1, 2:6, "regression", 0.75, "linear", 10, 0, 3, 0, 0.1, TRUE, 99)
stopifnot(regression$task == "regression", regression$train_n + regression$test_n == nrow(mtcars),
  nrow(regression$predictions) == regression$test_n, all(is.finite(regression$metrics$数值)),
  all(c("测试集 RMSE", "测试集 MAE", "测试集 R²") %in% regression$metrics$指标))

numeric_category <- data.frame(target = rep(c(0, 1), each = 10), x = seq_len(20))
stopifnot(prepare_svm_data(numeric_category, 1, 2, "auto")$task == "classification")
with_factor <- data.frame(target = rep(c("a", "b"), each = 10), group = rep(c("x", "y"), 10), value = seq_len(20))
factor_result <- prepare_svm_data(with_factor, 1, 2:3, "classification")
stopifnot(ncol(factor_result$x) >= 3)
with_constant <- cbind(iris, constant = 1)
constant_result <- prepare_svm_data(with_constant, 5, c(1, 6), "classification")
stopifnot(identical(constant_result$predictors, "Sepal.Length"), identical(constant_result$removed, "constant"))
invalid <- tryCatch({ fit_svm_analysis(iris, 5, 1:4, "classification", cost = 0); "unexpected" }, error = conditionMessage)
stopifnot(grepl("成本参数", invalid))

classification_plot <- build_svm_evaluation_plot(classification)
unified_evaluation <- classification_evaluation(classification$actual, classification$predicted, classification$probabilities)
regression_plot <- build_svm_evaluation_plot(regression)
stopifnot(inherits(classification_plot, "ggplot"), inherits(regression_plot, "ggplot"),
  nrow(unified_evaluation$per_class) == 3)
test_directory <- tempfile("easyr-svm-"); dir.create(test_directory)
plot_file <- file.path(test_directory, "evaluation.png")
ggplot2::ggsave(plot_file, classification_plot, width = 8, height = 5, dpi = 96)
stopifnot(file.info(plot_file)$size > 1000)

testServer(svm_server, args = list(data = reactive(iris), directory = reactive(test_directory)), {
  session$setInputs(outcome = "5", predictors = c("1", "2", "3", "4"), task = "auto",
    train_ratio = 0.8, kernel = "radial", cost = 1, gamma = 0, degree = 3,
    coef0 = 0, epsilon = 0.1, scale = TRUE, seed = 2026)
  session$setInputs(run = 1)
  stopifnot(!is.null(result()), result()$task == "classification", grepl("建模完成", status()))
  report_file <- output$download_report; prediction_file <- output$download_predictions
  stopifnot(file.exists(report_file), file.exists(prediction_file),
    grepl("支持向量机", paste(readLines(report_file, warn = FALSE), collapse = "")))
  session$setInputs(save_report = 1)
  stopifnot(length(list.files(test_directory, pattern = "easyr-svm.*[.]txt$")) == 1)
  session$setInputs(cost = 2)
  stopifnot(is.null(result()), grepl("已更新", status()))
})

unlink(test_directory, recursive = TRUE)
cat("支持向量机分类、回归、类别编码、分层划分、参数验证、评估图和结果导出检查通过。\n")
