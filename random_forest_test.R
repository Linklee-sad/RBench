source("setup.R")
library(shiny)
source("R/directory.R")
source("R/plot_editor.R")
source("R/regression.R")
source("R/random_forest.R")

classification_data <- prepare_random_forest_data(iris, 5, 1:4, "auto")
stopifnot(classification_data$task == "classification", nrow(classification_data$data) == nrow(iris))
classification_split <- stratified_rf_split(classification_data$data$outcome, 0.8, 2026, TRUE)
stopifnot(all(table(classification_data$data$outcome[classification_split$train]) > 0),
  all(table(classification_data$data$outcome[classification_split$test]) > 0))

classification <- fit_random_forest(iris, 5, 1:4, "auto", 0.8, 100, 0, 2026)
classification_repeat <- fit_random_forest(iris, 5, 1:4, "classification", 0.8, 100, 0, 2026)
stopifnot(classification$task == "classification", classification$train_n + classification$test_n == nrow(iris),
  nrow(classification$importance) == 4, nrow(classification$details) == 3,
  classification$metrics$数值[1] >= 0.7,
  is.matrix(classification$probabilities), nrow(classification$probabilities) == classification$test_n,
  identical(as.character(classification$predicted), as.character(classification_repeat$predicted)),
  grepl("测试集", classification$report), grepl("变量重要性", classification$report))

regression <- fit_random_forest(mtcars, 1, 2:6, "regression", 0.75, 100, 0, 99)
stopifnot(regression$task == "regression", regression$train_n + regression$test_n == nrow(mtcars),
  nrow(regression$predictions) == regression$test_n, all(is.finite(regression$metrics$数值)),
  all(c("测试集 RMSE", "测试集 MAE", "测试集 R²") %in% regression$metrics$指标))

numeric_category <- data.frame(target = rep(c(0, 1), each = 10), x = seq_len(20))
stopifnot(prepare_random_forest_data(numeric_category, 1, 2, "auto")$task == "classification")
with_constant <- cbind(iris, constant = 1)
constant_result <- prepare_random_forest_data(with_constant, 5, c(1, 6), "classification")
stopifnot(identical(constant_result$predictors, "Sepal.Length"), identical(constant_result$removed, "constant"))
invalid <- tryCatch({ fit_random_forest(iris, 5, 1:4, "classification", 0.8, 100, 8, 1); "unexpected" }, error = conditionMessage)
stopifnot(grepl("候选字段数", invalid))

evaluation_plot <- build_rf_evaluation_plot(classification)
unified_evaluation <- classification_evaluation(classification$actual, classification$predicted, classification$probabilities)
importance_plot <- build_rf_importance_plot(classification)
regression_plot <- build_rf_evaluation_plot(regression)
stopifnot(inherits(evaluation_plot, "ggplot"), inherits(importance_plot, "ggplot"), inherits(regression_plot, "ggplot"),
  nrow(unified_evaluation$per_class) == 3)
test_directory <- tempfile("easyr-random-forest-")
dir.create(test_directory)
evaluation_file <- file.path(test_directory, "evaluation.png")
importance_file <- file.path(test_directory, "importance.png")
ggplot2::ggsave(evaluation_file, evaluation_plot, width = 8, height = 5, dpi = 96)
ggplot2::ggsave(importance_file, importance_plot, width = 8, height = 5, dpi = 96)
stopifnot(file.info(evaluation_file)$size > 1000, file.info(importance_file)$size > 1000)

testServer(random_forest_server, args = list(data = reactive(iris), directory = reactive(test_directory)), {
  session$setInputs(outcome = "5", predictors = c("1", "2", "3", "4"), task = "auto",
    train_ratio = 0.8, seed = 2026, ntree = 100, mtry = 0)
  session$setInputs(run = 1)
  stopifnot(!is.null(result()), result()$task == "classification", grepl("建模完成", status()))
  report_file <- output$download_report
  prediction_file <- output$download_predictions
  stopifnot(file.exists(report_file), file.exists(prediction_file),
    grepl("随机森林", paste(readLines(report_file, warn = FALSE), collapse = "")))
  session$setInputs(save_report = 1)
  stopifnot(length(list.files(test_directory, pattern = "easyr-random-forest.*[.]txt$")) == 1)
  session$setInputs(seed = 7)
  stopifnot(is.null(result()), grepl("已更新", status()))
})

unlink(test_directory, recursive = TRUE)
cat("随机森林回归、分类、分层划分、评估指标、变量重要性、图形和结果导出检查通过。\n")
