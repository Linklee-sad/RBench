source("setup.R")
library(shiny)
source("R/directory.R")
source("R/plot_editor.R")
source("R/model_evaluation.R")
source("R/algorithm_tutorials.R")
source("R/advanced_regression.R")
source("R/model_comparison.R")

classification_data <- transform(iris, binary = factor(ifelse(Species == "setosa", "是", "否")))
prepared_classification <- prepare_model_comparison_data(classification_data, 6, 1:4, "classification", 5)
stopifnot(prepared_classification$task == "classification", nrow(prepared_classification$x) == nrow(iris),
  nlevels(prepared_classification$y) == 2)
folds <- comparison_fold_ids(prepared_classification$y, 5, 2026, TRUE)
stopifnot(identical(sort(unique(folds)), 1:5), all(table(prepared_classification$y, folds) > 0))

classification <- run_model_comparison(classification_data, 6, 1:4, "classification",
  c("logistic", "decision_tree", "random_forest", "svm"), 5, 2026)
stopifnot(classification$task == "classification", nrow(classification$ranking) == 4,
  all(classification$ranking$指标 == "Macro F1"),
  all(c("Accuracy", "Balanced Accuracy", "Macro F1", "Weighted F1", "AUC") %in% classification$fold_results$指标),
  all(classification$ranking$完成折数 == 5), nzchar(classification$best), grepl("交叉验证", classification$report))

regression <- run_model_comparison(mtcars, 1, 2:6, "regression",
  c("linear", "ridge", "lasso", "decision_tree", "random_forest", "svm"), 5, 99)
stopifnot(regression$task == "regression", nrow(regression$ranking) == 6,
  all(regression$ranking$指标 == "RMSE"), all(c("RMSE", "MAE", "R²") %in% regression$fold_results$指标),
  all(regression$ranking$完成折数 == 5))

plot <- build_model_comparison_plot(classification)
stopifnot(inherits(plot, "ggplot"))
test_directory <- tempfile("easyr-comparison-"); dir.create(test_directory)
plot_file <- file.path(test_directory, "comparison.png")
ggplot2::ggsave(plot_file, plot, width = 10, height = 6, dpi = 96)
stopifnot(file.info(plot_file)$size > 1000)

too_few <- data.frame(target = factor(c(rep("a", 4), rep("b", 26))), x = seq_len(30))
invalid <- tryCatch({ prepare_model_comparison_data(too_few, 1, 2, "classification", 5); "unexpected" }, error = conditionMessage)
stopifnot(grepl("每个类别", invalid))

testServer(model_comparison_server, args = list(data = reactive(classification_data), directory = reactive(test_directory)), {
  session$setInputs(outcome = "6", predictors = c("1", "2", "3", "4"), task = "classification",
    folds = "5", seed = 2026, algorithms = c("linear_family", "decision_tree"))
  session$setInputs(run = 1)
  stopifnot(!is.null(result()), nrow(result()$ranking) == 2, grepl("比较完成", status()))
  report_file <- output$download_report; results_file <- output$download_results
  stopifnot(file.exists(report_file), file.exists(results_file), grepl("模型比较", paste(readLines(report_file, warn = FALSE), collapse = "")))
  session$setInputs(save_report = 1)
  stopifnot(length(list.files(test_directory, pattern = "easyr-model-comparison.*[.]txt$")) == 1)
})

unlink(test_directory, recursive = TRUE)
cat("分类与回归交叉验证、统一折划分、模型排行榜、波动图、报告和结果导出检查通过。\n")
