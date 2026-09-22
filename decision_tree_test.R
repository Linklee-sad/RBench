source("setup.R")
library(shiny)
source("R/directory.R")
source("R/plot_editor.R")
source("R/model_evaluation.R")
source("R/algorithm_tutorials.R")
source("R/regression.R")
source("R/random_forest_tutorial.R")
source("R/random_forest.R")
source("R/decision_tree.R")

classification <- fit_decision_tree(iris, 5, 1:4, "classification", .8, .01, 4, 10, 2026)
repeat_classification <- fit_decision_tree(iris, 5, 1:4, "classification", .8, .01, 4, 10, 2026)
stopifnot(classification$task == "classification", classification$train_n + classification$test_n == nrow(iris),
  classification$leaves >= 2, classification$depth <= 4,
  nrow(classification$probabilities) == classification$test_n,
  identical(as.character(classification$predicted), as.character(repeat_classification$predicted)),
  classification$metrics$数值[classification$metrics$指标 == "Accuracy"] >= .7,
  nrow(classification$importance) >= 1, grepl("复杂度", classification$report))

regression <- fit_decision_tree(mtcars, 1, 2:6, "regression", .75, .01, 4, 5, 99)
stopifnot(regression$task == "regression", regression$train_n + regression$test_n == nrow(mtcars),
  nrow(regression$predictions) == regression$test_n,
  all(c("RMSE", "MAE", "R²") %in% regression$metrics$指标))

tree_plot <- build_decision_tree_plot(classification)
importance_plot <- build_tree_importance_plot(classification)
regression_plot <- build_tree_regression_plot(regression)
stopifnot(inherits(tree_plot, "ggplot"), inherits(importance_plot, "ggplot"), inherits(regression_plot, "ggplot"))
test_directory <- tempfile("easyr-tree-"); dir.create(test_directory)
tree_file <- file.path(test_directory, "tree.png")
ggplot2::ggsave(tree_file, tree_plot, width = 9, height = 6, dpi = 96)
stopifnot(file.info(tree_file)$size > 1000)

invalid <- tryCatch({ fit_decision_tree(iris, 5, 1:4, "classification", cp = .8); "unexpected" }, error = conditionMessage)
stopifnot(grepl("cp", invalid))

testServer(decision_tree_server, args = list(data = reactive(iris), directory = reactive(test_directory)), {
  session$setInputs(outcome = "5", predictors = c("1", "2", "3", "4"), task = "classification",
    train_ratio = .8, cp = .01, maxdepth = 4, minsplit = 10, seed = 2026)
  session$setInputs(run = 1)
  stopifnot(!is.null(result()), result()$task == "classification", grepl("建模完成", status()))
  report_file <- output$download_report; prediction_file <- output$download_predictions
  stopifnot(file.exists(report_file), file.exists(prediction_file), grepl("决策树", paste(readLines(report_file, warn = FALSE), collapse = "")))
  session$setInputs(save_report = 1)
  stopifnot(length(list.files(test_directory, pattern = "easyr-decision-tree.*[.]txt$")) == 1)
  session$setInputs(cp = .02)
  stopifnot(is.null(result()), grepl("已更新", status()))
})

unlink(test_directory, recursive = TRUE)
cat("决策树回归、分类、剪枝参数、树结构、变量重要性、统一评估和导出检查通过。\n")
