source("setup.R")
library(shiny)
source("R/directory.R")
source("R/plot_editor.R")
source("R/model_evaluation.R")

actual <- factor(c("否", "否", "是", "是"), levels = c("否", "是"))
probabilities <- cbind("否" = c(0.9, 0.8, 0.2, 0.1), "是" = c(0.1, 0.2, 0.8, 0.9))
evaluation <- classification_evaluation(actual, probabilities = probabilities,
  positive_class = "是", threshold = 0.5)
stopifnot(evaluation$binary, identical(evaluation$positive_class, "是"),
  evaluation$auc == 1, evaluation$summary$数值[1] == 1,
  nrow(evaluation$per_class) == 2, nrow(evaluation$threshold_table) >= 19)

changed <- classification_evaluation(actual, probabilities = probabilities,
  positive_class = "是", threshold = 0.85)
stopifnot(changed$summary$数值[1] < evaluation$summary$数值[1],
  !identical(as.character(changed$predicted), as.character(evaluation$predicted)))

multiclass <- classification_evaluation(
  factor(c("a", "b", "c", "a", "b", "c")),
  factor(c("a", "b", "c", "a", "c", "b")))
stopifnot(!multiclass$binary, is.null(multiclass$roc), nrow(multiclass$per_class) == 3,
  all(c("Accuracy", "Macro F1", "Weighted F1") %in% multiclass$summary$指标))

regression <- regression_evaluation(c(1, 2, 3), c(1, 2, 3))
stopifnot(regression$summary$数值[1] == 0, regression$summary$数值[2] == 0,
  regression$summary$数值[3] == 1)

confusion_plot <- build_confusion_plot(evaluation, "测试模型")
roc_plot <- build_roc_plot(evaluation, "测试模型")
threshold_plot <- build_threshold_plot(evaluation, "测试模型")
stopifnot(inherits(confusion_plot, "ggplot"), inherits(roc_plot, "ggplot"),
  inherits(threshold_plot, "ggplot"))

test_directory <- tempfile("easyr-model-evaluation-")
dir.create(test_directory)
ggplot2::ggsave(file.path(test_directory, "roc.png"), roc_plot, width = 7, height = 5, dpi = 96)
stopifnot(file.info(file.path(test_directory, "roc.png"))$size > 1000)

result <- reactiveVal(list(task = "classification", actual = actual,
  predicted = factor(c("否", "否", "是", "是"), levels = c("否", "是")),
  probabilities = probabilities))
testServer(model_evaluation_server, args = list(result = result,
  directory = reactive(test_directory), model_label = "测试模型"), {
  session$setInputs(positive_class = "是", threshold = 0.85)
  stopifnot(evaluation()$binary, evaluation()$threshold == 0.85,
    identical(evaluation()$positive_class, "是"))
})

unlink(test_directory, recursive = TRUE)
cat("统一分类与回归评估、ROC、AUC、阈值比较和图形检查通过。\n")
