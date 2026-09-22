source("setup.R")
library(shiny)
source("R/directory.R")
source("R/plot_editor.R")
source("R/model_evaluation.R")
source("R/algorithm_tutorials.R")
source("R/logistic_regression.R")

set.seed(41)
n <- 240
x1 <- stats::rnorm(n)
x2 <- stats::rnorm(n)
group <- factor(sample(c("A", "B"), n, replace = TRUE))
probability <- stats::plogis(-0.25 + 1.4 * x1 - 0.8 * x2 + 0.5 * (group == "B"))
outcome <- ifelse(stats::runif(n) < probability, "事件", "未发生")
example <- data.frame(outcome, x1, x2, group, constant = 1, check.names = FALSE)

prepared <- prepare_logistic_data(example, 1, 2:4, "事件")
stopifnot(prepared$positive_class == "事件", prepared$negative_class == "未发生",
  nrow(prepared$data) == n, length(prepared$predictors) == 3)
split <- logistic_split(prepared$data$outcome, .8, 2026)
stopifnot(all(table(prepared$data$outcome[split$train]) > 0), all(table(prepared$data$outcome[split$test]) > 0))

fit <- fit_logistic_regression(example, 1, 2:4, "事件", .8, 2026)
repeat_fit <- fit_logistic_regression(example, 1, 2:4, "事件", .8, 2026)
stopifnot(fit$task == "classification", fit$train_n + fit$test_n == n,
  fit$positive_class == "事件", nrow(fit$probabilities) == fit$test_n,
  identical(colnames(fit$probabilities), c("未发生", "事件")),
  all(abs(rowSums(fit$probabilities) - 1) < 1e-10),
  identical(as.character(fit$predicted), as.character(repeat_fit$predicted)),
  all(c("优势比OR", "OR下限95", "OR上限95", "p值") %in% names(fit$coefficients)),
  grepl("ROC AUC", fit$report), grepl("解释边界", fit$report))
evaluation <- classification_evaluation(fit$actual, fit$predicted, fit$probabilities, "事件", .5)
stopifnot(evaluation$binary, is.finite(evaluation$auc), nrow(evaluation$per_class) == 2)

removed <- prepare_logistic_data(example, 1, c(2, 5), "事件")
stopifnot(identical(removed$predictors, "x1"), identical(removed$removed, "constant"))
invalid_target <- tryCatch({ prepare_logistic_data(iris, 5, 1:4); "unexpected" }, error = conditionMessage)
stopifnot(grepl("两个类别", invalid_target))

test_directory <- tempfile("easyr-logistic-"); dir.create(test_directory)
testServer(logistic_regression_server, args = list(data = reactive(example), directory = reactive(test_directory)), {
  session$setInputs(outcome = "1", predictors = c("2", "3", "4"), positive_class = "事件",
    train_ratio = .8, seed = 2026)
  session$setInputs(run = 1)
  stopifnot(!is.null(result()), result()$positive_class == "事件", grepl("建模完成", status()))
  report_file <- output$download_report; prediction_file <- output$download_predictions
  stopifnot(file.exists(report_file), file.exists(prediction_file),
    grepl("逻辑回归", paste(readLines(report_file, warn = FALSE), collapse = "")))
  session$setInputs(save_report = 1)
  stopifnot(length(list.files(test_directory, pattern = "easyr-logistic-regression.*[.]txt$")) == 1)
  session$setInputs(seed = 7)
  stopifnot(is.null(result()), grepl("已更新", status()))
})

unlink(test_directory, recursive = TRUE)
cat("逻辑回归数据准备、分层划分、优势比、概率预测、统一评估、报告和导出检查通过。\n")
