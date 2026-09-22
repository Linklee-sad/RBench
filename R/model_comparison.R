if (!exists("ai_report_ui", mode = "function")) source("R/ai.R")
if (!exists("model_evaluation_theme", mode = "function")) source("R/model_evaluation.R")
if (!exists("advanced_elastic_fit", mode = "function")) source("R/advanced_regression.R")

comparison_number <- function(x) {
  if (!length(x) || !is.finite(x)) return("无法计算")
  format(signif(x, 4), trim = TRUE, scientific = abs(x) > 0 && abs(x) < .001)
}

prepare_model_comparison_data <- function(data, outcome, predictors, task = "auto", folds = 5) {
  outcome <- as.integer(outcome); predictors <- unique(as.integer(predictors)); folds <- as.integer(folds)
  if (length(outcome) != 1L || is.na(outcome) || !outcome %in% seq_along(data)) stop("请选择一个目标字段。", call. = FALSE)
  if (!length(predictors) || anyNA(predictors) || !all(predictors %in% seq_along(data))) stop("请至少选择一个预测字段。", call. = FALSE)
  if (outcome %in% predictors) stop("目标字段不能同时作为预测字段。", call. = FALSE)
  if (!task %in% c("auto", "regression", "classification")) stop("请选择有效的任务类型。", call. = FALSE)
  if (!is.finite(folds) || !folds %in% c(5L, 10L)) stop("交叉验证折数应为 5 或 10。", call. = FALSE)
  selected <- data[, c(outcome, predictors), drop = FALSE]
  supported <- vapply(selected, function(x) is.atomic(x) && is.null(dim(x)), logical(1))
  if (!all(supported)) stop("所选字段包含模型比较暂不支持的数据类型。", call. = FALSE)
  valid <- stats::complete.cases(selected)
  for (x in selected) if (is.numeric(x)) valid <- valid & is.finite(x)
  selected <- selected[valid, , drop = FALSE]
  if (nrow(selected) < max(30L, folds * 3L)) stop("有效数据量不足，无法进行可靠的交叉验证。", call. = FALSE)
  y <- selected[[1]]
  detected <- if (task == "auto") {
    if (!is.numeric(y) || length(unique(y)) <= 10L) "classification" else "regression"
  } else task
  if (detected == "regression") {
    if (!is.numeric(y) || length(unique(y)) < 2L) stop("回归比较要求目标字段为有变化的数值。", call. = FALSE)
    y <- as.numeric(y)
  } else {
    y <- droplevels(factor(as.character(y)))
    counts <- table(y)
    if (length(counts) < 2L) stop("分类目标至少需要两个类别。", call. = FALSE)
    if (length(counts) > 50L) stop("分类目标超过 50 类，请检查是否误选编号字段。", call. = FALSE)
    if (any(counts < folds)) stop(paste0("进行 ", folds, " 折交叉验证时，每个类别至少需要 ", folds, " 行。"), call. = FALSE)
  }
  xdata <- selected[-1]
  labels <- names(data)[predictors]
  keep <- logical(length(xdata)); removed <- character()
  for (j in seq_along(xdata)) {
    x <- xdata[[j]]; label <- labels[j]
    if (inherits(x, "Date") || inherits(x, "POSIXt")) x <- as.numeric(x)
    if (is.character(x) || is.logical(x)) x <- factor(as.character(x))
    if (is.factor(x)) {
      x <- droplevels(x)
      if (nlevels(x) > 50L) stop(paste0("预测字段“", label, "”超过 50 类，请先合并类别。"), call. = FALSE)
    }
    if (!(is.numeric(x) || is.factor(x))) stop(paste0("预测字段“", label, "”的数据类型不受支持。"), call. = FALSE)
    if (length(unique(x)) < 2L) removed <- c(removed, label) else { keep[j] <- TRUE; xdata[[j]] <- x }
  }
  if (!any(keep)) stop("所选预测字段都没有变化。", call. = FALSE)
  xdata <- xdata[, keep, drop = FALSE]; labels <- labels[keep]
  names(xdata) <- paste0("x", seq_along(xdata))
  x <- stats::model.matrix(~ . - 1, data = xdata)
  varying <- apply(x, 2, function(column) length(unique(column)) > 1L)
  x <- x[, varying, drop = FALSE]
  if (!ncol(x)) stop("字段编码后没有可用于比较的变化。", call. = FALSE)
  list(x = x, y = y, task = detected, outcome = names(data)[outcome], predictors = labels,
    used = nrow(selected), excluded = sum(!valid), removed = removed, folds = folds,
    analysis_summary = ai_analysis_summary(data.frame(.outcome = y, x, check.names = FALSE), c(names(data)[outcome], colnames(x))))
}

comparison_fold_ids <- function(y, folds, seed, classification = FALSE) {
  set.seed(as.integer(seed))
  ids <- integer(length(y))
  if (classification) {
    for (indices in split(seq_along(y), y)) ids[indices] <- sample(rep(seq_len(folds), length.out = length(indices)))
  } else ids <- sample(rep(seq_len(folds), length.out = length(y)))
  ids
}

comparison_algorithm_labels <- function() c(
  linear = "线性回归", logistic = "逻辑回归", ridge = "岭回归", lasso = "Lasso",
  decision_tree = "决策树",
  random_forest = "随机森林", svm = "支持向量机")

comparison_fit_predict <- function(algorithm, x_train, y_train, x_test, task, seed) {
  train <- data.frame(outcome = y_train, x_train, check.names = FALSE)
  test <- data.frame(x_test, check.names = FALSE)
  if (algorithm == "linear") {
    model <- stats::lm(outcome ~ ., data = train)
    return(list(predicted = as.numeric(stats::predict(model, test)), probabilities = NULL))
  }
  if (algorithm == "logistic") {
    if (task != "classification" || nlevels(y_train) != 2L) stop("逻辑回归只用于二分类。", call. = FALSE)
    model <- suppressWarnings(stats::glm(outcome ~ ., data = train, family = stats::binomial()))
    positive_probability <- as.numeric(stats::predict(model, test, type = "response"))
    if (any(!is.finite(positive_probability))) stop("逻辑回归未能产生有限概率。", call. = FALSE)
    classes <- levels(y_train); probabilities <- cbind(1 - positive_probability, positive_probability); colnames(probabilities) <- classes
    return(list(predicted = factor(ifelse(positive_probability >= .5, classes[2], classes[1]), levels = classes), probabilities = probabilities))
  }
  if (algorithm %in% c("ridge", "lasso")) {
    if (task != "regression") stop("岭回归和 Lasso 仅用于回归比较。", call. = FALSE)
    alpha <- if (algorithm == "lasso") 1 else 0
    lambdas <- advanced_lambda_grid(x_train, y_train, alpha, count = 24L)
    cv <- advanced_cv(x_train, y_train, lambdas, alpha, folds = 5L, seed = seed)
    model <- advanced_elastic_fit(x_train, y_train, cv$lambda[which.min(cv$mean_rmse)], alpha)
    return(list(predicted = predict_advanced_elastic(model, x_test), probabilities = NULL))
  }
  if (algorithm == "decision_tree") {
    model <- rpart::rpart(outcome ~ ., data = train, method = if (task == "classification") "class" else "anova",
      control = rpart::rpart.control(cp = .01, maxdepth = 6, minsplit = max(5L, floor(nrow(train) * .04)), xval = 0))
    if (task == "classification") {
      probabilities <- stats::predict(model, test, type = "prob"); classes <- levels(y_train)
      probabilities <- probabilities[, classes, drop = FALSE]
      return(list(predicted = factor(classes[max.col(probabilities, ties.method = "first")], levels = classes), probabilities = probabilities))
    }
    return(list(predicted = as.numeric(stats::predict(model, test)), probabilities = NULL))
  }
  if (algorithm == "random_forest") {
    set.seed(seed); p <- ncol(x_train); mtry <- if (task == "classification") max(1L, floor(sqrt(p))) else max(1L, floor(p / 3L))
    model <- randomForest::randomForest(x = x_train, y = y_train, ntree = 300, mtry = mtry)
    if (task == "classification") {
      probabilities <- stats::predict(model, x_test, type = "prob"); classes <- levels(y_train)
      return(list(predicted = factor(classes[max.col(probabilities[, classes, drop = FALSE], ties.method = "first")], levels = classes), probabilities = probabilities[, classes, drop = FALSE]))
    }
    return(list(predicted = as.numeric(stats::predict(model, x_test)), probabilities = NULL))
  }
  if (algorithm == "svm") {
    model <- e1071::svm(x = x_train, y = y_train,
      type = if (task == "classification") "C-classification" else "eps-regression",
      kernel = "radial", cost = 1, gamma = 1 / ncol(x_train), scale = TRUE,
      probability = task == "classification")
    prediction <- stats::predict(model, x_test, probability = task == "classification")
    if (task == "classification") {
      probabilities <- attr(prediction, "probabilities"); classes <- levels(y_train)
      probabilities <- probabilities[, classes, drop = FALSE]
      return(list(predicted = factor(prediction, levels = classes), probabilities = probabilities))
    }
    return(list(predicted = as.numeric(prediction), probabilities = NULL))
  }
  stop("包含未知算法。", call. = FALSE)
}

run_model_comparison <- function(data, outcome, predictors, task = "auto", algorithms = NULL,
                                 folds = 5, seed = 2026, progress = NULL) {
  required <- c("rpart", "randomForest", "e1071")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop(paste0("缺少模型依赖：", paste(missing, collapse = "、"), "。请重新点击 Run App 自动安装。"), call. = FALSE)
  prepared <- prepare_model_comparison_data(data, outcome, predictors, task, folds)
  allowed <- if (prepared$task == "regression") c("linear", "ridge", "lasso", "decision_tree", "random_forest", "svm") else c(if (nlevels(prepared$y) == 2L) "logistic", "decision_tree", "random_forest", "svm")
  algorithms <- unique(intersect(as.character(algorithms), allowed))
  if (!length(algorithms)) stop("请至少选择一个适用于当前任务的算法。", call. = FALSE)
  fold_id <- comparison_fold_ids(prepared$y, prepared$folds, seed, prepared$task == "classification")
  rows <- list(); failures <- character(); counter <- 0L; total <- length(algorithms) * prepared$folds
  for (algorithm in algorithms) for (fold in seq_len(prepared$folds)) {
    counter <- counter + 1L
    if (is.function(progress)) progress(counter / total, comparison_algorithm_labels()[[algorithm]], fold)
    test_index <- which(fold_id == fold); train_index <- which(fold_id != fold)
    attempt <- tryCatch(comparison_fit_predict(algorithm, prepared$x[train_index, , drop = FALSE], prepared$y[train_index],
      prepared$x[test_index, , drop = FALSE], prepared$task, as.integer(seed) + fold), error = identity)
    if (inherits(attempt, "error")) {
      failures <- c(failures, paste0(comparison_algorithm_labels()[[algorithm]], "第 ", fold, " 折：", conditionMessage(attempt)))
      next
    }
    if (prepared$task == "classification") {
      evaluation <- classification_evaluation(prepared$y[test_index], attempt$predicted, attempt$probabilities)
      values <- evaluation$summary$数值[match(c("Accuracy", "Macro Recall（平衡准确率）", "Macro F1", "Weighted F1"), evaluation$summary$指标)]
      names(values) <- c("Accuracy", "Balanced Accuracy", "Macro F1", "Weighted F1")
      if (isTRUE(evaluation$binary)) values <- c(values, AUC = evaluation$auc)
    } else {
      evaluation <- regression_evaluation(prepared$y[test_index], attempt$predicted)
      values <- evaluation$summary$数值; names(values) <- evaluation$summary$指标
    }
    rows[[length(rows) + 1L]] <- data.frame(算法 = comparison_algorithm_labels()[[algorithm]], 折 = fold,
      指标 = names(values), 数值 = as.numeric(values), check.names = FALSE)
  }
  if (!length(rows)) stop(paste0("所有算法都未能完成交叉验证。", if (length(failures)) paste0(" ", failures[1]) else ""), call. = FALSE)
  fold_results <- do.call(rbind, rows)
  keys <- unique(fold_results[c("算法", "指标")])
  summary <- do.call(rbind, lapply(seq_len(nrow(keys)), function(i) {
    subset <- fold_results$算法 == keys$算法[i] & fold_results$指标 == keys$指标[i]
    values <- fold_results$数值[subset]
    data.frame(算法 = keys$算法[i], 指标 = keys$指标[i], 平均值 = mean(values, na.rm = TRUE),
      标准差 = stats::sd(values, na.rm = TRUE), 最低 = min(values, na.rm = TRUE), 最高 = max(values, na.rm = TRUE),
      完成折数 = sum(is.finite(values)), check.names = FALSE)
  }))
  primary <- if (prepared$task == "classification") "Macro F1" else "RMSE"
  ranking <- summary[summary$指标 == primary, , drop = FALSE]
  ranking <- ranking[order(ranking$平均值, decreasing = prepared$task == "classification"), , drop = FALSE]
  ranking$排名 <- seq_len(nrow(ranking)); ranking <- ranking[, c("排名", "算法", "指标", "平均值", "标准差", "最低", "最高", "完成折数")]
  best <- if (nrow(ranking)) ranking$算法[1] else "无法确定"
  report <- c("模型比较：交叉验证报告", "一、比较设置",
    paste0("任务类型：", if (prepared$task == "classification") "分类" else "回归", "；目标字段：", prepared$outcome, "。"),
    paste0("预测字段：", paste(prepared$predictors, collapse = "、"), "。"),
    paste0("比较算法：", paste(comparison_algorithm_labels()[algorithms], collapse = "、"), "；", prepared$folds, " 折交叉验证；随机种子 ", seed, "。"),
    sprintf("使用 %d 行完整数据；排除 %d 行。每条观测恰好作为一次验证数据。", prepared$used, prepared$excluded),
    "二、主要结果",
    paste0("按 ", primary, " 的交叉验证平均值排序，当前第一名是“", best, "”。"),
    if (nrow(ranking)) paste(apply(ranking, 1, function(row) paste0("第 ", row[["排名"]], " 名：", row[["算法"]], "，平均 ", primary, " = ", comparison_number(as.numeric(row[["平均值"]])), "，折间标准差 = ", comparison_number(as.numeric(row[["标准差"]])), "。")), collapse = "\n") else "没有可用排名。",
    "三、怎样选择",
    "平均表现反映总体水平，折间标准差反映结果对样本划分的敏感程度。平均值接近时，通常优先考虑波动较小、解释更简单或更符合实际部署条件的模型。",
    "本页面使用一组通用起始参数，适合公平初筛，不等于每个算法的最优结果。确定候选模型后，应回到对应模块调整参数，并在独立测试集或外部数据上进行最终验证。",
    "四、解释边界",
    "交叉验证降低单次随机划分的偶然性，但不能修复数据泄漏、非独立观测或时间顺序问题。同一对象的重复记录应放在同一折；时间序列应使用按时间滚动验证，而不是普通随机折。",
    if (length(failures)) paste0("部分折失败：", paste(unique(failures), collapse = "；")) else "所有选定算法均完成全部交叉验证折。")
  list(task = prepared$task, outcome = prepared$outcome, predictors = prepared$predictors,
    algorithms = algorithms, folds = prepared$folds, seed = seed, fold_results = fold_results,
    summary = summary, ranking = ranking, best = best, failures = unique(failures),
    report = paste(report, collapse = "\n\n"), used = prepared$used, excluded = prepared$excluded,
    analysis_summary = prepared$analysis_summary)
}

build_model_comparison_plot <- function(result) {
  d <- result$fold_results
  primary_metrics <- if (result$task == "classification") c("Accuracy", "Balanced Accuracy", "Macro F1", "AUC") else c("RMSE", "MAE", "R²")
  d <- d[d$指标 %in% primary_metrics, , drop = FALSE]
  ggplot2::ggplot(d, ggplot2::aes(算法, 数值, fill = 算法)) +
    ggplot2::geom_boxplot(alpha = .28, outlier.shape = NA, width = .58) +
    ggplot2::geom_point(ggplot2::aes(group = 算法), position = ggplot2::position_jitter(width = .09, height = 0), size = 2, alpha = .75) +
    ggplot2::facet_wrap(~指标, scales = "free_y") + model_evaluation_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 18, hjust = 1), legend.position = "none") +
    ggplot2::labs(title = paste0(result$folds, " 折交叉验证：模型表现与波动"),
      subtitle = "每个点代表一个验证折；箱体越集中，模型对样本划分越稳定", x = NULL, y = "验证集指标")
}

model_comparison_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("模型比较"), p("让多个算法使用完全相同的交叉验证折，比较平均表现和对样本划分的敏感程度。"),
    fluidRow(column(5, selectInput(ns("outcome"), "目标字段", NULL)),
      column(7, selectizeInput(ns("predictors"), "预测字段（可多选）", NULL, multiple = TRUE))),
    fluidRow(column(4, selectInput(ns("task"), "任务类型", c("自动判断" = "auto", "回归" = "regression", "分类" = "classification"))),
      column(4, selectInput(ns("folds"), "交叉验证折数", c("5 折（推荐）" = 5, "10 折" = 10))),
      column(4, numericInput(ns("seed"), "随机种子", 2026, min = 1))),
    checkboxGroupInput(ns("algorithms"), "比较算法", choices = c("线性／逻辑回归" = "linear_family", "岭回归" = "ridge", "Lasso" = "lasso", "决策树" = "decision_tree", "随机森林" = "random_forest", "支持向量机" = "svm"),
      selected = c("linear_family", "decision_tree", "random_forest", "svm"), inline = TRUE),
    helpText("分类任务自动使用逻辑回归，且仅适用于二分类；回归任务使用线性回归。随机森林固定使用 300 棵树，其他算法也使用通用起始参数，以便先做公平初筛。"),
    actionButton(ns("run"), "开始交叉验证比较", class = "btn-primary"),
    tags$div(style = "margin:12px 0", textOutput(ns("status"))),
    tabsetPanel(
      tabPanel("模型排行榜", DT::DTOutput(ns("ranking")), h4("全部指标"), DT::DTOutput(ns("summary"))),
      tabPanel("各折表现", ggplot_editor_ui(ns("comparison_editor"), height = "560px")),
      tabPanel("专业解读", tags$div(style = "white-space:pre-wrap;line-height:1.9", textOutput(ns("report")))),
      tabPanel("AI 增强解读", ai_report_ui(ns("ai_report"))),
      tabPanel("每折结果", DT::DTOutput(ns("fold_results")))
    ), hr(), downloadButton(ns("download_report"), "下载模型比较报告 TXT"),
    downloadButton(ns("download_results"), "下载交叉验证结果 CSV"),
    actionButton(ns("save_report"), "保存模型比较报告到项目文件夹"), textOutput(ns("saved")))
}

model_comparison_server <- function(id, data, directory = reactive(getwd()), ai_config = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    result <- reactiveVal(NULL); status <- reactiveVal("选择字段和算法后，开始交叉验证比较。"); saved <- reactiveVal("")
    observeEvent(data(), {
      d <- data(); choices <- setNames(as.character(seq_along(d)), paste0(seq_along(d), ". ", names(d)))
      selected <- if (length(input$outcome) == 1L && input$outcome %in% unname(choices)) input$outcome else unname(head(choices, 1))
      updateSelectInput(session, "outcome", choices = choices, selected = selected)
    })
    observeEvent(list(data(), input$outcome), {
      d <- data(); eligible <- setdiff(seq_along(d), as.integer(input$outcome)); labels <- if (length(eligible)) paste0(eligible, ". ", names(d)[eligible]) else character()
      choices <- setNames(as.character(eligible), labels); updateSelectizeInput(session, "predictors", choices = choices, selected = intersect(input$predictors, unname(choices)))
    })
    observe({ result(NULL); status("数据、字段或设置已更新，请重新开始比较。"); data(); input$outcome; input$predictors; input$task; input$folds; input$seed; input$algorithms }, priority = 100)
    observeEvent(input$run, {
      result(NULL)
      tryCatch({
        algorithms <- setdiff(input$algorithms, "linear_family")
        if ("linear_family" %in% input$algorithms) {
          detected <- prepare_model_comparison_data(data(), input$outcome, input$predictors, input$task, input$folds)$task
          algorithms <- c(if (detected == "regression") "linear" else "logistic", algorithms)
        }
        fitted <- withProgress(message = "正在进行交叉验证", value = 0, {
          run_model_comparison(data(), input$outcome, input$predictors, input$task, algorithms,
            input$folds, input$seed, progress = function(value, algorithm, fold) {
              setProgress(value = value, detail = paste0(algorithm, " · 第 ", fold, " 折"))
            })
        })
        result(fitted); status(paste0("比较完成：", fitted$folds, " 折交叉验证；当前排名第一：", fitted$best, "。"))
      }, error = function(e) { message <- if (inherits(e, "shiny.silent.error")) "请先导入数据并选择字段。" else conditionMessage(e); status(paste("未能比较：", message)); showNotification(message, type = "error", duration = 10) })
    })
    output$status <- renderText(status()); output$report <- renderText({ req(result()); result()$report })
    ai_report_server("ai_report", ai_config, "模型比较与交叉验证", reactive(if (is.null(result())) "" else result()$report), reactive(if (is.null(result())) NULL else ai_model_context("模型比较与交叉验证", result())), data)
    output$ranking <- DT::renderDT({ req(result()); DT::datatable(result()$ranking, rownames = FALSE, options = list(dom = "t")) |> DT::formatRound(c("平均值", "标准差", "最低", "最高"), 4) })
    output$summary <- DT::renderDT({ req(result()); DT::datatable(result()$summary, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE)) |> DT::formatRound(c("平均值", "标准差", "最低", "最高"), 4) })
    output$fold_results <- DT::renderDT({ req(result()); DT::datatable(result()$fold_results, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE)) |> DT::formatRound("数值", 4) })
    comparison_plot <- reactive({ req(result()); build_model_comparison_plot(result()) })
    ggplot_editor_server("comparison_editor", comparison_plot, directory, "easyr-model-comparison")
    write_report <- function(file) { req(result()); writeLines(enc2utf8(result()$report), file, useBytes = TRUE) }
    write_results <- function(file) { req(result()); con <- file(file, "wb"); on.exit(close(con)); writeBin(charToRaw("\ufeff"), con); lines <- capture.output(write.csv(result()$fold_results, row.names = FALSE, na = "")); writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n"))), con) }
    output$download_report <- downloadHandler(filename = function() paste0("easyr-model-comparison-", Sys.Date(), ".txt"), content = write_report)
    output$download_results <- downloadHandler(filename = function() paste0("easyr-cross-validation-", Sys.Date(), ".csv"), content = write_results)
    output$saved <- renderText(saved())
    observeEvent(input$save_report, { req(result()); tryCatch({ path <- save_to_workdir(directory(), "easyr-model-comparison", ".txt", write_report); saved(paste("上次保存：", path)) }, error = function(e) showNotification(conditionMessage(e), type = "error")) })
    result
  })
}
