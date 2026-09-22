if (!exists("ai_report_ui", mode = "function")) source("R/ai.R")
if (!exists("rf_tutorial_ui", mode = "function")) source("R/random_forest_tutorial.R")
if (!exists("model_evaluation_ui", mode = "function")) source("R/model_evaluation.R")

rf_number <- function(x) {
  if (!length(x) || !is.finite(x)) return("无法计算")
  format(signif(x, 4), trim = TRUE, scientific = abs(x) > 0 && abs(x) < 0.001)
}

prepare_random_forest_data <- function(data, outcome, predictors, task = "auto") {
  outcome <- as.integer(outcome)
  predictors <- unique(as.integer(predictors))
  if (length(outcome) != 1L || is.na(outcome) || !outcome %in% seq_along(data)) {
    stop("请选择一个目标字段。", call. = FALSE)
  }
  if (!length(predictors) || anyNA(predictors) || !all(predictors %in% seq_along(data))) {
    stop("请至少选择一个预测字段。", call. = FALSE)
  }
  if (outcome %in% predictors) stop("目标字段不能同时作为预测字段。", call. = FALSE)
  if (!task %in% c("auto", "regression", "classification")) stop("请选择有效的任务类型。", call. = FALSE)

  selected <- data[, c(outcome, predictors), drop = FALSE]
  supported <- vapply(selected, function(x) is.atomic(x) && is.null(dim(x)), logical(1))
  if (!all(supported)) stop("所选字段包含随机森林暂不支持的数据类型。", call. = FALSE)
  valid <- stats::complete.cases(selected)
  for (x in selected) if (is.numeric(x)) valid <- valid & is.finite(x)
  selected <- selected[valid, , drop = FALSE]
  if (nrow(selected) < 10L) stop("有效数据不足 10 行，无法可靠划分训练集和测试集。", call. = FALSE)

  y <- selected[[1]]
  detected <- if (task == "auto") {
    if (!is.numeric(y) || length(unique(y)) <= 10L) "classification" else "regression"
  } else task
  if (detected == "regression") {
    if (!is.numeric(y)) stop("随机森林回归要求目标字段为数值。", call. = FALSE)
    if (length(unique(y)) < 2L) stop("目标字段没有变化，无法建立回归模型。", call. = FALSE)
    selected[[1]] <- as.numeric(y)
  } else {
    selected[[1]] <- droplevels(factor(as.character(y)))
    counts <- table(selected[[1]])
    if (length(counts) < 2L) stop("分类目标至少需要两个类别。", call. = FALSE)
    if (length(counts) > 50L) stop("分类目标超过 50 类，请检查是否误选了编号字段。", call. = FALSE)
    if (any(counts < 2L)) stop("每个目标类别至少需要 2 行，才能同时进入训练集和测试集。", call. = FALSE)
  }

  kept <- logical(length(predictors))
  removed <- character()
  for (j in seq_along(predictors)) {
    x <- selected[[j + 1L]]
    label <- names(data)[predictors[j]]
    if (inherits(x, "Date") || inherits(x, "POSIXt")) x <- as.numeric(x)
    if (is.character(x) || is.logical(x)) x <- factor(as.character(x))
    if (is.factor(x)) {
      x <- droplevels(x)
      if (nlevels(x) > 53L) stop(paste0("预测字段“", label, "”超过 53 类；请移除该字段或先合并类别。"), call. = FALSE)
    }
    if (!(is.numeric(x) || is.factor(x))) stop(paste0("预测字段“", label, "”的数据类型不受支持。"), call. = FALSE)
    if (length(unique(x)) < 2L) {
      removed <- c(removed, label)
    } else {
      kept[j] <- TRUE
      selected[[j + 1L]] <- x
    }
  }
  if (!any(kept)) stop("所选预测字段在有效数据中都没有变化。", call. = FALSE)
  selected <- selected[, c(TRUE, kept), drop = FALSE]
  kept_predictors <- predictors[kept]
  labels <- names(data)[kept_predictors]
  names(selected) <- c("outcome", paste0("predictor", seq_along(kept_predictors)))
  list(data = selected, task = detected, outcome = names(data)[outcome], predictors = labels,
    removed = removed, valid_rows = which(valid), excluded = sum(!valid))
}

stratified_rf_split <- function(y, ratio, seed, classification) {
  if (!is.finite(ratio) || ratio < 0.5 || ratio > 0.9) stop("训练集比例应在 50% 到 90% 之间。", call. = FALSE)
  set.seed(as.integer(seed))
  if (classification) {
    groups <- split(seq_along(y), y)
    train <- unlist(lapply(groups, function(index) {
      count <- max(1L, min(length(index) - 1L, floor(length(index) * ratio)))
      sample(index, count)
    }), use.names = FALSE)
  } else {
    count <- max(2L, min(length(y) - 2L, floor(length(y) * ratio)))
    train <- sample(seq_along(y), count)
  }
  list(train = sort(train), test = setdiff(seq_along(y), train))
}

fit_random_forest <- function(data, outcome, predictors, task = "auto", train_ratio = 0.8,
                              ntree = 500, mtry = 0, seed = 2026) {
  if (!requireNamespace("randomForest", quietly = TRUE)) stop("缺少 randomForest 包，请重新点击 Run App 让程序自动安装。", call. = FALSE)
  prepared <- prepare_random_forest_data(data, outcome, predictors, task)
  ntree <- as.integer(ntree); mtry <- as.integer(mtry); seed <- as.integer(seed)
  if (!is.finite(ntree) || ntree < 100L || ntree > 3000L) stop("树的数量应在 100 到 3000 之间。", call. = FALSE)
  if (!is.finite(seed)) stop("随机种子必须是整数。", call. = FALSE)
  p <- length(prepared$predictors)
  default_mtry <- if (prepared$task == "classification") max(1L, floor(sqrt(p))) else max(1L, floor(p / 3L))
  if (mtry == 0L) mtry <- default_mtry
  if (!is.finite(mtry) || mtry < 1L || mtry > p) stop(paste0("每次分裂候选字段数应在 1 到 ", p, " 之间，或填写 0 使用自动值。"), call. = FALSE)

  d <- prepared$data
  analysis_summary <- ai_analysis_summary(d, c(prepared$outcome, prepared$predictors))
  split <- stratified_rf_split(d$outcome, as.numeric(train_ratio), seed, prepared$task == "classification")
  train <- d[split$train, , drop = FALSE]
  test <- d[split$test, , drop = FALSE]
  set.seed(seed)
  model <- randomForest::randomForest(x = train[-1], y = train$outcome, ntree = ntree,
    mtry = mtry, importance = TRUE, na.action = na.fail)
  prediction <- stats::predict(model, newdata = test[-1])
  probabilities <- if (prepared$task == "classification") {
    stats::predict(model, newdata = test[-1], type = "prob")
  } else NULL

  importance_matrix <- randomForest::importance(model)
  permutation_column <- if ("MeanDecreaseAccuracy" %in% colnames(importance_matrix)) "MeanDecreaseAccuracy" else colnames(importance_matrix)[1]
  impurity_column <- if ("MeanDecreaseGini" %in% colnames(importance_matrix)) "MeanDecreaseGini" else if ("IncNodePurity" %in% colnames(importance_matrix)) "IncNodePurity" else colnames(importance_matrix)[ncol(importance_matrix)]
  importance <- data.frame(字段 = prepared$predictors,
    置换重要性 = as.numeric(importance_matrix[, permutation_column]),
    节点纯度重要性 = as.numeric(importance_matrix[, impurity_column]), check.names = FALSE)
  importance <- importance[order(importance$置换重要性, decreasing = TRUE, na.last = TRUE), , drop = FALSE]

  if (prepared$task == "regression") {
    actual <- test$outcome
    residual <- actual - as.numeric(prediction)
    rmse <- sqrt(mean(residual^2)); mae <- mean(abs(residual))
    denominator <- sum((actual - mean(actual))^2)
    r2 <- if (denominator > 0) 1 - sum(residual^2) / denominator else NA_real_
    oob_rmse <- sqrt(tail(model$mse, 1))
    metrics <- data.frame(指标 = c("测试集 RMSE", "测试集 MAE", "测试集 R²", "OOB RMSE", "OOB R²"),
      数值 = c(rmse, mae, r2, oob_rmse, tail(model$rsq, 1)), check.names = FALSE)
    predictions <- data.frame(原始行号 = prepared$valid_rows[split$test], 实际值 = actual,
      预测值 = as.numeric(prediction), 残差 = residual, check.names = FALSE)
    details <- data.frame()
    headline <- paste0("测试集 RMSE = ", rf_number(rmse), "，MAE = ", rf_number(mae), "，R² = ", rf_number(r2), "。")
    oob <- paste0("袋外（OOB）RMSE = ", rf_number(oob_rmse), "，OOB R² = ", rf_number(tail(model$rsq, 1)), "。")
  } else {
    levels_y <- levels(d$outcome)
    actual <- factor(test$outcome, levels = levels_y)
    prediction <- factor(prediction, levels = levels_y)
    confusion <- table(实际值 = actual, 预测值 = prediction)
    accuracy <- sum(diag(confusion)) / sum(confusion)
    details <- do.call(rbind, lapply(seq_along(levels_y), function(i) {
      tp <- confusion[i, i]; fp <- sum(confusion[, i]) - tp; fn <- sum(confusion[i, ]) - tp
      precision <- if (tp + fp) tp / (tp + fp) else NA_real_
      recall <- if (tp + fn) tp / (tp + fn) else NA_real_
      f1 <- if (is.finite(precision) && is.finite(recall) && precision + recall > 0) 2 * precision * recall / (precision + recall) else NA_real_
      data.frame(类别 = levels_y[i], 精确率 = precision, 召回率 = recall, F1 = f1, 样本数 = sum(confusion[i, ]), check.names = FALSE)
    }))
    balanced <- mean(details$召回率, na.rm = TRUE)
    oob_error <- tail(model$err.rate[, "OOB"], 1)
    metrics <- data.frame(指标 = c("测试集准确率", "测试集平衡准确率", "OOB 准确率"),
      数值 = c(accuracy, balanced, 1 - oob_error), check.names = FALSE)
    predictions <- data.frame(原始行号 = prepared$valid_rows[split$test], 实际类别 = as.character(actual),
      预测类别 = as.character(prediction), 是否正确 = actual == prediction, check.names = FALSE)
    probability_table <- as.data.frame(probabilities, check.names = FALSE)
    names(probability_table) <- paste0("概率_", names(probability_table))
    predictions <- data.frame(predictions, probability_table, check.names = FALSE)
    headline <- paste0("测试集准确率 = ", rf_number(accuracy), "，平衡准确率 = ", rf_number(balanced), "。")
    oob <- paste0("袋外（OOB）准确率 = ", rf_number(1 - oob_error), "。")
  }
  top <- head(importance$字段[is.finite(importance$置换重要性)], 5)
  report <- c(
    "随机森林：专业模型解读",
    "一、模型设定",
    paste0("任务类型：", if (prepared$task == "regression") "回归" else "分类", "；目标字段：", prepared$outcome, "。"),
    paste0("预测字段：", paste(prepared$predictors, collapse = "、"), "。"),
    sprintf("使用 %d 行完整数据；训练集 %d 行，测试集 %d 行；因缺失或非有限值排除 %d 行。", nrow(d), nrow(train), nrow(test), prepared$excluded),
    paste0("模型包含 ", ntree, " 棵树；每次分裂随机考虑 ", mtry, " 个字段；随机种子为 ", seed, "。"),
    if (length(prepared$removed)) paste0("以下无变化字段已自动移除：", paste(prepared$removed, collapse = "、"), "。") else "没有因无变化而移除的预测字段。",
    "二、样本外评估",
    headline, oob,
    "测试集指标来自未参与模型训练的数据；OOB 指标利用每棵树未抽中的训练样本估计泛化表现。单次随机划分仍具有偶然性，重要决策应使用重复交叉验证或独立外部数据复核。",
    "三、变量重要性",
    if (length(top)) paste0("按置换重要性排序，前五个字段为：", paste(top, collapse = "、"), "。") else "变量重要性无法可靠排序。",
    "置换重要性衡量打乱某字段后模型准确性的下降，适合比较本模型内的预测贡献；节点纯度重要性可能偏向连续变量或类别较多的字段。相关字段会分摊或替代彼此的重要性，因此重要性较低不等于该变量没有关系。",
    "四、解释边界",
    "随机森林能够拟合非线性和变量交互，但不会给出线性回归式的方向、系数或显著性检验。变量重要性表示预测贡献，不表示因果效应。",
    "模型结果依赖当前清洗方式、字段编码、训练测试划分和参数设置。类别不平衡、数据泄漏、时间顺序被打乱或同一对象重复出现在训练集和测试集，都可能使评估过于乐观。时间序列或分组数据应按时间或对象进行专门划分。"
  )
  list(model = model, task = prepared$task, outcome = prepared$outcome, predictors = prepared$predictors,
    metrics = metrics, details = details, predictions = predictions, importance = importance,
    report = paste(report, collapse = "\n\n"), used = nrow(d), excluded = prepared$excluded,
    train_n = nrow(train), test_n = nrow(test), actual = actual, predicted = prediction,
    confusion = if (prepared$task == "classification") confusion else NULL,
    probabilities = probabilities,
    analysis_summary = analysis_summary)
}

build_rf_evaluation_plot <- function(result) {
  if (result$task == "regression") {
    d <- data.frame(.actual = as.numeric(result$actual), .predicted = as.numeric(result$predicted))
    return(ggplot2::ggplot(d, ggplot2::aes(.actual, .predicted)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "#d97706", linewidth = 0.9) +
      ggplot2::geom_point(colour = "#2563eb", alpha = 0.7, size = 2.5) + ggplot2::coord_equal() +
      lm_plot_theme() + ggplot2::labs(title = "随机森林：测试集实际值与预测值",
        subtitle = "点越接近橙色 45° 虚线，预测越接近实际值", x = "实际值", y = "预测值"))
  }
  d <- as.data.frame(result$confusion)
  ggplot2::ggplot(d, ggplot2::aes(实际值, 预测值, fill = Freq)) +
    ggplot2::geom_tile(colour = "white") + ggplot2::geom_text(ggplot2::aes(label = Freq), colour = "#172b4d", size = 4) +
    ggplot2::scale_fill_gradient(low = "#dbeafe", high = "#2563eb") + lm_plot_theme() +
    ggplot2::labs(title = "随机森林：测试集混淆矩阵", subtitle = "对角线表示预测正确的样本",
      x = "实际类别", y = "预测类别", fill = "样本数")
}

build_rf_importance_plot <- function(result) {
  d <- head(result$importance, 20)
  d <- d[order(d$置换重要性), , drop = FALSE]
  d$字段 <- factor(d$字段, levels = d$字段)
  ggplot2::ggplot(d, ggplot2::aes(字段, 置换重要性)) +
    ggplot2::geom_col(fill = "#2563eb", alpha = 0.85) + ggplot2::coord_flip() + lm_plot_theme() +
    ggplot2::labs(title = "随机森林变量重要性", subtitle = "最多显示置换重要性最高的 20 个字段",
      x = NULL, y = "置换重要性（准确性下降）")
}

random_forest_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(".rf-title-row{display:flex;align-items:center;width:100%;gap:16px;margin-top:20px;margin-bottom:10px}.rf-title-row h3{margin:0;flex:0 0 auto;white-space:nowrap}.rf-title-row .rf-tutorial-toggle{margin-left:auto;flex:0 0 auto;border:1px solid #8db7ef;background:#eef6ff;color:#174a8b;border-radius:999px;font-weight:750;padding:4px 11px}.rf-title-row .rf-tutorial-toggle:hover,.rf-title-row .rf-tutorial-toggle:focus{background:#dcecff;color:#123f78;border-color:#6ea3e6}@media(max-width:600px){.rf-title-row{gap:8px;margin-top:16px}.rf-title-row .rf-tutorial-toggle{padding:3px 9px}}")),
    tags$div(class = "rf-title-row",
      h3("随机森林"),
      actionButton(ns("tutorial_toggle"), "原理演示", icon = icon("graduation-cap"), class = "btn-sm rf-tutorial-toggle")),
    p("使用左侧清洗后的数据建立随机森林回归或分类模型，并用独立测试集评估预测表现。"),
    conditionalPanel(sprintf("input['%s'] %% 2 === 1", ns("tutorial_toggle")),
      rf_tutorial_ui(ns("tutorial"))),
    fluidRow(column(5, selectInput(ns("outcome"), "目标字段", NULL)),
      column(7, selectizeInput(ns("predictors"), "预测字段（可多选）", NULL, multiple = TRUE))),
    fluidRow(column(4, selectInput(ns("task"), "任务类型", c("自动判断" = "auto", "回归" = "regression", "分类" = "classification"))),
      column(4, sliderInput(ns("train_ratio"), "训练集比例", min = 0.5, max = 0.9, value = 0.8, step = 0.05)),
      column(4, numericInput(ns("seed"), "随机种子", 2026, min = 1, step = 1))),
    fluidRow(column(6, numericInput(ns("ntree"), "树的数量", 500, min = 100, max = 3000, step = 100)),
      column(6, numericInput(ns("mtry"), "每次分裂候选字段数（0 = 自动）", 0, min = 0, step = 1))),
    ai_parameter_ui(ns("ai_params")),
    helpText("自动判断时，文字／类别目标以及不超过 10 个不同值的数值目标按分类处理，其余数值目标按回归处理。分类采用分层随机划分。"),
    actionButton(ns("run"), "运行随机森林", class = "btn-primary"),
    tags$div(style = "margin:12px 0", textOutput(ns("status"))),
    tabsetPanel(
      tabPanel("专业解读", tags$div(style = "white-space:pre-wrap;line-height:1.9", textOutput(ns("report")))),
      tabPanel("AI 增强解读", ai_report_ui(ns("ai_report"))),
      tabPanel("模型评估", model_evaluation_ui(ns("unified_evaluation"))),
      tabPanel("变量重要性", DT::DTOutput(ns("importance")), ggplot_editor_ui(ns("importance_editor"), height = "520px")),
      tabPanel("测试集预测", DT::DTOutput(ns("predictions")))
    ), hr(),
    downloadButton(ns("download_report"), "下载随机森林报告 TXT"),
    downloadButton(ns("download_predictions"), "下载测试集预测 CSV"),
    actionButton(ns("save_report"), "保存随机森林报告到项目文件夹"),
    tags$div(style = "overflow-wrap:anywhere", textOutput(ns("saved")))
  )
}

random_forest_server <- function(id, data, directory = reactive(getwd()), ai_config = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    tutorial <- rf_tutorial_server("tutorial")
    observeEvent(input$tutorial_toggle, {
      opened <- input$tutorial_toggle %% 2L == 1L
      updateActionButton(session, "tutorial_toggle", label = if (opened) "收起演示" else "原理演示",
        icon = icon(if (opened) "chevron-up" else "graduation-cap"))
    })
    result <- reactiveVal(NULL)
    status <- reactiveVal("选择字段后，点击“运行随机森林”。")
    saved <- reactiveVal("")
    ai_params <- ai_parameter_server("ai_params", ai_config, data, "随机森林",
      reactive(list(train_ratio = input$train_ratio, ntree = input$ntree, mtry = input$mtry, seed = input$seed)),
      list(train_ratio = "0.5 到 0.9", ntree = "100 到 3000", mtry = "0 到预测字段数量；0 表示自动", seed = "正整数"))
    observeEvent(ai_params$apply(), {
      p <- ai_params$proposal()$parameters; if (is.null(p)) return()
      if (!is.null(p$train_ratio)) updateSliderInput(session, "train_ratio", value = max(0.5, min(0.9, as.numeric(p$train_ratio))))
      if (!is.null(p$ntree)) updateNumericInput(session, "ntree", value = max(100L, min(3000L, as.integer(p$ntree))))
      max_mtry <- max(0L, length(input$predictors %||% character()))
      if (!is.null(p$mtry)) updateNumericInput(session, "mtry", value = max(0L, min(max_mtry, as.integer(p$mtry))))
      if (!is.null(p$seed) && is.finite(as.numeric(p$seed))) updateNumericInput(session, "seed", value = max(1L, as.integer(p$seed)))
      showNotification("AI 建议参数已填入；请检查后点击运行。", type = "message")
    }, ignoreInit = TRUE)
    observeEvent(data(), {
      d <- data(); choices <- setNames(as.character(seq_along(d)), paste0(seq_along(d), ". ", names(d)))
      selected <- if (length(input$outcome) == 1L && input$outcome %in% choices) input$outcome else unname(head(choices, 1))
      updateSelectInput(session, "outcome", choices = choices, selected = selected)
    })
    observeEvent(list(data(), input$outcome), {
      d <- data(); eligible <- setdiff(seq_along(d), as.integer(input$outcome))
      labels <- if (length(eligible)) paste0(eligible, ". ", names(d)[eligible]) else character()
      choices <- setNames(as.character(eligible), labels)
      updateSelectizeInput(session, "predictors", choices = choices, selected = intersect(input$predictors, unname(choices)))
    })
    observe({
      result(NULL); status("数据、字段或参数已更新，请点击“运行随机森林”。")
      data(); input$outcome; input$predictors; input$task; input$train_ratio; input$seed; input$ntree; input$mtry
    }, priority = 100)
    observeEvent(input$run, {
      result(NULL)
      tryCatch({
        fitted <- fit_random_forest(data(), input$outcome, input$predictors, input$task,
          input$train_ratio, input$ntree, input$mtry, input$seed)
        result(fitted)
        status(sprintf("建模完成：训练集 %d 行，测试集 %d 行，排除 %d 行。", fitted$train_n, fitted$test_n, fitted$excluded))
      }, error = function(e) {
        message <- if (inherits(e, "shiny.silent.error")) "请先导入数据并选择字段。" else conditionMessage(e)
        status(paste("未能建模：", message)); showNotification(message, type = "error", duration = 10)
      })
    })
    output$status <- renderText(status())
    output$report <- renderText({ req(result()); result()$report })
    ai_report_server("ai_report", ai_config, "随机森林",
      reactive(if (is.null(result())) "" else result()$report),
      reactive(if (is.null(result())) NULL else ai_model_context("随机森林", result())), data)
    output$metrics <- DT::renderDT({ req(result()); DT::datatable(result()$metrics, rownames = FALSE, options = list(dom = "t")) })
    output$details <- DT::renderDT({
      req(result())
      if (!nrow(result()$details)) return(DT::datatable(data.frame(), options = list(dom = "t")))
      DT::datatable(result()$details, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
    })
    output$importance <- DT::renderDT({ req(result()); DT::datatable(result()$importance, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) })
    output$predictions <- DT::renderDT({ req(result()); DT::datatable(result()$predictions, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) })
    importance_plot <- reactive({ req(result()); build_rf_importance_plot(result()) })
    ggplot_editor_server("importance_editor", importance_plot, directory, "easyr-random-forest-importance")
    model_evaluation_server("unified_evaluation", result, directory, "随机森林")
    write_report <- function(file) { req(result()); writeLines(enc2utf8(result()$report), file, useBytes = TRUE) }
    write_predictions <- function(file) {
      req(result()); con <- file(file, open = "wb"); on.exit(close(con)); writeBin(charToRaw("\ufeff"), con)
      lines <- capture.output(write.csv(result()$predictions, row.names = FALSE, na = ""))
      writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n"))), con)
    }
    output$download_report <- downloadHandler(filename = function() paste0("easyr-random-forest-", Sys.Date(), ".txt"), content = write_report)
    output$download_predictions <- downloadHandler(filename = function() paste0("easyr-random-forest-predictions-", Sys.Date(), ".csv"), content = write_predictions)
    output$saved <- renderText(saved())
    observeEvent(input$save_report, {
      req(result())
      tryCatch({
        path <- save_to_workdir(directory(), "easyr-random-forest", ".txt", write_report)
        saved(paste("上次保存：", path)); showNotification("随机森林报告已保存到 EasyR 项目文件夹。", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })
    result
  })
}
