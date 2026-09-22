if (!exists("ai_report_ui", mode = "function")) source("R/ai.R")
if (!exists("algorithm_title_ui", mode = "function")) source("R/algorithm_tutorials.R")
if (!exists("model_evaluation_ui", mode = "function")) source("R/model_evaluation.R")

svm_number <- function(x) {
  if (!length(x) || !is.finite(x)) return("无法计算")
  format(signif(x, 4), trim = TRUE, scientific = abs(x) > 0 && abs(x) < 0.001)
}

prepare_svm_data <- function(data, outcome, predictors, task = "auto") {
  outcome <- as.integer(outcome); predictors <- unique(as.integer(predictors))
  if (length(outcome) != 1L || is.na(outcome) || !outcome %in% seq_along(data)) stop("请选择一个目标字段。", call. = FALSE)
  if (!length(predictors) || anyNA(predictors) || !all(predictors %in% seq_along(data))) stop("请至少选择一个预测字段。", call. = FALSE)
  if (outcome %in% predictors) stop("目标字段不能同时作为预测字段。", call. = FALSE)
  if (!task %in% c("auto", "regression", "classification")) stop("请选择有效的任务类型。", call. = FALSE)

  selected <- data[, c(outcome, predictors), drop = FALSE]
  supported <- vapply(selected, function(x) is.atomic(x) && is.null(dim(x)), logical(1))
  if (!all(supported)) stop("所选字段包含支持向量机暂不支持的数据类型。", call. = FALSE)
  valid <- stats::complete.cases(selected)
  for (x in selected) if (is.numeric(x)) valid <- valid & is.finite(x)
  selected <- selected[valid, , drop = FALSE]
  if (nrow(selected) < 10L) stop("有效数据不足 10 行，无法可靠划分训练集和测试集。", call. = FALSE)

  y <- selected[[1]]
  detected <- if (task == "auto") {
    if (!is.numeric(y) || length(unique(y)) <= 10L) "classification" else "regression"
  } else task
  if (detected == "regression") {
    if (!is.numeric(y)) stop("支持向量机回归要求目标字段为数值。", call. = FALSE)
    if (length(unique(y)) < 2L) stop("目标字段没有变化，无法建立回归模型。", call. = FALSE)
    y <- as.numeric(y)
  } else {
    y <- droplevels(factor(as.character(y)))
    counts <- table(y)
    if (length(counts) < 2L) stop("分类目标至少需要两个类别。", call. = FALSE)
    if (length(counts) > 50L) stop("分类目标超过 50 类，请检查是否误选了编号字段。", call. = FALSE)
    if (any(counts < 2L)) stop("每个目标类别至少需要 2 行，才能同时进入训练集和测试集。", call. = FALSE)
  }

  xdata <- selected[-1]
  removed <- character()
  keep <- logical(ncol(xdata))
  for (j in seq_along(xdata)) {
    x <- xdata[[j]]; label <- names(data)[predictors[j]]
    if (inherits(x, "Date") || inherits(x, "POSIXt")) x <- as.numeric(x)
    if (is.character(x) || is.logical(x)) x <- factor(as.character(x))
    if (is.factor(x)) {
      x <- droplevels(x)
      if (nlevels(x) > 50L) stop(paste0("预测字段“", label, "”超过 50 类；请先合并类别。"), call. = FALSE)
    }
    if (!(is.numeric(x) || is.factor(x))) stop(paste0("预测字段“", label, "”的数据类型不受支持。"), call. = FALSE)
    if (length(unique(x)) < 2L) removed <- c(removed, label) else { keep[j] <- TRUE; xdata[[j]] <- x }
  }
  if (!any(keep)) stop("所选预测字段在有效数据中都没有变化。", call. = FALSE)
  xdata <- xdata[, keep, drop = FALSE]
  kept_labels <- names(data)[predictors[keep]]
  names(xdata) <- paste0("x", seq_along(xdata))
  x <- stats::model.matrix(~ . - 1, data = xdata)
  varying <- apply(x, 2, function(column) length(unique(column)) > 1L)
  x <- x[, varying, drop = FALSE]
  if (!ncol(x)) stop("字段编码后没有可用于建模的变化。", call. = FALSE)
  list(x = x, y = y, task = detected, outcome = names(data)[outcome], predictors = kept_labels,
    removed = removed, valid_rows = which(valid), excluded = sum(!valid))
}

svm_train_test_split <- function(y, ratio = 0.8, seed = 2026, classification = FALSE) {
  ratio <- as.numeric(ratio); seed <- as.integer(seed)
  if (!is.finite(ratio) || ratio < 0.5 || ratio > 0.9) stop("训练集比例应在 50% 到 90% 之间。", call. = FALSE)
  if (!is.finite(seed)) stop("随机种子必须是整数。", call. = FALSE)
  set.seed(seed)
  if (classification) {
    groups <- split(seq_along(y), y)
    train <- unlist(lapply(groups, function(index) sample(index, max(1L, min(length(index) - 1L, floor(length(index) * ratio))))), use.names = FALSE)
  } else {
    train <- sample(seq_along(y), max(2L, min(length(y) - 2L, floor(length(y) * ratio))))
  }
  list(train = sort(train), test = setdiff(seq_along(y), train))
}

fit_svm_analysis <- function(data, outcome, predictors, task = "auto", train_ratio = 0.8,
                             kernel = "radial", cost = 1, gamma = 0, degree = 3,
                             coef0 = 0, epsilon = 0.1, scale = TRUE, seed = 2026) {
  if (!requireNamespace("e1071", quietly = TRUE)) stop("缺少 e1071 包，请重新点击 Run App 让程序自动安装。", call. = FALSE)
  prepared <- prepare_svm_data(data, outcome, predictors, task)
  profile_data <- data.frame(.outcome = prepared$y, prepared$x, check.names = FALSE)
  analysis_summary <- ai_analysis_summary(profile_data, c(prepared$outcome, colnames(prepared$x)))
  if (!kernel %in% c("linear", "radial", "polynomial", "sigmoid")) stop("请选择有效的核函数。", call. = FALSE)
  cost <- as.numeric(cost); gamma <- as.numeric(gamma); degree <- as.integer(degree)
  coef0 <- as.numeric(coef0); epsilon <- as.numeric(epsilon); seed <- as.integer(seed)
  if (!is.finite(cost) || cost < 0.001 || cost > 100000) stop("成本参数 C 应在 0.001 到 100000 之间。", call. = FALSE)
  if (!is.finite(gamma) || gamma < 0 || gamma > 1000) stop("γ 应在 0 到 1000 之间；0 表示自动。", call. = FALSE)
  if (!is.finite(degree) || degree < 2L || degree > 10L) stop("多项式次数应在 2 到 10 之间。", call. = FALSE)
  if (!is.finite(coef0) || abs(coef0) > 1000) stop("核函数常数项应在 -1000 到 1000 之间。", call. = FALSE)
  if (!is.finite(epsilon) || epsilon < 0 || epsilon > 1000) stop("回归 ε 应在 0 到 1000 之间。", call. = FALSE)
  effective_gamma <- if (gamma == 0) 1 / ncol(prepared$x) else gamma
  split <- svm_train_test_split(prepared$y, train_ratio, seed, prepared$task == "classification")
  x_train <- prepared$x[split$train, , drop = FALSE]; x_test <- prepared$x[split$test, , drop = FALSE]
  y_train <- prepared$y[split$train]; y_test <- prepared$y[split$test]
  set.seed(seed)
  model <- e1071::svm(x = x_train, y = y_train,
    type = if (prepared$task == "classification") "C-classification" else "eps-regression",
    kernel = kernel, cost = cost, gamma = effective_gamma, degree = degree,
    coef0 = coef0, epsilon = epsilon, scale = isTRUE(scale),
    probability = prepared$task == "classification")
  prediction <- stats::predict(model, x_test, probability = prepared$task == "classification")
  probabilities <- if (prepared$task == "classification") attr(prediction, "probabilities") else NULL

  if (prepared$task == "regression") {
    actual <- as.numeric(y_test); predicted <- as.numeric(prediction); residual <- actual - predicted
    rmse <- sqrt(mean(residual^2)); mae <- mean(abs(residual)); denominator <- sum((actual - mean(actual))^2)
    r2 <- if (denominator > 0) 1 - sum(residual^2) / denominator else NA_real_
    metrics <- data.frame(指标 = c("测试集 RMSE", "测试集 MAE", "测试集 R²"), 数值 = c(rmse, mae, r2), check.names = FALSE)
    details <- data.frame()
    predictions <- data.frame(原始行号 = prepared$valid_rows[split$test], 实际值 = actual, 预测值 = predicted, 残差 = residual, check.names = FALSE)
    headline <- paste0("测试集 RMSE = ", svm_number(rmse), "，MAE = ", svm_number(mae), "，R² = ", svm_number(r2), "。")
    confusion <- NULL
  } else {
    levels_y <- levels(prepared$y)
    actual <- factor(y_test, levels = levels_y); predicted <- factor(prediction, levels = levels_y)
    confusion <- table(实际值 = actual, 预测值 = predicted)
    accuracy <- sum(diag(confusion)) / sum(confusion)
    details <- do.call(rbind, lapply(seq_along(levels_y), function(i) {
      tp <- confusion[i, i]; fp <- sum(confusion[, i]) - tp; fn <- sum(confusion[i, ]) - tp
      precision <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
      recall <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
      f1 <- if (is.finite(precision) && is.finite(recall) && precision + recall > 0) 2 * precision * recall / (precision + recall) else NA_real_
      data.frame(类别 = levels_y[i], 精确率 = precision, 召回率 = recall, F1 = f1, 样本数 = sum(confusion[i, ]), check.names = FALSE)
    }))
    balanced <- mean(details$召回率, na.rm = TRUE)
    metrics <- data.frame(指标 = c("测试集准确率", "测试集平衡准确率"), 数值 = c(accuracy, balanced), check.names = FALSE)
    predictions <- data.frame(原始行号 = prepared$valid_rows[split$test], 实际类别 = as.character(actual),
      预测类别 = as.character(predicted), 是否正确 = actual == predicted, check.names = FALSE)
    probability_table <- as.data.frame(probabilities, check.names = FALSE)
    names(probability_table) <- paste0("概率_", names(probability_table))
    predictions <- data.frame(predictions, probability_table, check.names = FALSE)
    headline <- paste0("测试集准确率 = ", svm_number(accuracy), "，平衡准确率 = ", svm_number(balanced), "。")
  }
  support_count <- nrow(model$SV)
  support_table <- if (prepared$task == "classification") {
    data.frame(类别 = levels(prepared$y), 支持向量数 = as.integer(model$nSV), check.names = FALSE)
  } else data.frame(类别 = "回归模型", 支持向量数 = support_count, check.names = FALSE)
  kernel_label <- c(linear = "线性核", radial = "径向基（RBF）核", polynomial = "多项式核", sigmoid = "Sigmoid 核")[[kernel]]
  report <- c(
    "支持向量机：专业模型解读", "一、模型设定",
    paste0("任务类型：", if (prepared$task == "regression") "回归" else "分类", "；目标字段：", prepared$outcome, "。"),
    paste0("预测字段：", paste(prepared$predictors, collapse = "、"), "。"),
    sprintf("使用 %d 行完整数据；训练集 %d 行，测试集 %d 行；排除 %d 行。", length(prepared$y), length(split$train), length(split$test), prepared$excluded),
    paste0("核函数：", kernel_label, "；C = ", svm_number(cost), "；γ = ", svm_number(effective_gamma),
      if (kernel == "polynomial") paste0("；次数 = ", degree, "；常数项 = ", svm_number(coef0)) else "",
      if (prepared$task == "regression") paste0("；ε = ", svm_number(epsilon)) else "", "；数值特征标准化：", if (isTRUE(scale)) "是" else "否", "。"),
    if (length(prepared$removed)) paste0("自动移除的无变化字段：", paste(prepared$removed, collapse = "、"), "。") else "没有因无变化而移除的字段。",
    "二、样本外评估", headline,
    "指标来自未参与训练的测试集。单次随机划分会受随机种子影响，重要应用应使用重复交叉验证或独立外部数据复核。",
    "三、支持向量与模型复杂度",
    paste0("模型使用 ", support_count, " 个支持向量，占训练集的 ", svm_number(100 * support_count / length(split$train)), "%。支持向量是决定分类边界或回归函数的关键训练样本；比例较高可能表示边界复杂、类别重叠或参数约束较弱，但不能单独作为模型好坏的判断。"),
    "四、参数含义与解释边界",
    "C 越大，模型通常越强调训练误差；C 越小，通常允许更宽松的间隔。RBF、多项式和 Sigmoid 核中的 γ 控制单个样本影响范围，过大容易形成复杂边界，过小可能欠拟合。参数应结合交叉验证选择。",
    "支持向量机适合高维和非线性预测，但非线性核不提供线性回归式的直接系数解释。预测表现不等于因果关系；类别不平衡、时间顺序、重复对象和数据泄漏都会影响评估。"
  )
  list(model = model, task = prepared$task, outcome = prepared$outcome, predictors = prepared$predictors,
    kernel = kernel, cost = cost, gamma = effective_gamma, degree = degree, coef0 = coef0, epsilon = epsilon,
    metrics = metrics, details = details, predictions = predictions, support = support_table,
    report = paste(report, collapse = "\n\n"), used = length(prepared$y), excluded = prepared$excluded,
    train_n = length(split$train), test_n = length(split$test), actual = actual, predicted = predicted,
    confusion = confusion, probabilities = probabilities, analysis_summary = analysis_summary)
}

build_svm_evaluation_plot <- function(result) {
  theme <- ggplot2::theme_minimal(base_size = 13) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"), panel.grid.minor = ggplot2::element_blank())
  if (result$task == "regression") {
    d <- data.frame(.actual = as.numeric(result$actual), .predicted = as.numeric(result$predicted))
    return(ggplot2::ggplot(d, ggplot2::aes(.actual, .predicted)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "#d97706", linewidth = 0.9) +
      ggplot2::geom_point(colour = "#2563eb", alpha = 0.72, size = 2.5) + ggplot2::coord_equal() + theme +
      ggplot2::labs(title = "支持向量机：测试集实际值与预测值", subtitle = "点越接近橙色 45° 虚线，预测越接近实际值", x = "实际值", y = "预测值"))
  }
  d <- as.data.frame(result$confusion)
  ggplot2::ggplot(d, ggplot2::aes(实际值, 预测值, fill = Freq)) +
    ggplot2::geom_tile(colour = "white") + ggplot2::geom_text(ggplot2::aes(label = Freq), colour = "#172b4d", size = 4) +
    ggplot2::scale_fill_gradient(low = "#dbeafe", high = "#2563eb") + theme +
    ggplot2::labs(title = "支持向量机：测试集混淆矩阵", subtitle = "对角线表示预测正确的样本", x = "实际类别", y = "预测类别", fill = "样本数")
}

svm_ui <- function(id) {
  ns <- NS(id)
  tagList(
    algorithm_title_ui(ns, "支持向量机（SVM）"),
    p("建立支持向量机分类或回归模型。非线性核可以处理弯曲的分类边界和非线性关系。"),
    conditionalPanel(sprintf("input['%s'] %% 2 === 1", ns("tutorial_toggle")),
      algorithm_tutorial_ui(ns("tutorial"), "svm")),
    fluidRow(column(5, selectInput(ns("outcome"), "目标字段", NULL)),
      column(7, selectizeInput(ns("predictors"), "预测字段（可多选）", NULL, multiple = TRUE))),
    fluidRow(column(4, selectInput(ns("task"), "任务类型", c("自动判断" = "auto", "回归" = "regression", "分类" = "classification"))),
      column(4, selectInput(ns("kernel"), "核函数", c("径向基 RBF（推荐起点）" = "radial", "线性核" = "linear", "多项式核" = "polynomial", "Sigmoid 核" = "sigmoid"))),
      column(4, sliderInput(ns("train_ratio"), "训练集比例", min = 0.5, max = 0.9, value = 0.8, step = 0.05))),
    fluidRow(column(3, numericInput(ns("cost"), "成本参数 C", 1, min = 0.001, max = 100000)),
      column(3, numericInput(ns("gamma"), "γ（0 = 自动）", 0, min = 0, max = 1000)),
      column(3, numericInput(ns("degree"), "多项式次数", 3, min = 2, max = 10, step = 1)),
      column(3, numericInput(ns("coef0"), "核函数常数项", 0, min = -1000, max = 1000))),
    fluidRow(column(4, numericInput(ns("epsilon"), "回归 ε", 0.1, min = 0, max = 1000)),
      column(4, checkboxInput(ns("scale"), "自动标准化数值特征", TRUE)),
      column(4, numericInput(ns("seed"), "随机种子", 2026, min = 1, step = 1))),
    ai_parameter_ui(ns("ai_params")),
    helpText("γ = 0 时自动使用 1 / 编码后的特征数。多项式次数只影响多项式核，ε 只影响回归。参数效果依赖数据，应结合测试集或交叉验证比较。"),
    actionButton(ns("run"), "运行支持向量机", class = "btn-primary"),
    tags$div(style = "margin:12px 0", textOutput(ns("status"))),
    tabsetPanel(
      tabPanel("专业解读", tags$div(style = "white-space:pre-wrap;line-height:1.9", textOutput(ns("report")))),
      tabPanel("AI 增强解读", ai_report_ui(ns("ai_report"))),
      tabPanel("模型评估", model_evaluation_ui(ns("unified_evaluation"))),
      tabPanel("支持向量", DT::DTOutput(ns("support"))),
      tabPanel("测试集预测", DT::DTOutput(ns("predictions")))
    ), hr(),
    downloadButton(ns("download_report"), "下载 SVM 报告 TXT"),
    downloadButton(ns("download_predictions"), "下载测试集预测 CSV"),
    actionButton(ns("save_report"), "保存 SVM 报告到项目文件夹"),
    tags$div(style = "overflow-wrap:anywhere", textOutput(ns("saved")))
  )
}

svm_server <- function(id, data, directory = reactive(getwd()), ai_config = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    algorithm_tutorial_server("tutorial", "svm")
    algorithm_tutorial_toggle_server(input, session)
    result <- reactiveVal(NULL); status <- reactiveVal("选择字段后，点击“运行支持向量机”。"); saved <- reactiveVal("")
    ai_params <- ai_parameter_server("ai_params", ai_config, data, "支持向量机（SVM）",
      reactive(list(kernel = input$kernel, train_ratio = input$train_ratio, cost = input$cost, gamma = input$gamma,
        degree = input$degree, coef0 = input$coef0, epsilon = input$epsilon, scale = input$scale, seed = input$seed)),
      list(kernel = c("linear", "radial", "polynomial", "sigmoid"), train_ratio = "0.5 到 0.9",
        cost = "0.001 到 100000", gamma = "0 到 1000；0 表示自动", degree = "2 到 10",
        coef0 = "-1000 到 1000", epsilon = "0 到 1000", scale = "true 或 false", seed = "正整数"))
    observeEvent(ai_params$apply(), {
      p <- ai_params$proposal()$parameters; if (is.null(p)) return()
      if (!is.null(p$kernel) && p$kernel %in% c("linear", "radial", "polynomial", "sigmoid")) updateSelectInput(session, "kernel", selected = p$kernel)
      if (!is.null(p$train_ratio)) updateSliderInput(session, "train_ratio", value = max(0.5, min(0.9, as.numeric(p$train_ratio))))
      if (!is.null(p$cost)) updateNumericInput(session, "cost", value = max(0.001, min(100000, as.numeric(p$cost))))
      if (!is.null(p$gamma)) updateNumericInput(session, "gamma", value = max(0, min(1000, as.numeric(p$gamma))))
      if (!is.null(p$degree)) updateNumericInput(session, "degree", value = max(2L, min(10L, as.integer(p$degree))))
      if (!is.null(p$coef0)) updateNumericInput(session, "coef0", value = max(-1000, min(1000, as.numeric(p$coef0))))
      if (!is.null(p$epsilon)) updateNumericInput(session, "epsilon", value = max(0, min(1000, as.numeric(p$epsilon))))
      if (!is.null(p$scale)) updateCheckboxInput(session, "scale", value = isTRUE(as.logical(p$scale)))
      if (!is.null(p$seed) && is.finite(as.numeric(p$seed))) updateNumericInput(session, "seed", value = max(1L, as.integer(p$seed)))
      showNotification("AI 建议参数已填入；请检查后点击运行。", type = "message")
    }, ignoreInit = TRUE)
    observeEvent(data(), {
      d <- data(); choices <- setNames(as.character(seq_along(d)), paste0(seq_along(d), ". ", names(d)))
      selected <- if (length(input$outcome) == 1L && input$outcome %in% unname(choices)) input$outcome else unname(head(choices, 1))
      updateSelectInput(session, "outcome", choices = choices, selected = selected)
    })
    observeEvent(list(data(), input$outcome), {
      d <- data(); eligible <- setdiff(seq_along(d), as.integer(input$outcome))
      labels <- if (length(eligible)) paste0(eligible, ". ", names(d)[eligible]) else character()
      choices <- setNames(as.character(eligible), labels)
      updateSelectizeInput(session, "predictors", choices = choices, selected = intersect(input$predictors, unname(choices)))
    })
    observe({
      result(NULL); status("数据、字段或参数已更新，请点击“运行支持向量机”。")
      data(); input$outcome; input$predictors; input$task; input$train_ratio; input$kernel; input$cost
      input$gamma; input$degree; input$coef0; input$epsilon; input$scale; input$seed
    }, priority = 100)
    observeEvent(input$run, {
      result(NULL)
      tryCatch({
        fitted <- fit_svm_analysis(data(), input$outcome, input$predictors, input$task, input$train_ratio,
          input$kernel, input$cost, input$gamma, input$degree, input$coef0, input$epsilon, isTRUE(input$scale), input$seed)
        result(fitted); status(sprintf("建模完成：训练集 %d 行，测试集 %d 行，排除 %d 行。", fitted$train_n, fitted$test_n, fitted$excluded))
      }, error = function(e) {
        message <- if (inherits(e, "shiny.silent.error")) "请先导入数据并选择字段。" else conditionMessage(e)
        status(paste("未能建模：", message)); showNotification(message, type = "error", duration = 10)
      })
    })
    output$status <- renderText(status()); output$report <- renderText({ req(result()); result()$report })
    ai_report_server("ai_report", ai_config, "支持向量机（SVM）",
      reactive(if (is.null(result())) "" else result()$report),
      reactive(if (is.null(result())) NULL else ai_model_context("支持向量机（SVM）", result())), data)
    output$metrics <- DT::renderDT({ req(result()); DT::datatable(result()$metrics, rownames = FALSE, options = list(dom = "t")) })
    output$details <- DT::renderDT({ req(result()); DT::datatable(result()$details, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE)) })
    output$support <- DT::renderDT({ req(result()); DT::datatable(result()$support, rownames = FALSE, options = list(dom = "t")) })
    output$predictions <- DT::renderDT({ req(result()); DT::datatable(result()$predictions, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) })
    model_evaluation_server("unified_evaluation", result, directory, "支持向量机")
    write_report <- function(file) { req(result()); writeLines(enc2utf8(result()$report), file, useBytes = TRUE) }
    write_predictions <- function(file) {
      req(result()); con <- file(file, open = "wb"); on.exit(close(con)); writeBin(charToRaw("\ufeff"), con)
      lines <- capture.output(write.csv(result()$predictions, row.names = FALSE, na = ""))
      writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n"))), con)
    }
    output$download_report <- downloadHandler(filename = function() paste0("easyr-svm-", Sys.Date(), ".txt"), content = write_report)
    output$download_predictions <- downloadHandler(filename = function() paste0("easyr-svm-predictions-", Sys.Date(), ".csv"), content = write_predictions)
    output$saved <- renderText(saved())
    observeEvent(input$save_report, {
      req(result())
      tryCatch({
        path <- save_to_workdir(directory(), "easyr-svm", ".txt", write_report)
        saved(paste("上次保存：", path)); showNotification("SVM 报告已保存到 EasyR 项目文件夹。", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })
    result
  })
}
