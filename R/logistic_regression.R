if (!exists("ai_report_ui", mode = "function")) source("R/ai.R")
if (!exists("algorithm_title_ui", mode = "function")) source("R/algorithm_tutorials.R")
if (!exists("model_evaluation_ui", mode = "function")) source("R/model_evaluation.R")

logistic_number <- function(x) {
  if (!length(x) || !is.finite(x)) return("无法计算")
  format(signif(x, 4), trim = TRUE, scientific = abs(x) > 0 && abs(x) < 0.001)
}

prepare_logistic_data <- function(data, outcome, predictors, positive_class = NULL) {
  outcome <- as.integer(outcome); predictors <- unique(as.integer(predictors))
  if (length(outcome) != 1L || is.na(outcome) || !outcome %in% seq_along(data)) stop("请选择一个目标字段。", call. = FALSE)
  if (!length(predictors) || anyNA(predictors) || !all(predictors %in% seq_along(data))) stop("请至少选择一个预测字段。", call. = FALSE)
  if (outcome %in% predictors) stop("目标字段不能同时作为预测字段。", call. = FALSE)
  selected <- data[, c(outcome, predictors), drop = FALSE]
  supported <- vapply(selected, function(x) is.atomic(x) && is.null(dim(x)), logical(1))
  if (!all(supported)) stop("所选字段包含逻辑回归暂不支持的数据类型。", call. = FALSE)
  valid <- stats::complete.cases(selected)
  for (x in selected) if (is.numeric(x)) valid <- valid & is.finite(x)
  selected <- selected[valid, , drop = FALSE]
  if (nrow(selected) < 20L) stop("有效数据不足 20 行，无法可靠划分训练集和测试集。", call. = FALSE)

  outcome_values <- as.character(selected[[1]])
  classes <- sort(unique(outcome_values))
  if (length(classes) != 2L) stop("逻辑回归目前要求目标字段正好包含两个类别。", call. = FALSE)
  if (is.null(positive_class) || !length(positive_class) || !positive_class %in% classes) positive_class <- classes[2L]
  negative_class <- setdiff(classes, positive_class)[1L]
  counts <- table(outcome_values)
  if (any(counts < 2L)) stop("两个类别都至少需要 2 行，才能同时进入训练集和测试集。", call. = FALSE)
  selected[[1]] <- factor(outcome_values, levels = c(negative_class, positive_class))

  keep <- logical(length(predictors)); removed <- character()
  for (j in seq_along(predictors)) {
    x <- selected[[j + 1L]]; label <- names(data)[predictors[j]]
    if (inherits(x, "Date") || inherits(x, "POSIXt")) x <- as.numeric(x)
    if (is.character(x) || is.logical(x)) x <- factor(as.character(x))
    if (is.factor(x)) {
      x <- droplevels(x)
      if (nlevels(x) > 50L) stop(paste0("预测字段“", label, "”超过 50 类，请先合并类别。"), call. = FALSE)
    }
    if (!(is.numeric(x) || is.factor(x))) stop(paste0("预测字段“", label, "”的数据类型不受支持。"), call. = FALSE)
    if (length(unique(x)) < 2L) removed <- c(removed, label) else { keep[j] <- TRUE; selected[[j + 1L]] <- x }
  }
  if (!any(keep)) stop("所选预测字段在有效数据中都没有变化。", call. = FALSE)
  selected <- selected[, c(TRUE, keep), drop = FALSE]
  labels <- names(data)[predictors[keep]]
  names(selected) <- c("outcome", paste0("x", seq_along(labels)))
  list(data = selected, outcome = names(data)[outcome], predictors = labels,
    positive_class = positive_class, negative_class = negative_class,
    valid_rows = which(valid), excluded = sum(!valid), removed = removed)
}

logistic_split <- function(y, ratio = 0.8, seed = 2026) {
  ratio <- as.numeric(ratio); seed <- as.integer(seed)
  if (!is.finite(ratio) || ratio < 0.5 || ratio > 0.9) stop("训练集比例应在 50% 到 90% 之间。", call. = FALSE)
  if (!is.finite(seed)) stop("随机种子必须是整数。", call. = FALSE)
  set.seed(seed)
  groups <- split(seq_along(y), y)
  train <- unlist(lapply(groups, function(index) {
    sample(index, max(1L, min(length(index) - 1L, floor(length(index) * ratio))))
  }), use.names = FALSE)
  list(train = sort(train), test = setdiff(seq_along(y), train))
}

logistic_term_labels <- function(terms, predictors) {
  output <- terms
  output[terms == "(Intercept)"] <- "截距"
  for (j in rev(seq_along(predictors))) {
    prefix <- paste0("x", j)
    matches <- startsWith(terms, prefix)
    suffix <- substring(terms[matches], nchar(prefix) + 1L)
    output[matches] <- ifelse(nzchar(suffix), paste0(predictors[j], "：", suffix), predictors[j])
  }
  output
}

fit_logistic_regression <- function(data, outcome, predictors, positive_class = NULL,
                                    train_ratio = 0.8, seed = 2026) {
  prepared <- prepare_logistic_data(data, outcome, predictors, positive_class)
  d <- prepared$data
  split <- logistic_split(d$outcome, train_ratio, seed)
  train <- d[split$train, , drop = FALSE]; test <- d[split$test, , drop = FALSE]
  warning_messages <- character()
  model <- withCallingHandlers(
    stats::glm(outcome ~ ., data = train, family = stats::binomial()),
    warning = function(w) { warning_messages <<- c(warning_messages, conditionMessage(w)); invokeRestart("muffleWarning") })
  probability_positive <- as.numeric(stats::predict(model, newdata = test, type = "response"))
  if (any(!is.finite(probability_positive))) stop("模型无法为测试集生成有限预测概率；请减少类别过多或高度相关的预测字段。", call. = FALSE)
  classes <- c(prepared$negative_class, prepared$positive_class)
  probabilities <- cbind(1 - probability_positive, probability_positive)
  colnames(probabilities) <- classes
  predicted <- factor(ifelse(probability_positive >= 0.5, prepared$positive_class, prepared$negative_class), levels = classes)
  actual <- factor(test$outcome, levels = classes)
  evaluation <- classification_evaluation(actual, predicted, probabilities, prepared$positive_class, 0.5)

  coefficient_matrix <- summary(model)$coefficients
  z <- stats::qnorm(0.975)
  estimates <- coefficient_matrix[, "Estimate"]
  lower <- estimates - z * coefficient_matrix[, "Std. Error"]
  upper <- estimates + z * coefficient_matrix[, "Std. Error"]
  safe_exp <- function(x) ifelse(x > log(.Machine$double.xmax), Inf, exp(x))
  coefficients <- data.frame(
    字段 = logistic_term_labels(rownames(coefficient_matrix), prepared$predictors),
    对数优势系数 = estimates,
    标准误 = coefficient_matrix[, "Std. Error"],
    z值 = coefficient_matrix[, "z value"],
    p值 = coefficient_matrix[, "Pr(>|z|)"],
    优势比OR = safe_exp(estimates),
    OR下限95 = safe_exp(lower), OR上限95 = safe_exp(upper), check.names = FALSE)
  predictions <- data.frame(原始行号 = prepared$valid_rows[split$test],
    实际类别 = as.character(actual), 预测类别 = as.character(predicted),
    是否正确 = actual == predicted, check.names = FALSE)
  probability_table <- as.data.frame(probabilities, check.names = FALSE)
  names(probability_table) <- paste0("概率_", names(probability_table))
  predictions <- data.frame(predictions, probability_table, check.names = FALSE)
  analysis_summary <- ai_analysis_summary(d, c(prepared$outcome, prepared$predictors))
  odds_interpretation <- vapply(seq_len(nrow(coefficients)), function(i) {
    if (coefficients$字段[i] == "截距") return(paste0("截距对应所有数值预测量为 0、类别变量处于参考组时的基准对数优势；OR = ", logistic_number(coefficients$优势比OR[i]), "。"))
    direction <- if (coefficients$优势比OR[i] >= 1) "提高" else "降低"
    paste0(coefficients$字段[i], "：在其他变量不变时，该项每增加 1 单位或相对参考组，正类“",
      prepared$positive_class, "”的优势预计", direction, "；OR = ", logistic_number(coefficients$优势比OR[i]),
      "，95% CI [", logistic_number(coefficients$OR下限95[i]), ", ", logistic_number(coefficients$OR上限95[i]),
      "]，p = ", logistic_number(coefficients$p值[i]), "。")
  }, character(1))
  auc <- evaluation$auc
  report <- c(
    "逻辑回归：专业模型解读", "一、模型设定",
    paste0("目标字段：", prepared$outcome, "；正类：", prepared$positive_class, "；参照负类：", prepared$negative_class, "。"),
    paste0("预测字段：", paste(prepared$predictors, collapse = "、"), "。"),
    sprintf("使用 %d 行完整数据；训练集 %d 行，测试集 %d 行；因缺失或非有限值排除 %d 行。", nrow(d), nrow(train), nrow(test), prepared$excluded),
    if (length(prepared$removed)) paste0("自动移除的无变化字段：", paste(prepared$removed, collapse = "、"), "。") else "没有因无变化而移除的预测字段。",
    "模型使用 logit 链接，将正类概率转换为对数优势：log[p/(1-p)] = β₀ + β₁X₁ + … + βₚXₚ。",
    "二、测试集预测表现",
    paste0("默认阈值 0.50 下，Accuracy = ", logistic_number(evaluation$summary$数值[evaluation$summary$指标 == "Accuracy"]),
      "，Macro F1 = ", logistic_number(evaluation$summary$数值[evaluation$summary$指标 == "Macro F1"]),
      "，ROC AUC = ", logistic_number(auc), "。"),
    "这些指标来自未参与训练的测试集。阈值会改变 Precision 与 Recall 的取舍；AUC衡量跨阈值的排序能力，不代表概率已经校准。",
    "三、系数与优势比", odds_interpretation,
    "OR 大于 1 表示正类优势增加，OR 小于 1 表示正类优势降低。OR 不是概率的倍数；当基准概率较高时，两者差异尤其明显。类别变量均相对其参考组解释。",
    "四、诊断与解释边界",
    paste0("模型收敛状态：", if (isTRUE(model$converged)) "已收敛" else "未正常收敛", "。",
      if (length(warning_messages)) paste0("拟合提示：", paste(unique(warning_messages), collapse = "；"), "。") else "未记录拟合警告。"),
    "常规推断假设观测相互独立、logit 与连续预测变量近似线性，并且不存在严重多重共线性或完全分离。样本稀少、类别极不平衡或完全分离会使系数和置信区间不稳定。",
    "系数表示控制模型中其他变量后的条件关联，不自动代表因果效应。重要应用应进一步使用交叉验证、外部验证、校准曲线，并依据误判成本选择分类阈值。")
  list(model = model, task = "classification", outcome = prepared$outcome,
    predictors = prepared$predictors, positive_class = prepared$positive_class,
    negative_class = prepared$negative_class, coefficients = coefficients,
    metrics = evaluation$summary, details = evaluation$per_class,
    predictions = predictions, report = paste(report, collapse = "\n\n"),
    used = nrow(d), excluded = prepared$excluded, train_n = nrow(train), test_n = nrow(test),
    actual = actual, predicted = predicted, probabilities = probabilities,
    confusion = evaluation$confusion, converged = isTRUE(model$converged),
    warnings = unique(warning_messages), analysis_summary = analysis_summary)
}

logistic_regression_ui <- function(id) {
  ns <- NS(id)
  tagList(
    algorithm_title_ui(ns, "逻辑回归"),
    p("建立二分类模型，解释变量与事件发生概率的关系，并在独立测试集上评价预测表现。"),
    conditionalPanel(sprintf("input['%s'] %% 2 === 1", ns("tutorial_toggle")),
      algorithm_tutorial_ui(ns("tutorial"), "logistic")),
    fluidRow(column(5, selectInput(ns("outcome"), "二分类目标字段", NULL)),
      column(7, selectizeInput(ns("predictors"), "预测字段（可多选）", NULL, multiple = TRUE))),
    fluidRow(column(4, selectInput(ns("positive_class"), "正类（要预测的事件）", NULL)),
      column(4, sliderInput(ns("train_ratio"), "训练集比例", min = 0.5, max = 0.9, value = 0.8, step = 0.05)),
      column(4, numericInput(ns("seed"), "随机种子", 2026, min = 1, step = 1))),
    ai_parameter_ui(ns("ai_params")),
    helpText("目标字段必须正好包含两个类别。正类决定系数、优势比和预测概率的解释方向；分类阈值可在模型评估中调整。"),
    actionButton(ns("run"), "运行逻辑回归", class = "btn-primary"),
    tags$div(style = "margin:12px 0", textOutput(ns("status"))),
    tabsetPanel(
      tabPanel("专业解读", tags$div(style = "white-space:pre-wrap;line-height:1.9", textOutput(ns("report")))),
      tabPanel("AI 增强解读", ai_report_ui(ns("ai_report"))),
      tabPanel("系数与优势比", DT::DTOutput(ns("coefficients")),
        helpText("OR 表示优势的倍数变化，并非概率的倍数变化；类别变量相对参考组解释。")),
      tabPanel("模型评估", model_evaluation_ui(ns("evaluation"))),
      tabPanel("测试集预测", DT::DTOutput(ns("predictions")))
    ), hr(),
    downloadButton(ns("download_report"), "下载逻辑回归报告 TXT"),
    downloadButton(ns("download_predictions"), "下载测试集预测 CSV"),
    actionButton(ns("save_report"), "保存逻辑回归报告到项目文件夹"),
    tags$div(style = "overflow-wrap:anywhere", textOutput(ns("saved")))
  )
}

logistic_regression_server <- function(id, data, directory = reactive(getwd()), ai_config = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    algorithm_tutorial_server("tutorial", "logistic")
    algorithm_tutorial_toggle_server(input, session)
    result <- reactiveVal(NULL); status <- reactiveVal("选择字段后，点击“运行逻辑回归”。"); saved <- reactiveVal("")
    ai_params <- ai_parameter_server("ai_params", ai_config, data, "逻辑回归",
      reactive(list(train_ratio = input$train_ratio, seed = input$seed)),
      list(train_ratio = "0.5 到 0.9", seed = "正整数"))
    observeEvent(ai_params$apply(), {
      p <- ai_params$proposal()$parameters; if (is.null(p)) return()
      if (!is.null(p$train_ratio)) updateSliderInput(session, "train_ratio", value = max(0.5, min(0.9, as.numeric(p$train_ratio))))
      if (!is.null(p$seed) && is.finite(as.numeric(p$seed))) updateNumericInput(session, "seed", value = max(1L, as.integer(p$seed)))
      showNotification("AI 建议参数已填入；请检查后点击运行。", type = "message")
    }, ignoreInit = TRUE)
    observeEvent(data(), {
      d <- data(); eligible <- which(vapply(d, function(x) length(unique(x[!is.na(x)])) == 2L, logical(1)))
      labels <- if (length(eligible)) paste0(eligible, ". ", names(d)[eligible]) else character()
      choices <- setNames(as.character(eligible), labels)
      selected <- if (length(input$outcome) == 1L && input$outcome %in% unname(choices)) input$outcome else unname(head(choices, 1))
      updateSelectInput(session, "outcome", choices = choices, selected = selected)
    })
    observeEvent(list(data(), input$outcome), {
      d <- data(); outcome <- as.integer(input$outcome)
      eligible <- if (length(outcome) == 1L && is.finite(outcome)) setdiff(seq_along(d), outcome) else seq_along(d)
      choices <- setNames(as.character(eligible), paste0(eligible, ". ", names(d)[eligible]))
      updateSelectizeInput(session, "predictors", choices = choices, selected = intersect(input$predictors, unname(choices)))
      classes <- if (length(outcome) == 1L && outcome %in% seq_along(d)) sort(unique(as.character(d[[outcome]][!is.na(d[[outcome]])]))) else character()
      selected_class <- if (length(input$positive_class) == 1L && input$positive_class %in% classes) input$positive_class else tail(classes, 1)
      updateSelectInput(session, "positive_class", choices = classes, selected = selected_class)
    })
    observe({
      result(NULL); status("数据、字段或参数已更新，请点击“运行逻辑回归”。")
      data(); input$outcome; input$predictors; input$positive_class; input$train_ratio; input$seed
    }, priority = 100)
    observeEvent(input$run, {
      result(NULL)
      tryCatch({
        fitted <- fit_logistic_regression(data(), input$outcome, input$predictors,
          input$positive_class, input$train_ratio, input$seed)
        result(fitted)
        status(sprintf("建模完成：训练集 %d 行，测试集 %d 行，排除 %d 行%s。",
          fitted$train_n, fitted$test_n, fitted$excluded, if (fitted$converged) "" else "；模型未正常收敛"))
      }, error = function(e) {
        message <- if (inherits(e, "shiny.silent.error")) "请先导入数据并选择字段。" else conditionMessage(e)
        status(paste("未能建模：", message)); showNotification(message, type = "error", duration = 10)
      })
    })
    output$status <- renderText(status())
    output$report <- renderText({ req(result()); result()$report })
    ai_report_server("ai_report", ai_config, "逻辑回归",
      reactive(if (is.null(result())) "" else result()$report),
      reactive(if (is.null(result())) NULL else ai_model_context("逻辑回归", result())), data)
    output$coefficients <- DT::renderDT({
      req(result())
      DT::datatable(result()$coefficients, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) |>
        DT::formatSignif(setdiff(names(result()$coefficients), "字段"), 5)
    })
    output$predictions <- DT::renderDT({ req(result()); DT::datatable(result()$predictions, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) })
    model_evaluation_server("evaluation", result, directory, "逻辑回归")
    write_report <- function(file) { req(result()); writeLines(enc2utf8(result()$report), file, useBytes = TRUE) }
    write_predictions <- function(file) {
      req(result()); con <- file(file, open = "wb"); on.exit(close(con)); writeBin(charToRaw("\ufeff"), con)
      lines <- capture.output(write.csv(result()$predictions, row.names = FALSE, na = ""))
      writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n"))), con)
    }
    output$download_report <- downloadHandler(filename = function() paste0("easyr-logistic-regression-", Sys.Date(), ".txt"), content = write_report)
    output$download_predictions <- downloadHandler(filename = function() paste0("easyr-logistic-predictions-", Sys.Date(), ".csv"), content = write_predictions)
    output$saved <- renderText(saved())
    observeEvent(input$save_report, {
      req(result())
      tryCatch({
        path <- save_to_workdir(directory(), "easyr-logistic-regression", ".txt", write_report)
        saved(paste("上次保存：", path)); showNotification("逻辑回归报告已保存到 RBench 项目文件夹。", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })
    result
  })
}
