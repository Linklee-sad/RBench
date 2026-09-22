if (!exists("ai_report_ui", mode = "function")) source("R/ai.R")
if (!exists("algorithm_title_ui", mode = "function")) source("R/algorithm_tutorials.R")
if (!exists("model_evaluation_ui", mode = "function")) source("R/model_evaluation.R")
if (!exists("prepare_random_forest_data", mode = "function")) source("R/random_forest.R")

tree_number <- function(x) {
  if (!length(x) || !is.finite(x)) return("无法计算")
  format(signif(x, 4), trim = TRUE, scientific = abs(x) > 0 && abs(x) < .001)
}

tree_variable_label <- function(name, predictors) {
  match <- suppressWarnings(as.integer(sub("^predictor([0-9]+).*$", "\\1", name)))
  if (is.finite(match) && match %in% seq_along(predictors)) predictors[match] else name
}

fit_decision_tree <- function(data, outcome, predictors, task = "auto", train_ratio = .8,
                              cp = .01, maxdepth = 5, minsplit = 20, seed = 2026) {
  if (!requireNamespace("rpart", quietly = TRUE)) stop("缺少 rpart 包，请重新点击 Run App 让程序自动安装。", call. = FALSE)
  prepared <- prepare_random_forest_data(data, outcome, predictors, task)
  cp <- as.numeric(cp); maxdepth <- as.integer(maxdepth); minsplit <- as.integer(minsplit); seed <- as.integer(seed)
  if (!is.finite(cp) || cp < 0 || cp > .5) stop("复杂度参数 cp 应在 0 到 0.5 之间。", call. = FALSE)
  if (!is.finite(maxdepth) || maxdepth < 1L || maxdepth > 15L) stop("最大深度应在 1 到 15 之间。", call. = FALSE)
  if (!is.finite(minsplit) || minsplit < 2L || minsplit > nrow(prepared$data)) stop("最小分裂样本数应在 2 与有效样本量之间。", call. = FALSE)
  split <- stratified_rf_split(prepared$data$outcome, train_ratio, seed, prepared$task == "classification")
  train <- prepared$data[split$train, , drop = FALSE]; test <- prepared$data[split$test, , drop = FALSE]
  set.seed(seed)
  model <- rpart::rpart(outcome ~ ., data = train,
    method = if (prepared$task == "classification") "class" else "anova",
    control = rpart::rpart.control(cp = cp, maxdepth = maxdepth, minsplit = minsplit, xval = 10),
    model = TRUE)
  probabilities <- NULL
  if (prepared$task == "classification") {
    probabilities <- stats::predict(model, newdata = test[-1], type = "prob")
    classes <- levels(prepared$data$outcome)
    probabilities <- probabilities[, classes, drop = FALSE]
    predicted <- factor(classes[max.col(probabilities, ties.method = "first")], levels = classes)
    actual <- factor(test$outcome, levels = classes)
    evaluation <- classification_evaluation(actual, predicted, probabilities)
    metrics <- evaluation$summary; details <- evaluation$per_class; confusion <- evaluation$confusion
    predictions <- data.frame(原始行号 = prepared$valid_rows[split$test], 实际类别 = as.character(actual),
      预测类别 = as.character(predicted), 是否正确 = actual == predicted, check.names = FALSE)
    probability_table <- as.data.frame(probabilities, check.names = FALSE)
    names(probability_table) <- paste0("概率_", names(probability_table))
    predictions <- data.frame(predictions, probability_table, check.names = FALSE)
    headline <- paste0("测试集 Accuracy = ", tree_number(metrics$数值[metrics$指标 == "Accuracy"]),
      "，Macro F1 = ", tree_number(metrics$数值[metrics$指标 == "Macro F1"]), "。")
  } else {
    actual <- as.numeric(test$outcome); predicted <- as.numeric(stats::predict(model, newdata = test[-1]))
    evaluation <- regression_evaluation(actual, predicted)
    metrics <- evaluation$summary; details <- data.frame(); confusion <- NULL
    predictions <- data.frame(原始行号 = prepared$valid_rows[split$test], 实际值 = actual,
      预测值 = predicted, 残差 = actual - predicted, check.names = FALSE)
    headline <- paste0("测试集 RMSE = ", tree_number(metrics$数值[metrics$指标 == "RMSE"]),
      "，MAE = ", tree_number(metrics$数值[metrics$指标 == "MAE"]),
      "，R² = ", tree_number(metrics$数值[metrics$指标 == "R²"]), "。")
  }
  raw_importance <- model$variable.importance
  importance <- if (is.null(raw_importance)) data.frame(字段 = character(), 重要性 = numeric(), check.names = FALSE) else {
    values <- as.numeric(raw_importance)
    data.frame(字段 = vapply(names(raw_importance), tree_variable_label, character(1), predictors = prepared$predictors),
      重要性 = values, check.names = FALSE)
  }
  analysis_summary <- ai_analysis_summary(prepared$data, c(prepared$outcome, prepared$predictors))
  leaves <- sum(model$frame$var == "<leaf>"); depth <- max(floor(log(as.numeric(row.names(model$frame)), base = 2)))
  report <- c("决策树：专业模型解读", "一、模型设定",
    paste0("任务类型：", if (prepared$task == "classification") "分类" else "回归", "；目标字段：", prepared$outcome, "。"),
    paste0("预测字段：", paste(prepared$predictors, collapse = "、"), "。"),
    sprintf("使用 %d 行完整数据；训练集 %d 行，测试集 %d 行；排除 %d 行。", nrow(prepared$data), nrow(train), nrow(test), prepared$excluded),
    paste0("参数：cp = ", tree_number(cp), "，最大深度 = ", maxdepth, "，最小分裂样本数 = ", minsplit, "。最终树深度为 ", depth, "，叶节点数为 ", leaves, "。"),
    "二、测试集表现", headline,
    "指标来自未参与训练的测试集。单棵树容易随样本变化而改变，重要应用应使用重复验证或独立外部数据复核。",
    "三、树结构与变量重要性",
    if (nrow(importance)) paste0("当前树的重要字段依次为：", paste(head(importance$字段, 5), collapse = "、"), "。") else "当前参数下没有产生有效分裂，模型只包含根节点。",
    "每个内部节点使用一个条件划分样本，叶节点给出最终预测。变量重要性汇总该字段带来的纯度改善；它表示本模型中的预测贡献，不代表因果效应。",
    "四、复杂度与解释边界",
    "较小 cp、较大最大深度和较小 minsplit 会允许更复杂的树，可能提高训练拟合但增加过拟合风险。较大的 cp 会剪去改善有限的分支。",
    "决策树能表达非线性和变量交互，但分裂点可能不稳定。类别不平衡、数据泄漏、时间顺序被打乱或同一对象跨训练集和测试集出现，都会使评估过于乐观。")
  list(model = model, task = prepared$task, outcome = prepared$outcome, predictors = prepared$predictors,
    metrics = metrics, details = details, predictions = predictions, importance = importance,
    report = paste(report, collapse = "\n\n"), used = nrow(prepared$data), excluded = prepared$excluded,
    train_n = nrow(train), test_n = nrow(test), actual = actual, predicted = predicted,
    probabilities = probabilities, confusion = confusion, analysis_summary = analysis_summary,
    leaves = leaves, depth = depth, cp = cp, maxdepth = maxdepth, minsplit = minsplit)
}

build_decision_tree_plot <- function(result) {
  frame <- result$model$frame
  nodes <- as.numeric(row.names(frame)); depth <- floor(log(nodes, base = 2))
  position <- (nodes - 2^depth + .5) / 2^depth
  labels <- ifelse(frame$var == "<leaf>",
    if (result$task == "classification") paste0("预测：", result$model$ylevels[frame$yval], "\nn = ", frame$n) else paste0("预测：", signif(frame$yval, 4), "\nn = ", frame$n),
    paste0(vapply(frame$var, tree_variable_label, character(1), predictors = result$predictors), "\nn = ", frame$n))
  d <- data.frame(node = nodes, x = position, y = -depth, label = labels, leaf = frame$var == "<leaf>")
  edges <- do.call(rbind, lapply(nodes[nodes != 1], function(node) {
    child <- d[d$node == node, ]; parent <- d[d$node == floor(node / 2), ]
    data.frame(x = parent$x, y = parent$y, xend = child$x, yend = child$y)
  }))
  ggplot2::ggplot() +
    ggplot2::geom_segment(data = edges, ggplot2::aes(x, y, xend = xend, yend = yend), colour = "#94a3b8", linewidth = .7) +
    ggplot2::geom_point(data = d, ggplot2::aes(x, y, fill = leaf), shape = 21, size = 9, colour = "white") +
    ggplot2::geom_label(data = d, ggplot2::aes(x, y, label = label, fill = leaf), size = 3.1, linewidth = .2, label.padding = grid::unit(.18, "lines")) +
    ggplot2::scale_fill_manual(values = c(`FALSE` = "#dbeafe", `TRUE` = "#bbf7d0"), guide = "none") +
    ggplot2::scale_y_continuous(breaks = -seq_len(result$depth + 1L) + 1, labels = seq_len(result$depth + 1L) - 1) +
    ggplot2::coord_cartesian(clip = "off") + model_evaluation_theme() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank()) +
    ggplot2::labs(title = "决策树结构", subtitle = "蓝色为分裂节点，绿色为叶节点；节点中的 n 是训练样本数", x = NULL, y = "树深度")
}

build_tree_importance_plot <- function(result) {
  d <- result$importance
  if (!nrow(d)) return(ggplot2::ggplot(data.frame(x = 0, y = 0), ggplot2::aes(x, y)) + model_evaluation_theme() + ggplot2::annotate("text", 0, 0, label = "当前树没有产生分裂") + ggplot2::theme(axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank()))
  d <- d[order(d$重要性), , drop = FALSE]; d$字段 <- factor(d$字段, levels = d$字段)
  ggplot2::ggplot(d, ggplot2::aes(字段, 重要性)) + ggplot2::geom_col(fill = "#2563eb", alpha = .85) +
    ggplot2::coord_flip() + model_evaluation_theme() + ggplot2::labs(title = "决策树变量重要性", x = NULL, y = "纯度改善")
}

build_tree_regression_plot <- function(result) {
  d <- data.frame(actual = result$actual, predicted = result$predicted)
  ggplot2::ggplot(d, ggplot2::aes(actual, predicted)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "#d97706") +
    ggplot2::geom_point(colour = "#2563eb", alpha = .7, size = 2.5) + ggplot2::coord_equal() +
    model_evaluation_theme() + ggplot2::labs(title = "决策树：测试集实际值与预测值", x = "实际值", y = "预测值")
}

decision_tree_ui <- function(id) {
  ns <- NS(id)
  tagList(
    algorithm_title_ui(ns, "决策树"),
    p("用一系列容易阅读的条件分裂建立回归或分类模型，并在独立测试集上评价预测表现。"),
    conditionalPanel(sprintf("input['%s'] %% 2 === 1", ns("tutorial_toggle")), algorithm_tutorial_ui(ns("tutorial"), "decision_tree")),
    fluidRow(column(5, selectInput(ns("outcome"), "目标字段", NULL)),
      column(7, selectizeInput(ns("predictors"), "预测字段（可多选）", NULL, multiple = TRUE))),
    fluidRow(column(3, selectInput(ns("task"), "任务类型", c("自动判断" = "auto", "回归" = "regression", "分类" = "classification"))),
      column(3, sliderInput(ns("train_ratio"), "训练集比例", .5, .9, .8, step = .05)),
      column(3, numericInput(ns("seed"), "随机种子", 2026, min = 1)),
      column(3, numericInput(ns("maxdepth"), "最大深度", 5, min = 1, max = 15))),
    fluidRow(column(6, numericInput(ns("cp"), "复杂度参数 cp", .01, min = 0, max = .5, step = .005)),
      column(6, numericInput(ns("minsplit"), "最小分裂样本数", 20, min = 2, step = 1))),
    ai_parameter_ui(ns("ai_params")),
    helpText("cp 越大，剪枝越强，树通常越简单；最大深度和最小分裂样本数共同限制模型复杂度。"),
    actionButton(ns("run"), "运行决策树", class = "btn-primary"),
    tags$div(style = "margin:12px 0", textOutput(ns("status"))),
    tabsetPanel(
      tabPanel("专业解读", tags$div(style = "white-space:pre-wrap;line-height:1.9", textOutput(ns("report")))),
      tabPanel("AI 增强解读", ai_report_ui(ns("ai_report"))),
      tabPanel("树结构", ggplot_editor_ui(ns("tree_editor"), height = "560px")),
      tabPanel("模型评估", model_evaluation_ui(ns("evaluation")), uiOutput(ns("regression_plot_ui"))),
      tabPanel("变量重要性", DT::DTOutput(ns("importance")), ggplot_editor_ui(ns("importance_editor"), height = "480px")),
      tabPanel("测试集预测", DT::DTOutput(ns("predictions")))
    ), hr(), downloadButton(ns("download_report"), "下载决策树报告 TXT"),
    downloadButton(ns("download_predictions"), "下载测试集预测 CSV"),
    actionButton(ns("save_report"), "保存决策树报告到项目文件夹"), textOutput(ns("saved")))
}

decision_tree_server <- function(id, data, directory = reactive(getwd()), ai_config = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    algorithm_tutorial_server("tutorial", "decision_tree"); algorithm_tutorial_toggle_server(input, session)
    result <- reactiveVal(NULL); status <- reactiveVal("选择字段后，点击“运行决策树”。"); saved <- reactiveVal("")
    ai_params <- ai_parameter_server("ai_params", ai_config, data, "决策树",
      reactive(list(train_ratio = input$train_ratio, cp = input$cp, maxdepth = input$maxdepth, minsplit = input$minsplit, seed = input$seed)),
      list(train_ratio = "0.5 到 0.9", cp = "0 到 0.5", maxdepth = "1 到 15", minsplit = "2 到有效样本量", seed = "正整数"))
    observeEvent(ai_params$apply(), {
      p <- ai_params$proposal()$parameters; if (is.null(p)) return()
      if (!is.null(p$train_ratio)) updateSliderInput(session, "train_ratio", value = max(.5, min(.9, as.numeric(p$train_ratio))))
      if (!is.null(p$cp)) updateNumericInput(session, "cp", value = max(0, min(.5, as.numeric(p$cp))))
      if (!is.null(p$maxdepth)) updateNumericInput(session, "maxdepth", value = max(1L, min(15L, as.integer(p$maxdepth))))
      if (!is.null(p$minsplit)) updateNumericInput(session, "minsplit", value = max(2L, as.integer(p$minsplit)))
      if (!is.null(p$seed)) updateNumericInput(session, "seed", value = max(1L, as.integer(p$seed)))
    }, ignoreInit = TRUE)
    observeEvent(data(), {
      d <- data(); choices <- setNames(as.character(seq_along(d)), paste0(seq_along(d), ". ", names(d)))
      selected <- if (length(input$outcome) == 1L && input$outcome %in% unname(choices)) input$outcome else unname(head(choices, 1))
      updateSelectInput(session, "outcome", choices = choices, selected = selected)
    })
    observeEvent(list(data(), input$outcome), {
      d <- data(); eligible <- setdiff(seq_along(d), as.integer(input$outcome)); labels <- if (length(eligible)) paste0(eligible, ". ", names(d)[eligible]) else character()
      choices <- setNames(as.character(eligible), labels); updateSelectizeInput(session, "predictors", choices = choices, selected = intersect(input$predictors, unname(choices)))
    })
    observe({ result(NULL); status("数据、字段或参数已更新，请点击“运行决策树”。"); data(); input$outcome; input$predictors; input$task; input$train_ratio; input$cp; input$maxdepth; input$minsplit; input$seed }, priority = 100)
    observeEvent(input$run, {
      result(NULL)
      tryCatch({
        fitted <- fit_decision_tree(data(), input$outcome, input$predictors, input$task, input$train_ratio, input$cp, input$maxdepth, input$minsplit, input$seed)
        result(fitted); status(sprintf("建模完成：树深度 %d，叶节点 %d；训练集 %d 行，测试集 %d 行。", fitted$depth, fitted$leaves, fitted$train_n, fitted$test_n))
      }, error = function(e) { message <- if (inherits(e, "shiny.silent.error")) "请先导入数据并选择字段。" else conditionMessage(e); status(paste("未能建模：", message)); showNotification(message, type = "error", duration = 10) })
    })
    output$status <- renderText(status()); output$report <- renderText({ req(result()); result()$report })
    ai_report_server("ai_report", ai_config, "决策树", reactive(if (is.null(result())) "" else result()$report), reactive(if (is.null(result())) NULL else ai_model_context("决策树", result())), data)
    output$importance <- DT::renderDT({ req(result()); DT::datatable(result()$importance, rownames = FALSE, options = list(dom = "t")) })
    output$predictions <- DT::renderDT({ req(result()); DT::datatable(result()$predictions, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) })
    tree_plot <- reactive({ req(result()); build_decision_tree_plot(result()) }); importance_plot <- reactive({ req(result()); build_tree_importance_plot(result()) })
    regression_plot <- reactive({ req(result(), result()$task == "regression"); build_tree_regression_plot(result()) })
    output$regression_plot_ui <- renderUI({ req(result()); if (result()$task == "regression") ggplot_editor_ui(session$ns("regression_editor"), height = "480px") })
    ggplot_editor_server("tree_editor", tree_plot, directory, "easyr-decision-tree-structure")
    ggplot_editor_server("importance_editor", importance_plot, directory, "easyr-decision-tree-importance")
    ggplot_editor_server("regression_editor", regression_plot, directory, "easyr-decision-tree-evaluation")
    model_evaluation_server("evaluation", result, directory, "决策树")
    write_report <- function(file) { req(result()); writeLines(enc2utf8(result()$report), file, useBytes = TRUE) }
    write_predictions <- function(file) { req(result()); con <- file(file, "wb"); on.exit(close(con)); writeBin(charToRaw("\ufeff"), con); lines <- capture.output(write.csv(result()$predictions, row.names = FALSE, na = "")); writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n"))), con) }
    output$download_report <- downloadHandler(filename = function() paste0("easyr-decision-tree-", Sys.Date(), ".txt"), content = write_report)
    output$download_predictions <- downloadHandler(filename = function() paste0("easyr-decision-tree-predictions-", Sys.Date(), ".csv"), content = write_predictions)
    output$saved <- renderText(saved())
    observeEvent(input$save_report, { req(result()); tryCatch({ path <- save_to_workdir(directory(), "easyr-decision-tree", ".txt", write_report); saved(paste("上次保存：", path)) }, error = function(e) showNotification(conditionMessage(e), type = "error")) })
    result
  })
}
