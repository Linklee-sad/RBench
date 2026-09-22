evaluation_ratio <- function(numerator, denominator) {
  if (!length(denominator) || !is.finite(denominator) || denominator <= 0) return(NA_real_)
  as.numeric(numerator) / as.numeric(denominator)
}

classification_evaluation <- function(actual, predicted = NULL, probabilities = NULL,
                                      positive_class = NULL, threshold = 0.5) {
  actual <- droplevels(factor(actual))
  classes <- levels(actual)
  if (length(classes) < 2L) stop("分类评估至少需要两个实际类别。", call. = FALSE)
  threshold <- as.numeric(threshold)
  if (length(threshold) != 1L || !is.finite(threshold) || threshold < 0 || threshold > 1) {
    stop("分类阈值应在 0 到 1 之间。", call. = FALSE)
  }

  probability_matrix <- NULL
  if (!is.null(probabilities)) {
    probability_matrix <- as.matrix(probabilities)
    storage.mode(probability_matrix) <- "double"
    if (nrow(probability_matrix) != length(actual)) stop("概率行数与测试集行数不一致。", call. = FALSE)
    if (is.null(colnames(probability_matrix))) stop("类别概率必须包含列名。", call. = FALSE)
    missing_classes <- setdiff(classes, colnames(probability_matrix))
    if (length(missing_classes)) stop("类别概率缺少实际类别列。", call. = FALSE)
    probability_matrix <- probability_matrix[, classes, drop = FALSE]
  }

  is_binary <- length(classes) == 2L && !is.null(probability_matrix)
  if (is_binary) {
    if (is.null(positive_class) || !length(positive_class) || !positive_class %in% classes) positive_class <- classes[2L]
    negative_class <- setdiff(classes, positive_class)[1L]
    score <- probability_matrix[, positive_class]
    predicted <- factor(ifelse(score >= threshold, positive_class, negative_class), levels = classes)
  } else {
    positive_class <- NULL
    score <- NULL
    if (is.null(predicted) && !is.null(probability_matrix)) {
      predicted <- factor(classes[max.col(probability_matrix, ties.method = "first")], levels = classes)
    } else {
      predicted <- factor(as.character(predicted), levels = classes)
    }
  }
  if (length(predicted) != length(actual)) stop("预测结果与测试集行数不一致。", call. = FALSE)

  confusion <- table(实际值 = actual, 预测值 = predicted)
  total <- sum(confusion)
  accuracy <- evaluation_ratio(sum(diag(confusion)), total)
  per_class <- do.call(rbind, lapply(seq_along(classes), function(i) {
    tp <- confusion[i, i]
    fp <- sum(confusion[, i]) - tp
    fn <- sum(confusion[i, ]) - tp
    tn <- total - tp - fp - fn
    precision <- evaluation_ratio(tp, tp + fp)
    recall <- evaluation_ratio(tp, tp + fn)
    specificity <- evaluation_ratio(tn, tn + fp)
    f1 <- if (is.finite(precision) && is.finite(recall) && precision + recall > 0) {
      2 * precision * recall / (precision + recall)
    } else NA_real_
    data.frame(类别 = classes[i], Precision = precision, Recall = recall,
      Specificity = specificity, F1 = f1, Support = sum(confusion[i, ]), check.names = FALSE)
  }))
  macro <- vapply(per_class[c("Precision", "Recall", "Specificity", "F1")], function(x) mean(x, na.rm = TRUE), numeric(1))
  weights <- per_class$Support / sum(per_class$Support)
  weighted_f1 <- sum(per_class$F1 * weights, na.rm = TRUE)

  roc <- NULL
  threshold_table <- NULL
  auc <- NA_real_
  if (is_binary) {
    positive <- actual == positive_class
    n_positive <- sum(positive)
    n_negative <- sum(!positive)
    if (n_positive > 0L && n_negative > 0L && all(is.finite(score))) {
      ranks <- rank(score, ties.method = "average")
      auc <- (sum(ranks[positive]) - n_positive * (n_positive + 1) / 2) / (n_positive * n_negative)
      cutoffs <- c(Inf, sort(unique(score), decreasing = TRUE), -Inf)
      roc <- do.call(rbind, lapply(cutoffs, function(cutoff) {
        selected <- score >= cutoff
        data.frame(阈值 = cutoff,
          FPR = evaluation_ratio(sum(selected & !positive), n_negative),
          TPR = evaluation_ratio(sum(selected & positive), n_positive), check.names = FALSE)
      }))
      grid <- sort(unique(c(seq(0.05, 0.95, 0.05), threshold)))
      threshold_table <- do.call(rbind, lapply(grid, function(cutoff) {
        selected <- score >= cutoff
        tp <- sum(selected & positive); fp <- sum(selected & !positive)
        fn <- sum(!selected & positive); tn <- sum(!selected & !positive)
        precision <- evaluation_ratio(tp, tp + fp)
        recall <- evaluation_ratio(tp, tp + fn)
        specificity <- evaluation_ratio(tn, tn + fp)
        f1 <- if (is.finite(precision) && is.finite(recall) && precision + recall > 0) 2 * precision * recall / (precision + recall) else NA_real_
        data.frame(阈值 = cutoff, Accuracy = evaluation_ratio(tp + tn, total),
          Precision = precision, Recall = recall, Specificity = specificity, F1 = f1, check.names = FALSE)
      }))
    }
  }

  summary <- data.frame(
    指标 = c("Accuracy", "Macro Precision", "Macro Recall（平衡准确率）", "Macro F1", "Weighted F1",
      if (is_binary) paste0("ROC AUC（正类：", positive_class, "）") else character()),
    数值 = c(accuracy, macro[["Precision"]], macro[["Recall"]], macro[["F1"]], weighted_f1,
      if (is_binary) auc else numeric()), check.names = FALSE)
  list(actual = actual, predicted = predicted, probabilities = probability_matrix,
    classes = classes, positive_class = positive_class, threshold = threshold,
    confusion = confusion, summary = summary, per_class = per_class,
    roc = roc, auc = auc, threshold_table = threshold_table, binary = is_binary)
}

regression_evaluation <- function(actual, predicted) {
  actual <- as.numeric(actual); predicted <- as.numeric(predicted)
  if (length(actual) != length(predicted) || !length(actual)) stop("实际值与预测值长度不一致。", call. = FALSE)
  residual <- actual - predicted
  denominator <- sum((actual - mean(actual))^2)
  r2 <- if (denominator > 0) 1 - sum(residual^2) / denominator else NA_real_
  list(actual = actual, predicted = predicted, residual = residual,
    summary = data.frame(指标 = c("RMSE", "MAE", "R²"),
      数值 = c(sqrt(mean(residual^2)), mean(abs(residual)), r2), check.names = FALSE))
}

model_evaluation_theme <- function() {
  ggplot2::theme_minimal(base_size = 13, base_family = plot_editor_family()) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(), legend.position = "bottom")
}

build_confusion_plot <- function(evaluation, model_label = "模型") {
  d <- as.data.frame(evaluation$confusion)
  ggplot2::ggplot(d, ggplot2::aes(实际值, 预测值, fill = Freq)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = Freq), colour = "#172b4d", size = 4) +
    ggplot2::scale_fill_gradient(low = "#dbeafe", high = "#2563eb") +
    model_evaluation_theme() +
    ggplot2::labs(title = paste0(model_label, "：测试集混淆矩阵"),
      subtitle = "对角线表示预测正确的样本", x = "实际类别", y = "预测类别", fill = "样本数")
}

build_roc_plot <- function(evaluation, model_label = "模型") {
  if (is.null(evaluation$roc) || !nrow(evaluation$roc)) stop("当前结果不能绘制 ROC 曲线。", call. = FALSE)
  ggplot2::ggplot(evaluation$roc, ggplot2::aes(FPR, TPR)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "#94a3b8") +
    ggplot2::geom_line(colour = "#2563eb", linewidth = 1.1) +
    ggplot2::coord_equal() + model_evaluation_theme() +
    ggplot2::labs(title = paste0(model_label, "：ROC 曲线"),
      subtitle = sprintf("正类：%s · AUC = %.3f", evaluation$positive_class, evaluation$auc),
      x = "假阳性率（FPR）", y = "真正率（TPR）")
}

build_threshold_plot <- function(evaluation, model_label = "模型") {
  d <- evaluation$threshold_table
  if (is.null(d) || !nrow(d)) stop("当前结果不能比较分类阈值。", call. = FALSE)
  long <- stats::reshape(d[, c("阈值", "Precision", "Recall", "F1")],
    varying = c("Precision", "Recall", "F1"), v.names = "数值", timevar = "指标",
    times = c("Precision", "Recall", "F1"), direction = "long")
  rownames(long) <- NULL
  ggplot2::ggplot(long, ggplot2::aes(阈值, 数值, colour = 指标)) +
    ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +
    ggplot2::geom_vline(xintercept = evaluation$threshold, linetype = "dashed", colour = "#d97706") +
    ggplot2::scale_colour_manual(values = c(Precision = "#2563eb", Recall = "#059669", F1 = "#d97706")) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) + model_evaluation_theme() +
    ggplot2::labs(title = paste0(model_label, "：分类阈值比较"),
      subtitle = "橙色虚线是当前阈值；降低阈值通常提高 Recall，升高阈值通常提高 Precision",
      x = "判为正类所需概率", y = "指标值", colour = NULL)
}

model_evaluation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(".model-evaluation-note{background:#eef5ff;border:1px solid #c8dcf5;border-radius:9px;padding:10px 12px;color:#36577e;margin:8px 0 12px}.model-evaluation-controls{max-width:720px}")),
    uiOutput(ns("content"))
  )
}

model_evaluation_server <- function(id, result, directory = reactive(getwd()), model_label = "模型") {
  moduleServer(id, function(input, output, session) {
    output$content <- renderUI({
      fitted <- result()
      if (is.null(fitted)) return(tags$div(class = "model-evaluation-note", "运行模型后，这里会显示统一口径的测试集评估。"))
      if (identical(fitted$task, "regression")) {
        return(tagList(
          tags$div(class = "model-evaluation-note", "回归任务显示 RMSE、MAE 和 R²；分类任务还会显示混淆矩阵、Precision、Recall、F1、ROC、AUC 与阈值比较。"),
          DT::DTOutput(ns("summary"))))
      }
      binary <- !is.null(fitted$probabilities) && length(levels(factor(fitted$actual))) == 2L
      tagList(
        tags$div(class = "model-evaluation-note",
          "所有指标都来自未参与训练的测试集。类别不平衡时，请同时查看 Macro Recall、F1 和每类结果，不要只看 Accuracy。阈值调整用于比较决策取舍，不会重新训练模型。"),
        if (binary) tags$div(class = "model-evaluation-controls", fluidRow(
          column(6, selectInput(ns("positive_class"), "正类", choices = levels(factor(fitted$actual)), selected = levels(factor(fitted$actual))[2L])),
          column(6, sliderInput(ns("threshold"), "分类阈值", min = 0.05, max = 0.95, value = 0.5, step = 0.05)))),
        tabsetPanel(
          tabPanel("指标总览", DT::DTOutput(ns("summary")), h4("按类别指标"), DT::DTOutput(ns("per_class"))),
          tabPanel("混淆矩阵", ggplot_editor_ui(ns("confusion_editor"), height = "480px")),
          if (binary) tabPanel("ROC 与 AUC", ggplot_editor_ui(ns("roc_editor"), height = "480px")),
          if (binary) tabPanel("阈值分析", ggplot_editor_ui(ns("threshold_editor"), height = "480px"), DT::DTOutput(ns("threshold_table")))
        ))
    })

    evaluation <- reactive({
      fitted <- result(); req(fitted)
      if (identical(fitted$task, "regression")) return(regression_evaluation(fitted$actual, fitted$predicted))
      classes <- levels(factor(fitted$actual))
      binary <- !is.null(fitted$probabilities) && length(classes) == 2L
      positive <- if (binary && !is.null(input$positive_class) && input$positive_class %in% classes) input$positive_class else if (binary) classes[2L] else NULL
      threshold <- if (binary && !is.null(input$threshold)) input$threshold else 0.5
      classification_evaluation(fitted$actual, fitted$predicted, fitted$probabilities, positive, threshold)
    })
    output$summary <- DT::renderDT({
      e <- evaluation()
      DT::datatable(e$summary, rownames = FALSE, options = list(dom = "t"),
        caption = if (!is.null(e$positive_class)) paste0("当前正类：", e$positive_class, "；阈值：", format(e$threshold, nsmall = 2)) else NULL) |>
        DT::formatRound("数值", 4)
    })
    output$per_class <- DT::renderDT({
      e <- evaluation(); req(!is.null(e$per_class))
      DT::datatable(e$per_class, rownames = FALSE, options = list(dom = "t", scrollX = TRUE)) |>
        DT::formatRound(c("Precision", "Recall", "Specificity", "F1"), 4)
    })
    output$threshold_table <- DT::renderDT({
      e <- evaluation(); req(!is.null(e$threshold_table))
      DT::datatable(e$threshold_table, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) |>
        DT::formatRound(names(e$threshold_table), 3)
    })
    confusion_plot <- reactive({ e <- evaluation(); req(!is.null(e$confusion)); build_confusion_plot(e, model_label) })
    roc_plot <- reactive({ e <- evaluation(); req(isTRUE(e$binary)); build_roc_plot(e, model_label) })
    threshold_plot <- reactive({ e <- evaluation(); req(isTRUE(e$binary)); build_threshold_plot(e, model_label) })
    slug <- gsub("(^-+|-+$)", "", tolower(gsub("[^A-Za-z0-9]+", "-", model_label)))
    if (!nzchar(slug)) slug <- "model"
    ggplot_editor_server("confusion_editor", confusion_plot, directory, paste0("easyr-", slug, "-confusion"))
    ggplot_editor_server("roc_editor", roc_plot, directory, paste0("easyr-", slug, "-roc"))
    ggplot_editor_server("threshold_editor", threshold_plot, directory, paste0("easyr-", slug, "-threshold"))
    evaluation
  })
}
