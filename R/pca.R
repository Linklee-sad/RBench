if (!exists("ai_report_ui", mode = "function")) source("R/ai.R")
if (!exists("algorithm_title_ui", mode = "function")) source("R/algorithm_tutorials.R")

pca_number <- function(x) {
  if (!length(x) || !is.finite(x)) return("无法计算")
  format(signif(x, 4), trim = TRUE, scientific = abs(x) > 0 && abs(x) < 0.001)
}

fit_pca_analysis <- function(data, variables, standardize = TRUE, group = NULL) {
  variables <- unique(as.integer(variables))
  if (length(variables) < 2L || anyNA(variables) || !all(variables %in% seq_along(data))) {
    stop("请至少选择两个数值字段。", call. = FALSE)
  }
  numeric_fields <- vapply(data[variables], is.numeric, logical(1))
  if (!all(numeric_fields)) stop("主成分分析只能使用数值字段。", call. = FALSE)

  selected <- data[, variables, drop = FALSE]
  valid <- stats::complete.cases(selected)
  for (x in selected) valid <- valid & is.finite(x)
  selected <- selected[valid, , drop = FALSE]
  if (nrow(selected) < 3L) stop("有效数据不足 3 行，无法进行主成分分析。", call. = FALSE)

  varying <- vapply(selected, function(x) length(unique(x)) > 1L && stats::sd(x) > 0, logical(1))
  removed <- names(selected)[!varying]
  selected <- selected[, varying, drop = FALSE]
  if (ncol(selected) < 2L) stop("移除无变化字段后不足两个数值字段，无法进行主成分分析。", call. = FALSE)
  analysis_summary <- ai_analysis_summary(selected)

  model <- stats::prcomp(selected, center = TRUE, scale. = isTRUE(standardize))
  component_count <- ncol(model$rotation)
  component_names <- paste0("PC", seq_len(component_count))
  eigenvalues <- model$sdev^2
  explained <- eigenvalues / sum(eigenvalues)
  variance <- data.frame(
    主成分 = component_names,
    特征值 = eigenvalues,
    方差解释率 = explained,
    累计解释率 = cumsum(explained),
    check.names = FALSE
  )
  loadings <- data.frame(字段 = rownames(model$rotation), model$rotation, check.names = FALSE, row.names = NULL)
  names(loadings)[-1] <- component_names
  scores <- data.frame(原始行号 = which(valid), model$x, check.names = FALSE, row.names = NULL)
  names(scores)[-1] <- component_names

  group_label <- NULL
  group_values <- NULL
  if (length(group) == 1L && nzchar(group)) {
    group <- as.integer(group)
    if (is.na(group) || !group %in% seq_along(data)) stop("请选择有效的分组字段。", call. = FALSE)
    group_values <- as.character(data[[group]][valid])
    group_values[is.na(group_values) | !nzchar(group_values)] <- "缺失"
    if (length(unique(group_values)) > 20L) stop("分组字段超过 20 类，请选择类别更少的字段或不分组。", call. = FALSE)
    group_label <- names(data)[group]
    scores$分组 <- group_values
  }

  threshold_component <- function(threshold) {
    index <- which(cumsum(explained) >= threshold)[1]
    if (is.na(index)) component_count else index
  }
  component_notes <- character(min(3L, component_count))
  for (i in seq_along(component_notes)) {
    contribution <- model$rotation[, i]^2
    contribution <- contribution / sum(contribution)
    top <- order(contribution, decreasing = TRUE)[seq_len(min(3L, length(contribution)))]
    directions <- ifelse(model$rotation[top, i] >= 0, "正向", "负向")
    items <- paste0(names(contribution)[top], "（", directions, "载荷 ",
      vapply(abs(model$rotation[top, i]), pca_number, character(1)), "）")
    component_notes[i] <- paste0(component_names[i], "解释 ", pca_number(explained[i] * 100),
      "% 的总变异，绝对载荷较高的字段为：", paste(items, collapse = "、"), "。")
  }
  report <- c(
    "主成分分析（PCA）：专业解读",
    "一、分析设定",
    paste0("分析字段：", paste(names(selected), collapse = "、"), "。"),
    paste0("预处理：各字段", if (isTRUE(standardize)) "已中心化并标准化为相同尺度" else "已中心化但未标准化，原始量纲和方差会影响结果", "。"),
    sprintf("原始数据 %d 行；用于分析 %d 行；因所选字段缺失或包含非有限数值而排除 %d 行。", nrow(data), nrow(selected), sum(!valid)),
    if (length(removed)) paste0("以下无变化字段已自动移除：", paste(removed, collapse = "、"), "。") else "没有因无变化而移除的字段。",
    "二、降维结果",
    paste0("共得到 ", component_count, " 个主成分。前两个主成分累计解释 ",
      pca_number(sum(explained[seq_len(min(2L, component_count))]) * 100), "% 的总变异。"),
    paste0("达到至少 80%、90% 和 95% 累计解释率分别需要 ", threshold_component(0.80), "、",
      threshold_component(0.90), " 和 ", threshold_component(0.95), " 个主成分。"),
    component_notes,
    "三、解释方法",
    "载荷的绝对值越大，字段对相应主成分的贡献通常越明显；同号字段沿该主成分共同变化，异号字段沿相反方向变化。主成分正负号可以整体翻转而不改变数学含义，因此应关注相对方向和绝对大小。",
    "得分图中距离较近的观测在所选数值字段的综合结构上较相似。若前两个主成分累计解释率较低，二维图只能展示数据结构的一部分，不能代表全部差异。",
    "四、分析边界",
    "PCA 是无监督的线性降维方法，主要描述方差结构，不使用结果变量，也不提供因果效应或显著性检验。异常值、变量尺度、缺失值处理和字段选择都会影响结果。对于明显非线性结构、时间依赖或类别变量，应结合其他方法分析。"
  )
  list(model = model, variance = variance, loadings = loadings, scores = scores,
    report = paste(report, collapse = "\n\n"), used = nrow(selected), excluded = sum(!valid),
    variables = names(selected), removed = removed, standardize = isTRUE(standardize),
    group_label = group_label, group_values = group_values, analysis_summary = analysis_summary)
}

build_pca_scree_plot <- function(result) {
  d <- result$variance
  d$主成分 <- factor(d$主成分, levels = d$主成分)
  ggplot2::ggplot(d, ggplot2::aes(主成分)) +
    ggplot2::geom_col(ggplot2::aes(y = 方差解释率), fill = "#2563eb", alpha = 0.82) +
    ggplot2::geom_line(ggplot2::aes(y = 累计解释率, group = 1), colour = "#d97706", linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(y = 累计解释率), colour = "#d97706", size = 2.4) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(x * 100), "%"), limits = c(0, 1)) +
    lm_plot_theme() + ggplot2::labs(title = "PCA 方差解释率碎石图",
      subtitle = "蓝柱为单个主成分解释率；橙线为累计解释率", x = "主成分", y = "方差解释率")
}

build_pca_scores_plot <- function(result) {
  d <- result$scores
  labels <- result$variance
  x_label <- paste0("PC1（", round(labels$方差解释率[1] * 100, 1), "%）")
  y_label <- paste0("PC2（", round(labels$方差解释率[2] * 100, 1), "%）")
  if (!is.null(result$group_label)) {
    return(ggplot2::ggplot(d, ggplot2::aes(PC1, PC2, colour = 分组)) +
      ggplot2::geom_point(alpha = 0.72, size = 2.5) + lm_plot_theme() +
      ggplot2::labs(title = "PCA 前两个主成分得分图", subtitle = paste0("按“", result$group_label, "”着色"),
        x = x_label, y = y_label, colour = result$group_label))
  }
  ggplot2::ggplot(d, ggplot2::aes(PC1, PC2)) +
    ggplot2::geom_point(colour = "#2563eb", alpha = 0.72, size = 2.5) + lm_plot_theme() +
    ggplot2::labs(title = "PCA 前两个主成分得分图", subtitle = "每个点代表一行有效数据", x = x_label, y = y_label)
}

build_pca_loadings_plot <- function(result) {
  d <- result$loadings
  circle <- data.frame(angle = seq(0, 2 * pi, length.out = 240))
  circle$x <- cos(circle$angle); circle$y <- sin(circle$angle)
  ggplot2::ggplot(d, ggplot2::aes(PC1, PC2)) +
    ggplot2::geom_path(data = circle, ggplot2::aes(x, y), inherit.aes = FALSE, colour = "#94a3b8") +
    ggplot2::geom_hline(yintercept = 0, colour = "#cbd5e1") + ggplot2::geom_vline(xintercept = 0, colour = "#cbd5e1") +
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = PC1, yend = PC2),
      arrow = grid::arrow(length = grid::unit(0.16, "cm")), colour = "#2563eb", linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = 字段), colour = "#172b4d", size = 3.8, check_overlap = TRUE, nudge_y = 0.025) +
    ggplot2::coord_equal(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1)) + lm_plot_theme() +
    ggplot2::labs(title = "PCA PC1–PC2 载荷图", subtitle = "箭头方向表示相关方向，长度表示在前两个主成分上的载荷强度",
      x = "PC1 载荷", y = "PC2 载荷")
}

pca_ui <- function(id) {
  ns <- NS(id)
  tagList(
    algorithm_title_ui(ns, "主成分分析（PCA）"),
    p("选择两个或更多数值字段，通过主成分提取数据中的主要变化结构。使用左侧清洗后的数据。"),
    conditionalPanel(sprintf("input['%s'] %% 2 === 1", ns("tutorial_toggle")),
      algorithm_tutorial_ui(ns("tutorial"), "pca")),
    selectizeInput(ns("variables"), "分析字段（数值，可多选）", NULL, multiple = TRUE),
    fluidRow(column(6, selectInput(ns("group"), "得分图分组字段（可选）", c("不分组" = ""))),
      column(6, checkboxInput(ns("standardize"), "分析前标准化字段（推荐）", TRUE))),
    ai_parameter_ui(ns("ai_params")),
    helpText("字段量纲或波动范围不同时建议保持标准化。分组字段只影响得分图颜色，不参与主成分计算。"),
    actionButton(ns("run"), "运行主成分分析", class = "btn-primary"),
    tags$div(style = "margin:12px 0", textOutput(ns("status"))),
    tabsetPanel(
      tabPanel("专业解读", tags$div(style = "white-space:pre-wrap;line-height:1.9", textOutput(ns("report")))),
      tabPanel("AI 增强解读", ai_report_ui(ns("ai_report"))),
      tabPanel("方差解释率", DT::DTOutput(ns("variance")), ggplot_editor_ui(ns("scree_editor"), height = "480px")),
      tabPanel("主成分得分图", ggplot_editor_ui(ns("scores_editor"), height = "520px")),
      tabPanel("载荷", DT::DTOutput(ns("loadings")), ggplot_editor_ui(ns("loadings_editor"), height = "540px")),
      tabPanel("主成分得分数据", DT::DTOutput(ns("scores")))
    ), hr(),
    downloadButton(ns("download_report"), "下载 PCA 报告 TXT"),
    downloadButton(ns("download_scores"), "下载主成分得分 CSV"),
    downloadButton(ns("download_loadings"), "下载载荷 CSV"),
    actionButton(ns("save_report"), "保存 PCA 报告到项目文件夹"),
    tags$div(style = "overflow-wrap:anywhere", textOutput(ns("saved")))
  )
}

pca_server <- function(id, data, directory = reactive(getwd()), ai_config = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    algorithm_tutorial_server("tutorial", "pca")
    algorithm_tutorial_toggle_server(input, session)
    result <- reactiveVal(NULL)
    status <- reactiveVal("选择数值字段后，点击“运行主成分分析”。")
    saved <- reactiveVal("")
    ai_params <- ai_parameter_server("ai_params", ai_config, data, "主成分分析（PCA）",
      reactive(list(standardize = input$standardize)),
      list(standardize = "true 或 false；字段量纲不同时通常为 true"))
    observeEvent(ai_params$apply(), {
      p <- ai_params$proposal()$parameters; if (is.null(p)) return()
      if (!is.null(p$standardize)) updateCheckboxInput(session, "standardize", value = isTRUE(as.logical(p$standardize)))
      showNotification("AI 建议参数已填入；请检查后点击运行。", type = "message")
    }, ignoreInit = TRUE)
    observeEvent(data(), {
      d <- data(); numeric <- which(vapply(d, is.numeric, logical(1)))
      labels <- if (length(numeric)) paste0(numeric, ". ", names(d)[numeric]) else character()
      choices <- setNames(as.character(numeric), labels)
      selected <- intersect(input$variables, unname(choices))
      if (length(selected) < 2L) selected <- unname(head(choices, min(4L, length(choices))))
      updateSelectizeInput(session, "variables", choices = choices, selected = selected)
      group_indices <- which(vapply(d, function(x) {
        values <- unique(x[!is.na(x)]); length(values) >= 2L && length(values) <= 20L
      }, logical(1)))
      group_labels <- if (length(group_indices)) paste0(group_indices, ". ", names(d)[group_indices]) else character()
      group_choices <- c("不分组" = "", setNames(as.character(group_indices), group_labels))
      updateSelectInput(session, "group", choices = group_choices,
        selected = if (length(input$group) && input$group %in% unname(group_choices)) input$group else "")
    })
    observe({
      result(NULL); status("数据、字段或设置已更新，请点击“运行主成分分析”。")
      data(); input$variables; input$group; input$standardize
    }, priority = 100)
    observeEvent(input$run, {
      result(NULL)
      tryCatch({
        fitted <- fit_pca_analysis(data(), input$variables, isTRUE(input$standardize), input$group)
        result(fitted)
        status(sprintf("分析完成：使用 %d 行、%d 个数值字段，排除 %d 行。", fitted$used, length(fitted$variables), fitted$excluded))
      }, error = function(e) {
        message <- if (inherits(e, "shiny.silent.error")) "请先导入数据并选择数值字段。" else conditionMessage(e)
        status(paste("未能分析：", message)); showNotification(message, type = "error", duration = 10)
      })
    })
    output$status <- renderText(status())
    output$report <- renderText({ req(result()); result()$report })
    ai_report_server("ai_report", ai_config, "主成分分析（PCA）",
      reactive(if (is.null(result())) "" else result()$report),
      reactive(if (is.null(result())) NULL else ai_model_context("主成分分析（PCA）", result())), data)
    output$variance <- DT::renderDT({ req(result()); DT::datatable(result()$variance, rownames = FALSE, options = list(dom = "t", scrollX = TRUE)) })
    output$loadings <- DT::renderDT({ req(result()); DT::datatable(result()$loadings, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) })
    output$scores <- DT::renderDT({ req(result()); DT::datatable(result()$scores, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) })
    scree_plot <- reactive({ req(result()); build_pca_scree_plot(result()) })
    scores_plot <- reactive({ req(result()); build_pca_scores_plot(result()) })
    loadings_plot <- reactive({ req(result()); build_pca_loadings_plot(result()) })
    ggplot_editor_server("scree_editor", scree_plot, directory, "easyr-pca-variance")
    ggplot_editor_server("scores_editor", scores_plot, directory, "easyr-pca-scores")
    ggplot_editor_server("loadings_editor", loadings_plot, directory, "easyr-pca-loadings")
    write_report <- function(file) { req(result()); writeLines(enc2utf8(result()$report), file, useBytes = TRUE) }
    write_csv_bom <- function(value, file) {
      con <- file(file, open = "wb"); on.exit(close(con)); writeBin(charToRaw("\ufeff"), con)
      lines <- capture.output(write.csv(value, row.names = FALSE, na = ""))
      writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n"))), con)
    }
    output$download_report <- downloadHandler(filename = function() paste0("easyr-pca-", Sys.Date(), ".txt"), content = write_report)
    output$download_scores <- downloadHandler(filename = function() paste0("easyr-pca-scores-", Sys.Date(), ".csv"), content = function(file) { req(result()); write_csv_bom(result()$scores, file) })
    output$download_loadings <- downloadHandler(filename = function() paste0("easyr-pca-loadings-", Sys.Date(), ".csv"), content = function(file) { req(result()); write_csv_bom(result()$loadings, file) })
    output$saved <- renderText(saved())
    observeEvent(input$save_report, {
      req(result())
      tryCatch({
        path <- save_to_workdir(directory(), "easyr-pca", ".txt", write_report)
        saved(paste("上次保存：", path)); showNotification("PCA 报告已保存到 EasyR 项目文件夹。", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })
    result
  })
}
