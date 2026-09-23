if (!exists("ai_report_ui", mode = "function")) source("R/ai.R")
if (!exists("algorithm_title_ui", mode = "function")) source("R/algorithm_tutorials.R")

kmeans_number <- function(x) {
  if (!length(x) || !is.finite(x)) return("无法计算")
  format(signif(x, 4), trim = TRUE, scientific = abs(x) > 0 && abs(x) < 0.001)
}

fit_kmeans_analysis <- function(data, variables, centers = 3, standardize = TRUE,
                                nstart = 25, iter_max = 100, seed = 2026) {
  variables <- unique(as.integer(variables))
  centers <- as.integer(centers); nstart <- as.integer(nstart); iter_max <- as.integer(iter_max); seed <- as.integer(seed)
  if (length(variables) < 2L || anyNA(variables) || !all(variables %in% seq_along(data))) {
    stop("请至少选择两个数值字段。", call. = FALSE)
  }
  if (!all(vapply(data[variables], is.numeric, logical(1)))) stop("K-means 只能使用数值字段。", call. = FALSE)
  if (!is.finite(centers) || centers < 2L || centers > 20L) stop("聚类数量应在 2 到 20 之间。", call. = FALSE)
  if (!is.finite(nstart) || nstart < 1L || nstart > 100L) stop("随机初始值次数应在 1 到 100 之间。", call. = FALSE)
  if (!is.finite(iter_max) || iter_max < 10L || iter_max > 1000L) stop("最大迭代次数应在 10 到 1000 之间。", call. = FALSE)
  if (!is.finite(seed)) stop("随机种子必须是整数。", call. = FALSE)

  selected <- data[, variables, drop = FALSE]
  valid <- stats::complete.cases(selected)
  for (x in selected) valid <- valid & is.finite(x)
  selected <- selected[valid, , drop = FALSE]
  if (nrow(selected) < centers + 1L) stop("有效行数必须大于聚类数量。", call. = FALSE)
  varying <- vapply(selected, function(x) length(unique(x)) > 1L && stats::sd(x) > 0, logical(1))
  removed <- names(selected)[!varying]
  selected <- selected[, varying, drop = FALSE]
  if (ncol(selected) < 2L) stop("移除无变化字段后不足两个数值字段，无法聚类。", call. = FALSE)
  if (nrow(unique(selected)) < centers) stop("不同数据点的数量小于聚类数量，请减少聚类数。", call. = FALSE)
  analysis_summary <- ai_analysis_summary(selected)

  if (isTRUE(standardize)) {
    matrix_data <- scale(selected, center = TRUE, scale = TRUE)
    field_center <- attr(matrix_data, "scaled:center")
    field_scale <- attr(matrix_data, "scaled:scale")
  } else {
    matrix_data <- as.matrix(selected)
    field_center <- rep(0, ncol(selected)); field_scale <- rep(1, ncol(selected))
  }
  colnames(matrix_data) <- names(selected)
  set.seed(seed)
  model <- stats::kmeans(matrix_data, centers = centers, iter.max = iter_max,
    nstart = nstart, algorithm = "Hartigan-Wong")

  original_centers <- sweep(sweep(model$centers, 2, field_scale, "*"), 2, field_center, "+")
  centers_table <- data.frame(聚类 = paste0("聚类 ", seq_len(centers)), original_centers,
    check.names = FALSE, row.names = NULL)
  distances <- sqrt(rowSums((matrix_data - model$centers[model$cluster, , drop = FALSE])^2))

  silhouette_limit <- 2000L
  silhouette_index <- seq_len(nrow(matrix_data))
  if (length(silhouette_index) > silhouette_limit) {
    set.seed(seed + 1L)
    silhouette_index <- sample(silhouette_index, silhouette_limit)
    missing_clusters <- setdiff(seq_len(centers), unique(model$cluster[silhouette_index]))
    for (cluster_id in missing_clusters) silhouette_index <- c(silhouette_index, which(model$cluster == cluster_id)[1])
  }
  silhouette_object <- cluster::silhouette(model$cluster[silhouette_index], stats::dist(matrix_data[silhouette_index, , drop = FALSE]))
  silhouette_data <- data.frame(
    原始行号 = which(valid)[silhouette_index],
    聚类 = factor(paste0("聚类 ", model$cluster[silhouette_index]), levels = paste0("聚类 ", seq_len(centers))),
    轮廓系数 = as.numeric(silhouette_object[, "sil_width"]), check.names = FALSE
  )
  average_silhouette <- mean(silhouette_data$轮廓系数)
  silhouette_by_cluster <- tapply(silhouette_data$轮廓系数, silhouette_data$聚类, mean)
  cluster_summary <- data.frame(
    聚类 = paste0("聚类 ", seq_len(centers)),
    样本数 = as.integer(model$size),
    样本占比 = as.integer(model$size) / sum(model$size),
    组内平方和 = model$withinss,
    平均中心距离 = vapply(seq_len(centers), function(i) mean(distances[model$cluster == i]), numeric(1)),
    平均轮廓系数 = as.numeric(silhouette_by_cluster[paste0("聚类 ", seq_len(centers))]),
    check.names = FALSE
  )
  quality <- data.frame(
    指标 = c("总组内平方和", "组间平方和", "总平方和", "组间变异占比", "平均轮廓系数", "实际迭代次数"),
    数值 = c(model$tot.withinss, model$betweenss, model$totss,
      model$betweenss / model$totss, average_silhouette, model$iter), check.names = FALSE
  )

  pca <- stats::prcomp(matrix_data, center = TRUE, scale. = FALSE)
  score_data <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2],
    聚类 = factor(paste0("聚类 ", model$cluster), levels = paste0("聚类 ", seq_len(centers))))
  score_centers <- stats::aggregate(cbind(PC1, PC2) ~ 聚类, score_data, mean)
  assignments <- data.frame(原始行号 = which(valid), selected,
    聚类 = paste0("聚类 ", model$cluster), 到中心距离 = distances, check.names = FALSE, row.names = NULL)

  diagnostic_index <- seq_len(nrow(matrix_data))
  if (length(diagnostic_index) > 10000L) {
    set.seed(seed + 2L); diagnostic_index <- sample(diagnostic_index, 10000L)
  }
  diagnostic_data <- matrix_data[diagnostic_index, , drop = FALSE]
  max_k <- min(max(10L, centers), 20L, nrow(unique(as.data.frame(diagnostic_data))), nrow(diagnostic_data) - 1L)
  elbow <- data.frame(聚类数 = seq_len(max_k), 组内平方和 = NA_real_, check.names = FALSE)
  for (k in seq_len(max_k)) {
    set.seed(seed + k)
    elbow$组内平方和[k] <- stats::kmeans(diagnostic_data, centers = k,
      iter.max = iter_max, nstart = min(nstart, 10L))$tot.withinss
  }

  silhouette_comment <- if (average_silhouette >= 0.5) {
    "平均轮廓系数达到 0.5，当前聚类呈现较清晰的分离结构。"
  } else if (average_silhouette >= 0.25) {
    "平均轮廓系数介于 0.25 和 0.5，当前聚类存在一定结构，但部分样本可能重叠。"
  } else {
    "平均轮廓系数低于 0.25，当前聚类结构较弱；建议检查字段、标准化设置或其他聚类数量。"
  }
  largest <- which.max(model$size); smallest <- which.min(model$size)
  report <- c(
    "K-means 聚类：专业分析报告",
    "一、分析设定",
    paste0("聚类字段：", paste(names(selected), collapse = "、"), "。"),
    paste0("预处理：", if (isTRUE(standardize)) "字段已标准化，距离不会直接被原始量纲较大的字段主导。" else "保留原始量纲，波动范围较大的字段会对欧氏距离产生更大影响。"),
    sprintf("原始数据 %d 行；用于聚类 %d 行；因缺失或非有限数值排除 %d 行。", nrow(data), nrow(selected), sum(!valid)),
    if (length(removed)) paste0("以下无变化字段已自动移除：", paste(removed, collapse = "、"), "。") else "没有因无变化而移除的字段。",
    paste0("设置 ", centers, " 个聚类，使用 ", nstart, " 组随机初始中心，最大迭代次数 ", iter_max, "，随机种子 ", seed, "。"),
    "二、聚类结果",
    paste0("最大聚类为聚类 ", largest, "（", model$size[largest], " 行）；最小聚类为聚类 ", smallest, "（", model$size[smallest], " 行）。"),
    paste0("总组内平方和 = ", kmeans_number(model$tot.withinss), "；组间变异占总变异的 ",
      kmeans_number(100 * model$betweenss / model$totss), "%；平均轮廓系数 = ", kmeans_number(average_silhouette), "。"),
    silhouette_comment,
    "三、如何阅读结果",
    "聚类中心表使用原始字段单位，表示各组的典型位置。到中心距离越小，样本越接近所属聚类的中心。轮廓系数接近 1 表示样本与本组匹配较好，接近 0 表示位于组间边界，小于 0 表示它可能更接近其他组。",
    "肘部图用于比较不同聚类数下的组内平方和；应关注下降速度开始明显放缓的位置，并结合轮廓系数、聚类规模和业务解释选择聚类数。PCA 得分图只用于二维展示，若前两个主成分解释的信息有限，图上重叠不等于高维空间中完全没有差异。",
    "四、分析边界",
    "K-means 使用欧氏距离并偏好近似球形、大小相近的聚类。结果依赖字段选择、标准化、异常值、聚类数量和随机初始中心。聚类编号本身没有大小顺序，也不表示真实类别或因果关系。对于非球形结构、类别字段、明显异常值或不同密度的数据，应考虑其他聚类方法并进行稳定性验证。"
  )
  list(model = model, centers = centers_table, summary = cluster_summary, quality = quality,
    assignments = assignments, silhouette = silhouette_data, elbow = elbow,
    score_data = score_data, score_centers = score_centers, pca = pca,
    report = paste(report, collapse = "\n\n"), used = nrow(selected), excluded = sum(!valid),
    variables = names(selected), removed = removed, standardize = isTRUE(standardize),
    average_silhouette = average_silhouette, analysis_summary = analysis_summary)
}

build_kmeans_cluster_plot <- function(result) {
  explained <- result$pca$sdev^2 / sum(result$pca$sdev^2)
  ggplot2::ggplot(result$score_data, ggplot2::aes(PC1, PC2, colour = 聚类)) +
    ggplot2::geom_point(alpha = 0.72, size = 2.5) +
    ggplot2::geom_point(data = result$score_centers, shape = 4, stroke = 1.6, size = 5) +
    lm_plot_theme() + ggplot2::labs(title = "K-means 聚类二维展示",
      subtitle = "使用 PCA 将聚类字段投影到二维；叉号为各组在图中的中心",
      x = paste0("PC1（", round(explained[1] * 100, 1), "%）"),
      y = paste0("PC2（", round(explained[2] * 100, 1), "%）"), colour = "聚类")
}

build_kmeans_elbow_plot <- function(result) {
  ggplot2::ggplot(result$elbow, ggplot2::aes(聚类数, 组内平方和)) +
    ggplot2::geom_line(colour = "#2563eb", linewidth = 1) +
    ggplot2::geom_point(colour = "#2563eb", size = 2.6) +
    ggplot2::geom_vline(xintercept = nrow(result$centers), linetype = "dashed", colour = "#d97706", linewidth = 0.9) +
    ggplot2::scale_x_continuous(breaks = result$elbow$聚类数) + lm_plot_theme() +
    ggplot2::labs(title = "K-means 肘部图", subtitle = "橙色虚线为当前选择的聚类数",
      x = "聚类数量 K", y = "总组内平方和")
}

build_kmeans_silhouette_plot <- function(result) {
  ggplot2::ggplot(result$silhouette, ggplot2::aes(聚类, 轮廓系数, fill = 聚类)) +
    ggplot2::geom_boxplot(alpha = 0.75, outlier.alpha = 0.45) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "#64748b") + lm_plot_theme() +
    ggplot2::labs(title = "各聚类轮廓系数分布",
      subtitle = paste0("整体平均轮廓系数：", kmeans_number(result$average_silhouette)),
      x = NULL, y = "轮廓系数", fill = "聚类")
}

kmeans_ui <- function(id) {
  ns <- NS(id)
  tagList(
    algorithm_title_ui(ns, "K-means 聚类"),
    p("选择两个或更多数值字段，根据样本之间的距离自动发现数据分组。使用左侧清洗后的数据。"),
    conditionalPanel(sprintf("input['%s'] %% 2 === 1", ns("tutorial_toggle")),
      algorithm_tutorial_ui(ns("tutorial"), "kmeans")),
    selectizeInput(ns("variables"), "聚类字段（数值，可多选）", NULL, multiple = TRUE),
    fluidRow(column(4, numericInput(ns("centers"), "聚类数量 K", 3, min = 2, max = 20, step = 1)),
      column(4, checkboxInput(ns("standardize"), "聚类前标准化字段（推荐）", TRUE)),
      column(4, numericInput(ns("seed"), "随机种子", 2026, min = 1, step = 1))),
    fluidRow(column(6, numericInput(ns("nstart"), "随机初始值次数", 25, min = 1, max = 100, step = 1)),
      column(6, numericInput(ns("iter_max"), "最大迭代次数", 100, min = 10, max = 1000, step = 10))),
    ai_parameter_ui(ns("ai_params")),
    helpText("字段单位或波动范围不同时建议保持标准化。增加随机初始值次数通常能降低落入较差局部解的风险，但会增加计算时间。"),
    actionButton(ns("run"), "运行 K-means 聚类", class = "btn-primary"),
    tags$div(style = "margin:12px 0", textOutput(ns("status"))),
    tabsetPanel(
      tabPanel("专业解读", tags$div(style = "white-space:pre-wrap;line-height:1.9", textOutput(ns("report")))),
      tabPanel("AI 增强解读", ai_report_ui(ns("ai_report"))),
      tabPanel("聚类质量", DT::DTOutput(ns("quality")), DT::DTOutput(ns("summary"))),
      tabPanel("二维聚类图", ggplot_editor_ui(ns("cluster_editor"), height = "520px")),
      tabPanel("肘部图", ggplot_editor_ui(ns("elbow_editor"), height = "480px")),
      tabPanel("轮廓系数", ggplot_editor_ui(ns("silhouette_editor"), height = "480px")),
      tabPanel("聚类中心", DT::DTOutput(ns("centers_table"))),
      tabPanel("聚类结果数据", DT::DTOutput(ns("assignments")))
    ), hr(),
    downloadButton(ns("download_report"), "下载 K-means 报告 TXT"),
    downloadButton(ns("download_assignments"), "下载聚类结果 CSV"),
    downloadButton(ns("download_centers"), "下载聚类中心 CSV"),
    actionButton(ns("save_report"), "保存 K-means 报告到项目文件夹"),
    tags$div(style = "overflow-wrap:anywhere", textOutput(ns("saved")))
  )
}

kmeans_server <- function(id, data, directory = reactive(getwd()), ai_config = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    algorithm_tutorial_server("tutorial", "kmeans")
    algorithm_tutorial_toggle_server(input, session)
    result <- reactiveVal(NULL)
    status <- reactiveVal("选择数值字段后，点击“运行 K-means 聚类”。")
    saved <- reactiveVal("")
    ai_params <- ai_parameter_server("ai_params", ai_config, data, "K-means 聚类",
      reactive(list(centers = input$centers, standardize = input$standardize, nstart = input$nstart,
        iter_max = input$iter_max, seed = input$seed)),
      list(centers = "2 到 20，且小于有效样本数", standardize = "true 或 false",
        nstart = "1 到 100", iter_max = "10 到 1000", seed = "正整数"))
    observeEvent(ai_params$apply(), {
      p <- ai_params$proposal()$parameters; if (is.null(p)) return()
      if (!is.null(p$centers)) updateNumericInput(session, "centers", value = max(2L, min(20L, as.integer(p$centers))))
      if (!is.null(p$standardize)) updateCheckboxInput(session, "standardize", value = isTRUE(as.logical(p$standardize)))
      if (!is.null(p$nstart)) updateNumericInput(session, "nstart", value = max(1L, min(100L, as.integer(p$nstart))))
      if (!is.null(p$iter_max)) updateNumericInput(session, "iter_max", value = max(10L, min(1000L, as.integer(p$iter_max))))
      if (!is.null(p$seed) && is.finite(as.numeric(p$seed))) updateNumericInput(session, "seed", value = max(1L, as.integer(p$seed)))
      showNotification("AI 建议参数已填入；请检查后点击运行。", type = "message")
    }, ignoreInit = TRUE)
    observeEvent(data(), {
      d <- data(); numeric <- which(vapply(d, is.numeric, logical(1)))
      labels <- if (length(numeric)) paste0(numeric, ". ", names(d)[numeric]) else character()
      choices <- setNames(as.character(numeric), labels)
      selected <- intersect(input$variables, unname(choices))
      if (length(selected) < 2L) selected <- unname(head(choices, min(4L, length(choices))))
      updateSelectizeInput(session, "variables", choices = choices, selected = selected)
    })
    observe({
      result(NULL); status("数据、字段或参数已更新，请点击“运行 K-means 聚类”。")
      data(); input$variables; input$centers; input$standardize; input$seed; input$nstart; input$iter_max
    }, priority = 100)
    observeEvent(input$run, {
      result(NULL)
      tryCatch({
        fitted <- fit_kmeans_analysis(data(), input$variables, input$centers, isTRUE(input$standardize),
          input$nstart, input$iter_max, input$seed)
        result(fitted)
        status(sprintf("聚类完成：使用 %d 行、%d 个字段，排除 %d 行；平均轮廓系数 %s。",
          fitted$used, length(fitted$variables), fitted$excluded, kmeans_number(fitted$average_silhouette)))
      }, error = function(e) {
        message <- if (inherits(e, "shiny.silent.error")) "请先导入数据并选择数值字段。" else conditionMessage(e)
        status(paste("未能聚类：", message)); showNotification(message, type = "error", duration = 10)
      })
    })
    output$status <- renderText(status())
    output$report <- renderText({ req(result()); result()$report })
    ai_report_server("ai_report", ai_config, "K-means 聚类",
      reactive(if (is.null(result())) "" else result()$report),
      reactive(if (is.null(result())) NULL else ai_model_context("K-means 聚类", result())), data)
    output$quality <- DT::renderDT({ req(result()); DT::datatable(result()$quality, rownames = FALSE, options = list(dom = "t")) })
    output$summary <- DT::renderDT({ req(result()); DT::datatable(result()$summary, rownames = FALSE, options = list(dom = "t", scrollX = TRUE)) })
    output$centers_table <- DT::renderDT({ req(result()); DT::datatable(result()$centers, rownames = FALSE, options = list(scrollX = TRUE)) })
    output$assignments <- DT::renderDT({ req(result()); DT::datatable(result()$assignments, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) })
    cluster_plot <- reactive({ req(result()); build_kmeans_cluster_plot(result()) })
    elbow_plot <- reactive({ req(result()); build_kmeans_elbow_plot(result()) })
    silhouette_plot <- reactive({ req(result()); build_kmeans_silhouette_plot(result()) })
    ggplot_editor_server("cluster_editor", cluster_plot, directory, "easyr-kmeans-clusters")
    ggplot_editor_server("elbow_editor", elbow_plot, directory, "easyr-kmeans-elbow")
    ggplot_editor_server("silhouette_editor", silhouette_plot, directory, "easyr-kmeans-silhouette")
    write_report <- function(file) { req(result()); writeLines(enc2utf8(result()$report), file, useBytes = TRUE) }
    write_csv_bom <- function(value, file) {
      con <- file(file, open = "wb"); on.exit(close(con)); writeBin(charToRaw("\ufeff"), con)
      lines <- capture.output(write.csv(value, row.names = FALSE, na = ""))
      writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n"))), con)
    }
    output$download_report <- downloadHandler(filename = function() paste0("easyr-kmeans-", Sys.Date(), ".txt"), content = write_report)
    output$download_assignments <- downloadHandler(filename = function() paste0("easyr-kmeans-results-", Sys.Date(), ".csv"), content = function(file) { req(result()); write_csv_bom(result()$assignments, file) })
    output$download_centers <- downloadHandler(filename = function() paste0("easyr-kmeans-centers-", Sys.Date(), ".csv"), content = function(file) { req(result()); write_csv_bom(result()$centers, file) })
    output$saved <- renderText(saved())
    observeEvent(input$save_report, {
      req(result())
      tryCatch({
        path <- save_to_workdir(directory(), "easyr-kmeans", ".txt", write_report)
        saved(paste("上次保存：", path)); showNotification("K-means 报告已保存到 RBench 项目文件夹。", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })
    result
  })
}
