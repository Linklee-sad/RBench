if (!exists("ai_report_ui", mode = "function")) source("R/ai.R")
if (!exists("algorithm_title_ui", mode = "function")) source("R/algorithm_tutorials.R")

lm_number <- function(x) {
  if (!is.finite(x)) return("无法计算")
  format(signif(x, 4), trim = TRUE, scientific = abs(x) > 0 && abs(x) < 0.001)
}

lm_pvalue <- function(p) {
  if (!is.finite(p)) "无法计算" else if (p < 0.001) "< 0.001" else paste0("= ", formatC(p, digits = 3, format = "f"))
}

fit_readable_lm <- function(data, outcome, predictors) {
  outcome <- as.integer(outcome)
  predictors <- unique(as.integer(predictors))
  if (length(outcome) != 1 || is.na(outcome) || !outcome %in% seq_along(data)) stop("请选择一个数值因变量。", call. = FALSE)
  if (!length(predictors) || anyNA(predictors) || !all(predictors %in% seq_along(data))) stop("请至少选择一个自变量。", call. = FALSE)
  if (outcome %in% predictors) stop("因变量不能同时作为自变量。", call. = FALSE)
  selected <- data[, c(outcome, predictors), drop = FALSE]
  if (!is.numeric(selected[[1]]) || !is.null(dim(selected[[1]]))) stop("因变量必须为数值字段。", call. = FALSE)
  valid <- rep(TRUE, nrow(selected))
  for (x in selected) {
    if (!is.atomic(x) || !is.null(dim(x))) stop("所选字段含有不支持的数据类型。", call. = FALSE)
    valid <- valid & !is.na(x)
    if (is.numeric(x)) valid <- valid & is.finite(x)
  }
  d <- selected[valid, , drop = FALSE]
  if (nrow(d) < 3) stop("有效数据不足 3 行，请检查所选字段的缺失值。", call. = FALSE)
  if (length(unique(d[[1]])) < 2) stop("因变量在有效数据中没有变化，无法进行有意义的回归。", call. = FALSE)
  analysis_summary <- ai_analysis_summary(d, names(data)[c(outcome, predictors)])
  # Only generated names enter the formula; uploaded column names are labels.
  names(d) <- c("outcome", paste0("predictor", seq_along(predictors)))
  labels <- c("截距")
  kind <- c("intercept")
  predictor_index <- c(NA_integer_)
  variables <- c("")
  levels_label <- c("")
  references <- c("")
  reference_notes <- character()
  for (j in seq_along(predictors)) {
    x <- d[[j + 1]]
    name <- names(data)[predictors[j]]
    if (is.numeric(x)) {
      if (length(unique(x)) < 2) stop(paste0("自变量“", name, "”在有效数据中没有变化，请移除它。"), call. = FALSE)
      labels <- c(labels, name); kind <- c(kind, "numeric")
      predictor_index <- c(predictor_index, j)
      variables <- c(variables, name); levels_label <- c(levels_label, ""); references <- c(references, "")
    } else {
      x <- factor(as.character(x))
      if (nlevels(x) < 2) stop(paste0("类别变量“", name, "”在有效数据中只有一个类别，请移除它。"), call. = FALSE)
      if (nlevels(x) > 30) stop(paste0("类别变量“", name, "”超过 30 类，可能是编号字段；请先检查或移除。"), call. = FALSE)
      contrasts(x) <- contr.treatment(levels(x), base = 1)
      d[[j + 1]] <- x
      ref <- levels(x)[1]
      others <- levels(x)[-1]
      labels <- c(labels, paste0(name, "：", others, " vs ", ref))
      kind <- c(kind, rep("category", length(others)))
      predictor_index <- c(predictor_index, rep(j, length(others)))
      variables <- c(variables, rep(name, length(others)))
      levels_label <- c(levels_label, others)
      references <- c(references, rep(ref, length(others)))
      reference_notes <- c(reference_notes, paste0("“", name, "”以“", ref, "”为参照类别。"))
    }
  }
  if (nrow(d) <= length(labels)) stop("有效样本数不足以估计这些系数及其不确定性，请减少自变量或增加数据。", call. = FALSE)
  formula <- reformulate(names(d)[-1], response = "outcome")
  model <- lm(formula, data = d, na.action = na.fail)
  if (model$rank < length(coef(model))) stop("自变量之间存在完全共线关系，部分系数无法单独估计。请移除重复或可由其他字段完全推算的变量。", call. = FALSE)
  summary <- suppressWarnings(summary(model))
  intervals <- suppressWarnings(confint(model))
  coefficients <- summary$coefficients
  stopifnot(nrow(coefficients) == length(labels))
  model_matrix <- stats::model.matrix(model)
  vif <- rep(NA_real_, ncol(model_matrix))
  if (ncol(model_matrix) == 2) vif[2] <- 1
  if (ncol(model_matrix) > 2) {
    for (column in 2:ncol(model_matrix)) {
      target <- model_matrix[, column]
      others <- model_matrix[, setdiff(2:ncol(model_matrix), column), drop = FALSE]
      auxiliary <- stats::lm.fit(cbind(1, others), target)
      total <- sum((target - mean(target))^2)
      r_squared <- if (total > 0) 1 - sum(auxiliary$residuals^2) / total else 1
      vif[column] <- if (r_squared >= 1) Inf else 1 / max(1 - r_squared, .Machine$double.eps)
    }
  }
  standardized <- rep(NA_real_, length(labels))
  outcome_sd <- stats::sd(d[[1]])
  numeric_terms <- which(kind == "numeric")
  for (i in numeric_terms) standardized[i] <- coefficients[i, 1] * stats::sd(d[[predictor_index[i] + 1]]) / outcome_sd
  table <- data.frame(字段 = labels, 系数 = coefficients[, 1], 标准化系数 = standardized, VIF = vif,
    标准误 = coefficients[, 2],
    t值 = coefficients[, 3], p值 = coefficients[, 4],
    下限95 = intervals[, 1], 上限95 = intervals[, 2], check.names = FALSE, row.names = NULL)
  f <- summary$fstatistic
  overall_p <- if (length(f) == 3) pf(f[1], f[2], f[3], lower.tail = FALSE) else NA_real_
  near_perfect <- sum(residuals(model)^2) <= .Machine$double.eps * sum((d[[1]] - mean(d[[1]]))^2)
  yname <- names(data)[outcome]
  overall_test <- if (near_perfect) {
    "模型接近完美拟合。此时残差方差趋近于零，标准误、检验统计量、p 值和置信区间可能出现数值不稳定；应优先检查变量间的确定性关系、数据泄漏或重复构造。"
  } else if (length(f) == 3 && all(is.finite(f)) && is.finite(overall_p)) {
    paste0("整体显著性检验：F(", lm_number(unname(f[2])), ", ", lm_number(unname(f[3])), ") = ",
      lm_number(unname(f[1])), "，p ", lm_pvalue(overall_p), "。该检验的原假设为除截距外所有回归系数同时等于 0。",
      if (overall_p < 0.05) "在 α = 0.05 水平下拒绝该原假设，模型整体具有统计显著性。" else
        "在 α = 0.05 水平下未拒绝该原假设，现有样本不足以支持模型整体具有统计显著性。")
  } else "整体 F 检验无法可靠计算。"
  finite_vif <- vif[is.finite(vif)]
  collinearity <- if (!length(finite_vif)) {
    "VIF 无法计算。"
  } else {
    maximum <- max(finite_vif)
    paste0("多重共线性诊断：最大 VIF = ", lm_number(maximum), "。",
      if (maximum >= 10) "存在较强共线性信号，系数与标准误可能对变量组合较敏感。" else if (maximum >= 5) {
        "存在值得关注的共线性，建议结合变量含义和相关结构进一步检查。"
      } else "未见 VIF ≥ 5 的明显共线性信号。",
      "VIF 阈值仅是经验性诊断，不应作为机械删变量的唯一依据。")
  }
  report <- c(
    "普通最小二乘线性回归：专业统计解读",
    "一、模型设定",
    paste0("因变量：", yname), paste0("自变量：", paste(names(data)[predictors], collapse = "、")),
    "估计方法：含截距的普通最小二乘法（OLS）；主效应相加，不含交互项、非线性项、权重或稳健标准误。",
    if (length(reference_notes)) paste0("类别变量编码：", paste(reference_notes, collapse = " ")) else "类别变量编码：本模型未包含类别自变量。",
    "二、分析样本",
    sprintf("原始数据 %d 行；完整案例 %d 行；因所选字段缺失或包含非有限数值而排除 %d 行。模型结果仅对应这些完整案例。", nrow(data), nrow(d), sum(!valid)),
    "三、模型拟合与整体检验",
    paste0("拟合优度：R² = ", lm_number(summary$r.squared), "；调整后 R² = ", lm_number(summary$adj.r.squared),
      "。R² 表示模型在当前拟合样本中解释的因变量变异比例，不是预测准确率。"),
    paste0("残差标准误 = ", lm_number(summary$sigma), "（残差自由度 = ", df.residual(model), "）；AIC = ",
      lm_number(AIC(model)), "；BIC = ", lm_number(BIC(model)), "。AIC/BIC 仅适合在相同因变量和可比分析样本的候选模型之间比较，数值越小通常表示拟合与复杂度的权衡更优。"),
    overall_test,
    collinearity,
    "四、回归系数（未标准化系数，采用字段原始单位）"
  )
  interpretations <- character(nrow(table))
  for (i in seq_len(nrow(table))) {
    estimate <- table$系数[i]
    statement <- if (kind[i] == "intercept") {
      paste0("截距：当所有数值自变量取 0、类别自变量处于参照水平时，模型对“", yname,
        "”条件均值的估计为 ", lm_number(estimate), "。若该自变量组合超出观测范围，截距主要承担模型定位作用，不宜作实质解释。")
    } else if (kind[i] == "numeric") {
      paste0("在其他模型变量保持不变的条件下，“", variables[i], "”每增加 1 个原始单位，“", yname,
        "”的条件均值估计", if (estimate >= 0) "增加 " else "减少 ", lm_number(abs(estimate)), " 个单位。")
    } else {
      paste0("在其他模型变量保持不变的条件下，“", variables[i], "”处于“", levels_label[i], "”时，“", yname,
        "”的条件均值估计比参照水平“", references[i], "”", if (estimate >= 0) "高 " else "低 ", lm_number(abs(estimate)), " 个单位。")
    }
    standardized_note <- if (kind[i] == "numeric") paste0("；标准化 β = ", lm_number(table$标准化系数[i])) else ""
    vif_note <- if (kind[i] != "intercept") paste0("；VIF = ", lm_number(table$VIF[i])) else ""
    statistics <- paste0("β = ", lm_number(estimate), standardized_note, vif_note, "，SE = ", lm_number(table$标准误[i]), "，t = ",
      lm_number(table$t值[i]), "，p ", lm_pvalue(table$p值[i]), "，95% CI [", lm_number(table$下限95[i]), ", ", lm_number(table$上限95[i]), "]。")
    inference <- if (near_perfect) "由于模型接近完美拟合，本项推断指标可能不稳定。" else if (!is.finite(table$p值[i])) {
      "本项推断统计量无法可靠计算。"
    } else if (table$p值[i] < 0.05) {
      "在模型假设成立且以 α = 0.05 为判定标准时，该系数与 0 的差异具有统计显著性；统计显著性不等同于实际重要性。"
    } else "在 α = 0.05 水平下未拒绝该系数等于 0 的原假设；这不构成“没有关联”的证据。"
    interpretations[i] <- paste0(table$字段[i], "。", statement, " ", statistics, " ", inference)
  }
  report <- c(report, interpretations,
    "五、推断前提与解释边界",
    "常规 OLS 推断依赖模型形式正确、关系近似线性且可加、观测误差相互独立、误差方差近似恒定，以及用于小样本检验和区间估计的误差正态性。残差图与 Q-Q 图只能用于诊断这些假设，不能证明假设成立。",
    "系数表示控制模型中其他变量后的条件关联。未观测混杂、反向因果、选择偏差和测量误差仍可能影响估计，因此不能仅凭本回归结果作因果解释。",
    "重复测量、聚类数据或时间序列可能违反独立性；异方差会影响常规标准误；高杠杆点和异常值可能显著影响结果。必要时应考虑聚类或稳健标准误、时间序列模型、变量变换、非线性项及影响点分析。",
    "结果基于左侧清洗后的完整案例。若此前使用缺失值填充，当前标准误未计入填补不确定性。本报告未进行样本外验证、多重检验校正或模型选择偏差调整。")
  list(model = model, summary = summary, coefficients = table,
    report = paste(report, collapse = "\n\n"), interpretation = interpretations,
    used = nrow(d), excluded = sum(!valid), outcome = yname,
    predictors = names(data)[predictors], near_perfect = near_perfect,
    analysis_summary = analysis_summary)
}

lm_plot_theme <- function() {
  family <- switch(Sys.info()[["sysname"]],
    Darwin = "Arial Unicode MS",
    Windows = "Microsoft YaHei",
    "sans")
  ggplot2::theme_minimal(base_size = 13, base_family = family) +
    ggplot2::theme(plot.title.position = "plot", legend.position = "bottom")
}

build_lm_fit_plot <- function(result) {
  model_data <- stats::model.frame(result$model)
  observed <- stats::model.response(model_data)
  fitted_values <- stats::fitted(result$model)
  if (length(result$predictors) == 1 && is.numeric(model_data[[2]])) {
    d <- data.frame(.x = model_data[[2]], .observed = observed)
    return(ggplot2::ggplot(d, ggplot2::aes(.x, .observed)) +
      ggplot2::geom_point(colour = "#2563eb", alpha = 0.65, size = 2.4) +
      ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
        colour = "#d97706", fill = "#fbbf24", alpha = 0.25, linewidth = 1) +
      lm_plot_theme() +
      ggplot2::labs(title = paste0(result$outcome, " 与 ", result$predictors[1], " 的线性拟合"),
        subtitle = "橙线为 lm 拟合值；阴影为平均响应的 95% 置信带",
        x = result$predictors[1], y = result$outcome))
  }
  d <- data.frame(.observed = observed, .fitted = fitted_values)
  ggplot2::ggplot(d, ggplot2::aes(.observed, .fitted)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "#d97706", linewidth = 0.9) +
    ggplot2::geom_point(colour = "#2563eb", alpha = 0.65, size = 2.4) +
    ggplot2::coord_equal() + lm_plot_theme() +
    ggplot2::labs(title = "实际值与模型拟合值", subtitle = "点越接近橙色 45° 虚线，样本内拟合越接近实际值",
      x = paste0(result$outcome, "（实际值）"), y = paste0(result$outcome, "（拟合值）"))
}

build_lm_residual_plot <- function(result) {
  d <- data.frame(.fitted = stats::fitted(result$model), .residual = stats::residuals(result$model))
  p <- ggplot2::ggplot(d, ggplot2::aes(.fitted, .residual)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "#64748b") +
    ggplot2::geom_point(colour = "#2563eb", alpha = 0.65, size = 2.3)
  if (nrow(d) >= 10 && length(unique(d$.fitted)) >= 5 && any(abs(d$.residual) > 0)) {
    p <- p + ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
      colour = "#d97706", linewidth = 1)
  }
  p + lm_plot_theme() +
    ggplot2::labs(title = "残差与拟合值", subtitle = "橙色趋势线应大致平坦并围绕 0",
      x = "模型拟合值", y = "残差")
}

build_lm_qq_plot <- function(result) {
  d <- data.frame(.residual = stats::residuals(result$model))
  ggplot2::ggplot(d, ggplot2::aes(sample = .residual)) +
    ggplot2::stat_qq(colour = "#2563eb", alpha = 0.65, size = 2.3) +
    ggplot2::stat_qq_line(colour = "#d97706", linewidth = 0.9) +
    lm_plot_theme() +
    ggplot2::labs(title = "残差正态 Q-Q 图", subtitle = "明显偏离橙色直线可能提示残差分布偏离正态",
      x = "理论分位数", y = "样本分位数")
}

regression_ui <- function(id) {
  ns <- NS(id)
  tagList(
    algorithm_title_ui(ns, "线性回归"),
    p("选择要解释的数值字段作为因变量，再选择一个或多个自变量。使用左侧清洗后的数据。"),
    conditionalPanel(sprintf("input['%s'] %% 2 === 1", ns("tutorial_toggle")),
      algorithm_tutorial_ui(ns("tutorial"), "regression")),
    fluidRow(column(5, selectInput(ns("outcome"), "因变量 Y（数值）", NULL)),
      column(7, selectizeInput(ns("predictors"), "自变量 X（可多选）", NULL, multiple = TRUE))),
    helpText("在自变量框中依次点击多个字段即可建立多元回归；点击已选字段旁的 × 可移除。模型包含截距。文字、逻辑值和因子作为类别变量，按类别排序后的第一类作为参照；类别编号若是数值，会按连续变量处理。"),
    actionButton(ns("run"), "运行线性回归", class = "btn-primary"),
    tags$div(style = "margin:12px 0", textOutput(ns("status"))),
    tabsetPanel(
      tabPanel("专业解读", tags$div(style = "white-space:pre-wrap;line-height:1.9", textOutput(ns("report")))),
      tabPanel("AI 增强解读", ai_report_ui(ns("ai_report"))),
      tabPanel("系数表", DT::DTOutput(ns("coefficients")),
        helpText("标准化系数仅对数值自变量计算，便于比较量纲不同的数值变量；VIF 用于诊断模型项的多重共线性。95% 区间使用 t 分布，p 值来自系数为 0 的双侧检验。")),
      tabPanel("拟合图（ggplot2）", ggplot_editor_ui(ns("fit_editor"), height = "480px"),
        p("只有一个数值自变量时显示 lm 拟合线和 95% 均值置信带；多自变量或包含类别变量时显示实际值与完整模型拟合值。图中展示的是样本内拟合，不代表样本外预测准确率。")),
      tabPanel("诊断图（ggplot2）",
        tabsetPanel(type = "pills",
          tabPanel("残差与拟合值", ggplot_editor_ui(ns("residual_editor"), height = "480px")),
          tabPanel("正态 Q-Q", ggplot_editor_ui(ns("qq_editor"), height = "480px"))),
        p("残差图若出现弯曲或漏斗形，可能提示非线性或异方差；Q-Q 图明显偏离直线，可能提示误差分布偏离正态。"))
    ), hr(),
    downloadButton(ns("download"), "下载专业解读报告 TXT"),
    actionButton(ns("save"), "保存专业报告到项目文件夹"),
    tags$div(style = "overflow-wrap:anywhere", textOutput(ns("saved")))
  )
}

regression_server <- function(id, data, directory = reactive(getwd()), ai_config = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    algorithm_tutorial_server("tutorial", "regression")
    algorithm_tutorial_toggle_server(input, session)
    result <- reactiveVal(NULL)
    status <- reactiveVal("选择变量后，点击“运行线性回归”。")
    saved <- reactiveVal("")
    observeEvent(data(), {
      d <- data()
      numeric <- which(vapply(d, is.numeric, logical(1)))
      labels <- if (length(numeric)) paste0(numeric, ". ", names(d)[numeric]) else character()
      choices <- setNames(as.character(numeric), labels)
      selected <- if (length(input$outcome) == 1 && input$outcome %in% choices) input$outcome else unname(head(choices, 1))
      updateSelectInput(session, "outcome", choices = choices, selected = selected)
    })
    observeEvent(list(data(), input$outcome), {
      d <- data()
      eligible <- setdiff(seq_along(d), as.integer(input$outcome))
      labels <- if (length(eligible)) paste0(eligible, ". ", names(d)[eligible]) else character()
      choices <- setNames(as.character(eligible), labels)
      updateSelectizeInput(session, "predictors", choices = choices, selected = intersect(input$predictors, unname(choices)))
    })
    # Any changed data or selection clears the old result before a new fit.
    observe({
      result(NULL)
      status("数据或变量选择已更新，请点击“运行线性回归”。")
      data(); input$outcome; input$predictors
    }, priority = 100)
    observeEvent(input$run, {
      result(NULL)
      tryCatch({
        fitted <- fit_readable_lm(data(), input$outcome, input$predictors)
        result(fitted)
        status(sprintf("拟合完成：使用 %d 行，排除 %d 行。", fitted$used, fitted$excluded))
      }, error = function(e) {
        message <- if (inherits(e, "shiny.silent.error")) "请先导入数据并保留至少一个字段。" else conditionMessage(e)
        status(paste("未能拟合：", message))
        showNotification(message, type = "error", duration = 10)
      })
    })
    output$status <- renderText(status())
    output$report <- renderText({ req(result()); result()$report })
    ai_report_server("ai_report", ai_config, "线性回归",
      reactive(if (is.null(result())) "" else result()$report),
      reactive(if (is.null(result())) NULL else ai_model_context("线性回归", result())), data)
    output$coefficients <- DT::renderDT({
      req(result())
      DT::formatSignif(DT::datatable(result()$coefficients, rownames = FALSE, escape = TRUE,
        options = list(pageLength = 10, scrollX = TRUE)), columns = 2:ncol(result()$coefficients), digits = 4)
    })
    fit_plot <- reactive({ req(result()); build_lm_fit_plot(result()) })
    residual_plot <- reactive({ req(result()); build_lm_residual_plot(result()) })
    qq_plot <- reactive({ req(result()); build_lm_qq_plot(result()) })
    ggplot_editor_server("fit_editor", fit_plot, directory, "easyr-regression-fit")
    ggplot_editor_server("residual_editor", residual_plot, directory, "easyr-regression-residual")
    ggplot_editor_server("qq_editor", qq_plot, directory, "easyr-regression-qq")
    write_report <- function(file) {
      req(result())
      writeLines(enc2utf8(result()$report), file, useBytes = TRUE)
    }
    output$download <- downloadHandler(filename = function() paste0("easyr-regression-", Sys.Date(), ".txt"), content = write_report)
    output$saved <- renderText(saved())
    observeEvent(input$save, {
      req(result())
      tryCatch({
        path <- save_to_workdir(directory(), "easyr-regression", ".txt", write_report)
        saved(paste("上次保存：", path))
        showNotification("回归专业解读报告已保存。", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })
    reactive(result())
  })
}
