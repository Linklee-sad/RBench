teaching_data_examples <- function() {
  data.frame(
    example = c("班级中每位学生的身高", "顾客是否再次购买", "一天内收到的订单数", "商品所属类别"),
    answer = c("连续数值变量", "二元类别变量", "离散数值变量", "多类别变量"),
    reason = c("身高可以在一个区间内取连续数值。", "结果只有是或否两个类别。",
      "订单数是从 0 开始的整数计数。", "类别名称用于分组，本身没有数值大小。"),
    stringsAsFactors = FALSE
  )
}

teaching_data_quiz_ui <- function(id) {
  ns <- NS(id); examples <- teaching_data_examples()
  tagList(
    selectInput(ns("example"), "选择一个例子", setNames(seq_len(nrow(examples)), examples$example)),
    radioButtons(ns("answer"), "它属于哪种变量？",
      c("连续数值变量", "离散数值变量", "二元类别变量", "多类别变量"), inline = TRUE),
    actionButton(ns("check"), "检查答案", icon = icon("check"), class = "btn-primary"),
    uiOutput(ns("feedback"))
  )
}

teaching_data_quiz_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    checked <- reactiveVal(FALSE)
    observeEvent(list(input$example, input$answer), checked(FALSE), ignoreInit = TRUE)
    observeEvent(input$check, checked(TRUE))
    output$feedback <- renderUI({
      req(checked(), input$example, input$answer)
      row <- teaching_data_examples()[as.integer(input$example), , drop = FALSE]
      correct <- identical(input$answer, row$answer)
      tags$div(class = paste("teaching-feedback", if (correct) "correct" else "incorrect"),
        tags$strong(if (correct) "回答正确。" else paste0("再想一想。正确答案是：", row$answer, "。")),
        tags$div(row$reason))
    })
  })
}

simulate_probability_experiment <- function(experiment = "coin", trials = 500L, probability = .5,
                                            sides = 6L, target = 6L, seed = 2026L) {
  trials <- as.integer(trials); sides <- as.integer(sides); target <- as.integer(target); seed <- as.integer(seed)
  if (!is.finite(trials) || trials < 1L || trials > 100000L) stop("实验次数应在 1 到 100,000 之间。", call. = FALSE)
  if (!is.finite(seed)) stop("随机种子必须是整数。", call. = FALSE)
  set.seed(seed)
  if (identical(experiment, "coin")) {
    if (!is.finite(probability) || probability < 0 || probability > 1) stop("正面概率必须在 0 到 1 之间。", call. = FALSE)
    event <- stats::rbinom(trials, 1L, probability) == 1L
    outcome <- ifelse(event, "正面", "反面")
    theoretical <- probability; event_label <- "出现正面"
  } else if (identical(experiment, "die")) {
    if (!is.finite(sides) || sides < 2L || sides > 100L) stop("骰子面数应在 2 到 100 之间。", call. = FALSE)
    if (!is.finite(target) || target < 1L || target > sides) stop("目标点数必须在骰子面数范围内。", call. = FALSE)
    outcome <- sample.int(sides, trials, replace = TRUE)
    event <- outcome == target
    theoretical <- 1 / sides; event_label <- paste0("掷出 ", target)
  } else stop("不支持的概率实验。", call. = FALSE)
  trace <- data.frame(实验次数 = seq_len(trials), 累计频率 = cumsum(event) / seq_len(trials), check.names = FALSE)
  counts <- as.data.frame(table(outcome), stringsAsFactors = FALSE)
  names(counts) <- c("结果", "出现次数"); counts$比例 <- counts$出现次数 / trials
  list(experiment = experiment, trials = trials, theoretical = theoretical, event_label = event_label,
    observed = mean(event), trace = trace, counts = counts)
}

build_probability_convergence_plot <- function(result) {
  ggplot2::ggplot(result$trace, ggplot2::aes(实验次数, 累计频率)) +
    ggplot2::geom_line(colour = "#2563eb", linewidth = .85) +
    ggplot2::geom_hline(yintercept = result$theoretical, colour = "#d97706", linetype = "dashed", linewidth = 1) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) + lm_plot_theme() +
    ggplot2::labs(title = paste0(result$event_label, "：频率如何接近概率"),
      subtitle = "蓝线是累计频率；橙色虚线是理论概率", x = "实验次数", y = "累计频率")
}

probability_lab_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(class = "teaching-intro-card",
      h3("概率从重复实验开始"),
      p("一次结果无法预测，但大量重复后，事件发生的相对频率通常会在理论概率附近波动。改变实验次数，观察蓝线是否逐渐稳定。"),
      shiny::withMathJax(HTML("\\[\\widehat{P}(A)=\\frac{\\text{事件 A 出现的次数}}{\\text{实验总次数}}\\]"))),
    fluidRow(
      column(3, selectInput(ns("experiment"), "随机实验", c("抛硬币" = "coin", "掷骰子" = "die"))),
      column(3, numericInput(ns("trials"), "实验次数", 500, min = 1, max = 100000, step = 100)),
      column(3, numericInput(ns("seed"), "随机种子", 2026, min = 1, step = 1)),
      column(3, conditionalPanel(sprintf("input['%s'] === 'coin'", ns("experiment")),
        sliderInput(ns("probability"), "正面概率", 0, 1, .5, step = .05)))),
    conditionalPanel(sprintf("input['%s'] === 'die'", ns("experiment")),
      fluidRow(column(4, sliderInput(ns("sides"), "骰子面数", 2, 20, 6, step = 1)),
        column(4, numericInput(ns("target"), "关注的点数", 6, min = 1, max = 20, step = 1)))),
    actionButton(ns("run"), "运行概率实验", icon = icon("dice"), class = "btn-primary"),
    tags$div(class = "teaching-status", textOutput(ns("status"))),
    ggplot_editor_ui(ns("plot_editor"), height = "470px"),
    DT::DTOutput(ns("counts")),
    tags$div(class = "teaching-misconception",
      tags$strong("常见误区"), "理论概率为 0.5，并不表示每 10 次一定恰好出现 5 次；它描述的是长期比例，而不是短期保证。")
  )
}

probability_lab_server <- function(id, directory = reactive(getwd())) {
  moduleServer(id, function(input, output, session) {
    result <- reactiveVal(NULL); status <- reactiveVal("调整参数后运行实验。")
    observeEvent(input$run, {
      tryCatch({
        value <- simulate_probability_experiment(input$experiment, input$trials, input$probability,
          input$sides, input$target, input$seed)
        result(value)
        status(sprintf("完成 %d 次实验：%s的理论概率为 %.4f，观察频率为 %.4f。",
          value$trials, value$event_label, value$theoretical, value$observed))
      }, error = function(e) status(conditionMessage(e)))
    })
    output$status <- renderText(status())
    output$counts <- DT::renderDT({ req(result()); DT::datatable(result()$counts, rownames = FALSE, options = list(dom = "t")) })
    plot <- reactive({ req(result()); build_probability_convergence_plot(result()) })
    ggplot_editor_server("plot_editor", plot, directory, "easyr-probability-frequency")
    result
  })
}

calculate_counting_methods <- function(n = 10L, r = 3L) {
  n <- as.integer(n); r <- as.integer(r)
  if (!is.finite(n) || n < 1L || n > 50L) stop("对象总数 n 应在 1 到 50 之间。", call. = FALSE)
  if (!is.finite(r) || r < 0L || r > n) stop("选取数量 r 应在 0 到 n 之间。", call. = FALSE)
  counts <- c(
    factorial(n) / factorial(n - r),
    choose(n, r),
    n^r,
    choose(n + r - 1L, r)
  )
  methods <- data.frame(
    方法 = c("不放回排列", "不放回组合", "放回排列", "放回组合"),
    顺序重要 = c("是", "否", "是", "否"),
    可以放回 = c("否", "否", "是", "是"),
    方案数 = counts,
    stringsAsFactors = FALSE
  )
  methods$显示值 <- vapply(methods$方案数, function(x) {
    if (x < 1e10) format(x, scientific = FALSE, big.mark = ",", trim = TRUE)
    else format(x, scientific = TRUE, digits = 4, trim = TRUE)
  }, character(1))
  list(n = n, r = r, methods = methods)
}

build_counting_methods_plot <- function(result) {
  d <- result$methods
  d$方法 <- factor(d$方法, levels = rev(d$方法))
  d$数量级 <- log10(d$方案数 + 1)
  ggplot2::ggplot(d, ggplot2::aes(数量级, 方法, fill = 方法)) +
    ggplot2::geom_col(width = .66, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = 显示值), hjust = -.08, colour = "#172b4d", fontface = "bold") +
    ggplot2::scale_fill_manual(values = c("不放回排列" = "#2563eb", "不放回组合" = "#60a5fa",
      "放回排列" = "#f59e0b", "放回组合" = "#fbbf24")) +
    ggplot2::coord_cartesian(xlim = c(0, max(d$数量级, 1) * 1.22), clip = "off") +
    lm_plot_theme() + ggplot2::theme(axis.text.y = ggplot2::element_text(colour = "#172b4d")) +
    ggplot2::labs(title = sprintf("从 %d 个不同对象中选 %d 个", result$n, result$r),
      subtitle = "柱长按 log10(方案数 + 1) 显示，数字标签是实际方案数", x = "方案数量级", y = NULL)
}

counting_methods_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(class = "teaching-intro-card",
      h3("排列与组合：先问两个问题"),
      p("从 n 个不同对象中选 r 个时，先判断顺序是否重要，再判断选过的对象能否放回。这两个判断决定使用哪一个公式。"),
      shiny::withMathJax(tags$div(class = "statistics-formulas",
        tags$div(class = "statistics-formula-card", tags$strong("不放回，顺序重要：排列"),
          HTML("\\[A_n^r=\\frac{n!}{(n-r)!}\\]"), "例如从 10 人中选出冠军、亚军和季军。"),
        tags$div(class = "statistics-formula-card", tags$strong("不放回，顺序不重要：组合"),
          HTML("\\[C_n^r=\\frac{n!}{r!(n-r)!}\\]"), "例如从 10 人中选 3 人组成小组。"),
        tags$div(class = "statistics-formula-card", tags$strong("放回，顺序重要"),
          HTML("\\[n^r\\]"), "例如使用 n 个字符组成长度为 r 的密码。"),
        tags$div(class = "statistics-formula-card", tags$strong("放回，顺序不重要"),
          HTML("\\[C_{n+r-1}^{r}\\]"), "例如从 n 种口味中选择 r 个冰淇淋球，可重复选择。")))),
    fluidRow(
      column(4, sliderInput(ns("n"), "对象总数 n", 1, 50, 10, step = 1)),
      column(4, sliderInput(ns("r"), "选取数量 r", 0, 10, 3, step = 1))),
    actionButton(ns("calculate"), "比较四种计数方法", icon = icon("list-ol"), class = "btn-primary"),
    tags$div(class = "teaching-status", textOutput(ns("status"))),
    ggplot_editor_ui(ns("plot_editor"), height = "470px"),
    DT::DTOutput(ns("table")),
    tags$div(class = "teaching-misconception", tags$strong("最常见的失分点"),
      "组合不考虑先后顺序。同一组对象如果只是选择成员，交换排列顺序不会产生新的组合；如果是名次、座位或密码，顺序通常重要。")
  )
}

counting_methods_server <- function(id, directory = reactive(getwd())) {
  moduleServer(id, function(input, output, session) {
    result <- reactiveVal(NULL); status <- reactiveVal("设置 n 和 r 后比较四种方法。")
    observeEvent(input$n, {
      req(input$n)
      current_r <- if (is.null(input$r) || !length(input$r)) 0 else input$r
      updateSliderInput(session, "r", max = input$n, value = min(current_r, input$n))
    }, ignoreInit = TRUE)
    observeEvent(input$calculate, {
      tryCatch({
        value <- calculate_counting_methods(input$n, input$r)
        result(value)
        status(sprintf("从 %d 个不同对象中选 %d 个：先看顺序是否重要，再看是否允许放回。", value$n, value$r))
      }, error = function(e) status(conditionMessage(e)))
    })
    output$status <- renderText(status())
    output$table <- DT::renderDT({
      req(result()); d <- result()$methods[c("方法", "顺序重要", "可以放回", "显示值")]
      names(d)[4] <- "方案数"
      DT::datatable(d, rownames = FALSE, options = list(dom = "t"))
    })
    plot <- reactive({ req(result()); build_counting_methods_plot(result()) })
    ggplot_editor_server("plot_editor", plot, directory, "easyr-counting-methods")
    result
  })
}

calculate_probability_rules <- function(p_a = .4, p_b_given_a = .7, p_b_given_not_a = .3) {
  values <- c(p_a, p_b_given_a, p_b_given_not_a)
  if (any(!is.finite(values)) || any(values < 0) || any(values > 1)) {
    stop("所有概率都必须在 0 到 1 之间。", call. = FALSE)
  }
  p_intersection <- p_a * p_b_given_a
  p_a_not_b <- p_a * (1 - p_b_given_a)
  p_not_a_b <- (1 - p_a) * p_b_given_not_a
  p_neither <- (1 - p_a) * (1 - p_b_given_not_a)
  p_b <- p_intersection + p_not_a_b
  p_union <- p_a + p_b - p_intersection
  cells <- data.frame(
    事件A = factor(c("A 发生", "A 发生", "A 不发生", "A 不发生"), levels = c("A 发生", "A 不发生")),
    事件B = factor(c("B 发生", "B 不发生", "B 发生", "B 不发生"), levels = c("B 发生", "B 不发生")),
    概率 = c(p_intersection, p_a_not_b, p_not_a_b, p_neither),
    情形 = c("A 与 B 都发生", "A 发生、B 不发生", "A 不发生、B 发生", "A 与 B 都不发生"), stringsAsFactors = FALSE)
  list(p_a = p_a, p_b = p_b, p_intersection = p_intersection, p_union = p_union,
    p_complement_a = 1 - p_a, p_b_given_a = p_b_given_a,
    p_b_given_not_a = p_b_given_not_a, independent = abs(p_b_given_a - p_b) < 1e-10,
    cells = cells)
}

build_probability_rules_plot <- function(result) {
  d <- result$cells
  d$标签 <- paste0(d$情形, "\n", format(round(d$概率 * 100, 1), nsmall = 1), "%")
  ggplot2::ggplot(d, ggplot2::aes(事件B, 事件A, fill = 概率)) +
    ggplot2::geom_tile(colour = "white", linewidth = 2) +
    ggplot2::geom_text(ggplot2::aes(label = 标签), colour = "#172b4d", fontface = "bold", size = 4.5) +
    ggplot2::scale_fill_gradient(low = "#eff6ff", high = "#3b82f6", limits = c(0, 1)) +
    ggplot2::coord_equal() + lm_plot_theme() +
    ggplot2::theme(legend.position = "none", panel.grid = ggplot2::element_blank()) +
    ggplot2::labs(title = "两个事件会形成四种互斥情形",
      subtitle = "四个格子的概率相加等于 1", x = "事件 B", y = "事件 A")
}

probability_rules_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(class = "teaching-intro-card",
      h3("概率的四条基本规则"),
      p("先把两个事件拆成四种不会重叠的情形，再计算补集、交集和并集。拖动条件概率，还可以观察两个事件何时独立。"),
      shiny::withMathJax(tags$div(class = "teaching-formula-row",
        tags$div(tags$strong("补集与加法公式"),
          HTML("\\[P(A^c)=1-P(A)\\]"),
          HTML("\\[P(A\\cup B)=P(A)+P(B)-P(A\\cap B)\\]")),
        tags$div(tags$strong("乘法与全概率公式"),
          HTML("\\[P(A\\cap B)=P(A)P(B\\mid A)\\]"),
          HTML("\\[P(B)=P(B\\mid A)P(A)+P(B\\mid A^c)P(A^c)\\]"))))),
    fluidRow(
      column(4, sliderInput(ns("p_a"), "P(A)：事件 A 的概率", 0, 1, .4, step = .05)),
      column(4, sliderInput(ns("p_b_given_a"), "P(B|A)：A 发生时 B 的概率", 0, 1, .7, step = .05)),
      column(4, sliderInput(ns("p_b_given_not_a"), "P(B|Aᶜ)：A 不发生时 B 的概率", 0, 1, .3, step = .05))),
    actionButton(ns("calculate"), "计算概率规则", icon = icon("calculator"), class = "btn-primary"),
    tags$div(class = "teaching-status", textOutput(ns("status"))),
    ggplot_editor_ui(ns("plot_editor"), height = "470px"),
    DT::DTOutput(ns("table")),
    tags$div(class = "teaching-misconception", tags$strong("怎样判断独立"),
      "如果知道 A 是否发生不会改变 B 的概率，即 P(B|A)=P(B)，那么 A 与 B 独立。互斥通常不等于独立。")
  )
}

probability_rules_server <- function(id, directory = reactive(getwd())) {
  moduleServer(id, function(input, output, session) {
    result <- reactiveVal(NULL)
    status <- reactiveVal("调整三个概率后开始计算。")
    observeEvent(input$calculate, {
      tryCatch({
        value <- calculate_probability_rules(input$p_a, input$p_b_given_a, input$p_b_given_not_a)
        result(value)
        relation <- if (value$independent) "A 与 B 独立" else "A 与 B 不独立"
        status(sprintf("P(Aᶜ)=%.2f，P(A∩B)=%.2f，P(B)=%.2f，P(A∪B)=%.2f；%s。",
          value$p_complement_a, value$p_intersection, value$p_b, value$p_union, relation))
      }, error = function(e) status(conditionMessage(e)))
    })
    output$status <- renderText(status())
    output$table <- DT::renderDT({
      req(result()); d <- result()$cells
      d$概率 <- paste0(format(round(d$概率 * 100, 2), nsmall = 2), "%")
      DT::datatable(d, rownames = FALSE, options = list(dom = "t"))
    })
    plot <- reactive({ req(result()); build_probability_rules_plot(result()) })
    ggplot_editor_server("plot_editor", plot, directory, "easyr-probability-rules")
    result
  })
}

calculate_bayes_screening <- function(population = 10000L, prevalence = .01,
                                      sensitivity = .9, specificity = .95) {
  population <- as.integer(population)
  values <- c(prevalence, sensitivity, specificity)
  if (!is.finite(population) || population < 100L || population > 1000000L) stop("模拟人数应在 100 到 1,000,000 之间。", call. = FALSE)
  if (any(!is.finite(values)) || any(values < 0) || any(values > 1)) stop("概率参数必须在 0 到 1 之间。", call. = FALSE)
  diseased <- round(population * prevalence); healthy <- population - diseased
  true_positive <- round(diseased * sensitivity); false_negative <- diseased - true_positive
  true_negative <- round(healthy * specificity); false_positive <- healthy - true_negative
  positive_total <- true_positive + false_positive
  posterior <- if (positive_total > 0L) true_positive / positive_total else NA_real_
  negative_total <- true_negative + false_negative
  negative_posterior <- if (negative_total > 0L) false_negative / negative_total else NA_real_
  cells <- data.frame(
    真实情况 = factor(c("患病", "患病", "未患病", "未患病"), levels = c("患病", "未患病")),
    检测结果 = factor(c("阳性", "阴性", "阳性", "阴性"), levels = c("阳性", "阴性")),
    人数 = c(true_positive, false_negative, false_positive, true_negative),
    类型 = c("真阳性", "假阴性", "假阳性", "真阴性"), stringsAsFactors = FALSE)
  cells$总体比例 <- cells$人数 / population
  list(population = population, prevalence = prevalence, sensitivity = sensitivity, specificity = specificity,
    cells = cells, posterior = posterior, negative_posterior = negative_posterior,
    positive_total = positive_total, true_positive = true_positive, false_positive = false_positive)
}

build_bayes_screening_plot <- function(result) {
  d <- result$cells
  d$标签 <- paste0(d$类型, "\n", format(d$人数, big.mark = ","), " 人\n",
    format(round(d$总体比例 * 100, 1), nsmall = 1), "%")
  ggplot2::ggplot(d, ggplot2::aes(检测结果, 真实情况, fill = 人数)) +
    ggplot2::geom_tile(colour = "white", linewidth = 2) +
    ggplot2::geom_text(ggplot2::aes(label = 标签), fontface = "bold", colour = "#172b4d", lineheight = 1.15) +
    ggplot2::scale_fill_gradient(low = "#dbeafe", high = "#2563eb") +
    ggplot2::coord_equal() + lm_plot_theme() +
    ggplot2::theme(legend.position = "none", panel.grid = ggplot2::element_blank()) +
    ggplot2::labs(title = "检测结果不等于真实情况",
      subtitle = "同时查看真阳性和假阳性，才能解释一次阳性结果", x = "检测结果", y = "真实情况")
}

bayes_lab_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(class = "teaching-intro-card",
      h3("条件概率与贝叶斯：先数人数"),
      p("条件概率就是把范围缩小：已经知道检测为阳性，只在所有阳性者中计算真正患病者所占的比例。下面的两个式子表达的是同一件事。"),
      shiny::withMathJax(tags$div(class = "teaching-formula-row",
        tags$div(tags$strong("人数版（建议先记这个）"),
          HTML("\\[P(\\text{患病}\\mid\\text{阳性})=\\frac{\\text{真阳性人数}}{\\text{真阳性人数}+\\text{假阳性人数}}\\]")),
        tags$div(tags$strong("概率版"),
          HTML("\\[P(\\text{患病}\\mid\\text{阳性})=\\frac{\\text{患病率}\\times\\text{灵敏度}}{\\text{患病率}\\times\\text{灵敏度}+(1-\\text{患病率})\\times(1-\\text{特异度})}\\]"))))),
    fluidRow(
      column(3, numericInput(ns("population"), "模拟人数", 10000, min = 100, max = 1000000, step = 100)),
      column(3, sliderInput(ns("prevalence"), "患病率", 0, .5, .01, step = .005)),
      column(3, sliderInput(ns("sensitivity"), "灵敏度", .5, 1, .9, step = .01)),
      column(3, sliderInput(ns("specificity"), "特异度", .5, 1, .95, step = .01))),
    actionButton(ns("run"), "计算贝叶斯结果", icon = icon("calculator"), class = "btn-primary"),
    tags$div(class = "teaching-status", textOutput(ns("status"))),
    ggplot_editor_ui(ns("plot_editor"), height = "470px"),
    DT::DTOutput(ns("table")),
    tags$div(class = "teaching-misconception", tags$strong("基准率误区"),
      "只看检测准确率会高估阳性结果的可信度。当目标事件本来很少见时，数量庞大的未患病人群也可能产生不少假阳性。")
  )
}

bayes_lab_server <- function(id, directory = reactive(getwd())) {
  moduleServer(id, function(input, output, session) {
    result <- reactiveVal(NULL); status <- reactiveVal("调整患病率和检测性能后计算。")
    observeEvent(input$run, {
      tryCatch({
        value <- calculate_bayes_screening(input$population, input$prevalence, input$sensitivity, input$specificity)
        result(value)
        posterior_text <- if (is.finite(value$posterior)) sprintf("%.1f%%", value$posterior * 100) else "无法计算（没有阳性结果）"
        status(sprintf("共有 %s 个阳性结果，其中 %s 个为真阳性。阳性后真正患病的概率为 %s。",
          format(value$positive_total, big.mark = ","), format(value$true_positive, big.mark = ","), posterior_text))
      }, error = function(e) status(conditionMessage(e)))
    })
    output$status <- renderText(status())
    output$table <- DT::renderDT({
      req(result()); d <- result()$cells
      d$总体比例 <- paste0(format(round(d$总体比例 * 100, 2), nsmall = 2), "%")
      DT::datatable(d, rownames = FALSE, options = list(dom = "t"))
    })
    plot <- reactive({ req(result()); build_bayes_screening_plot(result()) })
    ggplot_editor_server("plot_editor", plot, directory, "easyr-bayes-screening")
    result
  })
}

simulate_confidence_intervals <- function(population_mean = 0, population_sd = 1, sample_size = 30L,
                                          repetitions = 100L, confidence = .95, seed = 2026L) {
  sample_size <- as.integer(sample_size); repetitions <- as.integer(repetitions); seed <- as.integer(seed)
  if (!is.finite(population_mean)) stop("总体均值必须是有限数值。", call. = FALSE)
  if (!is.finite(population_sd) || population_sd <= 0) stop("总体标准差必须大于 0。", call. = FALSE)
  if (!is.finite(sample_size) || sample_size < 2L || sample_size > 10000L) stop("样本量应在 2 到 10,000 之间。", call. = FALSE)
  if (!is.finite(repetitions) || repetitions < 10L || repetitions > 500L) stop("重复次数应在 10 到 500 之间。", call. = FALSE)
  if (!is.finite(confidence) || confidence <= .5 || confidence >= 1) stop("置信水平必须在 0.5 到 1 之间。", call. = FALSE)
  set.seed(seed)
  samples <- matrix(stats::rnorm(sample_size * repetitions, population_mean, population_sd), nrow = sample_size)
  means <- colMeans(samples); standard_error <- population_sd / sqrt(sample_size)
  critical <- stats::qnorm(1 - (1 - confidence) / 2)
  lower <- means - critical * standard_error; upper <- means + critical * standard_error
  covered <- lower <= population_mean & upper >= population_mean
  intervals <- data.frame(重复编号 = seq_len(repetitions), 样本均值 = means,
    下限 = lower, 上限 = upper, 覆盖总体均值 = covered, check.names = FALSE)
  list(population_mean = population_mean, population_sd = population_sd, sample_size = sample_size,
    repetitions = repetitions, confidence = confidence, standard_error = standard_error,
    critical = critical, intervals = intervals, coverage = mean(covered))
}

build_confidence_interval_plot <- function(result) {
  d <- result$intervals
  d$覆盖情况 <- factor(ifelse(d$覆盖总体均值, "覆盖", "未覆盖"), levels = c("覆盖", "未覆盖"))
  ggplot2::ggplot(d, ggplot2::aes(y = 重复编号, colour = 覆盖情况)) +
    ggplot2::geom_segment(ggplot2::aes(x = 下限, xend = 上限, yend = 重复编号), linewidth = .7) +
    ggplot2::geom_point(ggplot2::aes(x = 样本均值), size = 1.5) +
    ggplot2::geom_vline(xintercept = result$population_mean, colour = "#172b4d", linetype = "dashed", linewidth = 1) +
    ggplot2::scale_colour_manual(values = c("覆盖" = "#2563eb", "未覆盖" = "#dc2626")) +
    lm_plot_theme() + ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(title = paste0(round(result$confidence * 100), "% 置信区间的重复抽样演示"),
      subtitle = "虚线是真实总体均值；红色区间没有覆盖它", x = "总体均值的区间估计", y = "重复抽样编号", colour = NULL)
}

sampling_inference_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(class = "teaching-intro-card",
      h3("从样本走向总体"),
      p("不同样本会产生不同的样本均值。标准误描述样本均值的波动，置信区间则给出与数据相容的总体均值范围。"),
      shiny::withMathJax(tags$div(class = "teaching-formula-row",
        HTML("\\[SE(\\bar X)=\\frac{\\sigma}{\\sqrt{n}}\\]"),
        HTML("\\[CI=\\bar X\\pm z_{1-\\alpha/2}SE(\\bar X)\\]")))),
    fluidRow(
      column(2, numericInput(ns("population_mean"), "总体均值", 0, step = .5)),
      column(2, numericInput(ns("population_sd"), "总体标准差", 1, min = .01, step = .1)),
      column(2, sliderInput(ns("sample_size"), "每次样本量", 5, 300, 30, step = 5)),
      column(2, sliderInput(ns("repetitions"), "重复抽样次数", 10, 200, 100, step = 10)),
      column(2, selectInput(ns("confidence"), "置信水平", c("90%" = .9, "95%" = .95, "99%" = .99), selected = .95)),
      column(2, numericInput(ns("seed"), "随机种子", 2026, min = 1, step = 1))),
    actionButton(ns("run"), "重复抽样并生成区间", icon = icon("arrows-left-right"), class = "btn-primary"),
    tags$div(class = "teaching-status", textOutput(ns("status"))),
    ggplot_editor_ui(ns("plot_editor"), height = "560px"),
    tags$div(class = "teaching-misconception",
      tags$strong("正确理解"), "95% 置信水平描述的是这套构造方法长期覆盖真实参数的比例，不表示某个已经算出的区间有 95% 概率包含固定的总体均值。")
  )
}

sampling_inference_server <- function(id, directory = reactive(getwd())) {
  moduleServer(id, function(input, output, session) {
    result <- reactiveVal(NULL); status <- reactiveVal("设置总体、样本量和置信水平后运行演示。")
    observeEvent(input$run, {
      tryCatch({
        value <- simulate_confidence_intervals(input$population_mean, input$population_sd,
          input$sample_size, input$repetitions, as.numeric(input$confidence), input$seed)
        result(value)
        status(sprintf("标准误为 %.4f；%d 个区间中有 %d 个覆盖真实总体均值，模拟覆盖率为 %.1f%%。",
          value$standard_error, value$repetitions, sum(value$intervals$覆盖总体均值), value$coverage * 100))
      }, error = function(e) status(conditionMessage(e)))
    })
    output$status <- renderText(status())
    plot <- reactive({ req(result()); build_confidence_interval_plot(result()) })
    ggplot_editor_server("plot_editor", plot, directory, "easyr-confidence-intervals")
    result
  })
}

simulate_hypothesis_tests <- function(null_mean = 0, true_mean = 0, population_sd = 1,
                                      sample_size = 30L, repetitions = 1000L, alpha = .05, seed = 2026L) {
  sample_size <- as.integer(sample_size); repetitions <- as.integer(repetitions); seed <- as.integer(seed)
  if (any(!is.finite(c(null_mean, true_mean)))) stop("均值必须是有限数值。", call. = FALSE)
  if (!is.finite(population_sd) || population_sd <= 0) stop("总体标准差必须大于 0。", call. = FALSE)
  if (!is.finite(sample_size) || sample_size < 2L || sample_size > 10000L) stop("样本量应在 2 到 10,000 之间。", call. = FALSE)
  if (!is.finite(repetitions) || repetitions < 100L || repetitions > 10000L) stop("重复检验次数应在 100 到 10,000 之间。", call. = FALSE)
  if (!is.finite(alpha) || alpha <= 0 || alpha >= .5) stop("显著性水平必须在 0 到 0.5 之间。", call. = FALSE)
  set.seed(seed)
  means <- colMeans(matrix(stats::rnorm(sample_size * repetitions, true_mean, population_sd), nrow = sample_size))
  standard_error <- population_sd / sqrt(sample_size)
  z <- (means - null_mean) / standard_error
  p_values <- 2 * stats::pnorm(-abs(z)); rejected <- p_values < alpha
  critical <- stats::qnorm(1 - alpha / 2)
  lower_boundary <- null_mean - critical * standard_error
  upper_boundary <- null_mean + critical * standard_error
  data <- data.frame(样本均值 = means, Z统计量 = z, P值 = p_values, 拒绝原假设 = rejected, check.names = FALSE)
  list(null_mean = null_mean, true_mean = true_mean, population_sd = population_sd,
    sample_size = sample_size, repetitions = repetitions, alpha = alpha, standard_error = standard_error,
    lower_boundary = lower_boundary, upper_boundary = upper_boundary, data = data, rejection_rate = mean(rejected))
}

build_hypothesis_test_plot <- function(result) {
  d <- result$data
  d$检验决定 <- factor(ifelse(d$拒绝原假设, "拒绝 H₀", "不拒绝 H₀"), levels = c("不拒绝 H₀", "拒绝 H₀"))
  ggplot2::ggplot(d, ggplot2::aes(样本均值, fill = 检验决定)) +
    ggplot2::geom_histogram(bins = 45, alpha = .82, colour = "white") +
    ggplot2::geom_vline(xintercept = result$null_mean, colour = "#172b4d", linetype = "dashed", linewidth = 1) +
    ggplot2::geom_vline(xintercept = c(result$lower_boundary, result$upper_boundary), colour = "#dc2626", linewidth = .9) +
    ggplot2::scale_fill_manual(values = c("不拒绝 H₀" = "#93c5fd", "拒绝 H₀" = "#f87171")) +
    lm_plot_theme() + ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(title = "样本均值与假设检验的拒绝区域",
      subtitle = "黑色虚线是假设的总体均值；红线外侧为双侧检验拒绝区域", x = "重复实验得到的样本均值", y = "次数", fill = NULL)
}

hypothesis_lab_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(class = "teaching-intro-card",
      h3("假设检验与 P 值"),
      p("先假设总体均值等于某个值，再判断当前样本在这个假设下是否过于极端。P 值越小，表示数据与原假设越不相容。"),
      shiny::withMathJax(tags$div(class = "teaching-formula-row",
        HTML("\\[H_0:\\mu=\\mu_0\\qquad H_1:\\mu\\ne\\mu_0\\]"),
        HTML("\\[Z=\\frac{\\bar X-\\mu_0}{\\sigma/\\sqrt n},\\qquad p=2P(Z\\ge |z_{obs}|)\\]")))),
    fluidRow(
      column(2, numericInput(ns("null_mean"), "原假设均值", 0, step = .2)),
      column(2, numericInput(ns("true_mean"), "模拟真实均值", 0, step = .2)),
      column(2, numericInput(ns("population_sd"), "总体标准差", 1, min = .01, step = .1)),
      column(2, sliderInput(ns("sample_size"), "每次样本量", 5, 300, 30, step = 5)),
      column(2, selectInput(ns("alpha"), "显著性水平 α", c("0.10" = .1, "0.05" = .05, "0.01" = .01), selected = .05)),
      column(2, numericInput(ns("seed"), "随机种子", 2026, min = 1, step = 1))),
    sliderInput(ns("repetitions"), "重复检验次数", 100, 5000, 1000, step = 100),
    actionButton(ns("run"), "运行重复检验", icon = icon("flask"), class = "btn-primary"),
    tags$div(class = "teaching-status", textOutput(ns("status"))),
    ggplot_editor_ui(ns("plot_editor"), height = "500px"),
    tags$div(class = "teaching-concept-grid",
      tags$div(class = "teaching-concept-card", h4("第一类错误"), p("原假设实际正确，却因为随机波动而拒绝它。显著性水平 α 控制这种长期错误率。")),
      tags$div(class = "teaching-concept-card", h4("检验功效"), p("原假设实际错误时，检验成功识别差异的概率。更大样本和更明显差异通常提高功效。"))),
    tags$div(class = "teaching-misconception", tags$strong("P 值不是什么"),
      "P 值不是“原假设为真的概率”，也不能单独表示效应是否重要。应同时报告估计值、区间和实际意义。")
  )
}

hypothesis_lab_server <- function(id, directory = reactive(getwd())) {
  moduleServer(id, function(input, output, session) {
    result <- reactiveVal(NULL); status <- reactiveVal("设置原假设和模拟真实情况后运行重复检验。")
    observeEvent(input$run, {
      tryCatch({
        value <- simulate_hypothesis_tests(input$null_mean, input$true_mean, input$population_sd,
          input$sample_size, input$repetitions, as.numeric(input$alpha), input$seed)
        result(value)
        situation <- if (isTRUE(all.equal(value$true_mean, value$null_mean))) "第一类错误率" else "检验功效"
        status(sprintf("%d 次检验中有 %d 次拒绝 H₀；模拟%s为 %.1f%%。",
          value$repetitions, sum(value$data$拒绝原假设), situation, value$rejection_rate * 100))
      }, error = function(e) status(conditionMessage(e)))
    })
    output$status <- renderText(status())
    plot <- reactive({ req(result()); build_hypothesis_test_plot(result()) })
    ggplot_editor_server("plot_editor", plot, directory, "easyr-hypothesis-tests")
    result
  })
}

teaching_mode_ui <- function() {
  tagList(
    tags$style(HTML(".teaching-mode-header{margin-bottom:14px}.teaching-mode-header h2{margin:0 0 5px;color:#142b50}.teaching-mode-header p{color:#667892}.teaching-route{display:grid;grid-template-columns:repeat(auto-fit,minmax(145px,1fr));gap:8px;margin:14px 0}.teaching-route-card{background:#eef5ff;border:1px solid #cfe0f5;border-radius:10px;padding:10px;color:#174a8b;font-size:12px}.teaching-route-card strong{display:block;font-size:14px;margin-bottom:3px}.teaching-intro-card{background:linear-gradient(145deg,#f8fbff,#fff);border:1px solid #d8e5f4;border-radius:12px;padding:15px 17px;margin-bottom:14px}.teaching-intro-card h3{margin-top:0;color:#173d70}.teaching-concept-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:10px;margin:12px 0}.teaching-concept-card{background:#f8fafc;border:1px solid #e0e8f2;border-radius:10px;padding:13px}.teaching-concept-card h4{margin:0 0 6px;color:#173d70}.teaching-feedback,.teaching-status,.teaching-misconception{border-radius:9px;padding:10px 12px;margin:12px 0}.teaching-feedback.correct{background:#ecfdf5;color:#166534}.teaching-feedback.incorrect{background:#fff7ed;color:#9a3412}.teaching-status{background:#eef5ff;color:#174a8b}.teaching-misconception{background:#fff7ed;color:#854d0e;border-left:4px solid #f59e0b}.teaching-formula-row{display:flex;gap:28px;flex-wrap:wrap}.teaching-formula-row>*{min-width:280px;flex:1}.teaching-mode-tabs>.tabbable>.nav-pills{display:flex;gap:5px;flex-wrap:wrap;background:#edf3fa;border-radius:10px;padding:5px;margin-bottom:16px}.teaching-mode-tabs>.tabbable>.nav-pills>li{float:none}.teaching-mode-tabs>.tabbable>.nav-pills>li>a{border-radius:7px;padding:8px 13px;color:#36577e;font-weight:750}.teaching-mode-tabs>.tabbable>.nav-pills>li.active>a{background:#2563eb;color:#fff}@media(max-width:850px){.teaching-route{grid-template-columns:1fr 1fr}.teaching-concept-grid{grid-template-columns:1fr}}")),
    tags$div(class = "teaching-mode-header", h2("教学模式"),
      p("面向零基础学习者：先建立直觉，再观察模拟，最后理解公式。每个实验都可以改变参数并重新运行。")),
    tags$div(class = "teaching-route",
      tags$div(class = "teaching-route-card", tags$strong("1 认识数据"), "总体、样本与变量"),
      tags$div(class = "teaching-route-card", tags$strong("2 描述数据"), "中心与离散程度"),
      tags$div(class = "teaching-route-card", tags$strong("3 理解概率"), "重复实验与长期频率"),
      tags$div(class = "teaching-route-card", tags$strong("4 排列组合"), "顺序、放回与计数"),
      tags$div(class = "teaching-route-card", tags$strong("5 概率规则"), "补集、交集、并集与独立"),
      tags$div(class = "teaching-route-card", tags$strong("6 条件概率"), "已知信息与贝叶斯更新"),
      tags$div(class = "teaching-route-card", tags$strong("7 认识分布"), "随机变量与常见分布"),
      tags$div(class = "teaching-route-card", tags$strong("8 学习推断"), "抽样、标准误与区间"),
      tags$div(class = "teaching-route-card", tags$strong("9 假设检验"), "P 值、错误率与功效")),
    tags$div(class = "teaching-mode-tabs",
      tabsetPanel(type = "pills", id = "teaching_tabs",
        tabPanel("学习路线", value = "route",
          tags$div(class = "teaching-intro-card", h3("怎样使用教学模式"),
            p("建议从左到右学习。每一部分都遵循“生活例子 → 动手实验 → 图形观察 → 数学公式 → 常见误区”的顺序。学习不会修改已导入的数据或模型结果。")),
          tags$div(class = "teaching-concept-grid",
            tags$div(class = "teaching-concept-card", h4("第一阶段：描述"), p("先学会区分变量，并用均值、中位数、方差和图形概括数据。")),
            tags$div(class = "teaching-concept-card", h4("第二阶段：随机性"), p("通过硬币和骰子认识事件、概率以及短期波动。")),
            tags$div(class = "teaching-concept-card", h4("第三阶段：分布"), p("理解随机变量的可能取值以及概率如何分配。")),
            tags$div(class = "teaching-concept-card", h4("第四阶段：推断"), p("理解为什么样本会变化，以及如何用样本估计总体。")))),
        tabPanel("认识数据", value = "data",
          tags$div(class = "teaching-intro-card", h3("总体、样本和变量"),
            p("总体是想研究的全部对象，样本是实际收集到的一部分对象。参数描述总体，统计量由样本计算得到。"),
            shiny::withMathJax(HTML("\\[\\text{总体参数 }\\mu,\\sigma^2\\qquad\\longleftarrow\\qquad\\text{样本统计量 }\\bar{x},s^2\\]"))),
          tags$div(class = "teaching-concept-grid",
            tags$div(class = "teaching-concept-card", h4("数值变量"), p("数值具有可解释的距离，可以计算加减、均值和方差。")),
            tags$div(class = "teaching-concept-card", h4("类别变量"), p("类别用于分组；类别编码成数字后，数字大小通常没有计算意义。"))),
          teaching_data_quiz_ui("teaching_data_quiz")),
        tabPanel("概率实验", value = "probability", probability_lab_ui("probability_lab")),
        tabPanel("排列组合", value = "counting", counting_methods_ui("counting_lab")),
        tabPanel("概率规则", value = "probability_rules", probability_rules_ui("probability_rules_lab")),
        tabPanel("条件概率", value = "bayes", bayes_lab_ui("bayes_lab")),
        tabPanel("描述统计与分布", value = "distributions", distribution_demo_ui("distributions")),
        tabPanel("抽样与推断", value = "inference", sampling_inference_ui("sampling_lab")),
        tabPanel("假设检验", value = "testing", hypothesis_lab_ui("hypothesis_lab"))
      ))
  )
}

teaching_mode_server <- function(directory = reactive(getwd())) {
  teaching_data_quiz_server("teaching_data_quiz")
  probability_lab_server("probability_lab", directory)
  counting_methods_server("counting_lab", directory)
  probability_rules_server("probability_rules_lab", directory)
  bayes_lab_server("bayes_lab", directory)
  distribution_demo_server("distributions", directory)
  sampling_inference_server("sampling_lab", directory)
  hypothesis_lab_server("hypothesis_lab", directory)
}
