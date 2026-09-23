function_plotter_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$script(HTML(sprintf("(function(){document.addEventListener('keydown',function(e){if(e.key==='Enter'&&e.target.closest('#%s .function-expression-input')){e.preventDefault();e.target.dispatchEvent(new Event('change',{bubbles:true}));setTimeout(function(){document.getElementById('%s').click();},80);}});})();",
      ns("workspace"), ns("add_function")))),
    tags$div(id = ns("workspace"), class = "function-plotter-workspace",
      tags$aside(class = "function-plotter-sidebar",
        tags$div(class = "function-plotter-sidebar-title", icon("chart-line"), tags$span("函数")),
        tags$p(class = "function-plotter-sidebar-hint", "普通函数输入 f(x)；椭圆等封闭图形请选择参数曲线。按 Enter 添加下一行。"),
        uiOutput(ns("function_rows")),
        actionButton(ns("add_function"), "添加函数", icon = icon("plus"), class = "function-add-button"),
        tags$hr(),
        numericInput(ns("points"), "每个函数的绘图点数", 1000, min = 100, max = 5000, step = 100),
        tags$div(class = "function-plotter-checks",
          checkboxInput(ns("show_axes"), "显示坐标轴", TRUE),
          checkboxInput(ns("show_grid"), "显示网格", TRUE),
          checkboxInput(ns("mark_roots"), "标记估算零点", FALSE)),
        tags$details(class = "function-plotter-style",
          tags$summary(icon("sliders-h"), " 图像设置"),
          tags$div(class = "function-plotter-style-body",
            textInput(ns("title"), "图表标题", "函数图像"),
            fluidRow(column(6, textInput(ns("x_label"), "横轴标题", "x")),
              column(6, textInput(ns("y_label"), "纵轴标题", "y"))),
            selectInput(ns("theme"), "主题", c("简洁" = "minimal", "经典" = "classic", "黑白网格" = "bw")),
            selectInput(ns("color"), "单函数颜色", c("蓝色" = "#3b82f6", "绿色" = "#059669", "紫色" = "#7c3aed", "橙色" = "#ea580c")),
            selectInput(ns("palette"), "多函数配色", c("蓝绿黄" = "D", "紫红黄" = "C", "暗紫橙" = "A")),
            sliderInput(ns("alpha"), "透明度", min = 0.1, max = 1, value = 0.8, step = 0.05),
            sliderInput(ns("line_width"), "线宽", min = 0.2, max = 4, value = 1.1, step = 0.1),
            numericInput(ns("font_size"), "字号", 12, min = 8, max = 24),
            fluidRow(column(6, numericInput(ns("width"), "宽度（英寸）", 10, min = 4, max = 20)),
              column(6, numericInput(ns("height"), "高度（英寸）", 6, min = 3, max = 16))),
            selectInput(ns("dpi"), "导出 DPI", c(96, 150, 300), selected = 150))),
        tags$div(class = "function-sidebar-actions",
          downloadButton(ns("png"), "下载 PNG"),
          actionButton(ns("save_png"), "保存", icon = icon("save"))),
        tags$div(class = "function-plotter-saved", textOutput(ns("saved_png")))),
      tags$main(class = "function-plotter-canvas",
        uiOutput(ns("empty_hint")),
        plotOutput(ns("plot"), height = "650px"),
        textOutput(ns("notes")),
        tableOutput(ns("stats")))
    )
  )
}

function_plotter_server <- function(id, directory = reactive(getwd())) {
  moduleServer(id, function(input, output, session) {
    function_ids <- reactiveVal(1L)
    next_id <- reactiveVal(2L)
    cached_values <- reactiveVal(list())

    snapshot_values <- function() {
      cache <- cached_values()
      for (i in function_ids()) {
        cache[[as.character(i)]] <- list(mode = isolate(input[[paste0("mode_", i)]]),
          expression = isolate(input[[paste0("expression_", i)]]), x_min = isolate(input[[paste0("x_min_", i)]]),
          x_max = isolate(input[[paste0("x_max_", i)]]), x_expression = isolate(input[[paste0("x_expression_", i)]]),
          y_expression = isolate(input[[paste0("y_expression_", i)]]), t_min = isolate(input[[paste0("t_min_", i)]]),
          t_max = isolate(input[[paste0("t_max_", i)]]))
      }
      cached_values(cache)
    }
    value_or <- function(id, field, fallback) {
      live <- isolate(input[[paste0(field, "_", id)]])
      if (!is.null(live) && length(live)) return(live)
      saved <- cached_values()[[as.character(id)]][[field]]
      if (!is.null(saved) && length(saved)) saved else fallback
    }

    output$function_rows <- renderUI({
      ids <- function_ids()
      palette <- c("#6d597a", "#f6c945", "#2a9d8f", "#e76f51", "#3b82f6", "#d946ef")
      tagList(lapply(seq_along(ids), function(position) {
        i <- ids[position]
        tags$div(class = "function-input-row",
          tags$div(class = "function-type-line",
            tags$span(class = "function-color-dot", style = sprintf("background:%s", palette[(position - 1L) %% length(palette) + 1L])),
            selectInput(session$ns(paste0("mode_", i)), NULL,
              c("普通函数" = "function", "参数曲线" = "parametric"), selected = value_or(i, "mode", "function")),
            actionButton(session$ns(paste0("remove_", i)), NULL, icon = icon("trash"), class = "function-remove-button", title = "删除这个函数")),
          conditionalPanel(sprintf("input['%s'] === 'function'", session$ns(paste0("mode_", i))),
            tags$div(class = "function-expression-line function-formula-line",
              tags$span(class = "function-prefix", paste0("f", position, "(x) =")),
              tags$div(class = "function-expression-input",
                textInput(session$ns(paste0("expression_", i)), NULL, value_or(i, "expression", ""),
                  placeholder = if (position == 1L) "例如 sin(x)" else "输入函数"))),
            tags$div(class = "function-domain-line",
              tags$span(class = "function-domain-symbol", "x ∈ ["),
              numericInput(session$ns(paste0("x_min_", i)), NULL, value_or(i, "x_min", -10)), tags$span(","),
              numericInput(session$ns(paste0("x_max_", i)), NULL, value_or(i, "x_max", 10)), tags$span("]"))),
          conditionalPanel(sprintf("input['%s'] === 'parametric'", session$ns(paste0("mode_", i))),
            tags$div(class = "function-expression-line function-formula-line",
              tags$span(class = "function-prefix", "x(t) ="),
              tags$div(class = "function-expression-input",
                textInput(session$ns(paste0("x_expression_", i)), NULL, value_or(i, "x_expression", ""), placeholder = "例如 3*cos(t)"))),
            tags$div(class = "function-expression-line function-formula-line",
              tags$span(class = "function-prefix", "y(t) ="),
              tags$div(class = "function-expression-input",
                textInput(session$ns(paste0("y_expression_", i)), NULL, value_or(i, "y_expression", ""), placeholder = "例如 2*sin(t)"))),
            tags$div(class = "function-domain-line",
              tags$span(class = "function-domain-symbol", "t ∈ ["),
              numericInput(session$ns(paste0("t_min_", i)), NULL, value_or(i, "t_min", 0)), tags$span(","),
              numericInput(session$ns(paste0("t_max_", i)), NULL, value_or(i, "t_max", 2 * pi)), tags$span("]"))))
      }))
    })

    observeEvent(input$add_function, {
      if (length(function_ids()) >= 20L) return(showNotification("一次最多绘制 20 个函数。", type = "warning"))
      snapshot_values(); id <- next_id(); function_ids(c(function_ids(), id)); next_id(id + 1L)
    })
    for (i in seq_len(40L)) local({
      id <- i
      observeEvent(input[[paste0("remove_", id)]], {
        snapshot_values(); remaining <- setdiff(function_ids(), id)
        if (!length(remaining)) { new_id <- next_id(); next_id(new_id + 1L); remaining <- new_id }
        function_ids(remaining)
      }, ignoreInit = TRUE)
    })

    specifications <- reactive({
      palette <- c("#6d597a", "#f6c945", "#2a9d8f", "#e76f51", "#3b82f6", "#d946ef")
      results <- lapply(seq_along(function_ids()), function(position) {
        i <- function_ids()[position]
        mode <- input[[paste0("mode_", i)]]
        if (is.null(mode)) mode <- "function"
        if (identical(mode, "parametric")) {
          x_text <- trimws(as.character(input[[paste0("x_expression_", i)]])[1])
          y_text <- trimws(as.character(input[[paste0("y_expression_", i)]])[1])
          if ((!length(x_text) || is.na(x_text) || !nzchar(x_text)) && (!length(y_text) || is.na(y_text) || !nzchar(y_text))) return(NULL)
          validate(need(nzchar(x_text) && nzchar(y_text), sprintf("参数曲线 %d 需要同时填写 x(t) 和 y(t)。", position)))
          parse_one <- function(text, coordinate) tryCatch(parse_function_lines(text, max_functions = 1L, variable = "t")[[1]]$expression,
            error = function(e) validate(need(FALSE, sprintf("参数曲线 %d 的 %s：%s", position, coordinate, conditionMessage(e)))))
          parsed <- list(type = "parametric", label = paste0("r", position, "(t)"),
            x_expression = parse_one(x_text, "x(t)"), y_expression = parse_one(y_text, "y(t)"),
            t_min = input[[paste0("t_min_", i)]], t_max = input[[paste0("t_max_", i)]])
        } else {
          text <- trimws(as.character(input[[paste0("expression_", i)]])[1])
          if (!length(text) || is.na(text) || !nzchar(text)) return(NULL)
          parsed <- tryCatch(parse_function_lines(text, max_functions = 1L)[[1]],
            error = function(e) validate(need(FALSE, sprintf("第 %d 行：%s", position, conditionMessage(e)))))
          if (identical(parsed$label, parsed$text)) parsed$label <- paste0("f", position, "(x)")
          parsed$type <- "function"; parsed$x_min <- input[[paste0("x_min_", i)]]; parsed$x_max <- input[[paste0("x_max_", i)]]
        }
        parsed$color <- palette[(position - 1L) %% length(palette) + 1L]
        parsed
      })
      Filter(Negate(is.null), results)
    })

    settings <- reactive({
      o <- plot_defaults()
      for (name in c("title", "x_label", "y_label", "theme", "color", "palette", "font_size", "alpha",
          "line_width", "width", "height", "dpi")) if (!is.null(input[[name]])) o[[name]] <- input[[name]]
      for (name in c("font_size", "alpha", "line_width", "width", "height", "dpi")) o[[name]] <- as.numeric(o[[name]])
      o
    })
    chart <- reactive({
      validate(need(length(specifications()) > 0L, "请在左侧输入至少一个函数。"))
      tryCatch(build_function_plot_specs(specifications(), input$points, settings(), input$show_axes, input$show_grid, input$mark_roots),
        error = function(e) validate(need(FALSE, conditionMessage(e))))
    })
    output$empty_hint <- renderUI({
      if (!length(specifications())) tags$div(class = "function-empty-hint", icon("chart-line"),
        tags$strong("从左侧输入函数开始"), tags$span("例如：sin(x)、x^2 或 dnorm(x)"))
    })
    output$plot <- renderPlot({ req(length(specifications()) > 0L); print(chart()$plot) })
    output$notes <- renderText({ req(length(specifications()) > 0L); chart()$notes })
    output$stats <- renderTable({ req(length(specifications()) > 0L); chart()$stats }, digits = 4, striped = TRUE)

    write_png <- function(file) {
      o <- settings()
      validate(need(is.finite(o$width) && o$width >= 4 && o$width <= 20 && is.finite(o$height) && o$height >= 3 && o$height <= 16 && o$dpi %in% c(96, 150, 300),
        "导出宽度应为 4～20 英寸，高度为 3～16 英寸，DPI 为 96、150 或 300。"))
      ggplot2::ggsave(file, plot = chart()$plot, device = "png", width = o$width, height = o$height, dpi = o$dpi, bg = "white")
    }
    output$png <- downloadHandler(filename = function() paste0("easyr-function-plot-", Sys.Date(), ".png"), content = write_png)
    saved <- reactiveVal("")
    output$saved_png <- renderText(saved())
    observeEvent(input$save_png, {
      tryCatch({ path <- save_to_workdir(directory(), "easyr-function-plot", ".png", write_png); saved(paste("上次保存：", path)); showNotification("函数图像已保存。", type = "message") },
        error = function(e) showNotification(conditionMessage(e), type = "error"))
    })
  })
}
