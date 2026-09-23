plot_editor_defaults <- function() list(
  title = "", subtitle = "", x_label = "", y_label = "",
  theme = "minimal", palette = "original", line_width = 1,
  point_size = 2.5, alpha = 0.75, font_size = 13,
  background = "white", width = 10, height = 6, dpi = 150
)

plot_editor_family <- function() {
  switch(Sys.info()[["sysname"]], Darwin = "Arial Unicode MS", Windows = "Microsoft YaHei", "sans")
}

plot_editor_palette <- function(name) {
  switch(name,
    ocean = c("#2563eb", "#06b6d4", "#0f766e", "#7c3aed", "#60a5fa", "#14b8a6"),
    warm = c("#dc2626", "#ea580c", "#d97706", "#be123c", "#f59e0b", "#9f1239"),
    green = c("#047857", "#059669", "#65a30d", "#0f766e", "#22c55e", "#84cc16"),
    mono = c("#111827", "#4b5563", "#6b7280", "#9ca3af", "#374151", "#d1d5db"),
    NULL)
}

apply_plot_editor <- function(plot, options = list()) {
  if (!inherits(plot, "ggplot")) stop("图像编辑器只支持 ggplot2 图形。", call. = FALSE)
  o <- modifyList(plot_editor_defaults(), options)
  if (!o$theme %in% c("minimal", "classic", "bw")) stop("请选择有效图形主题。", call. = FALSE)
  if (!o$palette %in% c("original", "ocean", "warm", "green", "mono")) stop("请选择有效配色。", call. = FALSE)
  numeric_rules <- list(line_width = c(0.2, 4), point_size = c(0.5, 10), alpha = c(0.1, 1),
    font_size = c(8, 28), width = c(4, 24), height = c(3, 20))
  for (name in names(numeric_rules)) {
    o[[name]] <- as.numeric(o[[name]])
    bounds <- numeric_rules[[name]]
    if (length(o[[name]]) != 1 || !is.finite(o[[name]]) || o[[name]] < bounds[1] || o[[name]] > bounds[2]) {
      stop(paste0(name, " 设置超出允许范围。"), call. = FALSE)
    }
  }
  p <- plot
  replacement <- list()
  for (name in c("title", "subtitle", "x_label", "y_label")) {
    if (length(o[[name]]) == 1 && nzchar(trimws(o[[name]]))) replacement[[name]] <- o[[name]]
  }
  if (length(replacement)) {
    labels <- list(title = replacement$title, subtitle = replacement$subtitle,
      x = replacement$x_label, y = replacement$y_label)
    labels <- labels[!vapply(labels, is.null, logical(1))]
    p <- p + do.call(ggplot2::labs, labels)
  }
  line_geoms <- c("GeomLine", "GeomPath", "GeomSmooth", "GeomSegment", "GeomHline", "GeomVline",
    "GeomAbline", "GeomStep", "GeomDensity", "GeomFunction", "GeomBoxplot", "GeomViolin")
  colour_set <- plot_editor_palette(o$palette)
  colour_index <- 1L
  for (i in seq_along(p$layers)) {
    layer <- p$layers[[i]]
    geom <- class(layer$geom)[1]
    mapped <- unique(c(names(p$mapping), names(layer$mapping)))
    if (geom %in% line_geoms) layer$aes_params$linewidth <- o$line_width
    if (identical(geom, "GeomPoint")) layer$aes_params$size <- o$point_size
    if (geom %in% c("GeomPoint", "GeomLine", "GeomPath", "GeomSmooth", "GeomDensity")) layer$aes_params$alpha <- o$alpha
    if (!is.null(colour_set) && !any(mapped %in% c("colour", "color")) &&
        geom %in% c(line_geoms, "GeomPoint")) {
      layer$aes_params$colour <- colour_set[(colour_index - 1L) %% length(colour_set) + 1L]
      colour_index <- colour_index + 1L
    }
    if (!is.null(colour_set) && !"fill" %in% mapped && geom %in% c("GeomBar", "GeomCol", "GeomRibbon", "GeomBoxplot", "GeomViolin", "GeomDensity")) {
      layer$aes_params$fill <- colour_set[min(2L, length(colour_set))]
    }
    p$layers[[i]] <- layer
  }
  if (!is.null(colour_set)) {
    p$scales$scales <- Filter(function(scale) !any(scale$aesthetics %in% c("colour", "color", "fill")), p$scales$scales)
    p <- p + ggplot2::scale_colour_manual(values = rep(colour_set, 20)) +
      ggplot2::scale_fill_manual(values = rep(colour_set, 20))
  }
  background <- switch(o$background, transparent = "transparent", light = "#f5f7fb", "white")
  theme_function <- switch(o$theme, classic = ggplot2::theme_classic, bw = ggplot2::theme_bw, ggplot2::theme_minimal)
  p + theme_function(base_size = o$font_size, base_family = plot_editor_family()) +
    ggplot2::theme(plot.title.position = "plot", legend.position = "bottom",
      plot.background = ggplot2::element_rect(fill = background, colour = NA),
      panel.background = ggplot2::element_rect(fill = if (identical(background, "transparent")) NA else background, colour = NA))
}

ggplot_editor_ui <- function(id, height = "500px") {
  ns <- NS(id)
  open_condition <- paste0("input['", ns("toggle"), "'] % 2 === 1")
  tagList(
    actionButton(ns("toggle"), "编辑图像", icon = icon("sliders-h")),
    conditionalPanel(open_condition, tags$div(class = "well", style = "margin-top:10px",
      fluidRow(column(6, textInput(ns("title"), "图像标题（留空保留原标题）")),
        column(6, textInput(ns("subtitle"), "副标题（留空保留原副标题）"))),
      fluidRow(column(6, textInput(ns("x_label"), "横轴标题（留空保留）")),
        column(6, textInput(ns("y_label"), "纵轴标题（留空保留）"))),
      fluidRow(column(4, selectInput(ns("theme"), "主题", c("简洁" = "minimal", "经典" = "classic", "黑白网格" = "bw"))),
        column(4, selectInput(ns("palette"), "配色", c("保留原图" = "original", "蓝绿" = "ocean", "暖色" = "warm", "绿色" = "green", "黑白灰" = "mono"))),
        column(4, selectInput(ns("background"), "背景", c("白色" = "white", "浅灰蓝" = "light", "透明" = "transparent")))),
      fluidRow(column(4, sliderInput(ns("line_width"), "线宽", 0.2, 4, 1, step = 0.1)),
        column(4, sliderInput(ns("point_size"), "点大小", 0.5, 10, 2.5, step = 0.5)),
        column(4, sliderInput(ns("alpha"), "透明度", 0.1, 1, 0.75, step = 0.05))),
      fluidRow(column(3, numericInput(ns("font_size"), "字号", 13, min = 8, max = 28)),
        column(3, numericInput(ns("width"), "导出宽度（英寸）", 10, min = 4, max = 24)),
        column(3, numericInput(ns("height"), "导出高度（英寸）", 6, min = 3, max = 20)),
        column(3, selectInput(ns("dpi"), "导出 DPI", c(96, 150, 300), selected = 150))),
      actionButton(ns("reset"), "恢复图像设置")
    )),
    plotOutput(ns("plot"), height = height),
    downloadButton(ns("download"), "下载 PNG"),
    actionButton(ns("save"), "保存 PNG 到项目文件夹"),
    tags$div(style = "overflow-wrap:anywhere", textOutput(ns("saved")))
  )
}

ggplot_editor_server <- function(id, plot, directory = reactive(getwd()), filename = "easyr-chart") {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$toggle, {
      updateActionButton(session, "toggle",
        label = if (input$toggle %% 2 == 1) "收起图像编辑" else "编辑图像", icon = icon("sliders-h"))
    }, ignoreInit = TRUE)
    settings <- reactive({
      defaults <- plot_editor_defaults()
      for (name in names(defaults)) if (!is.null(input[[name]])) defaults[[name]] <- input[[name]]
      defaults$dpi <- as.numeric(defaults$dpi)
      defaults
    })
    observeEvent(input$reset, {
      o <- plot_editor_defaults()
      for (name in c("title", "subtitle", "x_label", "y_label")) updateTextInput(session, name, value = o[[name]])
      for (name in c("theme", "palette", "background", "dpi")) updateSelectInput(session, name, selected = as.character(o[[name]]))
      for (name in c("line_width", "point_size", "alpha")) updateSliderInput(session, name, value = o[[name]])
      for (name in c("font_size", "width", "height")) updateNumericInput(session, name, value = o[[name]])
    })
    styled <- reactive({ req(plot()); apply_plot_editor(plot(), settings()) })
    output$plot <- renderPlot(print(styled()))
    write_png <- function(file) {
      o <- settings()
      background <- if (identical(o$background, "transparent")) "transparent" else if (identical(o$background, "light")) "#f5f7fb" else "white"
      ggplot2::ggsave(file, plot = styled(), device = "png", width = o$width, height = o$height,
        dpi = o$dpi, bg = background)
    }
    output$download <- downloadHandler(filename = function() paste0(filename, "-", Sys.Date(), ".png"), content = write_png)
    saved <- reactiveVal("")
    output$saved <- renderText(saved())
    observeEvent(input$save, {
      req(plot())
      tryCatch({
        path <- save_to_workdir(directory(), filename, ".png", write_png)
        saved(paste("上次保存：", path)); showNotification("PNG 已保存到 RBench 项目文件夹。", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })
    styled
  })
}
