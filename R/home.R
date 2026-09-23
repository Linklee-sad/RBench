startup_ui <- function(id) {
  ns <- NS(id)
  tags$div(class = "startup-shell",
    tags$div(class = "startup-card",
      tags$div(class = "startup-brand",
        tags$div(class = "startup-brand-mark", icon("chart-simple")),
        tags$div(tags$h1("RBench"), tags$p("数据分析工作台"))
      ),
      tags$div(class = "startup-actions",
        actionButton(ns("new_project"), "创建数据分析项目", icon = icon("plus"),
          class = "btn-primary startup-primary"),
        actionButton(ns("learning"), "进入学习模式", icon = icon("graduation-cap"),
          class = "btn-default startup-learning"),
        tags$p(class = "startup-learning-hint", "从概率与统计基础开始，通过公式、模拟和互动图形逐步学习。"),
        actionButton(ns("function_plotter"), "打开函数绘图工具", icon = icon("chart-line"),
          class = "btn-default startup-learning startup-function-plotter"),
        tags$p(class = "startup-learning-hint", "直接输入数学函数并绘图，不需要创建项目或导入数据。"),
        tags$div(class = "startup-divider", tags$span("打开已有分析项目")),
        fileInput(ns("project_file"), "打开已有 RBench 项目", accept = c(".easyr", ".rds"),
          buttonLabel = "选择项目文件", placeholder = "尚未选择项目文件"),
        tags$p(class = "startup-file-hint", "选择后自动打开项目"),
        tags$div(class = "startup-example-picker",
          selectInput(ns("example_dataset"), "选择示例数据集", easyr_example_choices(), selected = "iris"),
          tags$p(class = "startup-file-hint", textOutput(ns("example_description"))),
          actionButton(ns("sample"), "使用所选示例开始", icon = icon("flask"),
            class = "btn-default startup-sample")
        ),
        tags$div(class = "startup-status", textOutput(ns("status")))
      )
    )
  )
}

startup_server <- function(id, imported, project_channel, app_input, app_session, enter_workspace, enter_learning,
    enter_function_plotter = function() NULL) {
  moduleServer(id, function(input, output, session) {
    status <- reactiveVal("创建一个新项目，或打开之前保存的 .easyr 项目。")

    observeEvent(input$new_project, enter_workspace())
    observeEvent(input$learning, enter_learning())
    observeEvent(input$function_plotter, enter_function_plotter())

    observeEvent(input$sample, {
      example <- easyr_example_dataset(input$example_dataset)
      imported$add(stats::setNames(list(example$data), example$name))
      enter_workspace()
    })

    output$example_description <- renderText(easyr_example_dataset(input$example_dataset)$description)

    observeEvent(input$project_file, {
      req(input$project_file)
      tryCatch({
        project <- restore_easyr_project_upload(
          input$project_file, imported, project_channel, app_input, app_session
        )
        status(sprintf("恢复完成：%d 个数据集。", length(project$import$datasets)))
        enter_workspace()
        showNotification("RBench 项目已恢复。", type = "message")
      }, error = function(e) {
        status(paste0("打开失败：", conditionMessage(e)))
        showNotification(conditionMessage(e), type = "error", duration = 10)
      })
    })

    output$status <- renderText(status())
  })
}
