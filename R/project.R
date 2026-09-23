easyr_project_version <- 1L

project_column_supported <- function(column) {
  is.atomic(column) || is.factor(column) || inherits(column, c("Date", "POSIXct", "POSIXlt"))
}

validate_project_datasets <- function(datasets) {
  if (!is.list(datasets) || !length(datasets) || is.null(names(datasets)) ||
      any(!nzchar(names(datasets))) || anyDuplicated(names(datasets))) {
    stop("项目必须包含至少一个具有有效名称的数据集。", call. = FALSE)
  }
  for (name in names(datasets)) {
    value <- datasets[[name]]
    if (!is.data.frame(value) || !ncol(value) || any(!vapply(value, project_column_supported, logical(1)))) {
      stop(paste0("数据集“", name, "”包含不支持的数据结构。"), call. = FALSE)
    }
  }
  invisible(TRUE)
}

project_input_allowed <- function(name) {
  if (!grepl("^(analysis|regression|random_forest|svm|pca|kmeans|timeseries)-", name)) return(FALSE)
  if (grepl("(^|[-_])(run|save|download|apply|recommend|generate|tutorial|previous|next|reset|regenerate|remove|import)([-_]|$)", name)) return(FALSE)
  if (grepl("ai_(report|params)|api|key|consent|prompt|question|meaning|purpose", name, ignore.case = TRUE)) return(FALSE)
  TRUE
}

project_capture_ui_state <- function(input_values) {
  if (!is.list(input_values) || is.null(names(input_values))) return(list())
  keep <- vapply(names(input_values), project_input_allowed, logical(1))
  values <- input_values[keep]
  values <- values[vapply(values, function(value) {
    is.null(value) || ((is.atomic(value) || inherits(value, c("Date", "POSIXt"))) && length(value) <= 200L)
  }, logical(1))]
  lapply(values, function(value) {
    if (inherits(value, c("Date", "POSIXt"))) as.character(value) else value
  })
}

sanitize_project_workbench <- function(state, dataset_names) {
  if (!is.list(state)) return(list())
  result <- list(configurations = list(), working_versions = list(), column_missing_rules = list())
  for (field in names(result)) {
    values <- state[[field]]
    if (is.list(values) && length(values)) result[[field]] <- values[intersect(names(values), dataset_names)]
  }
  if (length(result$working_versions)) {
    valid <- vapply(result$working_versions, function(value) {
      is.data.frame(value) && ncol(value) > 0L && all(vapply(value, project_column_supported, logical(1)))
    }, logical(1))
    result$working_versions <- result$working_versions[valid]
  }
  valid_methods <- c("keep", "drop", "mean", "median", "mode", "zero", "forward", "backward", "linear")
  if (length(result$column_missing_rules)) result$column_missing_rules <- Filter(function(rules) {
    is.atomic(rules) && !is.null(names(rules)) && all(as.character(rules) %in% valid_methods)
  }, result$column_missing_rules)
  result
}

build_easyr_project <- function(import_state, workbench_state = list(), ui_state = list()) {
  if (!is.list(import_state)) stop("无法读取当前数据集状态。", call. = FALSE)
  validate_project_datasets(import_state$datasets)
  active <- import_state$active
  if (length(active) != 1L || !active %in% names(import_state$datasets)) active <- names(import_state$datasets)[1L]
  list(
    format = "EasyR Project",
    version = easyr_project_version,
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    import = list(datasets = import_state$datasets, active = active),
    workbench = sanitize_project_workbench(workbench_state, names(import_state$datasets)),
    ui = project_capture_ui_state(ui_state)
  )
}

validate_easyr_project <- function(project) {
  if (!is.list(project) || !identical(project$format, "EasyR Project")) {
    stop("这不是可识别的 RBench 项目文件。", call. = FALSE)
  }
  version <- suppressWarnings(as.integer(project$version))
  if (length(version) != 1L || is.na(version) || version < 1L || version > easyr_project_version) {
    stop("项目文件版本与当前 RBench 不兼容。", call. = FALSE)
  }
  if (!is.list(project$import)) stop("项目缺少数据集状态。", call. = FALSE)
  validate_project_datasets(project$import$datasets)
  if (!is.null(project$ui) && !is.list(project$ui)) stop("项目中的界面参数无效。", call. = FALSE)
  if (!is.null(project$workbench) && !is.list(project$workbench)) stop("项目中的整理状态无效。", call. = FALSE)
  if (length(project$import$active) != 1L || !project$import$active %in% names(project$import$datasets)) {
    project$import$active <- names(project$import$datasets)[1L]
  }
  project$workbench <- sanitize_project_workbench(project$workbench, names(project$import$datasets))
  project$ui <- project_capture_ui_state(project$ui)
  project
}

project_ui <- function(id) {
  ns <- NS(id)
  tags$details(class = "project-card",
    tags$summary(icon("folder-open"), tags$span("RBench 项目保存与恢复")),
    tags$div(class = "project-card-body",
      p(class = "project-card-hint", "保存全部数据集、当前数据集、整理状态以及分析和绘图参数。项目文件不包含 API Key。"),
      downloadButton(ns("save_project"), "保存 RBench 项目", class = "btn-primary project-save-button"),
      fileInput(ns("project_file"), "打开已有项目", accept = c(".easyr", ".rds"),
        buttonLabel = "选择项目", placeholder = "尚未选择项目文件"),
      actionButton(ns("restore_project"), "恢复这个项目", icon = icon("clock-rotate-left")),
      tags$div(class = "project-card-status", textOutput(ns("status")))
    )
  )
}

restore_easyr_project_upload <- function(upload, imported, project_channel, app_input, app_session) {
  if (is.null(upload) || !length(upload$datapath)) stop("请先选择 RBench 项目文件。", call. = FALSE)
  if (upload$size > 1500 * 1024^2) stop("项目文件超过 1500 MB，无法在当前界面恢复。", call. = FALSE)
  project <- validate_easyr_project(readRDS(upload$datapath))
  imported$restore(project$import)
  if (!is.null(project_channel$restore)) project_channel$restore(project$workbench)
  ui_state <- project$ui
  app_session$onFlushed(function() {
    if (length(ui_state)) for (name in names(ui_state)) {
      app_session$sendInputMessage(name, list(value = ui_state[[name]]))
    }
  }, once = TRUE)
  project
}

project_server <- function(id, imported, project_channel, app_input, app_session) {
  moduleServer(id, function(input, output, session) {
    status <- reactiveVal("尚未保存或恢复项目。")
    current_project <- function() {
      if (!length(imported$datasets())) stop("请先导入至少一个数据集，再保存项目。", call. = FALSE)
      workbench <- if (!is.null(project_channel$snapshot)) project_channel$snapshot() else list()
      ui <- project_capture_ui_state(reactiveValuesToList(app_input, all.names = TRUE))
      build_easyr_project(imported$snapshot(), workbench, ui)
    }
    output$save_project <- downloadHandler(
      filename = function() paste0("RBench-project-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".easyr"),
      content = function(file) {
        project <- current_project()
        saveRDS(project, file, compress = "gzip", version = 3)
        status(sprintf("项目已保存：%d 个数据集。", length(project$import$datasets)))
      },
      contentType = "application/octet-stream"
    )
    observeEvent(input$restore_project, {
      req(input$project_file)
      tryCatch({
        project <- restore_easyr_project_upload(
          input$project_file, imported, project_channel, app_input, app_session
        )
        status(sprintf("恢复完成：%d 个数据集，当前数据集为“%s”。模型需要重新点击运行。",
          length(project$import$datasets), project$import$active))
        showNotification("RBench 项目已恢复。", type = "message")
      }, error = function(e) {
        status(paste0("恢复失败：", conditionMessage(e)))
        showNotification(conditionMessage(e), type = "error", duration = 10)
      })
    })
    output$status <- renderText(status())
    list(status = reactive(status()), current_project = current_project)
  })
}
