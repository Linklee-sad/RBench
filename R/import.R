repair_import_names <- function(data) {
  column_names <- enc2utf8(names(data))
  if (!length(column_names)) return(data)

  empty <- is.na(column_names) | !nzchar(trimws(column_names))
  if (empty[1]) column_names[1] <- "序号"
  other_empty <- which(empty & (seq_along(column_names) != 1L))
  if (length(other_empty)) column_names[other_empty] <- paste0("未命名列_", other_empty)

  unique_names <- character(length(column_names))
  for (i in seq_along(column_names)) {
    candidate <- base <- column_names[i]
    suffix <- 1L
    while (i > 1L && any(candidate == unique_names[seq_len(i - 1L)])) {
      candidate <- paste0(base, "_", suffix)
      suffix <- suffix + 1L
    }
    unique_names[i] <- candidate
  }
  names(data) <- unique_names
  data
}

read_table_file <- function(path, filename, encoding = "UTF-8", sep = ",", sheet = NULL, header = TRUE) {
  ext <- tolower(tools::file_ext(filename))
  data <- switch(ext,
    csv = read.csv(path, fileEncoding = encoding, sep = sep,
                   header = isTRUE(header), check.names = FALSE, stringsAsFactors = FALSE),
    xlsx = as.data.frame(readxl::read_excel(path, sheet = sheet, col_names = isTRUE(header))),
    xls = as.data.frame(readxl::read_excel(path, sheet = sheet, col_names = isTRUE(header))),
    stop("请选择 CSV、XLS 或 XLSX 文件。")
  )
  if (!ncol(data)) stop("文件没有可读取的列。")
  data <- repair_import_names(data)
  data
}

format_file_size <- function(bytes) {
  if (!length(bytes) || is.na(bytes) || !is.finite(bytes) || bytes < 0) return("未知大小")
  units <- c("B", "KB", "MB", "GB")
  power <- min(floor(log(max(bytes, 1), 1024)), length(units) - 1)
  paste0(format(round(bytes / 1024^power, if (power < 2) 0 else 1), trim = TRUE), " ", units[power + 1])
}

easyr_example_catalog <- function() {
  list(
    iris = list(
      label = "鸢尾花 iris（分类 / 聚类 / PCA）",
      name = "示例 · 鸢尾花 iris",
      description = "150 行、4 个连续测量字段和 3 个品种，适合分类、聚类、PCA 与基础绘图。",
      data = function() datasets::iris
    ),
    mtcars = list(
      label = "汽车性能 mtcars（回归 / 机器学习）",
      name = "示例 · 汽车性能 mtcars",
      description = "32 款汽车的油耗、马力、重量等指标，适合线性回归、变量关系和机器学习。",
      data = function() {
        value <- datasets::mtcars
        data.frame(Model = rownames(value), value, row.names = NULL, check.names = FALSE)
      }
    ),
    boston = list(
      label = "波士顿房价 Boston（回归 / 房价预测）",
      name = "示例 · 波士顿房价 Boston",
      description = "506 个地区的房价中位数及犯罪率、房间数、交通等 13 项特征，适合回归、特征分析和房价预测。",
      data = function() as.data.frame(MASS::Boston, check.names = FALSE)
    ),
    titanic = list(
      label = "泰坦尼克号 Titanic（分类 / 逻辑回归）",
      name = "示例 · 泰坦尼克号 Titanic",
      description = "2,201 名乘客的舱位、性别、年龄与生还结果，适合分类和逻辑回归。",
      data = function() {
        counts <- as.data.frame(datasets::Titanic)
        value <- counts[rep(seq_len(nrow(counts)), counts$Freq), setdiff(names(counts), "Freq"), drop = FALSE]
        rownames(value) <- NULL
        value
      }
    ),
    airquality = list(
      label = "纽约空气质量 airquality（缺失值 / 回归）",
      name = "示例 · 纽约空气质量 airquality",
      description = "包含臭氧、太阳辐射、风速和温度，并保留真实缺失值，适合练习清洗和回归。",
      data = function() datasets::airquality
    ),
    usarrests = list(
      label = "美国州犯罪 USArrests（PCA / 聚类）",
      name = "示例 · 美国州犯罪 USArrests",
      description = "美国 50 个州的四项犯罪与城市化指标，适合标准化、PCA 和 K-means。",
      data = function() {
        value <- datasets::USArrests
        data.frame(State = rownames(value), value, row.names = NULL, check.names = FALSE)
      }
    ),
    airpassengers = list(
      label = "航空乘客 AirPassengers（时间序列 / 预测）",
      name = "示例 · 航空乘客 AirPassengers",
      description = "1949—1960 年的月度航空乘客量，具有趋势和季节性，适合时间序列分析与预测。",
      data = function() data.frame(
        Date = seq(as.Date("1949-01-01"), by = "month", length.out = length(datasets::AirPassengers)),
        Passengers = as.numeric(datasets::AirPassengers), check.names = FALSE
      )
    ),
    swiss = list(
      label = "瑞士社会经济 swiss（多元回归）",
      name = "示例 · 瑞士社会经济 swiss",
      description = "47 个地区的生育率和社会经济指标，适合相关分析与多元线性回归。",
      data = function() {
        value <- datasets::swiss
        data.frame(Province = rownames(value), value, row.names = NULL, check.names = FALSE)
      }
    )
  )
}

easyr_example_choices <- function() {
  catalog <- easyr_example_catalog()
  stats::setNames(names(catalog), vapply(catalog, `[[`, character(1), "label"))
}

easyr_example_dataset <- function(key = "iris") {
  catalog <- easyr_example_catalog()
  if (!length(key) || is.na(key) || !key %in% names(catalog)) key <- "iris"
  spec <- catalog[[key]]
  value <- spec$data()
  if (!is.data.frame(value) || !nrow(value) || !ncol(value)) stop("示例数据集不可用。", call. = FALSE)
  list(key = key, name = spec$name, description = spec$description, data = value)
}

validate_upload_size <- function(bytes, limit_mb) {
  limit_mb <- as.numeric(limit_mb)
  if (length(limit_mb) != 1 || !is.finite(limit_mb) || !limit_mb %in% c(100, 500, 1000)) {
    stop("请选择有效的上传上限。", call. = FALSE)
  }
  if (length(bytes) != 1 || is.na(bytes) || !is.finite(bytes) || bytes < 0) {
    stop("无法确认文件大小，请重新选择文件。", call. = FALSE)
  }
  if (bytes > limit_mb * 1024^2) {
    stop(sprintf("文件大小为 %s，超过当前 %s MB 上限。请提高上限后再次点击“导入数据”，或选择较小文件。",
      format_file_size(bytes), format(limit_mb, trim = TRUE)), call. = FALSE)
  }
  invisible(TRUE)
}

read_upload_batch <- function(files, encoding = "UTF-8", sep = ",", sheet = NULL, limit_mb = 500, header = TRUE) {
  if (is.null(files) || !nrow(files)) stop("请先选择文件。", call. = FALSE)
  extensions <- tolower(tools::file_ext(files$name))
  if (nrow(files) > 1 && any(extensions %in% c("xls", "xlsx"))) {
    stop("批量导入目前支持多个 CSV；Excel 请每次导入一个文件，以便选择工作表。", call. = FALSE)
  }
  result <- vector("list", nrow(files))
  names(result) <- make.unique(files$name, sep = " #")
  for (i in seq_len(nrow(files))) {
    validate_upload_size(files$size[i], limit_mb)
    result[[i]] <- tryCatch(
      read_table_file(files$datapath[i], files$name[i], encoding, sep, sheet, header),
      error = function(e) stop(paste0("“", files$name[i], "”读取失败：", conditionMessage(e)), call. = FALSE)
    )
  }
  result
}

dataset_bar_ui <- function(id) {
  ns <- NS(id)
  tags$div(class = "dataset-status-bar",
    fluidRow(
      column(4, selectInput(ns("active"), "当前数据集", choices = NULL)),
      column(6, tags$div(class = "dataset-status-text", textOutput(ns("dataset_status")))),
      column(2, actionButton(ns("remove"), "移除当前数据集", icon = icon("trash")))
    )
  )
}

import_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("导入数据"),
    selectInput(ns("upload_limit"), "单个文件上传上限",
      c("100 MB" = "100", "500 MB（推荐）" = "500", "1000 MB" = "1000"), selected = "500"),
    fileInput(ns("file"), "选择一个或多个表格", accept = c(".csv", ".xlsx", ".xls"), multiple = TRUE,
              buttonLabel = "选择文件", placeholder = "尚未选择文件"),
    textOutput(ns("file_info")),
    selectInput(ns("encoding"), "CSV 编码", c("UTF-8", "GB18030")),
    selectInput(ns("sep"), "CSV 分隔符", c("逗号" = ",", "分号" = ";", "制表符" = "\t")),
    checkboxInput(ns("header"), "第一行是字段名", TRUE),
    uiOutput(ns("sheet_ui")),
    actionButton(ns("import"), "导入并加入数据集", class = "btn-primary"),
    tags$div(class = "example-dataset-picker",
      selectInput(ns("example_dataset"), "示例数据集", easyr_example_choices(), selected = "iris"),
      actionButton(ns("demo"), "载入所选示例", icon = icon("flask")),
      tags$div(class = "example-dataset-description", textOutput(ns("example_description")))
    ),
    hr(), textOutput(ns("status")),
    helpText("可以一次选择多个 CSV。批量文件共用当前编码、分隔符和首行设置；Excel 请每次选择一个。取消“第一行是字段名”后，程序会自动生成字段名。表头中的空白字段也会自动命名；第一列为空时命名为“序号”。")
  )
}

import_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    datasets <- reactiveVal(list())
    active <- reactiveVal(NULL)
    revision <- reactiveVal(0L)
    status <- reactiveVal("选择一个或多个 CSV，或从下方载入示例数据集。")
    refresh_choices <- function(selected = active()) {
      choices <- names(datasets())
      if (!length(selected) || !selected %in% choices) selected <- head(choices, 1)
      active(if (length(selected)) selected else NULL)
      updateSelectInput(session, "active", choices = choices, selected = selected)
    }
    add_datasets <- function(values) {
      current <- datasets()
      added <- character()
      for (name in names(values)) {
        label <- name
        copy <- 2L
        while (label %in% names(current)) {
          label <- paste0(name, " (", copy, ")")
          copy <- copy + 1L
        }
        current[[label]] <- values[[name]]
        added <- c(added, label)
      }
      datasets(current)
      refresh_choices(added[1])
      revision(revision() + 1L)
      invisible(added)
    }
    snapshot_state <- function() {
      list(datasets = datasets(), active = active())
    }
    restore_state <- function(state) {
      if (!is.list(state) || !is.list(state$datasets) || !length(state$datasets)) {
        stop("项目中没有可恢复的数据集。", call. = FALSE)
      }
      if (is.null(names(state$datasets)) || any(!nzchar(names(state$datasets))) || anyDuplicated(names(state$datasets))) {
        stop("项目中的数据集名称无效。", call. = FALSE)
      }
      valid <- vapply(state$datasets, function(value) is.data.frame(value) && ncol(value) > 0L, logical(1))
      if (!all(valid)) stop("项目包含无法识别的数据集。", call. = FALSE)
      datasets(state$datasets)
      selected <- state$active
      if (length(selected) != 1L || !selected %in% names(state$datasets)) selected <- names(state$datasets)[1L]
      refresh_choices(selected)
      revision(revision() + 1L)
      status(sprintf("已从 RBench 项目恢复 %d 个数据集。", length(state$datasets)))
      invisible(selected)
    }
    output$file_info <- renderText({
      req(input$file)
      count <- nrow(input$file)
      total <- sum(input$file$size)
      shown <- paste(head(input$file$name, 4), collapse = "、")
      if (count > 4) shown <- paste0(shown, " 等")
      note <- if (any(input$file$size > 200 * 1024^2)) "；包含大型文件，导入可能需要较长时间和数 GB 内存" else ""
      paste0("已选择 ", count, " 个文件：", shown, "（合计 ", format_file_size(total), "）", note)
    })
    sheets <- reactive({
      req(input$file)
      if (nrow(input$file) != 1) return(NULL)
      validate_upload_size(input$file$size[1], input$upload_limit)
      if (!tolower(tools::file_ext(input$file$name[1])) %in% c("xls", "xlsx")) return(NULL)
      tryCatch(readxl::excel_sheets(input$file$datapath[1]), error = function(e) {
        showNotification(paste("无法读取工作表：", conditionMessage(e)), type = "error")
        NULL
      })
    })
    output$sheet_ui <- renderUI({
      if (length(sheets())) selectInput(session$ns("sheet"), "Excel 工作表", sheets())
    })
    observeEvent(input$import, {
      req(input$file)
      tryCatch({
        first_row_is_header <- if (is.null(input$header)) TRUE else isTRUE(input$header)
        result <- read_upload_batch(input$file, input$encoding, input$sep, input$sheet,
          input$upload_limit, first_row_is_header)
        add_datasets(result)
        status(sprintf("成功加入 %d 个数据集。可在页面顶部切换。", length(result)))
      }, error = function(e) {
        showNotification(paste("导入失败：", conditionMessage(e)), type = "error", duration = 10)
        status("导入失败。已经成功导入的数据集不受影响。")
      })
    })
    observeEvent(input$demo, {
      example <- easyr_example_dataset(input$example_dataset)
      add_datasets(stats::setNames(list(example$data), example$name))
      status(paste0("已加入", example$name, "。", example$description))
    })
    output$example_description <- renderText(easyr_example_dataset(input$example_dataset)$description)
    observeEvent(input$active, {
      if (length(input$active) == 1 && input$active %in% names(datasets())) active(input$active)
    })
    observeEvent(input$remove, {
      req(active())
      current <- datasets(); removed <- active(); current[[removed]] <- NULL; datasets(current)
      refresh_choices()
      status(paste0("已移除数据集：", removed, "。"))
    })
    output$status <- renderText(status())
    output$dataset_status <- renderText({
      if (!length(datasets())) return("尚未导入数据集")
      req(active(), active() %in% names(datasets()))
      value <- datasets()[[active()]]
      sprintf("已载入 %d 个数据集 · 当前：%s · %d 行 × %d 列", length(datasets()), active(), nrow(value), ncol(value))
    })
    current_data <- reactive({
      if (is.null(active()) || !active() %in% names(datasets())) return(NULL)
      datasets()[[active()]]
    })
    list(data = current_data, name = reactive(active()), datasets = reactive(datasets()), add = add_datasets,
      snapshot = snapshot_state, restore = restore_state, revision = reactive(revision()))
  })
}
