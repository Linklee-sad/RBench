source("setup.R")
library(shiny)
source("R/import.R")
source("R/project.R")
source("R/home.R")

rendered <- htmltools::renderTags(startup_ui("startup"))$html
stopifnot(
  grepl("startup-card", rendered),
  grepl("创建数据分析项目", rendered),
  grepl("进入学习模式", rendered),
  grepl("打开函数绘图工具", rendered),
  grepl("打开已有 EasyR 项目", rendered),
  grepl("选择后自动打开项目", rendered),
  !grepl("startup-open_project", rendered, fixed = TRUE),
  grepl("选择示例数据集", rendered),
  grepl("使用所选示例开始", rendered)
)

catalog <- easyr_example_catalog()
stopifnot(
  length(catalog) >= 7L,
  identical(unname(easyr_example_choices()), names(catalog)),
  all(vapply(names(catalog), function(key) {
    example <- easyr_example_dataset(key)
    is.data.frame(example$data) && nrow(example$data) > 0L && ncol(example$data) > 0L
  }, logical(1))),
  inherits(easyr_example_dataset("airpassengers")$data$Date, "Date"),
  anyNA(easyr_example_dataset("airquality")$data),
  identical(dim(easyr_example_dataset("boston")$data), c(506L, 14L)),
  "medv" %in% names(easyr_example_dataset("boston")$data),
  nrow(easyr_example_dataset("titanic")$data) == 2201L
)

values <- reactiveVal(list())
active <- reactiveVal(NULL)
revision <- reactiveVal(0L)
imported <- list(
  datasets = reactive(values()),
  name = reactive(active()),
  add = function(items) {
    values(items)
    active(names(items)[1L])
    revision(revision() + 1L)
  }
)
entered <- reactiveVal(0L)
learned <- reactiveVal(0L)
function_plotted <- reactiveVal(0L)
app_session <- new.env(parent = emptyenv())
app_session$onFlushed <- function(callback, once = FALSE) callback()
app_session$sendInputMessage <- function(inputId, message) invisible(NULL)
channel <- new.env(parent = emptyenv())

testServer(startup_server, args = list(
  imported = imported, project_channel = channel, app_input = reactiveValues(),
  app_session = app_session, enter_workspace = function() entered(entered() + 1L),
  enter_learning = function() learned(learned() + 1L),
  enter_function_plotter = function() function_plotted(function_plotted() + 1L)
), {
  session$setInputs(new_project = 1)
  session$flushReact()
  stopifnot(entered() == 1L)
  session$setInputs(learning = 1)
  session$flushReact()
  stopifnot(learned() == 1L)
  session$setInputs(function_plotter = 1)
  session$flushReact()
  stopifnot(function_plotted() == 1L)
  session$setInputs(example_dataset = "mtcars", sample = 1)
  session$flushReact()
  selected_example <- easyr_example_dataset("mtcars")
  stopifnot(entered() == 2L, identical(names(values()), selected_example$name),
    identical(values()[[1L]], selected_example$data))
})

app_env <- new.env(parent = globalenv())
sys.source("app.R", envir = app_env)
app_html <- htmltools::renderTags(app_env$ui)$html
stopifnot(
  grepl("startup-new_project", app_html, fixed = TRUE),
  grepl("startup-learning", app_html, fixed = TRUE),
  grepl("startup-function_plotter", app_html, fixed = TRUE),
  grepl("output.learning_ready", app_html, fixed = TRUE),
  grepl("output.function_plotter_ready", app_html, fixed = TRUE),
  grepl("data-value=\"data_workspace\"", app_html, fixed = TRUE),
  !grepl("data-value=\"distributions\">教学模式</a>", app_html, fixed = TRUE),
  !grepl("data-value=\"home\"", app_html, fixed = TRUE)
)

startup_project <- build_easyr_project(
  list(datasets = list("startup.csv" = data.frame(x = 1:3, y = 4:6)), active = "startup.csv")
)
startup_project_file <- tempfile(fileext = ".easyr")
saveRDS(startup_project, startup_project_file)
startup_upload <- data.frame(
  name = "startup.easyr", size = file.info(startup_project_file)$size,
  type = "application/octet-stream", datapath = startup_project_file,
  stringsAsFactors = FALSE
)

testServer(app_env$server, {
  session$flushReact()
  stopifnot(identical(workspace_started(), FALSE), identical(learning_started(), FALSE), identical(function_plotter_started(), FALSE))
  session$setInputs(`startup-function_plotter` = 1)
  session$flushReact()
  stopifnot(identical(workspace_started(), FALSE), identical(learning_started(), FALSE), identical(function_plotter_started(), TRUE))
  session$setInputs(leave_function_plotter = 1)
  session$flushReact()
  stopifnot(identical(function_plotter_started(), FALSE))
  session$setInputs(`startup-learning` = 1)
  session$flushReact()
  stopifnot(identical(workspace_started(), FALSE), identical(learning_started(), TRUE))
  session$setInputs(leave_learning = 1)
  session$flushReact()
  stopifnot(identical(workspace_started(), FALSE), identical(learning_started(), FALSE))
  session$setInputs(`startup-new_project` = 1)
  session$flushReact()
  stopifnot(identical(workspace_started(), TRUE), identical(learning_started(), FALSE))
})

testServer(app_env$server, {
  session$setInputs(`startup-project_file` = startup_upload)
  session$flushReact()
  stopifnot(identical(workspace_started(), TRUE), grepl("startup.csv", output[["import-dataset_status"]]))
})
unlink(startup_project_file)

cat("开始界面、分析项目、独立学习模式、独立函数绘图、示例数据和无首页工作台检查通过。\n")
