source("setup.R")
library(shiny)
source("R/import.R")
source("R/workbench.R")
source("R/project.R")

sample_datasets <- list(
  "sales.csv" = data.frame(date = as.Date("2026-01-01") + 0:2, value = c(10, 20, 30)),
  "groups.csv" = data.frame(id = 1:3, group = factor(c("A", "B", "A")))
)
working <- sample_datasets[[1]]
working$value[2] <- 25
workbench_state <- list(
  configurations = list("sales.csv" = list(columns = c("1", "2"), missing = "keep", deduplicate = FALSE)),
  working_versions = list("sales.csv" = working, "removed.csv" = data.frame(x = 1)),
  column_missing_rules = list("sales.csv" = c(value = "median"))
)
ui_values <- list(
  "regression-outcome" = "2", "regression-predictors" = "1",
  "analysis-bins" = 30, "random_forest-ntree" = 500,
  "random_forest-run" = 1, "regression-ai_report-purpose" = "private",
  "ai_settings-api_key" = "must-not-save"
)
project <- build_easyr_project(list(datasets = sample_datasets, active = "sales.csv"), workbench_state, ui_values)
stopifnot(project$format == "EasyR Project", project$version == 1L,
  identical(names(project$import$datasets), names(sample_datasets)), project$import$active == "sales.csv",
  identical(project$workbench$working_versions[["sales.csv"]], working),
  is.null(project$workbench$working_versions[["removed.csv"]]),
  all(c("regression-outcome", "analysis-bins", "random_forest-ntree") %in% names(project$ui)),
  !any(grepl("api|run|ai_report", names(project$ui), ignore.case = TRUE)),
  !any(vapply(project$ui, identical, logical(1), "must-not-save")))

file <- tempfile(fileext = ".easyr")
saveRDS(project, file, compress = "gzip", version = 3)
restored <- validate_easyr_project(readRDS(file))
stopifnot(identical(restored$import$datasets, sample_datasets), identical(restored$workbench$working_versions[["sales.csv"]], working))
unlink(file)

invalid <- project; invalid$format <- "Unknown"
stopifnot(inherits(try(validate_easyr_project(invalid), silent = TRUE), "try-error"))
future <- project; future$version <- 999L
stopifnot(inherits(try(validate_easyr_project(future), silent = TRUE), "try-error"))

testServer(import_server, {
  session$setInputs(example_dataset = "airpassengers", demo = 1)
  stopifnot(length(datasets()) == 1L,
    identical(names(datasets()), easyr_example_dataset("airpassengers")$name),
    inherits(current_data()$Date, "Date"))
  restore_state(list(datasets = sample_datasets, active = "groups.csv"))
  session$flushReact()
  stopifnot(length(datasets()) == 2L, identical(active(), "groups.csv"), identical(current_data(), sample_datasets[["groups.csv"]]))
})

channel <- new.env(parent = emptyenv())
active <- reactiveVal("sales.csv")
testServer(workbench_server, args = list(original = reactive(sample_datasets[[active()]]), directory = reactive(tempdir()),
  dataset_id = reactive(active()), project_channel = channel), {
  session$setInputs(columns = c("1", "2"), missing = "keep", deduplicate = FALSE)
  session$flushReact()
  state <- channel$snapshot()
  stopifnot(is.list(state$configurations), "sales.csv" %in% names(state$configurations))
  channel$restore(workbench_state)
  session$flushReact()
  stopifnot(identical(working_versions()[["sales.csv"]], working),
    identical(column_missing_rules()[["sales.csv"]], c(value = "median")))
})

app_env <- new.env(parent = globalenv())
sys.source("app.R", envir = app_env)
testServer(app_env$server, {
  session$setInputs(`import-demo` = 1); session$flushReact()
  stopifnot(grepl("1 个数据集", output[["import-dataset_status"]]))
  saved_project <- output[["project-save_project"]]
  stopifnot(file.exists(saved_project), grepl("[.]easyr$", saved_project))
  upload <- data.frame(name = "roundtrip.easyr", size = file.info(saved_project)$size,
    type = "application/octet-stream", datapath = saved_project, stringsAsFactors = FALSE)
  session$setInputs(`import-remove` = 1); session$flushReact()
  stopifnot(grepl("尚未导入", output[["import-dataset_status"]]))
  session$setInputs(`project-project_file` = upload, `project-restore_project` = 1); session$flushReact()
  stopifnot(grepl("1 个数据集", output[["import-dataset_status"]]),
    grepl("恢复完成", output[["project-status"]]))
})

cat("RBench 项目数据集、活动状态、整理版本、缺失值规则、分析参数、隐私过滤、文件往返和完整应用恢复检查通过。\n")
