fill_with_statistic <- function(x, statistic) {
  if (!is.numeric(x)) return(x)
  usable <- x[is.finite(x)]
  if (!length(usable)) return(x)
  replacement <- switch(statistic, mean = mean(usable), median = median(usable))
  x[is.na(x)] <- replacement
  x
}

fill_with_mode <- function(x) {
  usable <- x[!is.na(x)]
  if (!length(usable)) return(x)
  candidates <- unique(usable)
  counts <- vapply(seq_along(candidates), function(i) sum(usable == candidates[i], na.rm = TRUE), integer(1))
  x[is.na(x)] <- candidates[which.max(counts)]
  x
}

fill_adjacent <- function(x, direction = "forward") {
  if (length(x) < 2L) return(x)
  indices <- if (direction == "forward") seq_along(x) else rev(seq_along(x))
  previous <- NULL
  for (i in indices) {
    if (is.na(x[i])) {
      if (!is.null(previous)) x[i] <- previous
    } else {
      previous <- x[i]
    }
  }
  x
}

fill_linear <- function(x) {
  if (!is.numeric(x)) return(x)
  known <- which(is.finite(x))
  missing <- which(is.na(x))
  if (length(known) < 2L || !length(missing)) return(x)
  interpolated <- approx(known, x[known], xout = missing, rule = 1)$y
  fillable <- !is.na(interpolated)
  x[missing[fillable]] <- interpolated[fillable]
  x
}

clean_table <- function(data, columns, missing = "keep", deduplicate = FALSE) {
  clean_table_by_column(data, columns, missing, character(), deduplicate)
}

clean_table_by_column <- function(data, columns, missing = "keep", missing_rules = character(), deduplicate = FALSE) {
  if (!length(columns)) stop("请至少选择一个字段。")
  result <- data[, as.integer(columns), drop = FALSE]
  valid_methods <- c("keep", "drop", "mean", "median", "mode", "zero", "forward", "backward", "linear")
  if (!length(missing) || !missing %in% valid_methods) stop("请选择有效的缺失值处理方式。")
  if (length(missing_rules)) {
    if (is.null(names(missing_rules)) || any(!nzchar(names(missing_rules))) || any(!missing_rules %in% valid_methods)) {
      stop("字段缺失值规则无效。", call. = FALSE)
    }
  }
  methods <- setNames(rep(missing, ncol(result)), names(result))
  applicable <- intersect(names(missing_rules), names(result))
  methods[applicable] <- missing_rules[applicable]
  drop_columns <- names(methods)[methods == "drop"]
  if (length(drop_columns)) result <- result[rowSums(is.na(result[, drop_columns, drop = FALSE])) == 0L, , drop = FALSE]
  for (column in names(result)) {
    method <- methods[[column]]
    if (method %in% c("keep", "drop")) next
    if (method %in% c("mean", "median")) result[[column]] <- fill_with_statistic(result[[column]], method)
    if (method == "mode") result[[column]] <- fill_with_mode(result[[column]])
    if (method == "zero" && is.numeric(result[[column]])) result[[column]][is.na(result[[column]])] <- 0
    if (method == "forward") result[[column]] <- fill_adjacent(result[[column]], "forward")
    if (method == "backward") result[[column]] <- fill_adjacent(result[[column]], "backward")
    if (method == "linear") result[[column]] <- fill_linear(result[[column]])
  }
  if (deduplicate) result <- unique(result)
  row.names(result) <- NULL
  result
}

editor_choices <- function(data, include_all = FALSE) {
  indices <- seq_along(data)
  labels <- if (length(indices)) paste0(indices, ". ", names(data)) else character()
  choices <- setNames(as.character(indices), labels)
  if (isTRUE(include_all)) c("全部字段" = "0", choices) else choices
}

editor_scalar <- function(column, value, missing = FALSE) {
  if (isTRUE(missing)) {
    if (inherits(column, "Date")) return(as.Date(NA))
    if (inherits(column, "POSIXt")) return(as.POSIXct(NA_real_, origin = "1970-01-01"))
    if (is.factor(column)) return(factor(NA, levels = levels(column)))
    return(column[NA_integer_][1])
  }
  value <- as.character(value)
  if (inherits(column, "Date")) {
    parsed <- suppressWarnings(as.Date(value))
    if (is.na(parsed)) stop("日期请使用 YYYY-MM-DD 格式。", call. = FALSE)
    return(parsed)
  }
  if (inherits(column, "POSIXt")) {
    zone <- attr(column, "tzone"); if (!length(zone) || !nzchar(zone[1])) zone <- "UTC" else zone <- zone[1]
    parsed <- suppressWarnings(as.POSIXct(value, tz = zone))
    if (is.na(parsed)) stop("日期时间无法识别，请使用 YYYY-MM-DD HH:MM:SS。", call. = FALSE)
    return(parsed)
  }
  if (is.numeric(column)) {
    parsed <- suppressWarnings(as.numeric(value))
    if (!length(parsed) || !is.finite(parsed)) stop("请输入有效的数值。", call. = FALSE)
    if (is.integer(column)) {
      if (parsed != trunc(parsed)) stop("该字段是整数类型，请输入整数。", call. = FALSE)
      parsed <- as.integer(parsed)
    }
    return(parsed)
  }
  if (is.logical(column)) {
    normalized <- tolower(trimws(value))
    if (normalized %in% c("true", "t", "1", "是", "yes")) return(TRUE)
    if (normalized %in% c("false", "f", "0", "否", "no")) return(FALSE)
    stop("逻辑值请输入 TRUE/FALSE、是/否或 1/0。", call. = FALSE)
  }
  value
}

row_condition_mask <- function(data, column, operator, value = "", value2 = "") {
  column <- as.integer(column)
  if (length(column) != 1L || is.na(column) || !column %in% seq_along(data)) stop("请选择条件字段。", call. = FALSE)
  x <- data[[column]]
  if (!operator %in% c("gt", "ge", "lt", "le", "eq", "ne", "contains", "not_contains", "missing", "not_missing", "between")) {
    stop("请选择有效的条件。", call. = FALSE)
  }
  if (operator == "missing") return(is.na(x))
  if (operator == "not_missing") return(!is.na(x))
  if (operator %in% c("contains", "not_contains")) {
    if (!nzchar(value)) stop("请输入要查找的文字。", call. = FALSE)
    mask <- !is.na(x) & grepl(tolower(value), tolower(as.character(x)), fixed = TRUE)
    return(if (operator == "not_contains") !mask & !is.na(x) else mask)
  }
  target <- if (is.numeric(x) && !inherits(x, c("Date", "POSIXt"))) {
    parsed <- suppressWarnings(as.numeric(value))
    if (!length(parsed) || !is.finite(parsed)) stop("请输入有效的数值。", call. = FALSE)
    parsed
  } else editor_scalar(x, value)
  comparable <- if (is.factor(x)) as.character(x) else x
  if (is.factor(x)) target <- as.character(target)
  if (operator %in% c("gt", "ge", "lt", "le", "between") && !(is.numeric(x) || inherits(x, c("Date", "POSIXt")))) {
    stop("大于、小于和区间条件只适用于数值或日期字段。", call. = FALSE)
  }
  mask <- switch(operator,
    gt = comparable > target, ge = comparable >= target, lt = comparable < target,
    le = comparable <= target, eq = comparable == target, ne = comparable != target,
    between = {
      upper <- if (is.numeric(x) && !inherits(x, c("Date", "POSIXt"))) {
        parsed <- suppressWarnings(as.numeric(value2))
        if (!length(parsed) || !is.finite(parsed)) stop("请输入有效的结束值。", call. = FALSE)
        parsed
      } else editor_scalar(x, value2)
      if (upper < target) stop("结束值不能小于起始值。", call. = FALSE)
      comparable >= target & comparable <= upper
    })
  mask[is.na(mask)] <- FALSE
  mask
}

parse_row_spec <- function(spec, n) {
  spec <- gsub("，", ",", trimws(if (is.null(spec) || !length(spec)) "" else spec), fixed = TRUE)
  if (!nzchar(spec)) stop("请输入要删除的行号。", call. = FALSE)
  parts <- trimws(unlist(strsplit(spec, ",", fixed = TRUE)))
  rows <- integer()
  for (part in parts) {
    if (grepl("^[0-9]+[:：-][0-9]+$", part)) {
      bounds <- as.integer(strsplit(part, "[:：-]")[[1]])
      rows <- c(rows, seq(bounds[1], bounds[2]))
    } else if (grepl("^[0-9]+$", part)) rows <- c(rows, as.integer(part))
    else stop(paste0("无法识别行号：", part, "。请使用例如 1,3,10:15。"), call. = FALSE)
  }
  rows <- sort(unique(rows))
  if (!length(rows) || any(rows < 1L | rows > n)) stop(paste0("行号必须在 1 到 ", n, " 之间。"), call. = FALSE)
  rows
}

add_blank_row <- function(data) {
  blank <- lapply(data, function(x) {
    if (inherits(x, "Date")) as.Date(NA)
    else if (inherits(x, "POSIXt")) as.POSIXct(NA_real_, origin = "1970-01-01")
    else if (is.factor(x)) factor(NA, levels = levels(x))
    else x[NA_integer_][1]
  })
  blank <- as.data.frame(blank, stringsAsFactors = FALSE, optional = TRUE); names(blank) <- names(data)
  rbind(data, blank)
}

set_editor_cell <- function(data, row, column, value = "", missing = FALSE) {
  row <- as.integer(row); column <- as.integer(column)
  if (length(row) != 1L || is.na(row) || row < 1L || row > nrow(data)) stop(paste0("行号必须在 1 到 ", nrow(data), " 之间。"), call. = FALSE)
  if (length(column) != 1L || is.na(column) || !column %in% seq_along(data)) stop("请选择要修改的字段。", call. = FALSE)
  x <- data[[column]]; replacement <- editor_scalar(x, value, missing)
  if (is.factor(x) && !isTRUE(missing)) {
    replacement <- as.character(replacement)
    if (!replacement %in% levels(x)) levels(x) <- c(levels(x), replacement)
  }
  x[row] <- replacement; data[[column]] <- x
  data
}

make_editor_column <- function(n, type = "character", value = "", missing = TRUE) {
  prototype <- switch(type, numeric = numeric(), integer = integer(), logical = logical(),
    date = as.Date(character()), character = character(), stop("请选择有效的字段类型。", call. = FALSE))
  replacement <- editor_scalar(prototype, value, missing)
  rep(replacement, n)
}

find_data_rows <- function(data, column = 0L, query, exact = FALSE, case_sensitive = FALSE) {
  if (is.null(query) || !length(query) || !nzchar(query)) stop("请输入查找内容。", call. = FALSE)
  column <- as.integer(column)
  indices <- if (identical(column, 0L)) seq_along(data) else column
  if (!length(indices) || anyNA(indices) || !all(indices %in% seq_along(data))) stop("请选择有效的查找字段。", call. = FALSE)
  matched <- rep(FALSE, nrow(data))
  for (i in indices) {
    values <- as.character(data[[i]]); valid <- !is.na(values)
    if (isTRUE(exact)) {
      if (isTRUE(case_sensitive)) hit <- valid & values == query else hit <- valid & tolower(values) == tolower(query)
    } else if (isTRUE(case_sensitive)) {
      hit <- valid & grepl(query, values, fixed = TRUE)
    } else {
      hit <- valid & grepl(tolower(query), tolower(values), fixed = TRUE)
    }
    matched <- matched | hit
  }
  which(matched)
}

normalize_text_fields <- function(data, columns, trim = TRUE, empty_to_na = TRUE) {
  columns <- as.integer(columns)
  if (!length(columns) || anyNA(columns) || !all(columns %in% seq_along(data))) {
    stop("请选择至少一个文字字段。", call. = FALSE)
  }
  for (column in columns) {
    x <- data[[column]]
    if (!(is.character(x) || is.factor(x))) stop(paste0("字段“", names(data)[column], "”不是文字或类别字段。"), call. = FALSE)
    was_factor <- is.factor(x)
    values <- as.character(x)
    if (isTRUE(trim)) values <- trimws(values)
    if (isTRUE(empty_to_na)) values[!is.na(values) & values == ""] <- NA_character_
    data[[column]] <- if (was_factor) factor(values) else values
  }
  data
}

sort_table_rows <- function(data, column, direction = "ascending", missing_last = TRUE) {
  column <- as.integer(column)
  if (length(column) != 1L || is.na(column) || !column %in% seq_along(data)) stop("请选择排序字段。", call. = FALSE)
  if (!direction %in% c("ascending", "descending")) stop("请选择有效的排序方向。", call. = FALSE)
  order_index <- order(data[[column]], decreasing = identical(direction, "descending"),
    na.last = isTRUE(missing_last), method = "radix")
  result <- data[order_index, , drop = FALSE]
  row.names(result) <- NULL
  result
}

convert_editor_column <- function(x, type) {
  if (!type %in% c("character", "numeric", "integer", "logical", "date", "factor")) {
    stop("请选择有效的目标类型。", call. = FALSE)
  }
  values <- as.character(x)
  blank <- is.na(values) | trimws(values) == ""
  if (type == "character") return(replace(values, blank, NA_character_))
  if (type == "factor") return(factor(replace(values, blank, NA_character_)))
  if (type %in% c("numeric", "integer")) {
    cleaned <- gsub(",", "", trimws(values), fixed = TRUE)
    parsed <- suppressWarnings(as.numeric(cleaned)); invalid <- !blank & !is.finite(parsed)
    if (any(invalid)) stop(paste0("有 ", sum(invalid), " 个值无法转换为数值；请先清理这些内容。"), call. = FALSE)
    if (type == "integer" && any(!blank & parsed != trunc(parsed), na.rm = TRUE)) stop("字段包含小数，不能直接转换为整数。", call. = FALSE)
    return(if (type == "integer") as.integer(parsed) else parsed)
  }
  if (type == "date") {
    parsed <- rep(as.Date(NA), length(values))
    formats <- c("%Y-%m-%d", "%Y/%m/%d", "%Y%m%d", "%d/%m/%Y", "%Y-%m", "%Y/%m")
    for (format in formats) {
      pending <- is.na(parsed) & !blank
      parsed[pending] <- suppressWarnings(as.Date(values[pending], format = format))
    }
    invalid <- !blank & is.na(parsed)
    if (any(invalid)) stop(paste0("有 ", sum(invalid), " 个值无法转换为日期。"), call. = FALSE)
    return(parsed)
  }
  normalized <- tolower(trimws(values))
  true_values <- c("true", "t", "1", "是", "yes", "y")
  false_values <- c("false", "f", "0", "否", "no", "n")
  invalid <- !blank & !normalized %in% c(true_values, false_values)
  if (any(invalid)) stop(paste0("有 ", sum(invalid), " 个值无法转换为逻辑值。"), call. = FALSE)
  result <- rep(NA, length(values)); result[normalized %in% true_values] <- TRUE; result[normalized %in% false_values] <- FALSE
  result
}

bootstrap_expand_rows <- function(data, target_size, seed = 2026L, shuffle = FALSE) {
  if (!is.data.frame(data) || ncol(data) < 1L) stop("当前数据必须是至少包含一个字段的数据表。", call. = FALSE)
  current_size <- nrow(data)
  if (current_size < 1L) stop("当前数据没有可供抽样的行。", call. = FALSE)
  target_numeric <- suppressWarnings(as.numeric(target_size))
  if (length(target_numeric) != 1L || !is.finite(target_numeric) || target_numeric != floor(target_numeric)) {
    stop("目标行数必须是整数。", call. = FALSE)
  }
  if (target_numeric > 5000000 || target_numeric * ncol(data) > 50000000) {
    stop("扩充结果最多为 5,000,000 行且不超过 50,000,000 个单元格。", call. = FALSE)
  }
  target_size <- as.integer(target_numeric)
  if (target_size <= current_size) stop(paste0("目标行数必须大于当前的 ", current_size, " 行。"), call. = FALSE)
  seed_numeric <- suppressWarnings(as.numeric(seed))
  if (length(seed_numeric) != 1L || !is.finite(seed_numeric) || seed_numeric != floor(seed_numeric) ||
      seed_numeric < 0 || seed_numeric > .Machine$integer.max) {
    stop("随机种子必须是 0 到 2,147,483,647 之间的整数。", call. = FALSE)
  }
  set.seed(as.integer(seed_numeric))
  added_indices <- sample.int(current_size, target_size - current_size, replace = TRUE)
  result <- rbind(data, data[added_indices, , drop = FALSE])
  if (isTRUE(shuffle)) result <- result[sample.int(nrow(result)), , drop = FALSE]
  row.names(result) <- NULL
  result
}

workbench_ui <- function(id) {
  ns <- NS(id)
  section <- function(title, icon_name, ...) {
    tags$div(class = "clean-section",
      tags$div(class = "clean-card-title", icon(icon_name), tags$span(title)), ...)
  }
  tags$div(class = "clean-workbench",
    tags$div(class = "clean-heading", h3("02 整理数据"),
      p(class = "clean-subtitle", "按“清洗 → 筛选排序 → 编辑 → 导出”的顺序处理当前数据集。")),
    tags$div(class = "clean-overview",
      tags$div(class = "clean-overview-title", icon("table"), "当前处理结果"),
      textOutput(ns("data_quality"))
    ),
    tags$div(class = "clean-stage-tabs", tabsetPanel(id = ns("clean_stage"), type = "pills",
      tabPanel("① 清洗", value = "clean",
        section("选择字段与缺失值", "broom",
          p(class = "clean-tool-hint", "这些规则会立即应用；下方预览和所有分析模块同步更新。"),
          selectizeInput(ns("columns"), "保留字段", choices = NULL, multiple = TRUE),
          selectInput(ns("missing"), "默认缺失值处理", c(
            "保留原样" = "keep", "删除含缺失值的行" = "drop", "用均值填充数值字段" = "mean",
            "用中位数填充数值字段" = "median", "用众数填充各字段" = "mode", "用 0 填充数值字段" = "zero",
            "向前填充（使用上一行）" = "forward", "向后填充（使用下一行）" = "backward", "线性插值（数值字段）" = "linear"
          )),
          checkboxInput(ns("deduplicate"), "删除完全重复的行", FALSE),
          tags$div(class = "clean-divider"),
          tags$div(class = "clean-subsection-title", "按字段单独设置"),
          p(class = "clean-tool-hint", "单列规则会覆盖上面的默认处理方式。例如，收入用中位数，行业用众数，备注保留原样。"),
          selectInput(ns("missing_column"), "字段", choices = NULL),
          selectInput(ns("missing_column_method"), "这个字段的处理方式", c(
            "保留原样" = "keep", "删除该字段为空的行" = "drop", "用均值填充" = "mean",
            "用中位数填充" = "median", "用众数填充" = "mode", "用 0 填充" = "zero",
            "向前填充" = "forward", "向后填充" = "backward", "线性插值" = "linear"
          )),
          actionButton(ns("save_missing_rule"), "保存此字段规则", icon = icon("plus"), class = "btn-primary clean-action"),
          tags$div(class = "clean-rule-list", textOutput(ns("missing_rules_status"))),
          actionButton(ns("clear_missing_rules"), "清除全部单列规则", icon = icon("eraser"), class = "btn-link clean-action")
        ),
        section("清理文字内容", "font",
          p(class = "clean-tool-hint", "适合处理 CSV 中多余空格和看似空白的单元格。"),
          selectizeInput(ns("text_columns"), "文字字段（可多选）", choices = NULL, multiple = TRUE),
          checkboxInput(ns("trim_text"), "去除文字首尾空格", TRUE),
          checkboxInput(ns("empty_to_na"), "把空字符串转换为缺失值 NA", TRUE),
          actionButton(ns("clean_text"), "应用文字清理", icon = icon("wand-magic-sparkles"), class = "btn-primary clean-action")
        ),
        section("Bootstrap 扩充样本", "copy",
          p(class = "clean-tool-hint", "保留当前全部数据，再从现有行中有放回抽样，补足到目标行数。新增行会包含重复观测，不代表获得了新的独立信息。"),
          numericInput(ns("bootstrap_target"), "目标总行数", 300, min = 2, max = 5000000, step = 100),
          numericInput(ns("bootstrap_seed"), "随机种子", 2026, min = 0, max = .Machine$integer.max, step = 1),
          checkboxInput(ns("bootstrap_shuffle"), "完成后随机打乱全部行", FALSE),
          actionButton(ns("bootstrap_expand"), "执行 Bootstrap 扩充", icon = icon("copy"), class = "btn-primary clean-action")
        )
      ),
      tabPanel("② 筛选排序", value = "filter",
        section("按条件筛选行", "filter",
          p(class = "clean-tool-hint", "先选择字段和条件，再决定删除匹配行或只保留匹配行。"),
          selectInput(ns("condition_column"), "条件字段", choices = NULL),
          selectInput(ns("condition_operator"), "条件", c("大于" = "gt", "大于等于" = "ge", "小于" = "lt", "小于等于" = "le",
            "等于" = "eq", "不等于" = "ne", "包含文字" = "contains", "不包含文字" = "not_contains",
            "为空" = "missing", "不为空" = "not_missing", "介于两个值之间" = "between")),
          textInput(ns("condition_value"), "比较值 / 起始值"),
          textInput(ns("condition_value2"), "结束值（仅区间条件）"),
          selectInput(ns("condition_action"), "处理方式", c("删除符合条件的行" = "delete", "只保留符合条件的行" = "keep")),
          actionButton(ns("apply_condition"), "执行筛选", icon = icon("check"), class = "btn-primary clean-action")
        ),
        section("数据排序", "arrow-down-wide-short",
          selectInput(ns("sort_column"), "排序字段", choices = NULL),
          selectInput(ns("sort_direction"), "排序方向", c("升序" = "ascending", "降序" = "descending")),
          checkboxInput(ns("sort_missing_last"), "缺失值排在最后", TRUE),
          actionButton(ns("sort_rows"), "应用排序", icon = icon("arrow-down-wide-short"), class = "clean-action")
        )
      ),
      tabPanel("③ 编辑", value = "edit",
        section("字段操作", "columns",
          selectInput(ns("column_target"), "当前字段", choices = NULL),
          textInput(ns("rename_column_value"), "新的字段名"),
          tags$div(class = "clean-button-row",
            actionButton(ns("rename_column"), "重命名", icon = icon("pen")),
            actionButton(ns("delete_column"), "删除字段", icon = icon("trash"), class = "btn-danger")
          ),
          tags$div(class = "clean-divider"),
          selectInput(ns("convert_column_type"), "转换字段类型", c("文字" = "character", "数值" = "numeric", "整数" = "integer",
            "逻辑值" = "logical", "日期" = "date", "类别" = "factor")),
          actionButton(ns("convert_column"), "转换当前字段", icon = icon("right-left"), class = "clean-action"),
          tags$div(class = "clean-divider"),
          textInput(ns("new_column_name"), "新增字段名"),
          selectInput(ns("new_column_type"), "新增字段类型", c("文字" = "character", "数值" = "numeric", "整数" = "integer", "逻辑值" = "logical", "日期" = "date")),
          textInput(ns("new_column_value"), "默认值"), checkboxInput(ns("new_column_missing"), "默认填为缺失值 NA", TRUE),
          actionButton(ns("add_column"), "增加字段", icon = icon("plus"), class = "btn-primary clean-action")
        ),
        section("精确修改行和单元格", "pen-to-square",
          p(class = "clean-tool-hint", "适合少量修正；大批量删除建议使用条件筛选。"),
          numericInput(ns("cell_row"), "要修改的行号", 1, min = 1, step = 1),
          selectInput(ns("cell_column"), "要修改的字段", choices = NULL),
          textInput(ns("cell_value"), "新值"), checkboxInput(ns("cell_missing"), "设为缺失值 NA", FALSE),
          actionButton(ns("update_cell"), "修改单元格", icon = icon("pen"), class = "btn-primary clean-action"),
          tags$div(class = "clean-divider"),
          textInput(ns("delete_rows_spec"), "删除指定行", placeholder = "例如：1,3,10:15"),
          tags$div(class = "clean-button-row",
            actionButton(ns("add_row"), "增加空白行", icon = icon("plus")),
            actionButton(ns("delete_rows"), "删除指定行", icon = icon("trash"), class = "btn-danger")
          )
        )
      ),
      tabPanel("④ 查找导出", value = "export",
        section("查找数据", "magnifying-glass",
          p(class = "clean-tool-hint", "在指定字段或全部数据中查找，并返回当前处理结果中的行号。"),
          selectInput(ns("search_column"), "查找字段", choices = NULL), textInput(ns("search_query"), "查找内容"),
          checkboxInput(ns("search_exact"), "完全匹配", FALSE), checkboxInput(ns("search_case"), "区分大小写", FALSE),
          actionButton(ns("search"), "查找", icon = icon("magnifying-glass"), class = "btn-primary clean-action"),
          tags$div(class = "clean-search-result", textOutput(ns("search_status")))
        ),
        section("导出处理结果", "file-export",
          p(class = "clean-tool-hint", "导出的内容与数据预览和分析模块当前使用的数据完全一致。"),
          tags$div(class = "clean-export-card", downloadButton(ns("csv"), "下载处理后的 CSV"),
            actionButton(ns("save_csv"), "保存 CSV 到项目文件夹", icon = icon("floppy-disk"), class = "clean-action")),
          tags$div(class = "clean-tool-hint", textOutput(ns("saved_csv")))
        )
      )
    )),
    tags$div(class = "clean-status", icon("circle-info"), textOutput(ns("edit_status"))),
    tags$div(class = "clean-button-row clean-history-actions",
      actionButton(ns("undo"), "撤销上一步", icon = icon("rotate-left")),
      actionButton(ns("reset"), "恢复导入版本", icon = icon("clock-rotate-left"), class = "btn-warning")
    ),
    p(class = "clean-help", "筛选、排序和编辑操作会保存为当前数据集的工作版本。可逐步撤销，也可恢复最初导入的数据。")
  )
}

workbench_server <- function(id, original, directory = reactive(getwd()), dataset_id = reactive("default"), project_channel = NULL) {
  moduleServer(id, function(input, output, session) {
    configurations <- reactiveVal(list())
    working_versions <- reactiveVal(list())
    edit_histories <- reactiveVal(list())
    column_missing_rules <- reactiveVal(list())
    last_id <- reactiveVal(NULL)
    edit_status <- reactiveVal("尚未执行筛选或编辑操作。")
    search_status <- reactiveVal("")
    current_missing_rules <- reactive({
      id <- dataset_id(); all_rules <- column_missing_rules()
      if (!length(id) || is.null(all_rules[[id]])) character() else all_rules[[id]]
    })
    clear_current_missing_rules <- function() {
      id <- dataset_id(); if (!length(id)) return()
      all_rules <- column_missing_rules(); all_rules[[id]] <- NULL; column_missing_rules(all_rules)
    }
    base_data <- reactive({
      req(original())
      id <- dataset_id()
      versions <- working_versions()
      if (length(id) && !is.null(versions[[id]])) versions[[id]] else original()
    })
    capture_config <- function(id) {
      if (!length(id) || is.null(input$columns)) return()
      values <- configurations()
      values[[id]] <- list(columns = input$columns, missing = if (is.null(input$missing)) "keep" else input$missing,
        deduplicate = isTRUE(input$deduplicate))
      configurations(values)
    }
    restore_config <- function(id, force_default = FALSE, d = base_data()) {
      req(d)
      choices <- setNames(as.character(seq_along(d)), paste0(seq_along(d), ". ", names(d)))
      saved <- if (!force_default) configurations()[[id]] else NULL
      selected <- if (is.null(saved)) unname(choices) else intersect(saved$columns, unname(choices))
      if (!length(selected)) selected <- unname(choices)
      freezeReactiveValue(input, "columns"); freezeReactiveValue(input, "missing"); freezeReactiveValue(input, "deduplicate")
      updateSelectizeInput(session, "columns", choices = choices, selected = selected)
      updateSelectInput(session, "missing", selected = if (is.null(saved)) "keep" else saved$missing)
      updateCheckboxInput(session, "deduplicate", value = if (is.null(saved)) FALSE else saved$deduplicate)
    }
    commit_edit <- function(value, message, previous = isolate(data())) {
      id <- dataset_id(); req(length(id) == 1L, is.data.frame(value), ncol(value) > 0L)
      row.names(value) <- NULL
      histories <- edit_histories(); stack <- histories[[id]]
      if (is.null(stack)) stack <- list()
      stack[[length(stack) + 1L]] <- previous
      if (length(stack) > 30L) stack <- tail(stack, 30L)
      histories[[id]] <- stack; edit_histories(histories)
      versions <- working_versions(); versions[[id]] <- value; working_versions(versions)
      clear_current_missing_rules()
      configs <- configurations(); configs[[id]] <- NULL; configurations(configs)
      restore_config(id, force_default = TRUE, d = value)
      search_status(""); edit_status(message)
      invisible(value)
    }
    observeEvent(list(dataset_id(), original()), {
      new_id <- dataset_id()
      old_id <- last_id()
      if (length(old_id) && !identical(old_id, new_id)) capture_config(old_id)
      if (!length(new_id) || is.null(original())) { last_id(NULL); return() }
      restore_config(new_id)
      last_id(new_id)
      edit_status(if (is.null(working_versions()[[new_id]])) "尚未执行筛选或编辑操作。" else "已恢复当前数据集的工作版本。")
      search_status("")
    }, ignoreNULL = FALSE)
    observeEvent(list(input$columns, input$missing, input$deduplicate), {
      if (length(last_id())) capture_config(last_id())
    }, ignoreInit = TRUE)
    observeEvent(input$reset, {
      req(dataset_id(), original())
      values <- configurations(); values[[dataset_id()]] <- NULL; configurations(values)
      versions <- working_versions(); versions[[dataset_id()]] <- NULL; working_versions(versions)
      histories <- edit_histories(); histories[[dataset_id()]] <- NULL; edit_histories(histories)
      clear_current_missing_rules()
      restore_config(dataset_id(), force_default = TRUE, d = original())
      edit_status("已恢复当前数据集最初导入的内容。")
      search_status("")
    })
    observeEvent(input$undo, {
      id <- dataset_id(); req(length(id) == 1L)
      histories <- edit_histories(); stack <- histories[[id]]
      if (!length(stack)) {
        edit_status("没有可以撤销的操作。")
        return()
      }
      previous <- stack[[length(stack)]]; stack[[length(stack)]] <- NULL
      histories[[id]] <- if (length(stack)) stack else NULL; edit_histories(histories)
      versions <- working_versions(); versions[[id]] <- previous; working_versions(versions)
      clear_current_missing_rules()
      configs <- configurations(); configs[[id]] <- NULL; configurations(configs)
      restore_config(id, force_default = TRUE, d = previous)
      search_status(""); edit_status(paste0("已撤销上一步；当前为 ", nrow(previous), " 行 × ", ncol(previous), " 列。"))
    })
    data <- reactive({
      source <- base_data(); req(source)
      validate(need(length(input$columns) > 0, "请在左侧至少选择一个字段。"))
      indices <- as.integer(input$columns)
      req(all(indices %in% seq_along(source)))
      clean_table_by_column(source, indices, input$missing, current_missing_rules(), isTRUE(input$deduplicate))
    })
    output$data_quality <- renderText({
      d <- data()
      paste0(nrow(d), " 行 · ", ncol(d), " 列 · ", sum(is.na(d)), " 个缺失值 · ", sum(duplicated(d)), " 个重复行")
    })
    observeEvent(data(), {
      d <- data(); choices <- editor_choices(d); search_choices <- editor_choices(d, include_all = TRUE)
      selected <- function(old, choices) if (length(old) == 1L && old %in% unname(choices)) old else unname(head(choices, 1))
      updateSelectInput(session, "condition_column", choices = choices, selected = selected(input$condition_column, choices))
      updateSelectInput(session, "cell_column", choices = choices, selected = selected(input$cell_column, choices))
      updateSelectInput(session, "column_target", choices = choices, selected = selected(input$column_target, choices))
      updateSelectInput(session, "sort_column", choices = choices, selected = selected(input$sort_column, choices))
      missing_choices <- setNames(names(d), paste0(seq_along(d), ". ", names(d)))
      missing_selected <- if (length(input$missing_column) == 1L && input$missing_column %in% unname(missing_choices)) input$missing_column else unname(head(missing_choices, 1))
      updateSelectInput(session, "missing_column", choices = missing_choices, selected = missing_selected)
      text_indices <- which(vapply(d, function(x) is.character(x) || is.factor(x), logical(1)))
      text_choices <- if (length(text_indices)) setNames(as.character(text_indices), paste0(text_indices, ". ", names(d)[text_indices])) else character()
      text_selected <- intersect(input$text_columns, unname(text_choices))
      if (!length(text_selected)) text_selected <- unname(text_choices)
      updateSelectizeInput(session, "text_columns", choices = text_choices, selected = text_selected)
      updateSelectInput(session, "search_column", choices = search_choices,
        selected = if (length(input$search_column) == 1L && input$search_column %in% unname(search_choices)) input$search_column else "0")
      current_row <- if (is.null(input$cell_row) || !length(input$cell_row) || is.na(input$cell_row)) 1L else input$cell_row
      updateNumericInput(session, "cell_row", max = max(1L, nrow(d)), value = min(max(1L, current_row), max(1L, nrow(d))))
      suggested_target <- min(5000000L, max(nrow(d) + 1L, nrow(d) * 2L))
      updateNumericInput(session, "bootstrap_target", min = nrow(d) + 1L, max = 5000000L, value = suggested_target)
    }, ignoreNULL = FALSE)

    output$missing_rules_status <- renderText({
      rules <- current_missing_rules()
      if (!length(rules)) return("尚未设置单列规则，全部字段使用默认处理方式。")
      labels <- c(keep = "保留", drop = "删除空值行", mean = "均值", median = "中位数", mode = "众数",
        zero = "填 0", forward = "向前填充", backward = "向后填充", linear = "线性插值")
      paste0("当前规则：", paste(paste0(names(rules), " → ", unname(labels[rules])), collapse = "；"))
    })
    observeEvent(input$save_missing_rule, {
      tryCatch({
        d <- isolate(data()); column <- input$missing_column; method <- input$missing_column_method
        if (length(column) != 1L || !column %in% names(d)) stop("请选择要单独处理的字段。", call. = FALSE)
        if (method %in% c("mean", "median", "zero", "linear") && !is.numeric(d[[column]])) {
          stop("均值、中位数、0 和线性插值只适用于数值字段。", call. = FALSE)
        }
        id <- dataset_id(); all_rules <- column_missing_rules(); rules <- all_rules[[id]]
        if (is.null(rules)) rules <- character()
        rules[column] <- method; all_rules[[id]] <- rules; column_missing_rules(all_rules)
        edit_status(paste0("已保存“", column, "”的缺失值规则。"))
      }, error = function(e) { edit_status(paste0("规则保存失败：", conditionMessage(e))); showNotification(conditionMessage(e), type = "error") })
    })
    observeEvent(input$clear_missing_rules, {
      clear_current_missing_rules(); edit_status("已清除当前数据集的全部单列缺失值规则。")
    })

    observeEvent(input$clean_text, {
      tryCatch({
        current <- isolate(data())
        result <- normalize_text_fields(current, input$text_columns, isTRUE(input$trim_text), isTRUE(input$empty_to_na))
        commit_edit(result, paste0("文字清理完成：已处理 ", length(input$text_columns), " 个字段。"), current)
      }, error = function(e) { edit_status(paste0("文字清理失败：", conditionMessage(e))); showNotification(conditionMessage(e), type = "error") })
    })

    observeEvent(input$bootstrap_expand, {
      tryCatch({
        current <- isolate(data())
        result <- bootstrap_expand_rows(current, input$bootstrap_target, input$bootstrap_seed,
          isTRUE(input$bootstrap_shuffle))
        added <- nrow(result) - nrow(current)
        commit_edit(result, paste0("Bootstrap 扩充完成：保留原有 ", nrow(current), " 行，新增 ", added,
          " 行；当前共 ", nrow(result), " 行。"), current)
      }, error = function(e) {
        edit_status(paste0("Bootstrap 扩充失败：", conditionMessage(e)))
        showNotification(conditionMessage(e), type = "error", duration = 10)
      })
    })

    observeEvent(input$apply_condition, {
      tryCatch({
        current <- isolate(data())
        mask <- row_condition_mask(current, input$condition_column, input$condition_operator,
          input$condition_value, input$condition_value2)
        matched <- sum(mask)
        result <- if (identical(input$condition_action, "keep")) current[mask, , drop = FALSE] else current[!mask, , drop = FALSE]
        action <- if (identical(input$condition_action, "keep")) paste0("只保留 ", matched, " 行") else paste0("删除 ", matched, " 行")
        commit_edit(result, paste0("条件操作完成：", action, "；当前剩余 ", nrow(result), " 行。"), current)
      }, error = function(e) { edit_status(paste0("条件操作失败：", conditionMessage(e))); showNotification(conditionMessage(e), type = "error") })
    })
    observeEvent(input$sort_rows, {
      tryCatch({
        current <- isolate(data()); result <- sort_table_rows(current, input$sort_column, input$sort_direction, isTRUE(input$sort_missing_last))
        direction <- if (identical(input$sort_direction, "descending")) "降序" else "升序"
        commit_edit(result, paste0("已按“", names(current)[as.integer(input$sort_column)], "”", direction, "排序。"), current)
      }, error = function(e) { edit_status(paste0("排序失败：", conditionMessage(e))); showNotification(conditionMessage(e), type = "error") })
    })
    observeEvent(input$add_row, {
      tryCatch({
        current <- isolate(data()); result <- add_blank_row(current)
        commit_edit(result, paste0("已增加空白行，新行号为 ", nrow(result), "。可使用“修改单元格”填写内容。"), current)
      }, error = function(e) { edit_status(paste0("增加行失败：", conditionMessage(e))); showNotification(conditionMessage(e), type = "error") })
    })
    observeEvent(input$update_cell, {
      tryCatch({
        current <- isolate(data())
        result <- set_editor_cell(current, input$cell_row, input$cell_column, input$cell_value, isTRUE(input$cell_missing))
        commit_edit(result, paste0("已修改第 ", as.integer(input$cell_row), " 行的“", names(current)[as.integer(input$cell_column)], "”字段。"), current)
      }, error = function(e) { edit_status(paste0("修改失败：", conditionMessage(e))); showNotification(conditionMessage(e), type = "error") })
    })
    observeEvent(input$delete_rows, {
      tryCatch({
        current <- isolate(data()); rows <- parse_row_spec(input$delete_rows_spec, nrow(current))
        result <- current[-rows, , drop = FALSE]
        commit_edit(result, paste0("已删除 ", length(rows), " 行；当前剩余 ", nrow(result), " 行。"), current)
      }, error = function(e) { edit_status(paste0("删除行失败：", conditionMessage(e))); showNotification(conditionMessage(e), type = "error") })
    })
    observeEvent(input$rename_column, {
      tryCatch({
        current <- isolate(data()); column <- as.integer(input$column_target)
        if (length(column) != 1L || is.na(column) || !column %in% seq_along(current)) stop("请选择要重命名的字段。", call. = FALSE)
        new_name <- trimws(if (is.null(input$rename_column_value) || !length(input$rename_column_value)) "" else input$rename_column_value)
        if (!nzchar(new_name)) stop("请输入新的字段名。", call. = FALSE)
        if (new_name %in% names(current)[-column]) stop("字段名已经存在，请使用其他名称。", call. = FALSE)
        old_name <- names(current)[column]; names(current)[column] <- new_name
        previous <- isolate(data()); commit_edit(current, paste0("字段“", old_name, "”已重命名为“", new_name, "”。"), previous)
      }, error = function(e) { edit_status(paste0("重命名失败：", conditionMessage(e))); showNotification(conditionMessage(e), type = "error") })
    })
    observeEvent(input$delete_column, {
      tryCatch({
        current <- isolate(data()); column <- as.integer(input$column_target)
        if (ncol(current) <= 1L) stop("数据至少需要保留一个字段。", call. = FALSE)
        if (is.na(column) || !column %in% seq_along(current)) stop("请选择要删除的字段。", call. = FALSE)
        removed <- names(current)[column]; result <- current[, -column, drop = FALSE]
        commit_edit(result, paste0("已删除字段“", removed, "”。"), current)
      }, error = function(e) { edit_status(paste0("删除字段失败：", conditionMessage(e))); showNotification(conditionMessage(e), type = "error") })
    })
    observeEvent(input$convert_column, {
      tryCatch({
        current <- isolate(data()); column <- as.integer(input$column_target)
        if (is.na(column) || !column %in% seq_along(current)) stop("请选择要转换的字段。", call. = FALSE)
        old_type <- class(current[[column]])[1]; current[[column]] <- convert_editor_column(current[[column]], input$convert_column_type)
        commit_edit(current, paste0("字段“", names(current)[column], "”已从 ", old_type, " 转换为 ", class(current[[column]])[1], "。"), isolate(data()))
      }, error = function(e) { edit_status(paste0("类型转换失败：", conditionMessage(e))); showNotification(conditionMessage(e), type = "error") })
    })
    observeEvent(input$add_column, {
      tryCatch({
        current <- isolate(data())
        new_name <- trimws(if (is.null(input$new_column_name) || !length(input$new_column_name)) "" else input$new_column_name)
        if (!nzchar(new_name)) stop("请输入新增字段名。", call. = FALSE)
        if (new_name %in% names(current)) stop("字段名已经存在，请使用其他名称。", call. = FALSE)
        current[[new_name]] <- make_editor_column(nrow(current), input$new_column_type, input$new_column_value, isTRUE(input$new_column_missing))
        commit_edit(current, paste0("已增加字段“", new_name, "”，类型为 ", input$new_column_type, "。"), isolate(data()))
      }, error = function(e) { edit_status(paste0("增加字段失败：", conditionMessage(e))); showNotification(conditionMessage(e), type = "error") })
    })
    observeEvent(input$search, {
      tryCatch({
        current <- isolate(data()); rows <- find_data_rows(current, input$search_column, input$search_query,
          isTRUE(input$search_exact), isTRUE(input$search_case))
        shown <- if (length(rows)) paste(head(rows, 30L), collapse = "、") else "无"
        suffix <- if (length(rows) > 30L) "……（仅显示前 30 个行号）" else ""
        search_status(paste0("找到 ", length(rows), " 行。行号：", shown, suffix))
      }, error = function(e) { search_status(paste0("查找失败：", conditionMessage(e))); showNotification(conditionMessage(e), type = "error") })
    })
    if (!is.null(project_channel)) {
      project_channel$snapshot <- function() {
        if (length(last_id())) capture_config(last_id())
        list(configurations = configurations(), working_versions = working_versions(),
          column_missing_rules = column_missing_rules())
      }
      project_channel$restore <- function(state) {
        if (!is.list(state)) stop("项目中的整理数据状态无效。", call. = FALSE)
        configurations(if (is.list(state$configurations)) state$configurations else list())
        working_versions(if (is.list(state$working_versions)) state$working_versions else list())
        column_missing_rules(if (is.list(state$column_missing_rules)) state$column_missing_rules else list())
        edit_histories(list())
        last_id(NULL)
        edit_status("已恢复项目中的数据整理状态；新的操作可以继续撤销。")
        search_status("")
        invisible(TRUE)
      }
    }
    output$edit_status <- renderText(edit_status())
    output$search_status <- renderText(search_status())
    write_csv <- function(file) {
        value <- data()
        con <- file(file, open = "wb")
        on.exit(close(con))
        writeBin(charToRaw("\ufeff"), con)
        lines <- capture.output(write.csv(value, row.names = FALSE, na = ""))
        writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n"))), con)
    }
    output$csv <- downloadHandler(
      filename = function() paste0("easyr-", Sys.Date(), ".csv"),
      content = write_csv
    )
    saved_csv <- reactiveVal("")
    output$saved_csv <- renderText(saved_csv())
    observeEvent(input$save_csv, {
      req(data())
      tryCatch({
        path <- save_to_workdir(directory(), "easyr-data", ".csv", write_csv)
        saved_csv(paste("上次保存：", path))
        showNotification("CSV 已保存到 RBench 项目文件夹。", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })
    data
  })
}
