source("setup.R")
library(shiny)
source("R/workbench.R")

sample_data <- data.frame(
  amount = c(1, 5, 10, NA),
  label = c("Alpha", "Beta", "alphabet", NA),
  day = as.Date(c("2026-01-01", "2026-01-02", "2026-01-03", NA)),
  stringsAsFactors = FALSE
)

stopifnot(identical(which(row_condition_mask(sample_data, 1, "gt", "4")), 2:3))
integer_data <- data.frame(value = 1:3)
stopifnot(identical(which(row_condition_mask(integer_data, 1, "gt", "1.5")), 2:3))
stopifnot(identical(which(row_condition_mask(sample_data, 1, "between", "5", "10")), 2:3))
stopifnot(identical(which(row_condition_mask(sample_data, 2, "contains", "alpha")), c(1L, 3L)))
stopifnot(identical(which(row_condition_mask(sample_data, 1, "missing")), 4L))
stopifnot(identical(which(row_condition_mask(sample_data, 3, "ge", "2026-01-02")), 2:3))

stopifnot(identical(parse_row_spec("1,3,5:7", 10), c(1L, 3L, 5L, 6L, 7L)))
stopifnot(inherits(try(parse_row_spec("0,12", 10), silent = TRUE), "try-error"))

changed <- set_editor_cell(sample_data, 2, 1, "7.5")
stopifnot(identical(changed$amount[2], 7.5))
changed <- set_editor_cell(sample_data, 1, 3, "2027-03-04")
stopifnot(inherits(changed$day, "Date"), identical(changed$day[1], as.Date("2027-03-04")))
factor_data <- data.frame(group = factor(c("A", "B")))
factor_data <- set_editor_cell(factor_data, 1, 1, "C")
stopifnot(as.character(factor_data$group[1]) == "C", "C" %in% levels(factor_data$group))

with_blank <- add_blank_row(sample_data)
stopifnot(nrow(with_blank) == 5L, all(vapply(with_blank[5, , drop = FALSE], function(x) is.na(x[1]), logical(1))))
stopifnot(identical(make_editor_column(3, "numeric", "2.5", FALSE), rep(2.5, 3)))
stopifnot(inherits(make_editor_column(2, "date", "2026-05-06", FALSE), "Date"))
stopifnot(all(is.na(make_editor_column(2, "integer", missing = TRUE))))
stopifnot(identical(find_data_rows(sample_data, 0, "alpha"), c(1L, 3L)))
stopifnot(identical(find_data_rows(sample_data, 2, "Beta", exact = TRUE, case_sensitive = TRUE), 2L))
text_data <- data.frame(label = c("  Alpha ", "", NA), group = factor(c(" A ", "B", "B")))
normalized <- normalize_text_fields(text_data, 1:2)
stopifnot(identical(normalized$label, c("Alpha", NA, NA)), identical(as.character(normalized$group), c("A", "B", "B")))
sorted <- sort_table_rows(data.frame(value = c(3, NA, 1, 2)), 1, "descending", TRUE)
stopifnot(identical(sorted$value, c(3, 2, 1, NA)))
stopifnot(identical(convert_editor_column(c("1,200", "3.5", NA), "numeric"), c(1200, 3.5, NA_real_)))
stopifnot(inherits(convert_editor_column(c("2026-01-02", "2026/03/04"), "date"), "Date"))
bootstrap_source <- data.frame(id = 1:4, group = factor(c("A", "B", "A", "B")), day = as.Date("2026-01-01") + 0:3)
bootstrap_a <- bootstrap_expand_rows(bootstrap_source, 10, seed = 42)
bootstrap_b <- bootstrap_expand_rows(bootstrap_source, 10, seed = 42)
stopifnot(nrow(bootstrap_a) == 10L, identical(bootstrap_a, bootstrap_b),
  identical(bootstrap_a[1:4, , drop = FALSE], bootstrap_source),
  all(bootstrap_a$id[5:10] %in% bootstrap_source$id), is.factor(bootstrap_a$group), inherits(bootstrap_a$day, "Date"))
stopifnot(inherits(try(bootstrap_expand_rows(bootstrap_source, 4, 1), silent = TRUE), "try-error"))
missing_data <- data.frame(amount = c(1, NA, 3), group = c("A", NA, "A"), note = c("x", NA, "z"))
column_filled <- clean_table_by_column(missing_data, 1:3, "keep", c(amount = "mean", group = "mode"))
stopifnot(identical(column_filled$amount, c(1, 2, 3)), identical(column_filled$group, c("A", "A", "A")), is.na(column_filled$note[2]))
column_dropped <- clean_table_by_column(missing_data, 1:3, "keep", c(group = "drop"))
stopifnot(nrow(column_dropped) == 2L, identical(column_dropped$amount, c(1, 3)))

module_data <- data.frame(amount = c(1, 5, 10), label = c("a", "b", "c"), stringsAsFactors = FALSE)
testServer(workbench_server, args = list(
  original = reactive(module_data), directory = reactive(tempdir()), dataset_id = reactive("editing-test")
), {
  session$flushReact()
  session$setInputs(columns = c("1", "2"), missing = "keep", deduplicate = FALSE)
  session$setInputs(condition_column = "1", condition_operator = "gt", condition_value = "4",
    condition_value2 = "", condition_action = "delete", apply_condition = 1)
  session$setInputs(columns = c("1", "2"), missing = "keep", deduplicate = FALSE)
  stopifnot(identical(data()$amount, 1), grepl("删除 2 行", edit_status()))

  session$setInputs(undo = 1)
  session$setInputs(columns = c("1", "2"), missing = "keep", deduplicate = FALSE)
  stopifnot(identical(data(), module_data), grepl("已撤销", edit_status()))

  session$setInputs(reset = 1)
  session$setInputs(columns = c("1", "2"), missing = "keep", deduplicate = FALSE)
  stopifnot(identical(data(), module_data))

  session$setInputs(add_row = 1)
  session$setInputs(columns = c("1", "2"), missing = "keep", deduplicate = FALSE)
  stopifnot(nrow(data()) == 4L, all(is.na(data()[4, ])))
  session$setInputs(cell_row = 4, cell_column = "2", cell_value = "new", cell_missing = FALSE, update_cell = 1)
  session$setInputs(columns = c("1", "2"), missing = "keep", deduplicate = FALSE)
  stopifnot(identical(data()$label[4], "new"))
  session$setInputs(delete_rows_spec = "2,4", delete_rows = 1)
  session$setInputs(columns = c("1", "2"), missing = "keep", deduplicate = FALSE)
  stopifnot(identical(data()$amount, c(1, 10)))

  session$setInputs(new_column_name = "score", new_column_type = "numeric", new_column_value = "2.5",
    new_column_missing = FALSE, add_column = 1)
  session$setInputs(columns = c("1", "2", "3"), missing = "keep", deduplicate = FALSE)
  stopifnot(identical(data()$score, c(2.5, 2.5)))
  session$setInputs(column_target = "3", rename_column_value = "rating", rename_column = 1)
  session$setInputs(columns = c("1", "2", "3"), missing = "keep", deduplicate = FALSE)
  stopifnot("rating" %in% names(data()))
  session$setInputs(column_target = "3", delete_column = 1)
  session$setInputs(columns = c("1", "2"), missing = "keep", deduplicate = FALSE)
  stopifnot(identical(names(data()), c("amount", "label")))

  session$setInputs(search_column = "2", search_query = "c", search_exact = TRUE, search_case = FALSE, search = 1)
  stopifnot(grepl("找到 1 行", search_status()))
  session$setInputs(reset = 2)
  session$setInputs(columns = c("1", "2"), missing = "keep", deduplicate = FALSE)
  stopifnot(identical(data(), module_data))

  session$setInputs(bootstrap_target = 8, bootstrap_seed = 7, bootstrap_shuffle = FALSE, bootstrap_expand = 1)
  session$setInputs(columns = c("1", "2"), missing = "keep", deduplicate = FALSE)
  stopifnot(nrow(data()) == 8L, identical(data()[1:3, , drop = FALSE], module_data),
    grepl("新增 5 行", edit_status()))
  session$setInputs(undo = 2)
  session$setInputs(columns = c("1", "2"), missing = "keep", deduplicate = FALSE)
  stopifnot(identical(data(), module_data))
})

testServer(workbench_server, args = list(
  original = reactive(missing_data), directory = reactive(tempdir()), dataset_id = reactive("missing-rules-test")
), {
  session$flushReact()
  session$setInputs(columns = c("1", "2", "3"), missing = "keep", deduplicate = FALSE)
  session$setInputs(missing_column = "amount", missing_column_method = "mean", save_missing_rule = 1)
  session$flushReact()
  stopifnot(identical(data()$amount, c(1, 2, 3)), is.na(data()$group[2]))
  session$setInputs(missing_column = "group", missing_column_method = "mode", save_missing_rule = 2)
  session$flushReact()
  stopifnot(identical(data()$group, c("A", "A", "A")), grepl("amount", output$missing_rules_status))
  session$setInputs(clear_missing_rules = 1)
  session$flushReact()
  stopifnot(is.na(data()$amount[2]), is.na(data()$group[2]))
})

cat("整理数据的缺失值规则、文字清理、Bootstrap 扩充、筛选、排序、类型转换、行列编辑、查找、撤销和恢复检查通过。\n")
