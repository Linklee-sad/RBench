source("setup.R")
library(shiny)
source("R/directory.R")
source("R/plot_editor.R")
source("R/regression.R")
source("R/distributions.R")

default_parameters <- function(distribution) {
  specs <- distribution_parameter_specs(distribution)
  values <- lapply(specs, `[[`, "value")
  names(values) <- vapply(specs, `[[`, character(1), "id")
  values
}

for (distribution in unname(distribution_catalog())) {
  formula <- distribution_theory_formula(distribution)
  exam_formulas <- distribution_exam_formulas(distribution)
  result <- fit_distribution_demo(distribution, default_parameters(distribution),
    sample_size = 50, repetitions = 10, seed = 2026)
  stopifnot(length(result$sample) == 50, length(result$sample_means) == 10,
    nrow(result$statistics) == 8, grepl("模拟与教学解读", result$report),
    identical(names(formula), c("mean", "variance")), all(nzchar(formula)),
    identical(names(exam_formulas), c("probability", "mean", "variance", "key")),
    all(vapply(exam_formulas, nzchar, logical(1))))
}

normal <- fit_distribution_demo("normal", list(mean = 2, sd = 3), 2000, 100, 17)
stopifnot(identical(unname(normal$theory), c(2, 9)), abs(mean(normal$sample) - 2) < 0.3,
  abs(mean(normal$sample_means) - 2) < 0.1)
basic <- distribution_basic_statistics(normal$sample)
stopifnot(nrow(basic) == 9L, all(c("均值", "方差", "标准差", "四分位距") %in% basic$概念),
  abs(basic$计算结果[basic$概念 == "均值"] - mean(normal$sample)) < 1e-10,
  abs(basic$计算结果[basic$概念 == "方差"] - stats::var(normal$sample)) < 1e-10)
uniform <- fit_distribution_demo("uniform", list(min = -2, max = 4), 100, 10, 1)
stopifnot(isTRUE(all.equal(unname(uniform$theory), c(1, 3))))
cauchy <- fit_distribution_demo("cauchy", list(location = 0, scale = 1), 100, 10, 1)
stopifnot(all(is.na(cauchy$theory)), grepl("理论均值不存在", cauchy$report))
binomial <- fit_distribution_demo("binomial", list(size = 10, prob = 0.25), 500, 20, 3)
stopifnot(identical(unname(binomial$theory), c(2.5, 1.875)), binomial$discrete)
invalid <- tryCatch({ fit_distribution_demo("uniform", list(min = 2, max = 1), 100, 10, 1); "unexpected" }, error = conditionMessage)
stopifnot(grepl("上限", invalid))
too_large <- tryCatch({ fit_distribution_demo("normal", list(mean = 0, sd = 1), 100000, 5000, 1); "unexpected" }, error = conditionMessage)
stopifnot(grepl("5,000,000", too_large))

plots <- list(
  shape = build_distribution_shape_plot(normal),
  discrete = build_distribution_shape_plot(binomial),
  cdf = build_distribution_cdf_plot(normal),
  qq = build_distribution_qq_plot(normal),
  lln = build_distribution_lln_plot(normal),
  means = build_sampling_means_plot(normal),
  center = build_center_spread_plot(normal),
  variance = build_variance_comparison_plot(normal),
  cauchy_means = build_sampling_means_plot(cauchy)
)
stopifnot(all(vapply(plots, inherits, logical(1), "ggplot")))
ui_html <- as.character(distribution_demo_ui("distribution_render_test"))
stopifnot(grepl("shiny:value", ui_html, fixed = TRUE), grepl("typesetPromise", ui_html, fixed = TRUE),
  !any(grepl("\\\\binom", unlist(lapply(unname(distribution_catalog()), distribution_exam_formulas)))))
test_directory <- tempfile("easyr-distributions-")
dir.create(test_directory)
for (name in names(plots)) {
  file <- file.path(test_directory, paste0(name, ".png"))
  ggplot2::ggsave(file, plots[[name]], width = 8, height = 5, dpi = 96)
  stopifnot(file.info(file)$size > 1000)
}

testServer(distribution_demo_server, args = list(directory = reactive(test_directory)), {
  session$setInputs(distribution = "uniform")
  stopifnot(any(grepl("下限 a", as.character(output$parameters), fixed = TRUE)),
    any(grepl("上限 b", as.character(output$parameters), fixed = TRUE)),
    any(grepl("frac{a+b}{2}", as.character(output$theory_formula), fixed = TRUE)),
    any(grepl("frac{1}{b-a}", as.character(output$theory_formula), fixed = TRUE)),
    any(grepl("考试常用关系", as.character(output$theory_formula), fixed = TRUE)))
  session$setInputs(distribution = "normal", sample_size = 100, repetitions = 20, seed = 9,
    param_mean = 1, param_sd = 2)
  session$setInputs(run = 1)
  stopifnot(!is.null(result()), result()$sample_size == 100, result()$repetitions == 20,
    grepl("模拟完成", status()))
  report_file <- output$download_report
  sample_file <- output$download_sample
  means_file <- output$download_means
  stopifnot(file.exists(report_file), file.exists(sample_file), file.exists(means_file),
    nrow(read.csv(sample_file, fileEncoding = "UTF-8-BOM")) == 100,
    nrow(read.csv(means_file, fileEncoding = "UTF-8-BOM")) == 20)
  session$setInputs(seed = 10)
  stopifnot(is.null(result()), grepl("已更新", status()))
})

unlink(test_directory, recursive = TRUE)
cat("18 种分布、理论统计量、随机抽样、大数定律、中心极限定理图形和结果导出检查通过。\n")
