source("setup.R")
library(shiny)
source("R/directory.R")
source("R/plot_editor.R")
source("R/regression.R")
source("R/distributions.R")
source("R/teaching.R")

examples <- teaching_data_examples()
stopifnot(nrow(examples) == 4L, all(c("example", "answer", "reason") %in% names(examples)))

coin <- simulate_probability_experiment("coin", 10000, probability = .3, seed = 10)
die <- simulate_probability_experiment("die", 12000, sides = 6, target = 6, seed = 11)
stopifnot(abs(coin$observed - .3) < .02, abs(die$observed - 1 / 6) < .02,
  nrow(coin$trace) == 10000L, inherits(build_probability_convergence_plot(coin), "ggplot"))

counting <- calculate_counting_methods(10, 3)
stopifnot(identical(unname(counting$methods$方案数), c(720, 120, 1000, 220)),
  inherits(build_counting_methods_plot(counting), "ggplot"))

rules <- calculate_probability_rules(.4, .7, .3)
stopifnot(abs(rules$p_intersection - .28) < 1e-12, abs(rules$p_b - .46) < 1e-12,
  abs(rules$p_union - .58) < 1e-12, abs(sum(rules$cells$概率) - 1) < 1e-12,
  !rules$independent, inherits(build_probability_rules_plot(rules), "ggplot"))
independent_rules <- calculate_probability_rules(.4, .5, .5)
stopifnot(independent_rules$independent)

bayes <- calculate_bayes_screening(10000, prevalence = .01, sensitivity = .9, specificity = .95)
stopifnot(bayes$true_positive == 90L, bayes$false_positive == 495L,
  bayes$positive_total == 585L, abs(bayes$posterior - 90 / 585) < 1e-12,
  inherits(build_bayes_screening_plot(bayes), "ggplot"))

ci <- simulate_confidence_intervals(5, 2, sample_size = 40, repetitions = 500, confidence = .95, seed = 12)
stopifnot(abs(ci$standard_error - 2 / sqrt(40)) < 1e-12,
  ci$coverage > .91, ci$coverage < .99, nrow(ci$intervals) == 500L,
  inherits(build_confidence_interval_plot(ci), "ggplot"))

null_tests <- simulate_hypothesis_tests(0, 0, 1, sample_size = 30, repetitions = 10000, alpha = .05, seed = 13)
effect_tests <- simulate_hypothesis_tests(0, .5, 1, sample_size = 30, repetitions = 10000, alpha = .05, seed = 14)
stopifnot(null_tests$rejection_rate > .035, null_tests$rejection_rate < .065,
  effect_tests$rejection_rate > .70, effect_tests$rejection_rate > null_tests$rejection_rate,
  inherits(build_hypothesis_test_plot(null_tests), "ggplot"))

invalid_probability <- tryCatch({ simulate_probability_experiment("coin", 10, probability = 2); "unexpected" }, error = conditionMessage)
invalid_ci <- tryCatch({ simulate_confidence_intervals(0, 0, 30, 100, .95); "unexpected" }, error = conditionMessage)
invalid_bayes <- tryCatch({ calculate_bayes_screening(10000, prevalence = 2); "unexpected" }, error = conditionMessage)
invalid_rules <- tryCatch({ calculate_probability_rules(-.1, .5, .5); "unexpected" }, error = conditionMessage)
invalid_counting <- tryCatch({ calculate_counting_methods(5, 6); "unexpected" }, error = conditionMessage)
invalid_test <- tryCatch({ simulate_hypothesis_tests(0, 0, 1, repetitions = 50); "unexpected" }, error = conditionMessage)
stopifnot(grepl("0 到 1", invalid_probability), grepl("大于 0", invalid_ci),
  grepl("0 到 1", invalid_bayes), grepl("0 到 1", invalid_rules), grepl("0 到 n", invalid_counting),
  grepl("100 到 10,000", invalid_test))

ui_html <- as.character(teaching_mode_ui())
stopifnot(grepl("教学模式", ui_html, fixed = TRUE), grepl("概率实验", ui_html, fixed = TRUE),
  grepl("排列组合", ui_html, fixed = TRUE), grepl("概率规则", ui_html, fixed = TRUE),
  grepl("条件概率", ui_html, fixed = TRUE), grepl("抽样与推断", ui_html, fixed = TRUE),
  grepl("假设检验", ui_html, fixed = TRUE), grepl("distributions-distribution", ui_html, fixed = TRUE))

test_directory <- tempfile("easyr-teaching-"); dir.create(test_directory)
testServer(probability_lab_server, args = list(directory = reactive(test_directory)), {
  session$setInputs(experiment = "coin", trials = 1000, probability = .5, seed = 3, run = 1)
  session$flushReact()
  stopifnot(!is.null(result()), result()$trials == 1000L, grepl("理论概率", status()))
})
testServer(sampling_inference_server, args = list(directory = reactive(test_directory)), {
  session$setInputs(population_mean = 2, population_sd = 3, sample_size = 25,
    repetitions = 50, confidence = ".95", seed = 4, run = 1)
  session$flushReact()
  stopifnot(!is.null(result()), result()$sample_size == 25L, grepl("标准误", status()))
})
testServer(probability_rules_server, args = list(directory = reactive(test_directory)), {
  session$setInputs(p_a = .4, p_b_given_a = .7, p_b_given_not_a = .3, calculate = 1)
  session$flushReact()
  stopifnot(!is.null(result()), abs(result()$p_union - .58) < 1e-12, grepl("不独立", status()))
})
testServer(counting_methods_server, args = list(directory = reactive(test_directory)), {
  session$setInputs(n = 10, r = 3, calculate = 1)
  session$flushReact()
  stopifnot(!is.null(result()), result()$methods$方案数[2] == 120, grepl("顺序是否重要", status()))
})
testServer(bayes_lab_server, args = list(directory = reactive(test_directory)), {
  session$setInputs(population = 10000, prevalence = .01, sensitivity = .9, specificity = .95, run = 1)
  session$flushReact()
  stopifnot(!is.null(result()), result()$positive_total == 585L, grepl("15.4%", status(), fixed = TRUE))
})
testServer(hypothesis_lab_server, args = list(directory = reactive(test_directory)), {
  session$setInputs(null_mean = 0, true_mean = .5, population_sd = 1, sample_size = 30,
    repetitions = 1000, alpha = ".05", seed = 5, run = 1)
  session$flushReact()
  stopifnot(!is.null(result()), result()$repetitions == 1000L, grepl("检验功效", status()))
})
unlink(test_directory, recursive = TRUE)

cat("教学模式的数据基础、概率实验、排列组合、概率规则、贝叶斯、分布、推断与假设检验检查通过。\n")
