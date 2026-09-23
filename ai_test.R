source("setup.R")
library(shiny)
source("R/ai.R")

stopifnot(ai_config_ready(list(api_key = "secret", endpoint = "https://example.com/v1", model = "model")))
stopifnot(!ai_config_ready(list(api_key = "", endpoint = "https://example.com/v1", model = "model")))
stopifnot(ai_config_ready(list(provider = "custom", api_key = "", endpoint = "http://127.0.0.1:11434/v1", model = "local-model")))
stopifnot(identical(ai_clean_endpoint("https://example.com/v1", "chat"), "https://example.com/v1/chat/completions"))
stopifnot(identical(ai_clean_endpoint("https://example.com/v1/responses", "responses"), "https://example.com/v1/responses"))
stopifnot(identical(ai_clean_endpoint("https://generativelanguage.googleapis.com/v1beta/models", "gemini", "gemini-3.8-flash"),
  "https://generativelanguage.googleapis.com/v1beta/models/gemini-3.8-flash:generateContent"))
stopifnot(identical(ai_clean_endpoint("https://api.anthropic.com/v1", "anthropic", "claude-sonnet-5"),
  "https://api.anthropic.com/v1/messages"))
stopifnot(identical(ai_provider_defaults("gemini")$protocol, "gemini"),
  identical(ai_provider_defaults("claude")$protocol, "anthropic"),
  "gemini-3.8-flash" %in% ai_provider_models("gemini"),
  "claude-sonnet-5" %in% ai_provider_models("claude"))
stopifnot(identical(ai_auth_headers("google-key", "gemini")$`x-goog-api-key`, "google-key"),
  identical(ai_auth_headers("claude-key", "anthropic")$`x-api-key`, "claude-key"),
  identical(ai_auth_headers("claude-key", "anthropic")$`anthropic-version`, "2023-06-01"))

local_config_path <- tempfile("easyr-ai-config-", fileext = ".json")
local_value <- list(provider = "custom", model = "local-model", endpoint = "http://127.0.0.1:11434/v1",
  protocol = "chat", api_key = "fake-key", private_note = "must-not-be-saved")
ai_write_local_config(local_value, local_config_path)
local_loaded <- ai_read_local_config(local_config_path)
stopifnot(file.exists(local_config_path), identical(local_loaded$provider, "custom"),
  identical(local_loaded$api_key, "fake-key"), is.null(local_loaded$private_note),
  !grepl("must-not-be-saved", paste(readLines(local_config_path), collapse = ""), fixed = TRUE))
ai_delete_local_config(local_config_path)
stopifnot(!file.exists(local_config_path), is.null(ai_read_local_config(local_config_path)))

old_hosted_option <- getOption("easyr.hosted")
options(easyr.hosted = TRUE)
stopifnot(ai_hosted_mode())
hosted_ui <- as.character(ai_settings_ui("hosted_ai"))
stopifnot(grepl("在线部署模式", hosted_ui, fixed = TRUE),
  !grepl("hosted_ai-save_local", hosted_ui, fixed = TRUE),
  !grepl("hosted_ai-delete_local", hosted_ui, fixed = TRUE))
options(easyr.hosted = old_hosted_option)

d <- data.frame(name = c("甲", "乙"), email = c("a@example.com", "b@example.com"), value = c(1, NA))
summary_only <- ai_dataset_profile(d, FALSE)
with_sample <- ai_dataset_profile(d, TRUE)
configured_sample <- ai_profile_for_config(d, list(include_head = TRUE, head_rows = 1L))
stopifnot(!grepl("a@example.com", summary_only, fixed = TRUE))
stopifnot(!grepl("a@example.com", with_sample, fixed = TRUE))
stopifnot(grepl("<已隐藏敏感值>", with_sample, fixed = TRUE))
stopifnot(grepl('"sample_rows"', configured_sample, fixed = TRUE),
  length(jsonlite::fromJSON(configured_sample)$sample_rows[[1]]) == 1L,
  !grepl('"sample_rows"', ai_profile_for_config(d, list(include_head = FALSE)), fixed = TRUE),
  identical(ai_sample_rows(list(head_rows = 99L)), 10L))
stopifnot(all(c("name", "email") %in% ai_sensitive_columns(d)))

detailed <- ai_analysis_summary(iris)
stopifnot(identical(detailed$rows, 150L), identical(detailed$columns, 5L),
  length(detailed$fields) == 5L,
  all(c("p01", "p05", "q1", "median", "q3", "p95", "p99", "skewness", "excess_kurtosis") %in%
    names(detailed$fields[[1]]$summary)),
  nrow(detailed$correlations$pearson_strongest_pairs) == 6L,
  nrow(detailed$fields[[5]]$top_categories) == 3L)
large_context <- ai_context_json(list(results = data.frame(index = 1:150, value = c(1:149, Inf))))
stopifnot(grepl('"rows_total": 150', large_context, fixed = TRUE),
  grepl('"rows_included": 100', large_context, fixed = TRUE),
  !grepl("Infinity|NaN", large_context))

original_ai_call <- ai_call
captured_report <- new.env(parent = emptyenv())
ai_call <- function(config, system_prompt, user_prompt, max_tokens = 1600L, transport = NULL) {
  captured_report$prompt <- user_prompt
  "# 模拟报告"
}
testServer(ai_report_server, args = list(
  config = reactive(list(provider = "custom", api_key = "", endpoint = "http://localhost/v1", model = "demo", protocol = "chat", language = "zh", max_tokens = 1000L,
    include_head = TRUE, head_rows = 1L)),
  algorithm = "测试算法", local_report = reactive("本地报告"),
  computed_context = reactive(list(sample = list(n = 123L), metrics = data.frame(name = "RMSE", value = 1.25))),
  source_data = reactive(data.frame(email = "hidden@example.com", score = 88))
), {
  session$setInputs(confirm = TRUE, generate = 1)
  session$flushReact()
  stopifnot(grepl("computed_results_json", captured_report$prompt, fixed = TRUE),
    grepl('"n": 123', captured_report$prompt, fixed = TRUE),
    grepl("dataset_profile_with_head", captured_report$prompt, fixed = TRUE),
    grepl('"score": 88', captured_report$prompt, fixed = TRUE),
    !grepl("hidden@example.com", captured_report$prompt, fixed = TRUE),
    grepl("只使用所提供的数据", captured_report$prompt, fixed = TRUE))
})
ai_call <- original_ai_call

captured <- new.env(parent = emptyenv())
fake_transport <- function(endpoint, key, payload, protocol) {
  captured$endpoint <- endpoint; captured$key <- key; captured$payload <- payload; captured$protocol <- protocol
  "模拟成功"
}
config <- list(api_key = "top-secret", endpoint = "https://example.com/v1", model = "demo", protocol = "chat")
answer <- ai_call(config, "system", "user", 321, fake_transport)
stopifnot(identical(answer, "模拟成功"), identical(captured$protocol, "chat"))
stopifnot(identical(captured$endpoint, "https://example.com/v1/chat/completions"))
stopifnot(identical(captured$payload$messages[[2]]$content, "user"))
stopifnot(identical(captured$payload$max_tokens, 321L))

gemini_config <- c(ai_provider_defaults("gemini"), list(provider = "gemini", api_key = "google-key"))
invisible(ai_call(gemini_config, "system", "user", 222, fake_transport))
stopifnot(identical(captured$protocol, "gemini"), grepl("gemini-3.8-flash:generateContent$", captured$endpoint),
  identical(captured$payload$system_instruction$parts[[1]]$text, "system"),
  identical(captured$payload$contents[[1]]$parts[[1]]$text, "user"),
  identical(captured$payload$generationConfig$maxOutputTokens, 222L))

claude_config <- c(ai_provider_defaults("claude"), list(provider = "claude", api_key = "claude-key"))
invisible(ai_call(claude_config, "system", "user", 333, fake_transport))
stopifnot(identical(captured$protocol, "anthropic"), identical(captured$endpoint, "https://api.anthropic.com/v1/messages"),
  identical(captured$payload$system, "system"), identical(captured$payload$messages[[1]]$content, "user"),
  identical(captured$payload$max_tokens, 333L))

settings_path <- tempfile("easyr-ai-settings-", fileext = ".json")
settings_ui <- htmltools::renderTags(ai_settings_ui("ai_settings"))$html
stopifnot(
  grepl("ai-provider-character-stage", settings_ui, fixed = TRUE),
  grepl("ai-characters/chatgpt.png", settings_ui, fixed = TRUE),
  grepl("ai-characters/deepseek.png", settings_ui, fixed = TRUE),
  grepl("ai-characters/gemini.png", settings_ui, fixed = TRUE),
  grepl("ai-characters/claude.png", settings_ui, fixed = TRUE),
  grepl("ai-characters/qwen.png", settings_ui, fixed = TRUE),
  grepl("ai-characters/custom.png", settings_ui, fixed = TRUE),
  !grepl("ai-character-caption", settings_ui, fixed = TRUE),
  !grepl("数据分析助手", settings_ui, fixed = TRUE),
  grepl("ai_settings-provider", settings_ui, fixed = TRUE),
  file.exists("www/ai-characters/chatgpt.png"),
  file.exists("www/ai-characters/deepseek.png"),
  file.exists("www/ai-characters/gemini.png"),
  file.exists("www/ai-characters/claude.png"),
  file.exists("www/ai-characters/qwen.png"),
  file.exists("www/ai-characters/custom.png")
)
testServer(ai_settings_server, args = list(config_path = settings_path), {
  session$setInputs(provider = "custom", model = "third-party-model", api_key = "", endpoint = "http://127.0.0.1:11434/v1", protocol = "chat")
  session$flushReact()
  current <- config()
  stopifnot(identical(current$provider, "custom"), identical(current$model, "third-party-model"),
    identical(current$endpoint, "http://127.0.0.1:11434/v1"), identical(current$protocol, "chat"), identical(current$api_key, ""))
  session$setInputs(include_head = TRUE, head_rows = 7)
  session$flushReact()
  current <- config()
  stopifnot(isTRUE(current$include_head), identical(current$data_mode, "sample"), identical(current$head_rows, 7L))
  session$setInputs(api_key = "saved-key", allow_local_save = TRUE, save_local = 1)
  session$flushReact()
  stopifnot(file.exists(settings_path), identical(ai_read_local_config(settings_path)$api_key, "saved-key"))
  session$setInputs(delete_local = 1)
  session$flushReact()
  stopifnot(!file.exists(settings_path))
})

provider_settings_path <- tempfile("easyr-ai-provider-settings-", fileext = ".json")
testServer(ai_settings_server, args = list(config_path = provider_settings_path), {
  session$setInputs(provider = "gemini", model = "gemini-3.8-flash", api_key = "google-key")
  session$flushReact()
  stopifnot(identical(config()$provider, "gemini"), identical(config()$protocol, "gemini"),
    identical(config()$endpoint, ai_provider_defaults("gemini")$endpoint))
  session$setInputs(provider = "claude", model = "claude-sonnet-5", api_key = "claude-key")
  session$flushReact()
  stopifnot(identical(config()$provider, "claude"), identical(config()$protocol, "anthropic"),
    identical(config()$endpoint, ai_provider_defaults("claude")$endpoint))
})

stopifnot(identical(ai_extract_text(list(output_text = "ok"), "responses"), "ok"))
stopifnot(identical(ai_extract_text(list(choices = list(list(message = list(content = "ok")))), "chat"), "ok"))
stopifnot(identical(ai_extract_text(list(candidates = list(list(content = list(parts = list(list(text = "gemini ok")))))), "gemini"), "gemini ok"))
stopifnot(identical(ai_extract_text(list(content = list(list(type = "text", text = "claude ok"))), "anthropic"), "claude ok"))
parsed <- ai_extract_json("```json\n{\"parameters\":{\"k\":3},\"explanation\":\"测试\"}\n```")
stopifnot(identical(parsed$parameters$k, 3L), identical(parsed$explanation, "测试"))

rendered <- as.character(ai_markdown_html("# 标题\n\n**重点**\n\n| 参数 | 数值 |\n|---|---|\n| K | 3 |\n\n<script>alert(1)</script>\n\n[危险链接](javascript:alert(1))"))
stopifnot(grepl("<h1>标题</h1>", rendered, fixed = TRUE))
stopifnot(grepl("<table>", rendered, fixed = TRUE))
stopifnot(!grepl("<script>", rendered, fixed = TRUE))
stopifnot(!grepl('href="javascript:', rendered, fixed = TRUE))

long_markdown <- paste(vapply(1:6, function(i) paste0("## 第", i, "节\n\n", paste(rep(paste0("第", i, "节内容。"), 180), collapse = "")), character(1)), collapse = "\n\n")
pages <- ai_paginate_markdown(long_markdown, target_chars = 1200)
stopifnot(length(pages) >= 3L, all(vapply(pages, function(x) nzchar(trimws(x)), logical(1))))
stopifnot(sum(grepl("^## 第", unlist(strsplit(pages, "\n")))) == 6L)

pager_test_server <- function(id) moduleServer(id, function(input, output, session) {
  answer <- reactiveVal(long_markdown)
  pager <- ai_pager_server(input, output, answer)
})
testServer(pager_test_server, {
  session$flushReact()
  stopifnot(pager$count() >= 2L, pager$page() == 1L)
  session$setInputs(next_page = 1); session$flushReact()
  stopifnot(pager$page() == 2L)
  session$setInputs(previous_page = 1); session$flushReact()
  stopifnot(pager$page() == 1L)
})

source("R/regression.R")
lm_result <- fit_readable_lm(iris, 1, c(2, 5))
lm_context <- ai_model_context("线性回归", lm_result)
lm_context_json <- ai_context_json(lm_context)
stopifnot(!is.null(lm_context$analysis_sample_summary),
  !is.null(lm_context$fit_statistics$r_squared),
  !is.null(lm_context$coefficients_with_uncertainty),
  !is.null(lm_context$diagnostics$cooks_distance),
  grepl("pearson_strongest_pairs", lm_context_json, fixed = TRUE))

cat("AI 配置、请求协议、数据摘要脱敏、完整计算上下文、Markdown 安全渲染与分页、模拟调用和参数 JSON 解析检查通过。\n")
