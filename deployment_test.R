manifest <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
stopifnot(
  identical(manifest$metadata$appmode, "shiny"),
  "app.R" %in% names(manifest$files),
  "R/ai.R" %in% names(manifest$files),
  all(c("shiny", "ggplot2", "plotly", "readxl", "DT", "randomForest", "e1071", "quantmod", "tseries") %in% names(manifest$packages))
)

dockerfile <- paste(readLines("Dockerfile", warn = FALSE), collapse = "\n")
blueprint <- paste(readLines("render.yaml", warn = FALSE), collapse = "\n")
readme <- paste(readLines("README.md", warn = FALSE), collapse = "\n")
stopifnot(
  grepl("0.0.0.0", dockerfile, fixed = TRUE),
  grepl("Sys.getenv('PORT'", dockerfile, fixed = TRUE),
  grepl("EASYR_HOSTED=1", dockerfile, fixed = TRUE),
  grepl("runtime: docker", blueprint, fixed = TRUE),
  grepl("healthCheckPath: /", blueprint, fixed = TRUE),
  grepl("render.com/deploy?repo=https://github.com/Linklee-sad/EasyR", readme, fixed = TRUE),
  grepl("Connect Cloud", readme, fixed = TRUE)
)

source("R/ai.R", local = TRUE)
old_hosted <- getOption("easyr.hosted")
options(easyr.hosted = TRUE)
stopifnot(ai_hosted_mode())
options(easyr.hosted = old_hosted)

cat("Docker、Render、Connect Cloud 依赖清单与在线模式安全设置检查通过。\n")
