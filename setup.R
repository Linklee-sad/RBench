local({
  packages <- c("shiny", "readxl", "DT", "ggplot2", "plotly", "jsonlite", "httr2", "commonmark", "randomForest", "e1071", "rpart", "cluster", "quantmod", "tseries")
  # Keep downloaded packages in a writable, version-specific project library.
  version <- paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1], sep = ".")
  project_library <- file.path(".R-library", R.version$platform, version)
  if (dir.exists(project_library)) .libPaths(c(project_library, .libPaths()))
  missing_packages <- function() {
    packages[!vapply(packages, function(package) {
      suppressMessages(suppressWarnings(requireNamespace(package, quietly = TRUE)))
    }, logical(1))]
  }
  missing <- missing_packages()
  if (length(missing)) {
    message("首次启动需要安装：", paste(missing, collapse = ", "),
            "。请保持联网，安装进度会显示在 Console 中，完成后自动继续。")
    dir.create(project_library, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(project_library) || file.access(project_library, 2) != 0) {
      stop("无法创建依赖目录。请将项目放到可写入的文件夹后，再点击 Run App。", call. = FALSE)
    }
    .libPaths(c(project_library, .libPaths()))
    old_options <- options(timeout = max(300, getOption("timeout", 60)))
    tryCatch({
      tryCatch(
        install.packages(missing, lib = project_library, repos = "https://cloud.r-project.org"),
        error = function(e) message("安装过程中出现问题：", conditionMessage(e))
      )
    }, finally = options(old_options))
    remaining <- missing_packages()
    if (length(remaining)) {
      stop("依赖尚未安装成功：", paste(remaining, collapse = ", "),
           "。请检查网络及上方 Console 安装信息，解决后再次点击 Run App；已安装的依赖会保留。",
           call. = FALSE)
    }
  }
  message("依赖已就绪。")
})
