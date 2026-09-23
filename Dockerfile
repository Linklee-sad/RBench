FROM rocker/r-ver:latest

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff-dev \
    libjpeg-dev \
  && rm -rf /var/lib/apt/lists/*

RUN install2.r --error --skipinstalled --ncpus -1 \
    shiny readxl DT ggplot2 plotly jsonlite httr2 commonmark \
    randomForest e1071 rpart cluster quantmod tseries

WORKDIR /app
COPY app.R setup.R ./
COPY R ./R
COPY www ./www

ENV PORT=3838 \
    EASYR_HOSTED=1
EXPOSE 3838

CMD ["R", "-e", "options(shiny.launch.browser = FALSE); shiny::runApp('/app', host = '0.0.0.0', port = as.integer(Sys.getenv('PORT', '3838')))" ]
