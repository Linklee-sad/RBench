distribution_catalog <- function() c(
  "正态分布" = "normal", "均匀分布" = "uniform", "指数分布" = "exponential",
  "Gamma 分布" = "gamma", "Beta 分布" = "beta", "卡方分布" = "chisq",
  "t 分布" = "t", "F 分布" = "f", "对数正态分布" = "lognormal",
  "Weibull 分布" = "weibull", "Logistic 分布" = "logistic", "Cauchy 分布" = "cauchy",
  "Bernoulli 分布" = "bernoulli", "二项分布" = "binomial", "Poisson 分布" = "poisson",
  "几何分布" = "geometric", "负二项分布" = "negative_binomial", "超几何分布" = "hypergeometric"
)

distribution_parameter_specs <- function(distribution) {
  item <- function(id, label, value, min = NULL, max = NULL, step = NULL) {
    list(id = id, label = label, value = value, min = min, max = max, step = step)
  }
  switch(distribution,
    normal = list(item("mean", "均值 μ", 0), item("sd", "标准差 σ", 1, 0.0001, NULL, 0.1)),
    uniform = list(item("min", "下限 a", 0), item("max", "上限 b", 1)),
    exponential = list(item("rate", "率参数 λ", 1, 0.0001, NULL, 0.1)),
    gamma = list(item("shape", "形状参数 shape", 2, 0.0001, NULL, 0.1), item("rate", "率参数 rate", 1, 0.0001, NULL, 0.1)),
    beta = list(item("shape1", "形状参数 α", 2, 0.0001, NULL, 0.1), item("shape2", "形状参数 β", 5, 0.0001, NULL, 0.1)),
    chisq = list(item("df", "自由度 df", 5, 0.0001, NULL, 1)),
    t = list(item("df", "自由度 df", 5, 0.0001, NULL, 1)),
    f = list(item("df1", "分子自由度 df1", 5, 0.0001, NULL, 1), item("df2", "分母自由度 df2", 10, 0.0001, NULL, 1)),
    lognormal = list(item("meanlog", "对数均值 meanlog", 0), item("sdlog", "对数标准差 sdlog", 1, 0.0001, NULL, 0.1)),
    weibull = list(item("shape", "形状参数 shape", 2, 0.0001, NULL, 0.1), item("scale", "尺度参数 scale", 1, 0.0001, NULL, 0.1)),
    logistic = list(item("location", "位置参数 location", 0), item("scale", "尺度参数 scale", 1, 0.0001, NULL, 0.1)),
    cauchy = list(item("location", "位置参数 location", 0), item("scale", "尺度参数 scale", 1, 0.0001, NULL, 0.1)),
    bernoulli = list(item("prob", "成功概率 p", 0.5, 0, 1, 0.05)),
    binomial = list(item("size", "试验次数 n", 10, 1, NULL, 1), item("prob", "成功概率 p", 0.5, 0, 1, 0.05)),
    poisson = list(item("lambda", "均值参数 λ", 4, 0.0001, NULL, 0.5)),
    geometric = list(item("prob", "成功概率 p", 0.3, 0.0001, 1, 0.05)),
    negative_binomial = list(item("size", "目标成功次数 size", 5, 0.0001, NULL, 1), item("prob", "成功概率 p", 0.5, 0.0001, 1, 0.05)),
    hypergeometric = list(item("m", "总体成功元素数 m", 20, 1, NULL, 1), item("n", "总体失败元素数 n", 30, 1, NULL, 1), item("k", "抽取数量 k", 10, 1, NULL, 1)),
    stop("不支持的分布。", call. = FALSE)
  )
}

distribution_label <- function(distribution) {
  labels <- names(distribution_catalog()); values <- unname(distribution_catalog())
  labels[match(distribution, values)]
}

distribution_is_discrete <- function(distribution) {
  distribution %in% c("bernoulli", "binomial", "poisson", "geometric", "negative_binomial", "hypergeometric")
}

validate_distribution_parameters <- function(distribution, p) {
  positive <- function(names) if (any(!is.finite(unlist(p[names])) | unlist(p[names]) <= 0)) stop("尺度、率、形状和自由度参数必须大于 0。", call. = FALSE)
  if (distribution == "normal") positive("sd")
  if (distribution == "uniform" && (!is.finite(p$min) || !is.finite(p$max) || p$max <= p$min)) stop("均匀分布的上限必须大于下限。", call. = FALSE)
  if (distribution == "exponential") positive("rate")
  if (distribution == "gamma") positive(c("shape", "rate"))
  if (distribution == "beta") positive(c("shape1", "shape2"))
  if (distribution %in% c("chisq", "t")) positive("df")
  if (distribution == "f") positive(c("df1", "df2"))
  if (distribution == "lognormal") positive("sdlog")
  if (distribution == "weibull") positive(c("shape", "scale"))
  if (distribution %in% c("logistic", "cauchy")) positive("scale")
  if (distribution %in% c("bernoulli", "binomial") && (!is.finite(p$prob) || p$prob < 0 || p$prob > 1)) stop("概率 p 必须在 0 到 1 之间。", call. = FALSE)
  if (distribution %in% c("geometric", "negative_binomial") && (!is.finite(p$prob) || p$prob <= 0 || p$prob > 1)) stop("该分布的概率 p 必须大于 0 且不超过 1。", call. = FALSE)
  if (distribution == "binomial" && (!is.finite(p$size) || p$size < 1 || p$size != floor(p$size))) stop("二项分布的试验次数必须是正整数。", call. = FALSE)
  if (distribution == "poisson") positive("lambda")
  if (distribution == "negative_binomial") positive("size")
  if (distribution == "hypergeometric") {
    if (any(!is.finite(c(p$m, p$n, p$k))) || any(c(p$m, p$n, p$k) < 1) || any(c(p$m, p$n, p$k) != floor(c(p$m, p$n, p$k)))) stop("超几何分布参数必须是正整数。", call. = FALSE)
    if (p$k > p$m + p$n) stop("抽取数量 k 不能超过总体元素数 m + n。", call. = FALSE)
  }
  invisible(TRUE)
}

draw_distribution <- function(distribution, n, p) {
  switch(distribution,
    normal = stats::rnorm(n, p$mean, p$sd), uniform = stats::runif(n, p$min, p$max),
    exponential = stats::rexp(n, p$rate), gamma = stats::rgamma(n, p$shape, rate = p$rate),
    beta = stats::rbeta(n, p$shape1, p$shape2), chisq = stats::rchisq(n, p$df),
    t = stats::rt(n, p$df), f = stats::rf(n, p$df1, p$df2),
    lognormal = stats::rlnorm(n, p$meanlog, p$sdlog), weibull = stats::rweibull(n, p$shape, p$scale),
    logistic = stats::rlogis(n, p$location, p$scale), cauchy = stats::rcauchy(n, p$location, p$scale),
    bernoulli = stats::rbinom(n, 1, p$prob), binomial = stats::rbinom(n, p$size, p$prob),
    poisson = stats::rpois(n, p$lambda), geometric = stats::rgeom(n, p$prob),
    negative_binomial = stats::rnbinom(n, p$size, p$prob),
    hypergeometric = stats::rhyper(n, p$m, p$n, p$k))
}

distribution_theory <- function(distribution, p) {
  values <- switch(distribution,
    normal = c(p$mean, p$sd^2), uniform = c((p$min + p$max) / 2, (p$max - p$min)^2 / 12),
    exponential = c(1 / p$rate, 1 / p$rate^2), gamma = c(p$shape / p$rate, p$shape / p$rate^2),
    beta = c(p$shape1 / (p$shape1 + p$shape2), p$shape1 * p$shape2 / ((p$shape1 + p$shape2)^2 * (p$shape1 + p$shape2 + 1))),
    chisq = c(p$df, 2 * p$df),
    t = c(if (p$df > 1) 0 else NA_real_, if (p$df > 2) p$df / (p$df - 2) else if (p$df > 1) Inf else NA_real_),
    f = c(if (p$df2 > 2) p$df2 / (p$df2 - 2) else NA_real_,
      if (p$df2 > 4) 2 * p$df2^2 * (p$df1 + p$df2 - 2) / (p$df1 * (p$df2 - 2)^2 * (p$df2 - 4)) else if (p$df2 > 2) Inf else NA_real_),
    lognormal = c(exp(p$meanlog + p$sdlog^2 / 2), (exp(p$sdlog^2) - 1) * exp(2 * p$meanlog + p$sdlog^2)),
    weibull = c(p$scale * gamma(1 + 1 / p$shape), p$scale^2 * (gamma(1 + 2 / p$shape) - gamma(1 + 1 / p$shape)^2)),
    logistic = c(p$location, pi^2 * p$scale^2 / 3), cauchy = c(NA_real_, NA_real_),
    bernoulli = c(p$prob, p$prob * (1 - p$prob)), binomial = c(p$size * p$prob, p$size * p$prob * (1 - p$prob)),
    poisson = c(p$lambda, p$lambda), geometric = c((1 - p$prob) / p$prob, (1 - p$prob) / p$prob^2),
    negative_binomial = c(p$size * (1 - p$prob) / p$prob, p$size * (1 - p$prob) / p$prob^2),
    hypergeometric = { total <- p$m + p$n; c(p$k * p$m / total, p$k * p$m / total * p$n / total * (total - p$k) / (total - 1)) })
  names(values) <- c("mean", "variance"); values
}

distribution_theory_formula <- function(distribution) {
  formulas <- switch(distribution,
    normal = c("\\mathrm{E}(X)=\\mu", "\\mathrm{Var}(X)=\\sigma^2"),
    uniform = c("\\mathrm{E}(X)=\\frac{a+b}{2}", "\\mathrm{Var}(X)=\\frac{(b-a)^2}{12}"),
    exponential = c("\\mathrm{E}(X)=\\frac{1}{\\lambda}", "\\mathrm{Var}(X)=\\frac{1}{\\lambda^2}"),
    gamma = c("\\mathrm{E}(X)=\\frac{\\alpha}{\\beta}", "\\mathrm{Var}(X)=\\frac{\\alpha}{\\beta^2}"),
    beta = c("\\mathrm{E}(X)=\\frac{\\alpha}{\\alpha+\\beta}",
      "\\mathrm{Var}(X)=\\frac{\\alpha\\beta}{(\\alpha+\\beta)^2(\\alpha+\\beta+1)}"),
    chisq = c("\\mathrm{E}(X)=\\nu", "\\mathrm{Var}(X)=2\\nu"),
    t = c("\\mathrm{E}(X)=0\\quad(\\nu>1)",
      "\\mathrm{Var}(X)=\\frac{\\nu}{\\nu-2}\\quad(\\nu>2)"),
    f = c("\\mathrm{E}(X)=\\frac{d_2}{d_2-2}\\quad(d_2>2)",
      "\\mathrm{Var}(X)=\\frac{2d_2^2(d_1+d_2-2)}{d_1(d_2-2)^2(d_2-4)}\\quad(d_2>4)"),
    lognormal = c("\\mathrm{E}(X)=e^{\\mu+\\sigma^2/2}",
      "\\mathrm{Var}(X)=(e^{\\sigma^2}-1)e^{2\\mu+\\sigma^2}"),
    weibull = c("\\mathrm{E}(X)=\\lambda\\,\\Gamma(1+1/k)",
      "\\mathrm{Var}(X)=\\lambda^2[\\Gamma(1+2/k)-\\Gamma^2(1+1/k)]"),
    logistic = c("\\mathrm{E}(X)=\\mu", "\\mathrm{Var}(X)=\\frac{\\pi^2s^2}{3}"),
    cauchy = c("\\mathrm{E}(X)\\text{ 不存在}", "\\mathrm{Var}(X)\\text{ 不存在}"),
    bernoulli = c("\\mathrm{E}(X)=p", "\\mathrm{Var}(X)=p(1-p)"),
    binomial = c("\\mathrm{E}(X)=np", "\\mathrm{Var}(X)=np(1-p)"),
    poisson = c("\\mathrm{E}(X)=\\lambda", "\\mathrm{Var}(X)=\\lambda"),
    geometric = c("\\mathrm{E}(X)=\\frac{1-p}{p}", "\\mathrm{Var}(X)=\\frac{1-p}{p^2}"),
    negative_binomial = c("\\mathrm{E}(X)=\\frac{r(1-p)}{p}", "\\mathrm{Var}(X)=\\frac{r(1-p)}{p^2}"),
    hypergeometric = c("\\mathrm{E}(X)=k\\frac{m}{N}",
      "\\mathrm{Var}(X)=k\\frac{m}{N}\\frac{n}{N}\\frac{N-k}{N-1},\\quad N=m+n"),
    stop("不支持的分布。", call. = FALSE)
  )
  names(formulas) <- c("mean", "variance")
  formulas
}

distribution_exam_formulas <- function(distribution) {
  formulas <- switch(distribution,
    normal = list(
      probability = "f(x)=\\frac{1}{\\sigma\\sqrt{2\\pi}}\\exp\\!\\left[-\\frac{(x-\\mu)^2}{2\\sigma^2}\\right],\\quad -\\infty<x<\\infty",
      mean = "\\mathrm{E}(X)=\\mu",
      variance = "\\mathrm{Var}(X)=\\sigma^2",
      key = "Z=\\frac{X-\\mu}{\\sigma}\\sim N(0,1)"),
    uniform = list(
      probability = "f(x)=\\frac{1}{b-a},\\quad a\\le x\\le b",
      mean = "\\mathrm{E}(X)=\\frac{a+b}{2}",
      variance = "\\mathrm{Var}(X)=\\frac{(b-a)^2}{12}",
      key = "F(x)=\\frac{x-a}{b-a},\\quad a\\le x\\le b"),
    exponential = list(
      probability = "f(x)=\\lambda e^{-\\lambda x},\\quad x\\ge0",
      mean = "\\mathrm{E}(X)=\\frac{1}{\\lambda}",
      variance = "\\mathrm{Var}(X)=\\frac{1}{\\lambda^2}",
      key = "P(X>s+t\\mid X>s)=P(X>t)=e^{-\\lambda t}"),
    gamma = list(
      probability = "f(x)=\\frac{\\beta^{\\alpha}}{\\Gamma(\\alpha)}x^{\\alpha-1}e^{-\\beta x},\\quad x>0",
      mean = "\\mathrm{E}(X)=\\frac{\\alpha}{\\beta}",
      variance = "\\mathrm{Var}(X)=\\frac{\\alpha}{\\beta^2}",
      key = "\\Gamma(\\alpha)=\\int_0^\\infty t^{\\alpha-1}e^{-t}\\,dt"),
    beta = list(
      probability = "f(x)=\\frac{x^{\\alpha-1}(1-x)^{\\beta-1}}{B(\\alpha,\\beta)},\\quad 0<x<1",
      mean = "\\mathrm{E}(X)=\\frac{\\alpha}{\\alpha+\\beta}",
      variance = "\\mathrm{Var}(X)=\\frac{\\alpha\\beta}{(\\alpha+\\beta)^2(\\alpha+\\beta+1)}",
      key = "B(\\alpha,\\beta)=\\frac{\\Gamma(\\alpha)\\Gamma(\\beta)}{\\Gamma(\\alpha+\\beta)}"),
    chisq = list(
      probability = "f(x)=\\frac{x^{\\nu/2-1}e^{-x/2}}{2^{\\nu/2}\\Gamma(\\nu/2)},\\quad x>0",
      mean = "\\mathrm{E}(X)=\\nu",
      variance = "\\mathrm{Var}(X)=2\\nu",
      key = "Z_1^2+\\cdots+Z_\\nu^2\\sim\\chi_\\nu^2,\\quad Z_i\\sim N(0,1)"),
    t = list(
      probability = "f(t)=\\frac{\\Gamma((\\nu+1)/2)}{\\sqrt{\\nu\\pi}\\,\\Gamma(\\nu/2)}\\left(1+\\frac{t^2}{\\nu}\\right)^{-(\\nu+1)/2}",
      mean = "\\mathrm{E}(T)=0\\quad(\\nu>1)",
      variance = "\\mathrm{Var}(T)=\\frac{\\nu}{\\nu-2}\\quad(\\nu>2)",
      key = "T=\\frac{Z}{\\sqrt{U/\\nu}},\\quad Z\\sim N(0,1),\\ U\\sim\\chi_\\nu^2"),
    f = list(
      probability = "f(x)=\\frac{\\Gamma((d_1+d_2)/2)}{\\Gamma(d_1/2)\\Gamma(d_2/2)}\\left(\\frac{d_1}{d_2}\\right)^{d_1/2}\\frac{x^{d_1/2-1}}{(1+d_1x/d_2)^{(d_1+d_2)/2}},\\quad x>0",
      mean = "\\mathrm{E}(F)=\\frac{d_2}{d_2-2}\\quad(d_2>2)",
      variance = "\\mathrm{Var}(F)=\\frac{2d_2^2(d_1+d_2-2)}{d_1(d_2-2)^2(d_2-4)}\\quad(d_2>4)",
      key = "X=\\frac{U_1/d_1}{U_2/d_2}\\sim F_{d_1,d_2}\\Rightarrow\\frac1X\\sim F_{d_2,d_1}"),
    lognormal = list(
      probability = "f(x)=\\frac{1}{x\\sigma\\sqrt{2\\pi}}\\exp\\!\\left[-\\frac{(\\ln x-\\mu)^2}{2\\sigma^2}\\right],\\quad x>0",
      mean = "\\mathrm{E}(X)=e^{\\mu+\\sigma^2/2}",
      variance = "\\mathrm{Var}(X)=(e^{\\sigma^2}-1)e^{2\\mu+\\sigma^2}",
      key = "\\ln X\\sim N(\\mu,\\sigma^2)"),
    weibull = list(
      probability = "f(x)=\\frac{k}{\\lambda}(x/\\lambda)^{k-1}e^{-(x/\\lambda)^k},\\quad x\\ge0",
      mean = "\\mathrm{E}(X)=\\lambda\\Gamma(1+1/k)",
      variance = "\\mathrm{Var}(X)=\\lambda^2[\\Gamma(1+2/k)-\\Gamma^2(1+1/k)]",
      key = "P(X>x)=e^{-(x/\\lambda)^k}"),
    logistic = list(
      probability = "f(x)=\\frac{e^{-(x-\\mu)/s}}{s[1+e^{-(x-\\mu)/s}]^2},\\quad -\\infty<x<\\infty",
      mean = "\\mathrm{E}(X)=\\mu",
      variance = "\\mathrm{Var}(X)=\\frac{\\pi^2s^2}{3}",
      key = "F(x)=\\frac{1}{1+e^{-(x-\\mu)/s}}"),
    cauchy = list(
      probability = "f(x)=\\frac{1}{\\pi\\gamma[1+((x-x_0)/\\gamma)^2]},\\quad -\\infty<x<\\infty",
      mean = "\\mathrm{E}(X)\\text{ 不存在}",
      variance = "\\mathrm{Var}(X)\\text{ 不存在}",
      key = "F(x)=\\frac12+\\frac{1}{\\pi}\\arctan\\!\\left(\\frac{x-x_0}{\\gamma}\\right)"),
    bernoulli = list(
      probability = "P(X=x)=p^x(1-p)^{1-x},\\quad x\\in\\{0,1\\}",
      mean = "\\mathrm{E}(X)=p",
      variance = "\\mathrm{Var}(X)=p(1-p)",
      key = "P(X=1)=p,\\quad P(X=0)=1-p"),
    binomial = list(
      probability = "P(X=x)=\\frac{n!}{x!(n-x)!}p^x(1-p)^{n-x},\\quad x=0,1,\\ldots,n",
      mean = "\\mathrm{E}(X)=np",
      variance = "\\mathrm{Var}(X)=np(1-p)",
      key = "X=X_1+\\cdots+X_n,\\quad X_i\\sim\\mathrm{Bernoulli}(p)"),
    poisson = list(
      probability = "P(X=x)=e^{-\\lambda}\\frac{\\lambda^x}{x!},\\quad x=0,1,2,\\ldots",
      mean = "\\mathrm{E}(X)=\\lambda",
      variance = "\\mathrm{Var}(X)=\\lambda",
      key = "X_i\\sim\\mathrm{Pois}(\\lambda_i)\\Rightarrow\\sum_iX_i\\sim\\mathrm{Pois}(\\sum_i\\lambda_i)"),
    geometric = list(
      probability = "P(X=x)=(1-p)^xp,\\quad x=0,1,2,\\ldots",
      mean = "\\mathrm{E}(X)=\\frac{1-p}{p}",
      variance = "\\mathrm{Var}(X)=\\frac{1-p}{p^2}",
      key = "P(X>s+t\\mid X>s)=P(X>t)\\quad\\text{（X 计首次成功前的失败数）}"),
    negative_binomial = list(
      probability = "P(X=x)=\\frac{(x+r-1)!}{x!(r-1)!}p^r(1-p)^x,\\quad x=0,1,2,\\ldots",
      mean = "\\mathrm{E}(X)=\\frac{r(1-p)}{p}",
      variance = "\\mathrm{Var}(X)=\\frac{r(1-p)}{p^2}",
      key = "X\\text{ 计第 }r\\text{ 次成功前的失败次数}"),
    hypergeometric = list(
      probability = "P(X=x)=\\frac{C_m^xC_n^{k-x}}{C_{m+n}^k}",
      mean = "\\mathrm{E}(X)=k\\frac{m}{N},\\quad N=m+n",
      variance = "\\mathrm{Var}(X)=k\\frac{m}{N}\\frac{n}{N}\\frac{N-k}{N-1}",
      key = "\\max(0,k-n)\\le x\\le\\min(k,m)\\quad\\text{（不放回抽样）}"),
    stop("不支持的分布。", call. = FALSE)
  )
  formulas
}

distribution_density <- function(distribution, x, p) {
  switch(distribution,
    normal = stats::dnorm(x, p$mean, p$sd), uniform = stats::dunif(x, p$min, p$max),
    exponential = stats::dexp(x, p$rate), gamma = stats::dgamma(x, p$shape, rate = p$rate),
    beta = stats::dbeta(x, p$shape1, p$shape2), chisq = stats::dchisq(x, p$df),
    t = stats::dt(x, p$df), f = stats::df(x, p$df1, p$df2),
    lognormal = stats::dlnorm(x, p$meanlog, p$sdlog), weibull = stats::dweibull(x, p$shape, p$scale),
    logistic = stats::dlogis(x, p$location, p$scale), cauchy = stats::dcauchy(x, p$location, p$scale),
    bernoulli = stats::dbinom(x, 1, p$prob), binomial = stats::dbinom(x, p$size, p$prob),
    poisson = stats::dpois(x, p$lambda), geometric = stats::dgeom(x, p$prob),
    negative_binomial = stats::dnbinom(x, p$size, p$prob),
    hypergeometric = stats::dhyper(x, p$m, p$n, p$k))
}

distribution_cdf <- function(distribution, x, p) {
  switch(distribution,
    normal = stats::pnorm(x, p$mean, p$sd), uniform = stats::punif(x, p$min, p$max),
    exponential = stats::pexp(x, p$rate), gamma = stats::pgamma(x, p$shape, rate = p$rate),
    beta = stats::pbeta(x, p$shape1, p$shape2), chisq = stats::pchisq(x, p$df),
    t = stats::pt(x, p$df), f = stats::pf(x, p$df1, p$df2),
    lognormal = stats::plnorm(x, p$meanlog, p$sdlog), weibull = stats::pweibull(x, p$shape, p$scale),
    logistic = stats::plogis(x, p$location, p$scale), cauchy = stats::pcauchy(x, p$location, p$scale),
    bernoulli = stats::pbinom(x, 1, p$prob), binomial = stats::pbinom(x, p$size, p$prob),
    poisson = stats::ppois(x, p$lambda), geometric = stats::pgeom(x, p$prob),
    negative_binomial = stats::pnbinom(x, p$size, p$prob),
    hypergeometric = stats::phyper(x, p$m, p$n, p$k))
}

distribution_quantile <- function(distribution, probability, p) {
  switch(distribution,
    normal = stats::qnorm(probability, p$mean, p$sd), uniform = stats::qunif(probability, p$min, p$max),
    exponential = stats::qexp(probability, p$rate), gamma = stats::qgamma(probability, p$shape, rate = p$rate),
    beta = stats::qbeta(probability, p$shape1, p$shape2), chisq = stats::qchisq(probability, p$df),
    t = stats::qt(probability, p$df), f = stats::qf(probability, p$df1, p$df2),
    lognormal = stats::qlnorm(probability, p$meanlog, p$sdlog), weibull = stats::qweibull(probability, p$shape, p$scale),
    logistic = stats::qlogis(probability, p$location, p$scale), cauchy = stats::qcauchy(probability, p$location, p$scale),
    bernoulli = stats::qbinom(probability, 1, p$prob), binomial = stats::qbinom(probability, p$size, p$prob),
    poisson = stats::qpois(probability, p$lambda), geometric = stats::qgeom(probability, p$prob),
    negative_binomial = stats::qnbinom(probability, p$size, p$prob),
    hypergeometric = stats::qhyper(probability, p$m, p$n, p$k))
}

distribution_description <- function(distribution) switch(distribution,
  normal = "正态分布是对称钟形分布，由均值控制位置、标准差控制离散程度。",
  uniform = "均匀分布在给定区间内各位置具有相同密度。",
  exponential = "指数分布常用于描述 Poisson 过程中的等待时间，并具有无记忆性。",
  gamma = "Gamma 分布是正值右偏分布，可用于等待时间、寿命和金额数据。",
  beta = "Beta 分布定义在 0 到 1 之间，常用于概率或比例。",
  chisq = "卡方分布是若干独立标准正态变量平方和的分布，常用于方差推断。",
  t = "t 分布对称但尾部比正态分布更厚，自由度增大时逐渐接近正态分布。",
  f = "F 分布是两个独立卡方变量按自由度标准化后的比值，常用于方差分析。",
  lognormal = "对数正态分布表示取对数后服从正态分布的正值变量。",
  weibull = "Weibull 分布常用于寿命与可靠性分析，形状参数控制风险随时间的变化。",
  logistic = "Logistic 分布与正态分布类似但尾部更厚，并用于 Logistic 模型的误差结构。",
  cauchy = "Cauchy 分布具有极厚尾部，理论均值和方差都不存在，是大数定律失效的经典演示。",
  bernoulli = "Bernoulli 分布描述一次只有成功或失败两种结果的试验。",
  binomial = "二项分布描述固定次数独立 Bernoulli 试验中的成功次数。",
  poisson = "Poisson 分布描述固定时间或空间区间内独立事件的发生次数。",
  geometric = "几何分布描述首次成功之前的失败次数，并具有离散无记忆性。",
  negative_binomial = "负二项分布描述达到指定成功次数之前的失败次数，也常用于过度离散计数数据。",
  hypergeometric = "超几何分布描述有限总体中不放回抽样得到的成功元素数量。")

distribution_value_text <- function(x) {
  if (is.na(x)) "不存在" else if (is.infinite(x)) "无穷大" else format(signif(x, 5), trim = TRUE)
}

distribution_basic_statistics <- function(x) {
  x <- as.numeric(x); x <- x[is.finite(x)]
  if (!length(x)) stop("样本中没有有限数值。", call. = FALSE)
  variance <- if (length(x) > 1L) stats::var(x) else NA_real_
  quartiles <- stats::quantile(x, c(.25, .75), names = FALSE, type = 7)
  data.frame(
    概念 = c("样本量", "均值", "中位数", "方差", "标准差", "第一四分位数", "第三四分位数", "四分位距", "极差"),
    它回答的问题 = c("一共有多少个观测？", "数据的平衡中心在哪里？", "排序后中间的位置在哪里？",
      "数据到均值的平方距离平均有多大？", "数据通常偏离均值多少？", "25% 的数据不超过什么值？",
      "75% 的数据不超过什么值？", "中间 50% 的数据有多宽？", "最大值与最小值相差多少？"),
    计算结果 = c(length(x), mean(x), stats::median(x), variance, sqrt(variance), quartiles[1], quartiles[2],
      stats::IQR(x), diff(range(x))), check.names = FALSE
  )
}

build_center_spread_plot <- function(result) {
  x <- as.numeric(result$sample); x <- x[is.finite(x)]
  limit <- stats::quantile(x, c(.01, .99), names = FALSE, type = 8)
  shown <- x[x >= limit[1] & x <= limit[2]]
  if (length(shown) > 600L) shown <- shown[unique(round(seq(1, length(shown), length.out = 600L)))]
  center <- mean(x); middle <- stats::median(x); spread <- stats::sd(x)
  d <- data.frame(数值 = shown, 行 = 0)
  plot <- ggplot2::ggplot(d, ggplot2::aes(数值, 行)) +
    ggplot2::geom_jitter(height = .16, width = 0, alpha = .35, colour = "#2563eb", size = 1.8) +
    ggplot2::geom_vline(xintercept = middle, colour = "#059669", linetype = "dashed", linewidth = 1) +
    ggplot2::scale_y_continuous(NULL, breaks = NULL) + lm_plot_theme() +
    ggplot2::labs(title = "均值、中位数与数据的离散程度",
      subtitle = "蓝点是样本；橙线是均值；绿虚线是中位数；浅橙区域表示均值 ± 1 个标准差", x = "样本数值")
  if (is.finite(center) && center >= limit[1] && center <= limit[2]) {
    if (is.finite(spread) && spread > 0) {
      plot <- plot + ggplot2::annotate("rect", xmin = max(limit[1], center - spread), xmax = min(limit[2], center + spread),
        ymin = -Inf, ymax = Inf, fill = "#f59e0b", alpha = .12)
    }
    plot <- plot + ggplot2::geom_vline(xintercept = center, colour = "#d97706", linewidth = 1.1)
  } else {
    plot <- plot + ggplot2::labs(caption = "样本均值受到极端值影响，落在当前 1%～99% 显示范围之外。")
  }
  plot
}

build_variance_comparison_plot <- function(result) {
  x <- as.numeric(result$sample); x <- x[is.finite(x)]
  center <- mean(x); spread <- stats::sd(x)
  if (!is.finite(spread) || spread == 0) {
    z <- stats::scale(seq(-1, 1, length.out = max(20L, min(200L, length(x)))))[, 1]
    base_spread <- 1
  } else {
    z <- (x - center) / spread
    if (length(z) > 500L) z <- z[unique(round(seq(1, length(z), length.out = 500L)))]
    base_spread <- spread
  }
  scales <- c(.5, 1, 2)
  d <- do.call(rbind, lapply(scales, function(multiplier) data.frame(
    数值 = center + z * base_spread * multiplier, 行 = 0,
    情景 = factor(paste0("标准差 × ", multiplier), levels = paste0("标准差 × ", scales)))))
  ggplot2::ggplot(d, ggplot2::aes(数值, 行)) +
    ggplot2::geom_jitter(height = .15, width = 0, alpha = .3, colour = "#2563eb", size = 1.5) +
    ggplot2::geom_vline(xintercept = center, colour = "#d97706", linewidth = .9) +
    ggplot2::facet_wrap(~情景, ncol = 1, scales = "free_x") +
    ggplot2::scale_y_continuous(NULL, breaks = NULL) + lm_plot_theme() +
    ggplot2::labs(title = "相同均值，不同方差",
      subtitle = "三组数据的中心保持不变；标准差增大时，数值分布得更分散", x = "样本数值")
}

fit_distribution_demo <- function(distribution, parameters, sample_size = 1000,
                                  repetitions = 500, seed = 2026) {
  if (!distribution %in% unname(distribution_catalog())) stop("请选择有效的概率分布。", call. = FALSE)
  validate_distribution_parameters(distribution, parameters)
  sample_size <- as.integer(sample_size); repetitions <- as.integer(repetitions); seed <- as.integer(seed)
  if (!is.finite(sample_size) || sample_size < 10L || sample_size > 100000L) stop("单次样本量应在 10 到 100,000 之间。", call. = FALSE)
  if (!is.finite(repetitions) || repetitions < 10L || repetitions > 5000L) stop("重复抽样次数应在 10 到 5,000 之间。", call. = FALSE)
  if (sample_size * repetitions > 5000000) stop("样本量 × 重复次数不能超过 5,000,000，请降低其中一个设置。", call. = FALSE)
  if (!is.finite(seed)) stop("随机种子必须是整数。", call. = FALSE)
  set.seed(seed)
  sample <- draw_distribution(distribution, sample_size, parameters)
  repeated <- matrix(draw_distribution(distribution, sample_size * repetitions, parameters), nrow = sample_size)
  sample_means <- colMeans(repeated)
  theory <- distribution_theory(distribution, parameters)
  sample_table <- data.frame(序号 = seq_along(sample), 数值 = sample, check.names = FALSE)
  means_table <- data.frame(重复编号 = seq_along(sample_means), 样本均值 = sample_means, check.names = FALSE)
  statistics <- data.frame(
    指标 = c("均值", "方差", "标准差", "中位数", "最小值", "最大值", "样本均值的均值", "样本均值的方差"),
    理论值 = c(theory["mean"], theory["variance"], sqrt(theory["variance"]), NA, NA, NA,
      theory["mean"], theory["variance"] / sample_size),
    模拟值 = c(mean(sample), stats::var(sample), stats::sd(sample), stats::median(sample), min(sample), max(sample),
      mean(sample_means), stats::var(sample_means)), check.names = FALSE
  )
  label <- distribution_label(distribution)
  mean_note <- if (is.finite(theory["mean"])) {
    paste0("理论均值为 ", distribution_value_text(theory["mean"]), "，本次样本均值为 ", distribution_value_text(mean(sample)), "。")
  } else paste0(label, "的理论均值不存在，因此累计样本均值不一定稳定收敛。")
  variance_note <- if (is.finite(theory["variance"])) {
    paste0("理论方差为 ", distribution_value_text(theory["variance"]), "，本次样本方差为 ", distribution_value_text(stats::var(sample)), "。")
  } else paste0("理论方差", if (is.infinite(theory["variance"])) "为无穷大" else "不存在", "，常规标准误与正态中心极限定理不能直接套用。")
  clt_note <- if (is.finite(theory["mean"]) && is.finite(theory["variance"])) {
    paste0("在独立同分布且方差有限的条件下，样本均值近似服从均值 ", distribution_value_text(theory["mean"]),
      "、方差 ", distribution_value_text(theory["variance"] / sample_size), " 的正态分布。重复抽样结果可与该近似比较。")
  } else "由于理论均值或方差不满足常规条件，本例不叠加样本均值的正态近似曲线；这可用于演示中心极限定理的适用边界。"
  report <- paste(c(
    paste0(label, "：模拟与教学解读"),
    "一、先理解中心和离散程度", "均值描述数据的平衡中心，但容易受到极端值影响；中位数描述排序后的中间位置，通常更稳健。方差是各观测与均值之差平方的平均，标准差是方差的平方根，与原数据单位相同。",
    "二、分布特点", distribution_description(distribution),
    "三、本次设置", paste0("单次样本量：", sample_size, "；重复抽样次数：", repetitions, "；随机种子：", seed, "。"),
    "四、理论与模拟", mean_note, variance_note,
    "五、大数定律", "当理论均值存在且观测独立同分布时，累计样本均值通常会随样本量增加而接近理论均值。短期波动、厚尾和极端值会影响收敛速度。",
    "六、样本均值与中心极限定理", clt_note,
    "七、教学提示", "单次模拟具有随机性。改变随机种子、样本量和分布参数并重复比较，可以观察小样本波动、偏态、厚尾、离散性以及样本均值分布如何变化。模拟接近理论值不等于每次都完全一致。"
  ), collapse = "\n\n")
  list(distribution = distribution, label = label, parameters = parameters, sample = sample,
    sample_means = sample_means, sample_table = sample_table, means_table = means_table,
    statistics = statistics, theory = theory, sample_size = sample_size,
    repetitions = repetitions, report = report, discrete = distribution_is_discrete(distribution))
}

build_distribution_shape_plot <- function(result) {
  x <- result$sample; p <- result$parameters
  if (result$discrete) {
    observed <- as.data.frame(prop.table(table(x)), stringsAsFactors = FALSE)
    names(observed) <- c("数值", "样本比例"); observed$数值 <- as.numeric(as.character(observed$数值))
    lower <- floor(distribution_quantile(result$distribution, 0.0005, p)); upper <- ceiling(distribution_quantile(result$distribution, 0.9995, p))
    lower <- max(0, min(lower, min(x))); upper <- max(upper, max(x))
    support <- if (upper - lower <= 300) lower:upper else unique(round(seq(lower, upper, length.out = 300)))
    theoretical <- data.frame(数值 = support, 理论概率 = distribution_density(result$distribution, support, p))
    return(ggplot2::ggplot(observed, ggplot2::aes(数值, 样本比例)) +
      ggplot2::geom_col(fill = "#93c5fd", colour = "#2563eb", alpha = 0.8) +
      ggplot2::geom_line(data = theoretical, ggplot2::aes(数值, 理论概率), colour = "#d97706", linewidth = 1) +
      ggplot2::geom_point(data = theoretical, ggplot2::aes(数值, 理论概率), colour = "#d97706", size = 2) +
      lm_plot_theme() + ggplot2::labs(title = paste0(result$label, "：样本与理论概率"),
        subtitle = "蓝柱为样本比例；橙线与点为理论概率质量函数", x = "数值", y = "概率"))
  }
  limits <- stats::quantile(x, c(0.002, 0.998), names = FALSE, type = 8)
  if (!all(is.finite(limits)) || limits[1] == limits[2]) limits <- range(x)
  grid <- seq(limits[1], limits[2], length.out = 600)
  theoretical <- data.frame(数值 = grid, 理论密度 = distribution_density(result$distribution, grid, p))
  plot_sample <- x[x >= limits[1] & x <= limits[2]]
  ggplot2::ggplot(data.frame(数值 = plot_sample), ggplot2::aes(数值)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), bins = 35,
      fill = "#93c5fd", colour = "white", alpha = 0.85) +
    ggplot2::geom_line(data = theoretical, ggplot2::aes(数值, 理论密度), colour = "#d97706", linewidth = 1.1) +
    lm_plot_theme() + ggplot2::labs(title = paste0(result$label, "：样本与理论密度"),
      subtitle = "蓝色直方图为模拟样本；橙线为理论概率密度", x = "数值", y = "密度")
}

build_distribution_cdf_plot <- function(result) {
  values <- sort(result$sample); empirical <- seq_along(values) / length(values)
  d <- data.frame(数值 = values, 经验分布 = empirical,
    理论分布 = distribution_cdf(result$distribution, values, result$parameters))
  ggplot2::ggplot(d, ggplot2::aes(数值)) +
    ggplot2::geom_step(ggplot2::aes(y = 经验分布), colour = "#2563eb", linewidth = 0.9) +
    ggplot2::geom_line(ggplot2::aes(y = 理论分布), colour = "#d97706", linewidth = 1) +
    lm_plot_theme() + ggplot2::labs(title = paste0(result$label, "：经验与理论分布函数"),
      subtitle = "蓝线为经验 CDF；橙线为理论 CDF", x = "数值", y = "累计概率")
}

build_distribution_qq_plot <- function(result) {
  probability <- stats::ppoints(length(result$sample))
  theoretical <- distribution_quantile(result$distribution, probability, result$parameters)
  d <- data.frame(理论分位数 = theoretical, 样本分位数 = sort(result$sample))
  finite <- is.finite(d$理论分位数) & is.finite(d$样本分位数); d <- d[finite, , drop = FALSE]
  limits <- range(c(d$理论分位数, d$样本分位数))
  if (limits[1] == limits[2]) limits <- limits + c(-0.5, 0.5)
  ggplot2::ggplot(d, ggplot2::aes(理论分位数, 样本分位数)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "#d97706", linewidth = 0.9) +
    ggplot2::geom_point(colour = "#2563eb", alpha = 0.6, size = 2) +
    ggplot2::coord_equal(xlim = limits, ylim = limits) + lm_plot_theme() +
    ggplot2::labs(title = paste0(result$label, "：理论 Q-Q 图"),
      subtitle = "点接近橙色 45° 线表示样本分位数接近理论分布", x = "理论分位数", y = "样本分位数")
}

build_distribution_lln_plot <- function(result) {
  d <- data.frame(样本量 = seq_along(result$sample), 累计均值 = cumsum(result$sample) / seq_along(result$sample))
  plot <- ggplot2::ggplot(d, ggplot2::aes(样本量, 累计均值)) +
    ggplot2::geom_line(colour = "#2563eb", linewidth = 0.8) + lm_plot_theme() +
    ggplot2::labs(title = paste0(result$label, "：累计均值与大数定律"),
      subtitle = if (is.finite(result$theory["mean"])) "橙色虚线为理论均值" else "该分布的理论均值不存在",
      x = "累计样本量", y = "累计样本均值")
  if (is.finite(result$theory["mean"])) plot <- plot + ggplot2::geom_hline(yintercept = result$theory["mean"], linetype = "dashed", colour = "#d97706", linewidth = 1)
  plot
}

build_sampling_means_plot <- function(result) {
  d <- data.frame(样本均值 = result$sample_means)
  plot <- ggplot2::ggplot(d, ggplot2::aes(样本均值)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), bins = 35,
      fill = "#86efac", colour = "white", alpha = 0.85) + lm_plot_theme() +
    ggplot2::labs(title = paste0(result$label, "：重复抽样的样本均值分布"),
      subtitle = paste0(result$repetitions, " 次重复，每次样本量 ", result$sample_size), x = "样本均值", y = "密度")
  if (is.finite(result$theory["mean"]) && is.finite(result$theory["variance"]) && result$theory["variance"] > 0) {
    limits <- range(result$sample_means); grid <- seq(limits[1], limits[2], length.out = 500)
    normal <- data.frame(样本均值 = grid,
      密度 = stats::dnorm(grid, result$theory["mean"], sqrt(result$theory["variance"] / result$sample_size)))
    plot <- plot + ggplot2::geom_line(data = normal, ggplot2::aes(样本均值, 密度), colour = "#d97706", linewidth = 1.1) +
      ggplot2::labs(subtitle = paste0(result$repetitions, " 次重复，每次样本量 ", result$sample_size, "；橙线为中心极限定理正态近似"))
  } else if (is.finite(result$theory["mean"]) && identical(as.numeric(result$theory["variance"]), 0)) {
    plot <- plot + ggplot2::geom_vline(xintercept = result$theory["mean"], colour = "#d97706", linewidth = 1.1) +
      ggplot2::labs(subtitle = paste0(result$repetitions, " 次重复，每次样本量 ", result$sample_size, "；该参数设置下样本均值固定"))
  }
  plot
}

distribution_demo_ui <- function(id) {
  ns <- NS(id)
  formula_output_id <- ns("theory_formula")
  mathjax_refresh <- sprintf("(function(){
    if(window.__easyRMathRefreshInstalled) return;
    window.__easyRMathRefreshInstalled=true;
    $(document).on('shiny:value',function(event){
      if(event.name!==%s) return;
      [30,250,800].forEach(function(delay){setTimeout(function(){
        var node=document.getElementById(%s); if(!node||!window.MathJax) return;
        if(MathJax.Hub&&MathJax.Hub.Queue) MathJax.Hub.Queue(['Typeset',MathJax.Hub,node]);
        else if(MathJax.typesetPromise) MathJax.typesetPromise([node]);
      },delay);});
    });
  })();", jsonlite::toJSON(formula_output_id, auto_unbox = TRUE), jsonlite::toJSON(formula_output_id, auto_unbox = TRUE))
  tagList(
    tags$script(HTML(mathjax_refresh)),
    tags$style(HTML(".distribution-learning-path{display:flex;gap:8px;flex-wrap:wrap;margin:10px 0 16px}.distribution-learning-step{background:#eef5ff;color:#174a8b;border:1px solid #cfe0f5;border-radius:999px;padding:6px 11px;font-size:12px;font-weight:750}.statistics-foundation{background:linear-gradient(145deg,#f8fbff,#fff);border:1px solid #d8e5f4;border-radius:12px;padding:14px 16px;margin-bottom:14px}.statistics-foundation h4{margin:0 0 8px;color:#173d70;font-weight:850}.statistics-formulas{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:10px;margin:12px 0}.statistics-formula-card{background:#f1f5f9;border-radius:9px;padding:10px 12px;color:#24476f;min-height:105px}.statistics-formula-card strong{display:block;color:#173d70;margin-bottom:4px}.statistics-formula-card .MathJax_Display{margin:.5em 0}.distribution-theory-formula{background:#f8fbff;border:1px solid #d8e5f4;border-radius:10px;padding:12px 14px;margin:4px 0 14px}.distribution-theory-formula h4{margin:0 0 4px;color:#173d70;font-weight:800}.distribution-theory-formula>p{color:#667892;margin:3px 0 10px}.distribution-theory-formula-row{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:9px}.distribution-exam-formula{background:#fff;border:1px solid #e1e9f3;border-radius:9px;padding:8px 10px;overflow-x:auto}.distribution-exam-formula strong{color:#36577e}.distribution-exam-formula .MathJax_Display{margin:.45em 0}@media(max-width:760px){.statistics-formulas,.distribution-theory-formula-row{grid-template-columns:1fr}}")),
    h3("统计基础与概率分布"),
    p("从均值、方差和标准差开始，再认识概率分布、随机抽样、大数定律和中心极限定理。"),
    tags$div(class = "distribution-learning-path",
      tags$span(class = "distribution-learning-step", "① 中心位置"),
      tags$span(class = "distribution-learning-step", "② 离散程度"),
      tags$span(class = "distribution-learning-step", "③ 分布形状"),
      tags$span(class = "distribution-learning-step", "④ 抽样规律")),
    fluidRow(column(5, selectInput(ns("distribution"), "概率分布", distribution_catalog())),
      column(3, numericInput(ns("sample_size"), "每次样本量", 1000, min = 10, max = 100000, step = 10)),
      column(2, numericInput(ns("repetitions"), "重复抽样次数", 500, min = 10, max = 5000, step = 10)),
      column(2, numericInput(ns("seed"), "随机种子", 2026, min = 1, step = 1))),
    uiOutput(ns("parameters")),
    uiOutput(ns("theory_formula")),
    helpText("样本量 × 重复次数最多为 5,000,000。改变参数与随机种子后重新生成，可以比较分布形状、抽样波动和收敛速度。"),
    actionButton(ns("run"), "生成分布样本", class = "btn-primary"),
    tags$div(style = "margin:12px 0", textOutput(ns("status"))),
    tabsetPanel(
      tabPanel("基础：均值与方差",
        shiny::withMathJax(tags$div(class = "statistics-foundation",
          h4("先回答两个问题：数据集中在哪里？数据有多分散？"),
          p("均值和中位数描述中心；方差、标准差、四分位距和极差描述离散程度。标准差与原数据单位相同，通常比方差更容易解释。"),
          tags$div(class = "statistics-formulas",
            tags$div(class = "statistics-formula-card", tags$strong("样本均值"),
              HTML("\\[\\bar{x}=\\frac{1}{n}\\sum_{i=1}^{n}x_i\\]"), "把所有观测加总后除以样本量。"),
            tags$div(class = "statistics-formula-card", tags$strong("样本方差"),
              HTML("\\[s^2=\\frac{1}{n-1}\\sum_{i=1}^{n}(x_i-\\bar{x})^2\\]"), "衡量观测到均值的平方距离；n − 1 是自由度修正。"),
            tags$div(class = "statistics-formula-card", tags$strong("样本标准差"),
              HTML("\\[s=\\sqrt{s^2}\\]"), "标准差与原始数据使用相同单位。"),
            tags$div(class = "statistics-formula-card", tags$strong("标准化分数"),
              HTML("\\[z_i=\\frac{x_i-\\bar{x}}{s}\\]"), "表示某个观测距离均值多少个标准差。")))),
        DT::DTOutput(ns("basic_statistics")),
        ggplot_editor_ui(ns("center_editor"), height = "430px"),
        ggplot_editor_ui(ns("variance_editor"), height = "520px")),
      tabPanel("教学解读", tags$div(style = "white-space:pre-wrap;line-height:1.9", textOutput(ns("report")))),
      tabPanel("理论与样本统计", DT::DTOutput(ns("statistics"))),
      tabPanel("分布形状", ggplot_editor_ui(ns("shape_editor"), height = "500px")),
      tabPanel("分布函数 CDF", ggplot_editor_ui(ns("cdf_editor"), height = "500px")),
      tabPanel("Q-Q 图", ggplot_editor_ui(ns("qq_editor"), height = "500px")),
      tabPanel("大数定律", ggplot_editor_ui(ns("lln_editor"), height = "500px")),
      tabPanel("样本均值与中心极限定理", ggplot_editor_ui(ns("means_editor"), height = "500px")),
      tabPanel("模拟数据", DT::DTOutput(ns("sample_table")), DT::DTOutput(ns("means_table")))
    ), hr(),
    downloadButton(ns("download_report"), "下载教学报告 TXT"),
    downloadButton(ns("download_sample"), "下载随机样本 CSV"),
    downloadButton(ns("download_means"), "下载重复抽样均值 CSV")
  )
}

distribution_demo_server <- function(id, directory = reactive(getwd())) {
  moduleServer(id, function(input, output, session) {
    result <- reactiveVal(NULL)
    status <- reactiveVal("设置分布和参数后，点击“生成分布样本”。")
    output$parameters <- renderUI({
      req(input$distribution); specs <- distribution_parameter_specs(input$distribution)
      fluidRow(lapply(specs, function(spec) {
        arguments <- list(inputId = session$ns(paste0("param_", spec$id)), label = spec$label, value = spec$value)
        if (!is.null(spec$min)) arguments$min <- spec$min
        if (!is.null(spec$max)) arguments$max <- spec$max
        if (!is.null(spec$step)) arguments$step <- spec$step
        column(max(3, floor(12 / length(specs))), do.call(numericInput, arguments))
      }))
    })
    output$theory_formula <- renderUI({
      req(input$distribution)
      formula <- distribution_exam_formulas(input$distribution)
      labels <- c(probability = if (distribution_is_discrete(input$distribution)) "概率质量函数（PMF）" else "概率密度函数（PDF）",
        mean = "期望", variance = "方差", key = "考试常用关系")
      shiny::withMathJax(tags$div(class = "distribution-theory-formula",
        h4("当前分布的考试公式"),
        p("先认清取值范围与参数定义，再记概率函数、期望和方差。不同教材的参数化可能不同，请以这里标出的定义为准。"),
        tags$div(class = "distribution-theory-formula-row",
          lapply(names(formula), function(name) tags$div(class = "distribution-exam-formula",
            tags$strong(labels[[name]]), HTML(paste0("\\[", formula[[name]], "\\]")))))))
    })
    parameter_values <- reactive({
      req(input$distribution)
      specs <- distribution_parameter_specs(input$distribution)
      values <- lapply(specs, function(spec) input[[paste0("param_", spec$id)]])
      req(all(vapply(values, function(x) length(x) == 1L, logical(1))))
      names(values) <- vapply(specs, `[[`, character(1), "id"); values
    })
    observe({
      result(NULL); status("分布或模拟设置已更新，请点击“生成分布样本”。")
      input$distribution; input$sample_size; input$repetitions; input$seed; parameter_values()
    }, priority = 100)
    observeEvent(input$run, {
      result(NULL)
      tryCatch({
        fitted <- fit_distribution_demo(input$distribution, parameter_values(), input$sample_size, input$repetitions, input$seed)
        result(fitted)
        status(sprintf("模拟完成：生成 %d 个样本，并完成 %d 次重复抽样。", fitted$sample_size, fitted$repetitions))
      }, error = function(e) {
        message <- conditionMessage(e); status(paste("未能生成：", message)); showNotification(message, type = "error", duration = 10)
      })
    })
    output$status <- renderText(status())
    output$report <- renderText({ req(result()); result()$report })
    output$basic_statistics <- DT::renderDT({
      req(result())
      DT::datatable(distribution_basic_statistics(result()$sample), rownames = FALSE,
        options = list(dom = "t", scrollX = TRUE), escape = TRUE)
    })
    output$statistics <- DT::renderDT({ req(result()); DT::datatable(result()$statistics, rownames = FALSE, options = list(dom = "t", scrollX = TRUE)) })
    output$sample_table <- DT::renderDT({ req(result()); DT::datatable(result()$sample_table, rownames = FALSE, options = list(pageLength = 10)) })
    output$means_table <- DT::renderDT({ req(result()); DT::datatable(result()$means_table, rownames = FALSE, options = list(pageLength = 10)) })
    shape_plot <- reactive({ req(result()); build_distribution_shape_plot(result()) })
    cdf_plot <- reactive({ req(result()); build_distribution_cdf_plot(result()) })
    qq_plot <- reactive({ req(result()); build_distribution_qq_plot(result()) })
    lln_plot <- reactive({ req(result()); build_distribution_lln_plot(result()) })
    means_plot <- reactive({ req(result()); build_sampling_means_plot(result()) })
    center_plot <- reactive({ req(result()); build_center_spread_plot(result()) })
    variance_plot <- reactive({ req(result()); build_variance_comparison_plot(result()) })
    ggplot_editor_server("center_editor", center_plot, directory, "easyr-mean-median-spread")
    ggplot_editor_server("variance_editor", variance_plot, directory, "easyr-same-mean-different-variance")
    ggplot_editor_server("shape_editor", shape_plot, directory, "easyr-distribution-shape")
    ggplot_editor_server("cdf_editor", cdf_plot, directory, "easyr-distribution-cdf")
    ggplot_editor_server("qq_editor", qq_plot, directory, "easyr-distribution-qq")
    ggplot_editor_server("lln_editor", lln_plot, directory, "easyr-distribution-lln")
    ggplot_editor_server("means_editor", means_plot, directory, "easyr-distribution-sample-means")
    write_csv_bom <- function(value, file) {
      con <- file(file, open = "wb"); on.exit(close(con)); writeBin(charToRaw("\ufeff"), con)
      lines <- capture.output(write.csv(value, row.names = FALSE, na = ""))
      writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n"))), con)
    }
    output$download_report <- downloadHandler(filename = function() paste0("easyr-distribution-", Sys.Date(), ".txt"),
      content = function(file) { req(result()); writeLines(enc2utf8(result()$report), file, useBytes = TRUE) })
    output$download_sample <- downloadHandler(filename = function() paste0("easyr-distribution-sample-", Sys.Date(), ".csv"),
      content = function(file) { req(result()); write_csv_bom(result()$sample_table, file) })
    output$download_means <- downloadHandler(filename = function() paste0("easyr-distribution-means-", Sys.Date(), ".csv"),
      content = function(file) { req(result()); write_csv_bom(result()$means_table, file) })
    result
  })
}
