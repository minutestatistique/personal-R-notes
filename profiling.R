source("utils.R")

devtools::install_github("hadley/lineprof")

MyRequire(lineprof)
MyRequire(microbenchmark)
MyRequire(speedglm)
MyRequire(fastmatch)
MyRequire(zoo)
MyRequire(TTR)
MyRequire(RcppRoll)
MyRequire(caTools)

### measuring performance
f <- function() {
  pause(0.1)
  g()
  h()
}
g <- function() {
  pause(0.1)
  h()
}
h <- function() {
  pause(0.1)
}

tmp <- tempfile()
Rprof(tmp, interval = 0.1)
f()
Rprof(NULL)

source("src/R/profiling-example.R")
l <- lineprof(f())
l
shine(l)

### code organisation
mean1 <- function(x) mean(x)
mean2 <- function(x) sum(x) / length(x)

x <- runif(100)
stopifnot(all.equal(mean1(x), mean2(x)))

microbenchmark(
  mean1(x),
  mean2(x)
)

### has someone already solved the problem?
stopifnot(all.equal(
  coef(speedglm::speedlm(Sepal.Length ~ Sepal.Width + Species, data = iris)),
  coef(lm(Sepal.Length ~ Sepal.Width + Species, data = iris))))

microbenchmark::microbenchmark(
  speedglm::speedlm(Sepal.Length ~ Sepal.Width + Species, data = iris),
  lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
)

eps <- rnorm(100000)
x1 <- rnorm(100000, 5, 3)
x2 <- rep(c("a", "b"), 50000)
y <- 7 * x1 + (x2 == "a") + eps
td <- data.frame(y = y, x1 = x1, x2 = x2, eps = eps)

stopifnot(all.equal(
  coef(speedglm::speedlm(y ~ x1 + x2, data = td)),
  coef(lm(y ~ x1 + x2, data = td))))

microbenchmark::microbenchmark(
  speedglm::speedlm(y ~ x1 + x2, data = td),
  lm(y ~ x1 + x2, data = td)
)

set.seed(1)
table <- 1L:100000L
x <- sample(table, 10000, replace = TRUE)

stopifnot(all.equal(match(x, table), fastmatch::fmatch(x, table)))

microbenchmark::microbenchmark(
  match(x, table),
  fastmatch::fmatch(x, table)
)

zoo::rollmean(1:10, 2, na.pad = TRUE, align = "left")
zoo::rollapply(1:10, 2, mean, fill = NA, align = "left")

TTR::SMA(1:10, 2)

RcppRoll::roll_mean(1:10, n = 2, fill = NA, align = "left")

caTools::runmean(1:10, k = 2, endrule = "NA", align = "left")

### do as little as possible
x <- runif(1e2)

microbenchmark(
  mean(x),
  mean.default(x)
)

quickdf <- function(l) {
  class(l) <- "data.frame"
  attr(l, "row.names") <- .set_row_names(length(l[[1]]))
  l
}

l <- lapply(1:26, function(i) runif(1e3))
names(l) <- letters

microbenchmark(
  quick_df      = quickdf(l),
  as.data.frame = as.data.frame(l)
)

quickdf(list(x = 1, y = 1:2))

diff1 <- function (x, lag = 1L, differences = 1L) {
  ismat <- is.matrix(x)
  xlen <- if (ismat) dim(x)[1L] else length(x)
  if (length(lag) > 1L || length(differences) > 1L || 
      lag < 1L || differences < 1L)
    stop("'lag' and 'differences' must be integers >= 1")
  
  if (lag * differences >= xlen) {
    return(x[0L])
  }
  
  r <- unclass(x)
  i1 <- -seq_len(lag)
  if (ismat) {
    for (i in seq_len(differences)) {
      r <- r[i1, , drop = FALSE] - 
        r[-nrow(r):-(nrow(r) - lag + 1L), , drop = FALSE]
    }
  } else {
    for (i in seq_len(differences)) {
      r <- r[i1] - r[-length(r):-(length(r) - lag + 1L)]
    }
  }
  class(r) <- oldClass(x)
  r
}

diff2 <- function (x, lag = 1L, differences = 1L) {
  xlen <- length(x)
  if (length(lag) > 1L || length(differences) > 1L || 
      lag < 1L || differences < 1L)
    stop("'lag' and 'differences' must be integers >= 1")
  
  if (lag * differences >= xlen) {
    return(x[0L])
  }
  
  i1 <- -seq_len(lag)
  for (i in seq_len(differences)) {
    x <- x[i1] - x[-length(x):-(length(x) - lag + 1L)]
  }
  x
}
diff2(cumsum(0:10))

diff3 <- function (x, lag = 1L) {
  xlen <- length(x)
  if (length(lag) > 1L || lag < 1L)
    stop("'lag' must be integer >= 1")
  
  if (lag >= xlen) {
    return(x[0L])
  }
  
  i1 <- -seq_len(lag)
  x[i1] - x[-length(x):-(length(x) - lag + 1L)]
}
diff3(cumsum(0:10))

diff4 <- function (x) {
  xlen <- length(x)
  if (xlen <= 1) return(x[0L])
  
  x[-1] - x[-xlen]
}
diff4(cumsum(0:10))

x <- runif(100)
microbenchmark(
  diff1(x),
  diff2(x),
  diff3(x),
  diff4(x)
)

sample_rows <- function(df, i) sample.int(nrow(df), i, 
                                          replace = TRUE)

# Generate a new data frame containing randomly selected rows
boot_cor1 <- function(df, i) {
  sub <- df[sample_rows(df, i), , drop = FALSE]
  cor(sub$x, sub$y)
}

# Generate new vectors from random rows
boot_cor2 <- function(df, i ) {
  idx <- sample_rows(df, i)
  cor(df$x[idx], df$y[idx])
}

df <- data.frame(x = runif(100), y = runif(100))
microbenchmark(
  boot_cor1(df, 10),
  boot_cor2(df, 10)
)

x <- runif(1e2)
microbenchmark::microbenchmark(
  mean(x),
  mean.default(x)
)

x <- runif(1e4)
microbenchmark::microbenchmark(
  mean(x),
  mean.default(x),
  unit = "ns"
)

x <- runif(1e6)
microbenchmark::microbenchmark(
  mean(x),
  mean.default(x),
  unit = "ns"
)

rowSums2 <- function(df) {
  out <- df[[1L]]
  if (ncol(df) == 1) return(out)
  
  for (i in 2:ncol(df)) {
    out <- out + df[[i]]
  }
  out
}

df <- as.data.frame(
  replicate(1e3, sample(100, 1e4, replace = TRUE))
)
system.time(rowSums(df))
system.time(rowSums2(df))

.rowSums
rowSums

chisq.test2 <- function(x, y){
  
  # Input
  if (!is.numeric(x)) {
    stop("x must be numeric")}
  if (!is.numeric(y)) {
    stop("y must be numeric")}
  if (length(x) != length(y)) {
    stop("x and y must have the same length")}
  if (length(x) <= 1) {
    stop("length of x must be greater one")}
  if (any(c(x, y) < 0)) {
    stop("all entries of x and y must be greater or equal zero")}
  if (sum(complete.cases(x, y)) != length(x)) {
    stop("there must be no missing values in x and y")}
  if (any(is.null(c(x, y)))) {
    stop("entries of x and y must not be NULL")}
  
  # Help variables
  m <- rbind(x, y)
  margin1 <- rowSums(m)
  margin2 <- colSums(m)
  n <- sum(m)
  me <- tcrossprod(margin1, margin2) / n
  
  # Output
  x_stat = sum((m - me)^2 / me)
  dof <- (length(margin1) - 1) * (length(margin2) - 1)
  p <- pchisq(x_stat, df = dof, lower.tail = FALSE)
  
  return(list(x_stat = x_stat, df = dof, `p-value` = p))
}

a <- 21:25
b <- c(21, 23, 25, 27, 29)
m_test <- cbind(a, b)

chisq.test(m_test)
chisq.test2(a, b)
chisq.test2c <- compiler::cmpfun(chisq.test2)

microbenchmark::microbenchmark(
  chisq.test(m_test),
  chisq.test2(a, b),
  chisq.test2c(a, b)
)

n <- 1e6
df <- data.frame(a = rnorm(n), b = rnorm(n))

cor_df <- function(df, n) {
  i <- sample(seq(n), n, replace = FALSE)
  cor(df[i, , drop = FALSE])[2, 1]
}
