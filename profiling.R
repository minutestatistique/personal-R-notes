source("utils.R")

devtools::install_github("hadley/lineprof")
source("http://bioconductor.org/biocLite.R") 
biocLite(c("AnnotationDbi", "impute", "GO.db", "preprocessCore")) 

MyRequire(lineprof)
MyRequire(microbenchmark)
MyRequire(speedglm)
MyRequire(fastmatch)
MyRequire(zoo)
MyRequire(TTR)
MyRequire(RcppRoll)
MyRequire(caTools)
MyRequire(WGCNA)
MyRequire(ggplot2)

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

i <- sample(seq(n), n)

# old version
cor_v1 <- function() {
  cor(df[i, , drop = FALSE])[2, 1]
}

# cbind instead of internal as.matrix
cor_v2 <- function() {
  m <- cbind(df$a[i], df$b[i])
  cor(m)[2, 1]
}

# cbind + vector subsetting of the output matrix
cor_v3 <- function() {
  m <- cbind(df$a[i], df$b[i])
  cor(m)[2]
}

# use vector input within the cor function, so that no conversion is needed
cor_v4 <- function() {
  cor(df$a[i], df$b[i])
}

# check if all return the same result (if you don't get the same result on your
# machine, you might wanna check if it is due to the precision and istead
# run cor_v1(), cor_v2() etc.)
cor_list <- list(cor_v1, cor_v2, cor_v3, cor_v4)
ulapply <- function(X, FUN, ...) unlist(lapply(X, FUN, ...))
ulapply(cor_list, function(x) identical(x(), cor_v1()))
#> [1] TRUE TRUE TRUE TRUE

# benchmark
set.seed(1)
microbenchmark::microbenchmark(
  cor_v1(),
  cor_v2(),
  cor_v3(),
  cor_v4()
)

cor_df <- function() {
  i <- sample(seq(n), n, replace = FALSE)
  cor(df[i, , drop = FALSE])[2, 1]
}

cor_df2 <- function() {
  i <- sample(seq(n), n)
  m <- cbind(df$a[i], df$b[i])
  cor(m)[2, 1]
}

microbenchmark::microbenchmark(
  cor_df(),
  cor_df2()
)

seq_n <- seq(n)

microbenchmark::microbenchmark(
  sample(seq_n, n),
  sample.int(n, n),
  .Internal(sample(n, n, replace = FALSE, prob = NULL))
)

cor_df3 <- function() {
  i <- sample.int(n, n)
  m <- cbind(df$a[i], df$b[i])
  cor(m)[2, 1]
}

microbenchmark::microbenchmark(
  cor_df(),
  cor_df2(),
  cor_df3()
)

#WGCNA version (matrix and vector). Note that I don't use a local setup which uses
#the full potential of this function. For furter information see ?WGCNA::cor
cor_df4m <- function() {
  i <- sample.int(n, n)
  m <- cbind(df$a[i], df$b[i])
  WGCNA::cor(m)[2]
}

cor_df4v <- function() {
  i <- sample.int(n, n)
  WGCNA::cor(df$a[i], df$b[i], quick = 1)[1]
}

#New implementation of underlying cor function
#A definition can be found for example here
#http://www.socscistatistics.com/tests/pearson/
cor2 <- function(x, y){
  xm <- sum(x) / length(x)
  ym <- sum(y) / length(y)
  x_xm <- x - xm
  y_ym <- y - ym
  numerator <- sum((x_xm) * (y_ym))
  denominator <- sqrt(sum(x_xm^2)) * sqrt(sum(y_ym^2))
  return(numerator / denominator)
}

cor2 <- compiler::cmpfun(cor2)

cor_df5 <- function() {
  i <- sample.int(n, n)
  cor2(df$a[i], df$b[i])
}

cor_df_c <- compiler::cmpfun(cor_df)
cor_df2_c <- compiler::cmpfun(cor_df2)
cor_df3_c <- compiler::cmpfun(cor_df3)
cor_df4m_c <- compiler::cmpfun(cor_df4m)
cor_df4v_c <- compiler::cmpfun(cor_df4v)
cor_df5_c <- compiler::cmpfun(cor_df5)

microbenchmark::microbenchmark(
  cor_df(),
  cor_df2(),
  cor_df3(),
  cor_df4m(),
  cor_df4v(),
  cor_df5(),
  cor_df_c(),
  cor_df2_c(),
  cor_df3_c(),
  cor_df4m_c(),
  cor_df4v_c(),
  cor_df5_c()
)

set.seed(1)
cor_df()
set.seed(1)
cor_df5_c()

cor2m <- function(x, y){
  n_row <- nrow(x)
  xm <- colSums(x) / n_row
  ym <- colSums(y) / n_row
  x_xm <- t(t(x) - xm)
  y_ym <- t(t(y) - ym)
  numerator <- colSums((x_xm) * (y_ym))
  denominator <- sqrt(colSums(x_xm^2)) * sqrt(colSums(y_ym^2))
  return(numerator / denominator)
}

cor_df_v <- function(i){
  indices <- replicate(i, sample.int(n, n), simplify = "array")
  x <- matrix(df$a[indices], ncol = i)
  y <- matrix(df$b[indices], ncol = i)
  cor2m(x, y)
}
cor_df_v <- compiler::cmpfun(cor_df_v)

ulapply2 <- function(X, FUN, ...) unlist(lapply(X, FUN, ...), use.names = FALSE)

microbenchmark::microbenchmark(
  cor_df5_c(),
  ulapply2(1:100, function(x) cor_df5_c()),
  cor_df_v(100)
)

### vectorise
lookup <- setNames(as.list(sample(100, 26)), letters)

x1 <- "j"
x10 <- sample(letters, 10)
x100 <- sample(letters, 100, replace = TRUE)

microbenchmark(
  lookup[x1],
  lookup[x10],
  lookup[x100]
)

?dnorm

dimensions <- c(1e0, 1e1, 1e2, 1e3, 0.5e4, 1e4)
matrices <- lapply(dimensions,
                   function(x) tcrossprod(rnorm(x), rnorm(x)))

bench_rs <- lapply(matrices,
                   function(x) fivenum(microbenchmark(rowSums(x),
                                                      unit = "ns")$time))

bench_rs <- data.frame(time = unlist(bench_rs), 
                       call = "rowSums", stringsAsFactors = FALSE)

bench_apply <- lapply(matrices,
                      function(x) fivenum(microbenchmark(apply(x, 1, sum),
                                                         unit = "ns")$time))
bench_apply <- data.frame(time = unlist(bench_apply),
                          call = "apply", stringsAsFactors = FALSE)

df <- rbind(bench_rs, bench_apply)

df$dimension <- rep(dimensions, each = 5)
df$aggr <- rep(c("min", "lq", "median", "uq", "max"),
               times = length(dimensions))
df$aggr_size <- rep(c(1, 2, 3, 2, 1), times = length(dimensions))
df$group <- paste(as.character(df$call), as.character(df$aggr), sep = " ")

ggplot(df, aes(x = dimension, y = time, colour = call, group = group)) +
  geom_point() + 
  geom_line(aes(linetype = factor(aggr_size, levels = c("3", "2", "1"))),
            show.legend = FALSE)

a <- rnorm(10)
b <- rnorm(10)
sum(a * b) - crossprod(a, b)[1]

dimensions <- c(1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 0.5e7, 1e7)
xvector <- lapply(dimensions, rnorm)
weights <- lapply(dimensions, rnorm)

bench_sum <- Map(function(x, y) fivenum(microbenchmark(sum(x * y))$time),
                 xvector, weights)
bench_sum <- data.frame(time = unlist(bench_sum),
                        call = "sum",
                        stringsAsFactors = FALSE)
bench_cp <- Map(function(x, y) fivenum(microbenchmark(crossprod(x, y)[1])$time),
                xvector, weights)
bench_cp <- data.frame(time = unlist(bench_cp),
                       call = "crossproduct",
                       stringsAsFactors = FALSE)

df <- rbind(bench_sum, bench_cp)

df$dimension <- rep(dimensions, each = 5)
df$aggr <- rep(c("min", "lq", "median", "uq", "max"), times = length(dimensions))
df$aggr_size <- rep(c(1, 2, 3, 2, 1), times = length(dimensions))
df$group <- paste(as.character(df$call), as.character(df$aggr), sep = " ")

ggplot(df, aes(x = dimension, y = time, colour = call, group = group)) +
  geom_point() + 
  geom_line(aes(linetype = factor(aggr_size, levels = c("3", "2", "1"))),
            show.legend = FALSE) +
  scale_y_continuous(expand = c(0, 0))

### avoid copies
random_string <- function() {
  paste(sample(letters, 50, replace = TRUE), collapse = "")
}
strings10 <- replicate(10, random_string())
strings100 <- replicate(100, random_string())

collapse <- function(xs) {
  out <- ""
  for (x in xs) {
    out <- paste0(out, x)
  }
  out
}

microbenchmark(
  loop10  = collapse(strings10),
  loop100 = collapse(strings100),
  vec10   = paste(strings10, collapse = ""),
  vec100  = paste(strings100, collapse = "")
)

### byte code compilation
lapply2 <- function(x, f, ...) {
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- f(x[[i]], ...)
  }
  out
}

lapply2_c <- compiler::cmpfun(lapply2)

x <- list(1:10, letters, c(F, T), NULL)
microbenchmark(
  lapply2(x, is.null),
  lapply2_c(x, is.null),
  lapply(x, is.null)
)

### case study: t-test