source("utils.R")

devtools::install_github("hadley/lineprof")

MyRequire(lineprof)

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