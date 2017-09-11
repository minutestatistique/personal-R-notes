source("utils.R")

MyRequire(microbenchmark)
MyRequire(ggplot2)
MyRequire(tidyr)
MyRequire(dplyr)
MyRequire(broom)

### microbenchmarking
x <- runif(100)
microbenchmark(
  sqrt(x),
  x ^ 0.5
)

n <- 1:1e6
system.time(for (i in 1:n) sqrt(x)) / length(n)
system.time(for (i in 1:n) x ^ 0.5) / length(n)

x <- runif(100)
microbenchmark::microbenchmark(
  sqrt(x),
  x ^ 0.5,
  x ^ (1 / 2),
  exp(log(x) / 2)
)

mb_integer <- microbenchmark::microbenchmark(
  1L + 1L, 1L - 1L, 1L * 1L, 1L / 1L, 1L ^ 1L, 
  times = 1000000,
  control = list(order = "random",
                 warmup = 20000))

mb_double <- microbenchmark::microbenchmark(
  1 + 1, 1 - 1, 1 * 1, 1 / 1, 1 ^ 1, 
  times = 1000000,
  control = list(order = "random",
                 warmup = 20000))

mb_median <- data.frame(operator = c("+", "-", "*", "/", "^"),
                        int = mb_integer$time,
                        dbl = mb_double$time,
                        stringsAsFactors = FALSE)

mb_median <- tidyr::gather(mb_median, type, time, int, dbl)
mb_median <- dplyr::mutate(mb_median, type = factor(type, levels = c("int", "dbl")))

ggplot(mb_median, aes(x = type, y = time, group = operator, color = operator)) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE, size = 1.5) +
  geom_label(aes(label = operator), show.legend = FALSE) +
  theme_minimal() +
  ylab("time in nanoseconds") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 10)) +
  scale_y_continuous(breaks = seq(0, max(mb_median$time), 10))

x <- runif(100)
microbenchmark::microbenchmark(
  sqrt(x),
  x ^ 0.5,
  x ^ (1 / 2),
  exp(log(x) / 2),
  unit = "eps"
)

### language performance
# extreme dynamism
f <- function(x) NULL

s3 <- function(x) UseMethod("s3")
s3.integer <- f

A <- setClass("A", representation(a = "list"))
setGeneric("s4", function(x) standardGeneric("s4"))
setMethod(s4, "A", f)

B <- setRefClass("B", methods = list(rc = f))

a <- A()
b <- B$new()

microbenchmark(
  fun = f(),
  S3 = s3(1L),
  S4 = s4(a),
  RC = b$rc()
)

# name lookup with mutable environments
a <- 1
f <- function() {
  g <- function() {
    print(a)
    assign("a", 2, envir = parent.frame())
    print(a)
    a <- 3
    print(a)
  }
  g()
}
f()

# f calls four functions
f <- function(x, y) {
  (x + y) ^ 2
}

random_env <- function(parent = globalenv()) {
  letter_list <- setNames(as.list(runif(26)), LETTERS)
  list2env(letter_list, envir = new.env(parent = parent))
}
set_env <- function(f, e) {
  environment(f) <- e
  f
}
f2 <- set_env(f, random_env())
f3 <- set_env(f, random_env(environment(f2)))
f4 <- set_env(f, random_env(environment(f3)))

microbenchmark(
  f(1, 2),
  f2(1, 2),
  f3(1, 2),
  f4(1, 2),
  times = 10000
)

# lazy evaluation overhead
f0 <- function() NULL
f1 <- function(a = 1) NULL
f2 <- function(a = 1, b = 1) NULL
f3 <- function(a = 1, b = 2, c = 3) NULL
f4 <- function(a = 1, b = 2, c = 4, d = 4) NULL
f5 <- function(a = 1, b = 2, c = 4, d = 4, e = 5) NULL
microbenchmark(f0(), f1(), f2(), f3(), f4(), f5(), times = 10000)

(mb_prom <- microbenchmark::microbenchmark(
  scan(text = "1 2 3", quiet = T),
  times = 100000,
  unit = "ns",
  control = list(warmup = 1000)
))
mb_prom_median <- broom::tidy(mb_prom)[2, ]$median

### implementation performance
# extracting a single value from a data frame
microbenchmark(
  "[32, 11]"      = mtcars[32, 11],
  "$carb[32]"     = mtcars$carb[32],
  "[[c(11, 32)]]" = mtcars[[c(11, 32)]],
  "[[11]][32]"    = mtcars[[11]][32],
  ".subset2"      = .subset2(mtcars, 11)[32]
)

# ifelse(), pmin() and pmax() 
squish_ife <- function(x, a, b) {
  ifelse(x <= a, a, ifelse(x >= b, b, x))
}
squish_p <- function(x, a, b) {
  pmax(pmin(x, b), a)
}
squish_in_place <- function(x, a, b) {
  x[x <= a] <- a
  x[x >= b] <- b
  x
}

x <- runif(100, -1.5, 1.5)
microbenchmark(
  squish_ife      = squish_ife(x, -1, 1),
  squish_p        = squish_p(x, -1, 1),
  squish_in_place = squish_in_place(x, -1, 1),
  unit = "us"
)

microbenchmark(
  squish_in_place = squish_in_place(x, -1, 1),
  squish_cpp      = squish_cpp(x, -1, 1),
  unit = "us"
)

# alternative R implementations
x <- runif(1e6)
y <- runif(1e6)
z <- sample(c(T, F), 1e6, rep = TRUE)

sum((x + y)[z])

cond_sum_r <- function(x, y, z) {
  sum((x + y)[z])
}

microbenchmark(
  cond_sum_cpp = cond_sum_cpp(x, y, z),
  cond_sum_r = cond_sum_r(x, y, z),
  unit = "ms"
)
