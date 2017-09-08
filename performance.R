source("utils.R")

devtools::install_github("hadley/lineprof")

MyRequire(microbenchmark)
MyRequire(ggplot2)
MyRequire(tidyr)
MyRequire(dplyr)

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
