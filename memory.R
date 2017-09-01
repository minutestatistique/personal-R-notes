# memory usage & garbage collection
mem_used()

mem_change(x <- 1:1e6)
mem_change(rm(x))

mem_change(NULL)
mem_change(NULL)

mem_change(x <- 1:1e6)
mem_change(y <- x)
mem_change(rm(x))
mem_change(rm(y))

f1 <- function() {
  x <- 1:1e6
  10
}
mem_change(x <- f1())
object_size(x)

f2 <- function() {
  x <- 1:1e6
  a ~ b
}
mem_change(y <- f2())
object_size(y)

f3 <- function() {
  x <- 1:1e6
  function() 10
}
mem_change(z <- f3())
object_size(z)

# memory profiling with lineprof
if (!dir.exists("data")) {
  dir.create("data")
}
write.csv(diamonds, "data/diamonds.csv", row.names = FALSE)

source("code/read-delim.R")
prof <- lineprof(read_delim("data/diamonds.csv"))
shine(prof)

prof_2 <- lineprof(read_delim_2("data/diamonds.csv"))
shine(prof_2)

prof_3 <- lineprof(f())
shine(prof_3)

# modification in place