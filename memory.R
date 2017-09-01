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
x <- 1:10
x[5] <- 10
x

# RStudio
x <- 1:10
c(address(x), refs(x))

y <- x
c(address(y), refs(y))

x <- 1:5
y <- x
rm(y)
# Should really be 1, because we've deleted y
refs(x)

x <- 1:5
y <- x
z <- x
# Should really be 3
refs(x)

x <- 1:10
y <- x
c(address(x), address(y))

x[5] <- 6L
c(address(x), address(y))

x <- 1:10
tracemem(x)
x[5] <- 6L

y <- x
x[5] <- 6L

# Touching the object forces an increment
f <- function(x) x
{x <- 1:10; f(x); refs(x)}

# Sum is primitive, so no increment
{x <- 1:10; sum(x); refs(x)}

# f() and g() never evaluate x, so refs don't increment
f <- function(x) 10
g <- function(x) substitute(x)

{x <- 1:10; f(x); refs(x)}
{x <- 1:10; g(x); refs(x)}

# loops
x <- data.frame(matrix(runif(100 * 1e4), ncol = 100))
medians <- vapply(x, median, numeric(1))

for(i in seq_along(medians)) {
  x[, i] <- x[, i] - medians[i]
}

for(i in 1:5) {
  x[, i] <- x[, i] - medians[i]
  print(c(address(x), refs(x)))
}

y <- as.list(x)

for(i in 1:5) {
  y[[i]] <- y[[i]] - medians[i]
  print(c(address(y), refs(y)))
}

# my personal tests
#-------------------------------------------------------------------------------
dt <- data.table(1:5)
dt
address(dt)
f <- function(x) {
  x <- data.table(6:10)
  print(address(x))
}
f(dt)
dt
address(dt)

f_2 <- function(x) {
  x[, V2 := V1]
  print(address(x))
}
f_2(dt)
dt
address(dt)
