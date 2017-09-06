source("utils.R")

devtools::install_github("hadley/lineprof")

MyRequire(pryr)
MyRequire(data.table)
MyRequire(ggplot2)
MyRequire(pryr)
MyRequire(devtools)

object_size(1:10)
object_size(mean)
object_size(mtcars)

# zero-length vector with size != 0 & size is not proportionate with length
sizes <- sapply(0:50, function(n) object_size(seq_len(n)))
plot(0:50, sizes, xlab = "Length", ylab = "Size (bytes)", type = "s")

# zero-length vector size
object_size(numeric())
object_size(logical())
object_size(raw())
object_size(list())

plot(0:50, sizes - 40, xlab = "Length", ylab = "Bytes excluding overhead", type = "n")
abline(h = 0, col = "grey80")
abline(h = c(8, 16, 32, 48, 64, 128), col = "grey80")
abline(a = 0, b = 4, col = "grey90", lwd = 4)
lines(sizes - 40, type = "s")

# shared components
x <- 1:1e6
object_size(x)

y <- list(x, x, x)
object_size(y)

object_size(x, y)

# no sharing
x1 <- 1:1e6
y1 <- list(1:1e6, 1:1e6, 1:1e6)
object_size(x1)
object_size(y1)
object_size(x1, y1)
object_size(x1) + object_size(y1) == object_size(x1, y1)

# strings pool
object_size("banana")
object_size(rep("banana", 10))

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

x <- 1:1e6
y <- x
object_size(x)
object_size(y)
object_size(x, y)

address(x)
address(y)

x[1] <- 10L
address(x)
address(y)
object_size(x, y)

dt <- data.table(X1 = 1:1e6)
dt
address(dt)
object_size(dt)

f <- function(x) {
  x[, X2 := X1]
}
f(dt)
dt
address(dt)
object_size(dt)

df <- data.frame(X1 = 1:1e6)
head(df)
address(df)
object_size(df)

f <- function(df) {
  df$X2 <- df$X1
  df
}
df <- f(df)
head(df)
address(df)
object_size(df)
df$X2[1] <- 10

dt <- data.table(1:1e6)
dt_2 <- dt
object_size(dt)
object_size(dt_2)
object_size(dt, dt_2)

address(dt)
address(dt_2)

dt_2[1, V1 := 10L]

dt_2
dt

address(dt)
address(dt_2)
object_size(dt, dt_2)

dt <- data.table(1:5, letters[1:5])
dt <- data.table(1:5, letters[1:5], LETTERS[1:5])
address(dt)
track_dt <- track_copy(dt)
f <- function(x) {
  setnames(x, names(x), c("X1", "X2"))
  x[, X3 := X1]
}
track_dt()
f(dt[, .(V1, V2)])
f(dt)
address(dt)

dt <- data.table(1:5, letters[1:5], LETTERS[1:5])
track_dt <- track_copy(dt)
track_dt()
tracemem(dt)
dt_bis <- dt[, .(V1, V2)]
dt_bis$V1[1] <- 200

address(dt)
address(dt_bis)

track_dt <- track_copy(dt)
track_dt()
dt_bis[, V4 := V1]
