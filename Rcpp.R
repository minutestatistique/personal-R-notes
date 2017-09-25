source("utils.R")

MyRequire(Rcpp)
MyRequire(microbenchmark)

### getting started with C++
cppFunction('int add(int x, int y, int z) {
  int sum = x + y + z;
  return sum;
}')

add

add(1, 2, 3)

# no inputs, scalar output
oneR <- function() 1L

cppFunction('int oneC() {
  return 1;
}')

# scalar input, scalar output
signR <- function(x) {
  if (x > 0) {
    1
  } else if (x == 0) {
    0
  } else {
    -1
  }
}

cppFunction('int signC(int x) {
  if (x > 0) {
    return 1;
  } else if (x == 0) {
    return 0;
  } else {
    return -1;
  }
}')

# vector input, scalar output
sumR <- function(x) {
  total <- 0
  for (i in seq_along(x)) {
    total <- total + x[i]
  }
  total
}

cppFunction('double sumC(NumericVector x) {
  int n = x.size();
  double total = 0;
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total;
}')

x <- runif(1e3)
microbenchmark(
  sum(x),
  sumC(x),
  sumR(x)
)

# vector input, vector output
pdistR <- function(x, ys) {
  sqrt((x - ys) ^ 2)
}

cppFunction('NumericVector pdistC(double x, NumericVector ys) {
  int n = ys.size();
  NumericVector out(n);
            
  for(int i = 0; i < n; ++i) {
    out[i] = sqrt(pow(ys[i] - x, 2.0));
  }
  return out;
}')

# matrix input, vector output
cppFunction('NumericVector rowSumsC(NumericMatrix x) {
  int nrow = x.nrow(), ncol = x.ncol();
  NumericVector out(nrow);
            
  for (int i = 0; i < nrow; i++) {
    double total = 0;
    for (int j = 0; j < ncol; j++) {
      total += x(i, j);
    }
    out[i] = total;
  }
  return out;
}')
set.seed(1014)
x <- matrix(sample(100), 10)
rowSums(x)
rowSumsC(x)

### using sourceCpp
Rcpp::sourceCpp('src/cpp/mean.cpp')

### missing values
Rcpp::sourceCpp('src/cpp/scalar_missings.cpp')
str(scalar_missings())

evalCpp("NAN == 1")
evalCpp("NAN < 1")
evalCpp("NAN > 1")
evalCpp("NAN == NAN")

evalCpp("NAN && TRUE")
evalCpp("NAN || FALSE")

evalCpp("NAN + 1")
evalCpp("NAN - 1")
evalCpp("NAN / 1")
evalCpp("NAN * 1")

Rcpp::sourceCpp('src/cpp/missing_sampler.cpp')
str(missing_sampler())

Rcpp::sourceCpp('src/cpp/is_na.cpp')
is_naC(c(NA, 5.4, 3.2, NA))
is_naC2(c(NA, 5.4, 3.2, NA))

### Rcpp sugar
# arithmetic and logical operators
pdistR <- function(x, ys) {
  sqrt((x - ys) ^ 2)
}

Rcpp::sourceCpp('src/cpp/sugar.cpp')

set.seed(12345)
y <- rnorm(1e6)
x <- mean(y)
microbenchmark(
  pdistR = pdistR(x, y),
  pdistC2 = pdistC2(x, y)
)

# logical summary functions
any_naR <- function(x) any(is.na(x))

Rcpp::sourceCpp('src/cpp/sugar.cpp')

x0 <- runif(1e5)
x1 <- c(x0, NA)
x2 <- c(NA, x0)

microbenchmark(
  any_naR(x0), any_naC(x0),
  any_naR(x1), any_naC(x1),
  any_naR(x2), any_naC(x2)
)

### the STL
# using iterators
Rcpp::sourceCpp('src/cpp/the_stl.cpp')

# algorithms
Rcpp::sourceCpp('src/cpp/the_stl.cpp')

# vectors
Rcpp::sourceCpp('src/cpp/the_stl.cpp')

# sets
Rcpp::sourceCpp('src/cpp/the_stl.cpp')

# maps
Rcpp::sourceCpp('src/cpp/the_stl.cpp')

### case studies
# Gibbs sampler
gibbs_r <- function(N, thin) {
  mat <- matrix(nrow = N, ncol = 2)
  x <- y <- 0
  
  for (i in 1:N) {
    for (j in 1:thin) {
      x <- rgamma(1, 3, y * y + 4)
      y <- rnorm(1, 1 / (x + 1), 1 / sqrt(2 * (x + 1)))
    }
    mat[i, ] <- c(x, y)
  }
  mat
}

Rcpp::sourceCpp('src/cpp/gibbs.cpp')

microbenchmark(
  gibbs_r(100, 10),
  gibbs_cpp(100, 10)
)

# R vectorisation vs. C++ vectorisation
vacc1a <- function(age, female, ily) {
  p <- 0.25 + 0.3 * 1 / (1 - exp(0.04 * age)) + 0.1 * ily
  p <- p * if (female) 1.25 else 0.75
  p <- max(0, p)
  p <- min(1, p)
  p
}

vacc1 <- function(age, female, ily) {
  n <- length(age)
  out <- numeric(n)
  for (i in seq_len(n)) {
    out[i] <- vacc1a(age[i], female[i], ily[i])
  }
  out
}

vacc2 <- function(age, female, ily) {
  p <- 0.25 + 0.3 * 1 / (1 - exp(0.04 * age)) + 0.1 * ily
  p <- p * ifelse(female, 1.25, 0.75)
  p <- pmax(0, p)
  p <- pmin(1, p)
  p
}

Rcpp::sourceCpp('src/cpp/vacc3.cpp')

n <- 1000
age <- rnorm(n, mean = 50, sd = 10)
female <- sample(c(T, F), n, rep = TRUE)
ily <- sample(c(T, F), n, prob = c(0.8, 0.2), rep = TRUE)

stopifnot(
  all.equal(vacc1(age, female, ily), vacc2(age, female, ily)),
  all.equal(vacc1(age, female, ily), vacc3(age, female, ily))
)

microbenchmark(
  vacc1 = vacc1(age, female, ily),
  vacc2 = vacc2(age, female, ily),
  vacc3 = vacc3(age, female, ily)
)

### using Rcpp in a package
# Rcpp.package.skeleton("NewPackage", attributes = TRUE)

# Rcpp.package.skeleton("NewPackage", example_code = FALSE,
#                       cpp_files = c("convolve.cpp"))
