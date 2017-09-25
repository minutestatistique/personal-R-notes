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
sourceCpp('src/cpp/mean.cpp')
