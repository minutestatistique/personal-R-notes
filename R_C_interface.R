source("utils.R")

MyRequire(pryr)
MyRequire(inline)

rinternals <- file.path(R.home("include"), "Rinternals.h")
file.show(rinternals)

### calling C functions from R
add <- cfunction(c(a = "integer", b = "integer"), "
  SEXP result = PROTECT(allocVector(REALSXP, 1));
  REAL(result)[0] = asReal(a) + asReal(b);
  UNPROTECT(1);

  return result;
")
add(1, 5)

### C data structures
sexp_type(10L)
sexp_type("a")
sexp_type(T)
sexp_type(list(a = 1))
sexp_type(pairlist(a = 1))

### creating and modifying vectors
# creating vectors and garbage collection
dummy <- cfunction(body = '
  SEXP dbls = PROTECT(allocVector(REALSXP, 4));
  SEXP lgls = PROTECT(allocVector(LGLSXP, 4));
  SEXP ints = PROTECT(allocVector(INTSXP, 4));
 
  SEXP vec = PROTECT(allocVector(VECSXP, 3));
  SET_VECTOR_ELT(vec, 0, dbls);
  SET_VECTOR_ELT(vec, 1, lgls);
  SET_VECTOR_ELT(vec, 2, ints);
 
  UNPROTECT(4);
  return vec;
')
dummy()

zeroes <- cfunction(c(n_ = "integer"), '
  int n = asInteger(n_);
  SEXP out = PROTECT(allocVector(INTSXP, n));
  memset(INTEGER(out), 0, n * sizeof(int));
  UNPROTECT(1);
                    
  return out;                  
')
zeroes(10)

# missing and non-finite values