read_delim <- function(file, header = TRUE, sep = ",") {
  # Determine number of fields by reading first line
  first <- scan(file, what = character(1), nlines = 1,
                sep = sep, quiet = TRUE)
  p <- length(first)
  
  # Load all fields as character vectors
  all <- scan(file, what = as.list(rep("character", p)),
              sep = sep, skip = if (header) 1 else 0, quiet = TRUE)
  
  # Convert from strings to appropriate types (never to factors)
  all[] <- lapply(all, type.convert, as.is = TRUE)
  
  # Set column names
  if (header) {
    names(all) <- first
  } else {
    names(all) <- paste0("V", seq_along(all))
  }
  
  # Convert list into data frame
  as.data.frame(all)
}
to_df <- function(x) {
  class(x) <- "data.frame"
  attr(x, "row.names") <- .set_row_names(length(x[[1]]))
  x
}
read_delim_2 <- function(file, header = TRUE, sep = ",") {
  # Determine number of fields by reading first line
  first <- scan(file, what = character(1), nlines = 1,
                sep = sep, quiet = TRUE)
  p <- length(first)
  
  # Load all fields as character vectors
  all <- scan(file, what = as.list(rep("character", p)),
              sep = sep, skip = if (header) 1 else 0, quiet = TRUE)
  
  # Convert from strings to appropriate types (never to factors)
  all[] <- lapply(all, type.convert, as.is = TRUE)
  
  # Set column names
  if (header) {
    names(all) <- first
  } else {
    names(all) <- paste0("V", seq_along(all))
  }
  
  # Convert list into data frame
  to_df(all)
}
f <- function(n = 1e5) {
  x <- rep(1, n)
  rm(x)
}