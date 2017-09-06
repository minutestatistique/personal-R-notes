MyRequire <- function(p) {
  p_name <- as.character(substitute(p))
  suppressWarnings(
    if (!require(p_name, character.only = TRUE)) {
      install.packages(p_name)
    }
  )
  require(p_name, character.only = TRUE)
}