# packages loading
#-------------------------------------------------------------------------------
source("utl/utl.R", encoding = "UTF-8")
MyRequire(data.table)
MyRequire(bit64)
MyRequire(stringr)
MyRequire(lubridate)

# global variables
#-------------------------------------------------------------------------------
myRStudioMode <- RStudio.Version()$mode
myOSType <- Sys.info()["sysname"]
myUser <- "mn-huynh"
myAllData <- FALSE
myOutPath <- "."

# script begins
#-------------------------------------------------------------------------------
# detect RStudio mode & platform
if (myRStudioMode == "desktop") {
  if (myOSType == "Windows") {
    hist_path <- file.path("C:", "Users", myUser, "AppData",
                           "Local", "RStudio-Desktop")
    # who still uses win xp, seriously?
    # hist_path <- file.path("%USERPROFILE%", "Local Settings",
    #                        "Application Data", "RStudio-Desktop")
  } else if (myOSType %in% c("Darwin", "Linux")) {
    hist_path <- path.expand("~/.rstudio-desktop")
  } else {
    stop("Unsupported OS!")
  }
} else if(myRStudioMode == "server") {
  hist_path <- path.expand("~/.rstudio")
} else {
  stop("Invalid RStudio mode!")
}

# locate history database
hist_file_name <- file.path(hist_path, "history_database")
print(hist_file_name)

# read data
lns <- readLines(hist_file_name) %>% str_split(pattern = ":", n = 2)

# process data
hist_db <- data.table(epoch = as.integer64(sapply(lns, "[[", 1)),
                      history = sapply(lns, "[[", 2))
hist_db[, DateTime := as.POSIXct(epoch / 1000., origin = "1970-01-01",
                                 tz = "CET")]
hist_db[, Date := date(DateTime)]

# file operations
file_ops <- hist_db[grepl("file\\.|zip\\(|tar\\(", history, perl = TRUE), ]
out_file <- paste(format(Sys.time(), "%Y-%m-%d_%H%M%S"),
                  "file_ops", myUser, sep = "_")
save(file_ops, file = file.path(myOutPath, out_file))

# output data
if (myAllData) {
  out_file <- paste(format(Sys.time(), "%Y-%m-%d_%H%M%S"),
                    "hist_db", myUser, sep = "_")
  save(hist_db, file = file.path(myOutPath, out_file))
}

# clean up
rm(list = c("hist_db", "hist_file_name", "hist_path", "lns", "myOSType",
            "myRStudioMode", "myUser", "myAllData", "out_file", "file_ops"))
