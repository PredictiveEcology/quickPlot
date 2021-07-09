r_version <- function() {
  as.character(getRversion())
}

sysname <- function() {
  Sys.info()["sysname"][[1]]
}

fingerprint <- function(fingerprints, id, rversion, sysname) {
  fingerprints[test_id == id & r_version == rversion & sys_name == sysname, ][["value"]]
}

updateFingerprint <- function(newValue, fingerprints) {
  cols <- grep("value", names(fingerprints), invert = TRUE, value = TRUE)
  temp1 <- fingerprints[newValue, on = cols, nomatch = 0][, value := i.value]
  set(temp1, NULL, "i.value", NULL)
  temp2 <- fingerprints[!newValue, on = .(test_id, r_version, sys_name)]
  fingerprints <- if (nrow(temp1) > 0) rbind(temp2, temp1) else rbind(temp2, newValue)
  return(fingerprints)
}

fingerprintFile <- function(dir = getwd()) {
  file.path(dir, "tests", "fingerprints", "fingerprints.csv")
}

setupTestFingerprints <- function(cwd = getwd()) {
  library(visualTest)

  fingerprints <- data.table::fread(fingerprintFile(cwd))

  return(fingerprints)
}

#' @importFrom utils write.csv
teardownTestFingerprints <- function(cwd = getwd()) {
  if (Sys.getenv("R_QUICKPLOT_NEW_FINGERPRINTS") == "TRUE") {
    write.csv(fingerprints, file = fingerprintFile(cwd), row.names = FALSE)
  }
}
