globalVariables(c(
  "i.value", "sys_name", "test_id", "value"
))

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
  if (interactive()) {
    normalizePath(file.path(dir, "tests", "fingerprints", "fingerprints.csv"), mustWork = TRUE)
  } else {
    normalizePath(file.path(dir, "..", "fingerprints", "fingerprints.csv"), mustWork = TRUE)
  }
}

#' @importFrom data.table fread
setupTestFingerprints <- function(cwd = getwd()) {
  fread(fingerprintFile(cwd))
}

#' @importFrom data.table fwrite setkey
teardownTestFingerprints <- function(fingerprints, cwd = getwd()) {
  if (Sys.getenv("R_QUICKPLOT_NEW_FINGERPRINTS") == "TRUE") {
    setkey(fingerprints, r_version, sys_name, test_id)
    fwrite(fingerprints, file = fingerprintFile(cwd))
  }
}
