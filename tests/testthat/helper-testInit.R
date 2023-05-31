testInit <- function(libraries = character(), ask = FALSE, verbose, tmpFileExt = "",
                     opts = NULL, needGoogleDriveAuth = FALSE) {

  set.randomseed()

  pf <- parent.frame()

  if (isTRUE(needGoogleDriveAuth))
    libraries <- c(libraries, "googledrive")
  if (length(libraries)) {
    libraries <- unique(libraries)
    loadedAlready <- vapply(libraries, function(pkg)
      any(grepl(paste0("package:", pkg), search())), FUN.VALUE = logical(1))
    libraries <- libraries[!loadedAlready]

    if (length(libraries)) {
      pkgsLoaded <- unlist(lapply(libraries, requireNamespace, quietly = TRUE))
      if (!all(pkgsLoaded)) {
        lapply(libraries[!pkgsLoaded], skip_if_not_installed)
      }
      lapply(libraries, withr::local_package, .local_envir = pf)
    }
  }


  skip_gauth <- identical(Sys.getenv("SKIP_GAUTH"), "true") # only set in setup.R for covr
  if (isTRUE(needGoogleDriveAuth) && !skip_gauth) {
    if (interactive()) {
      if (!googledrive::drive_has_token()) {
        getAuth <- FALSE
        if (is.null(getOption("gargle_oauth_email"))) {
          possLocalCache <- "c:/Eliot/.secret"
          cache <- if (file.exists(possLocalCache))
            possLocalCache else TRUE
          switch(Sys.info()["user"],
                 emcintir = {options(gargle_oauth_email = "eliotmcintire@gmail.com",
                                     gargle_oauth_cache = cache)},
                 NULL)
        }
        if (is.null(getOption("gargle_oauth_email"))) {
          if (.isRstudioServer()) {
            .requireNamespace("httr", stopOnFALSE = TRUE)
            options(httr_oob_default = TRUE)
          }
        }
        getAuth <- TRUE
        if (isTRUE(getAuth))
          googledrive::drive_auth()
      }
    }
    skip_if_no_token()
  }

  out <- list()
  withr::local_options("reproducible.ask" = ask, .local_envir = pf)
  if (!missing(verbose))
    withr::local_options("reproducible.verbose" = verbose, .local_envir = pf)
  if (!is.null(opts))
    withr::local_options(opts, .local_envir = pf)
  tmpdir <- Require::normPath(withr::local_tempdir(tmpdir = Require::tempdir2(), .local_envir = pf))
  tmpCache <- Require::normPath(withr::local_tempdir(tmpdir = tmpdir, .local_envir = pf))
  if (isTRUE(any(nzchar(tmpFileExt)))) {
    dotStart <- startsWith(tmpFileExt, ".")
    if (any(!dotStart))
      tmpFileExt[!dotStart] <- paste0(".", tmpFileExt)
    out$tmpfile <- Require::normPath(withr::local_tempfile(fileext = tmpFileExt))
  }
  withr::local_dir(tmpdir, .local_envir = pf)

  out <- append(out, list(tmpdir = tmpdir, tmpCache = tmpCache))
  list2env(out, envir = pf)
  return(out)
}

set.randomseed <- function(set.seed = TRUE) {
  digits <- 9
  newSeed <- as.numeric(Sys.time()) * 10^(digits - 3) # microseconds
  newSeed <- as.integer(round(newSeed, -digits) - newSeed)
  if (isTRUE(set.seed))
    set.seed(newSeed)
  return(invisible(newSeed))
}
