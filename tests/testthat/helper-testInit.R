testInit <- function(libraries = character(), ask = FALSE, verbose, tmpFileExt = "",
                     opts = NULL, dev = FALSE) {

  set.randomseed()

  pf <- parent.frame()

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

  out <- list()
  withr::local_options("reproducible.ask" = ask, .local_envir = pf)
  if (!missing(verbose))
    withr::local_options("reproducible.verbose" = verbose, .local_envir = pf)
  if (!is.null(opts))
    withr::local_options(opts, .local_envir = pf)
  td <- normalizePath(file.path(tempdir(), substr(basename(tempfile()), 5, 16)),
                      mustWork = FALSE, winslash = "/")
  tmpdir <- withr::local_tempdir(tmpdir = td, .local_envir = pf)
  tmpCache <- withr::local_tempdir(tmpdir = tmpdir, .local_envir = pf)
  if (isTRUE(any(nzchar(tmpFileExt)))) {
    dotStart <- startsWith(tmpFileExt, ".")
    if (any(!dotStart))
      tmpFileExt[!dotStart] <- paste0(".", tmpFileExt)
    out$tmpfile <- withr::local_tempfile(fileext = tmpFileExt)
  }
  withr::local_dir(tmpdir, .local_envir = pf)

  out <- append(out, list(tmpdir = tmpdir, tmpCache = tmpCache))
  list2env(out, envir = pf)
  if (isTRUE(dev) && interactive() && !isRstudioServer())
    dev()
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
