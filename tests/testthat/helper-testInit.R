testInit <- function(libraries = character(), ask = FALSE, verbose, tmpFileExt = "",
                     opts = NULL, dev = FALSE, envirHere = parent.frame()) {

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
      suppressWarnings(lapply(libraries, withr::local_package, .local_envir = pf))
    }
  }

  out <- list()

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

  desc <- get("desc", whereInStack("desc"))
  desc <- gsub(" ", "_", desc)
  counter <- 0
  out <- append(
    out,
    list(
      tmpdir = tmpdir, tmpCache = tmpCache,
      desc = desc, counter = counter,
      envirHere = envirHere
    )
  )
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

# vers <- c("4.4", "4.3.99")
lower <- c("4.4", "4.1")
upper <- c("5.10", "4.3.99")
# ineqV <- c(">=", "<=")
df <- data.frame(lower = lower, upper = upper)

compareVersionW <- function(ineqV, verOrig, vers) {
  eval(parse(text = paste0("`", ineqV[match(verOrig, vers)], "`")))(getRversion(), verOrig)
}

filName <- function(df, verRow, tmpdir, prevLastPlotNumber, fn) {
  lower <- df$lower[verRow]
  upper <- df$upper[verRow]
  ver <- paste0("_", lower, "_to_", upper)
  ver <- gsub("\\.", "_", paste0("_", ver))
  fil <- file.path(tmpdir, paste0("test", prevLastPlotNumber + fn, ver, ".png"))
}

oses <- c("Win", "Linux")

fn <- function(tmpdir, desc, counter, os, envir = parent.frame()) {
  fil <- file.path(tmpdir, paste0(desc, counter, os, ".png"))
  counter <- counter + 1
  assign("counter", counter, envir = envir)
  fil
}

correctOS <- function(os)
  (os %in% "Win" && isWindows()) || os %in% "Linux" && isLinux()
