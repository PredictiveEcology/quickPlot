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
  fp <- fingerprints[test_id == id & r_version == rversion & sys_name == sysname, ][["value"]]

  ## get most recent fingerprint if none currently available for current R version
  if (length(fp) == 0) {
    vers <- tail(sort(fingerprints[test_id == id & sys_name == sysname, ][["r_version"]]), n = 1)
    fp <- fingerprints[test_id == id & r_version == vers & sys_name == sysname, ][["value"]]
  }

  return(fp)
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

aPoly <- function() {
  structure(list(
    geometry = structure(
      list(structure(
        list(structure(
          c(-78.5773568721957,
            -78.4561010073263, -78.3360252426567, -78.2942630021489, -78.3087626980844,
            -78.758222304854, -78.9568189310093, -79.0086200473651, -79.0109596538855,
            -78.0538331643863, -77.2484311046662, -76.8903159886873, -76.7509970987552,
            -77.3931410543151, -78.1050429196269, -78.7872617201722, -79.3810972637791,
            -79.8070585571741, -80.4527353709026, -80.5841726534793, -80.8799943689937,
            -80.9317438584144, -81.1203604929234, -81.1387122079174, -81.1400338422818,
            -81.1789904060282, -81.1951836041997, -81.0184283168507, -80.9784870444358,
            -80.7357885043729, -80.2312521131021, -78.9889385386834, -78.5773568721957,
            60.5473898695047, 60.1320517279154, 59.8394427916515, 59.779794305308,
            59.7262969971236, 59.186738441412, 58.9375311364244, 58.7445765234304,
            58.4680122860819, 58.0586727878731, 57.6177645987749, 57.1264970545583,
            56.7729788021724, 56.8057542179419, 56.8559358467915, 56.9444652556184,
            57.0905500531872, 57.1527183805358, 57.138613143791, 57.583401690746,
            58.5511136373586, 58.6859454347735, 59.2443142591762, 59.2879068994951,
            59.531584345576, 60.1356872653678, 60.187486762055, 60.6518098855843,
            60.7146476897084, 60.8422295133282, 60.9662038192074, 61.1324454714526,
            60.5473898695047), dim = c(33L, 2L))),
        class = c("XY", "POLYGON", "sfg")),
        structure(list(structure(
          c(-76.7509970987552, -76.7721539460945,
            -76.8859467350662, -77.0085731907616, -77.6231528814494, -77.9014062336642,
            -78.1894421781098, -78.596791077421, -79.0131412496004, -79.5645373883873,
            -80.0393036061031, -80.2607755513472, -80.2961359413858, -80.3638242855291,
            -80.4466803908379, -80.4527353709026, -79.8070585571741, -79.158343499277,
            -78.7872617201722, -77.7859770434067, -76.7509970987552, 56.7729788021724,
            56.7249022551497, 56.3755255764812, 56.2139831220854, 55.6942835727817,
            55.4811250557031, 55.3489496063811, 55.2056577061681, 55.0875994559815,
            54.9918831715833, 54.852115851778, 55.3605880454647, 55.4102815025884,
            55.5887481856359, 56.3000911119485, 57.138613143791, 57.1527183805358,
            57.0451179494795, 56.9444652556184, 56.8254237282228, 56.7729788021724
          ), dim = c(21L, 2L))), class = c("XY", "POLYGON", "sfg"))), n_empty = 0L,
      class = c("sfc_POLYGON", "sfc"),
      precision = 0,
      bbox = structure(c(xmin = -81.1951836041997, ymin = 54.852115851778,
                         xmax = -76.7509970987552, ymax = 61.1324454714526), class = "bbox"),
      crs = structure(list(input = NA_character_, wkt = NA_character_), class = "crs"))),
    row.names = 1:2, sf_column = "geometry",
    agr = structure(integer(0), class = "factor",
                    levels = c("constant", "aggregate", "identity"), names = character(0)),
    class = c("sf", "data.frame"))
}
