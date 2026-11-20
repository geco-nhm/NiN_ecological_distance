# Run from the repository root after generating type profiles.
source("script/ed_functions.R")
source("script/type_profiles.R")
reference <- read.csv("output/lec_reference_steps.csv")
stopifnot(!anyDuplicated(paste(reference$category, reference$basic_step)),
          all(reference$basic_step %in% c("0", letters)))
for (category in unique(reference$category)) {
  x <- reference[reference$category == category, ]
  stopifnot(identical(x$position, seq_len(nrow(x))),
            identical(x$basic_step, c("0", letters)[c("0", letters) %in% x$basic_step]))
}
for (scale in c("005", "020")) {
  profiles <- read.csv(paste0("output/type_profiles_", scale, ".csv"))
  matrix <- read.csv(paste0("output/ed_matrix_", scale, ".csv"), row.names = 1, check.names = FALSE)
  stopifnot(!anyDuplicated(paste(profiles$type, profiles$category)),
            setequal(unique(profiles$type), rownames(matrix)),
            all(profiles$kind %in% c("gradient", "factor", "criterion")),
            all(is.na(profiles$position_shared) | is.finite(profiles$position_shared)),
            all(is.na(profiles$position_major_type) | is.finite(profiles$position_major_type)))
  for (category in unique(profiles$category)) {
    x <- profiles[profiles$category == category, ]
    stopifnot(setequal(x$type, rownames(matrix)))
    ok <- x$shared_status == "ok"
    if (any(ok)) {
      steps <- reference$basic_step[reference$category == category]
      expected <- gradient.positions(steps, x$range[ok], require_all = TRUE)
      stopifnot(isTRUE(all.equal(expected, x$position_shared[ok], tolerance = 1e-12)))
    }
    for (i in which(x$major_type_status == "ok")) {
      steps <- strsplit(x$major_type_steps[i], "|", fixed = TRUE)[[1]]
      expected <- gradient.positions(steps, x$range[i], require_all = TRUE)
      stopifnot(isTRUE(all.equal(expected, x$position_major_type[i], tolerance = 1e-12)))
    }
  }
  gradient <- profiles$kind == "gradient"
  factor <- profiles$kind == "factor"
  criterion <- profiles$kind == "criterion"
  stopifnot(all(is.na(profiles$position_shared[factor | criterion])),
            all(is.na(profiles$position_major_type[factor | criterion])),
            all(!is.na(profiles$source[gradient & !is.na(profiles$range)])),
            all(is.na(profiles$position_shared[gradient & is.na(profiles$range)])),
            all(profiles$kind[profiles$category == "KA"] == "gradient"),
            !any(grepl("[A-Z]", na.omit(profiles$range[profiles$category == "KA"]))))
  # The malformed LC01 row must not become a fabricated KA_c or a KA factor.
  if (scale == "005") {
    ka <- profiles[profiles$type == "LC01-05" & profiles$category == "KA", ]
    vt <- profiles[profiles$type == "LC01-05" & profiles$category == "VT", ]
    stopifnot(nrow(ka) == 1, is.na(ka$range), is.na(ka$source),
              is.na(ka$position_shared), is.na(ka$position_major_type),
              vt$range == "C", vt$kind == "factor", vt$source == "individual")
  }
  labels <- c(rownames(matrix)[1], NA, rownames(matrix)[1])
  for (option in c("shared", "major_type")) {
    result <- reclassify.types(labels, "KA", profiles, reference = option)
    stopifnot(length(result) == 3, is.na(result[2]), identical(result[1], result[3]))
  }
  cat(scale, ": checked ", nrow(profiles), " profile rows for ", nrow(matrix), " types\n", sep = "")
}
