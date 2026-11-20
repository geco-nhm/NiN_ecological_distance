# Run from the repository root: Rscript tests/test_type_profiles.R
source("script/ed_functions.R")
source("script/type_profiles.R")
expect_error <- function(expr) stopifnot(inherits(tryCatch({force(expr); NULL}, error = identity), "error"))

# LC01-05 contains two tokens, not a categorical KA value or a KA_c override.
stopifnot(identical(clean.lec.definitions(c("[KA_w] VT_C", "KA_ab, [KA_w]", "[KA_w], VT_C", "KA_ab", NA)),
                    c("VT_C", "KA_ab", "VT_C", "KA_ab", NA_character_)))

# The extracted position calculation retains the existing spacing and matching rule.
stopifnot(isTRUE(all.equal(gradient.positions(c("ab", "c"), c("a", "ab", "bc", NA)),
                          c(0.8, 1, 1.6, NA))),
          is.na(gradient.positions(c("A", "B"), "A")),
          is.nan(gradient.positions(c("a", "b"), "z")))
expect_error(gradient.positions(c("a", "b"), "az", require_all = TRUE))
stopifnot(is.nan(compute.ed(c("a", "b"), "A", "a")),
          is.nan(compute.ed(c("a", "b"), "", "a")),
          compute.ed(c("a", "b"), "bC", "a") == 1)

reference <- create.shared.reference(c(KA = "abc", ST = "0AB", UF = "ac"))
stopifnot(identical(reference$basic_step[reference$category == "UF"], c("a", "c")),
          identical(reference$position[reference$category == "UF"], 1:2),
          !"ST" %in% reference$category)
units <- data.frame(code = c("A01-01", "A01-02", "B01-01"), mt = c("A01", "A01", "B01"))
lecs <- data.frame(KA = c("ab", "bc", "ab"), ST = c("0", "A", "B"))
sources <- data.frame(KA = c("individual", "major_type", "major_type_group"), ST = "individual")
criteria <- data.frame(code = units$code, mt = units$mt, above_forest_line = c(0, NA, 1))
steps <- list(list(KA = c("a", "b", "c"), ST = c("0", "A", "B")),
              list(KA = c("a", "b", "c"), ST = c("0", "A", "B")),
              list(KA = c("ab", "c"), ST = c("0", "A", "B")))
minor <- matrix(3, 2, 2, dimnames = list(c("A01", "B01"), c("KA", "ST")))
mapping <- minor
mapping["B01", "KA"] <- 2
profiles <- create.type.profiles(units, lecs, sources, criteria, steps, reference, minor, mapping)
ka <- profiles[profiles$category == "KA", ]
stopifnot(isTRUE(all.equal(ka$position_shared, c(1.5, 2.5, 1.5))),
          isTRUE(all.equal(ka$position_major_type, c(1.5, 2.5, 1))),
          isTRUE(all.equal(ka$major_type_adjustment, c(1, 1, 1.5))),
          identical(ka$source, sources$KA), all(ka$kind == "gradient"),
          ka$major_type_steps[3] == "ab|c")
# Same letters give the same shared position despite different local partitions.
stopifnot(ka$position_shared[1] == ka$position_shared[3],
          ka$position_major_type[1] != ka$position_major_type[3])
# Within one major type, position differences reproduce the gradient ED.
stopifnot(abs(diff(ka$position_major_type[1:2])) == compute.ed(steps[[1]]$KA, "ab", "bc"))
stopifnot(all(is.na(profiles$position_shared[profiles$category == "ST"])),
          all(profiles$kind[profiles$category == "ST"] == "factor"))

labels <- c(first = "B01-01", second = "A01-01", third = "B01-01", missing = NA)
stopifnot(isTRUE(all.equal(reclassify.types(labels, "KA", profiles),
                          c(first = 1.5, second = 1.5, third = 1.5, missing = NA))),
          isTRUE(all.equal(reclassify.types(labels, "KA", profiles, reference = "major_type"),
                          c(first = 1, second = 1.5, third = 1, missing = NA))),
          identical(unname(reclassify.types(labels, "KA", profiles, representation = "range")),
                    c("ab", "ab", "ab", NA_character_)),
          identical(unname(reclassify.types(units$code, "ST", profiles, representation = "range")),
                    c("0", "A", "B")),
          isTRUE(all.equal(reclassify.types(units$code, "above_forest_line", profiles), c(0, NA, 1))),
          length(reclassify.types(character(), "KA", profiles)) == 0)
expect_error(reclassify.types("unknown", "KA", profiles))
expect_error(reclassify.types(units$code, "unknown", profiles))
expect_error(reclassify.types(units$code, "KA", rbind(profiles, profiles)))
expect_error(create.type.profiles(units, lecs, sources, criteria[3:1, ], steps, reference, minor, mapping))

# Provenance follows aggregation membership, including exclusions and mixed sources.
base_units <- data.frame(mt = c("A01", "A01", "A01", "B01"), scale_005 = c("01", "01", "#", "01"))
base_sources <- data.frame(KA = c("individual", "major_type", "major_type_group", "major_type_group"),
                            UF = c(NA, NA, "individual", "major_type"))
a <- aggregate.profile.sources(base_units, base_sources, "scale_005", "A01")
b <- aggregate.profile.sources(base_units, base_sources, "scale_005", "B01")
stopifnot(a[1, "KA"] == "individual;major_type", is.na(a[1, "UF"]),
          b[1, "KA"] == "major_type_group", b[1, "UF"] == "major_type")
# An invalid uppercase KA entry must never turn KA into a factor variable.
bad_ka <- lecs
bad_ka$KA[1] <- "C"
bad_steps <- steps
bad_steps[[1]]$KA <- bad_steps[[2]]$KA <- c("a", "C", "bc")
flagged <- create.type.profiles(units, bad_ka, sources, criteria, bad_steps, reference, minor, mapping)
flagged <- flagged[flagged$category == "KA", ]
stopifnot(all(flagged$kind == "gradient"), flagged$shared_status[1] == "invalid_range",
          flagged$major_type_status[2] == "invalid_reference",
          is.na(flagged$position_shared[1]), is.finite(flagged$position_shared[2]))

# Both options survive the analyst-facing CSV round trip.
f <- tempfile(fileext = ".csv")
write.csv(profiles, f, row.names = FALSE)
roundtrip <- read.csv(f)
stopifnot(isTRUE(all.equal(reclassify.types(labels, "KA", roundtrip), reclassify.types(labels, "KA", profiles))))
unlink(f)
cat("All type profile tests passed\n")
