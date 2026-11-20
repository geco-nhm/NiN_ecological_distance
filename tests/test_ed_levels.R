source("script/ed_functions.R")
source("script/ed_decomposition.R")
source("script/type_profiles.R")
source("script/ed_levels.R")
units <- data.frame(code = c("A01-01", "A01-02", "B01-01"), ht = c("A01", "A01", "B01"),
                    scale_005 = c("1", "2", "1"), scale_020 = c("1", "1", "1"),
                    scale_050 = c("1", "1", "#"))
lec <- data.frame(KA = c("a", "b", "c"))
criteria <- data.frame(code = units$code, ht = units$ht, above_forest_line = c(0, 0, 1),
                       ssg_helophytes = 0, ssg_trees = 0, ssg_removed = 0)
base <- list(units = units, lec = lec, sources = data.frame(KA = c("individual", "major_type", "individual")),
             criteria_data = criteria, hlec = rep(list("KA"), 3), hlec_dlec = rep(list("KA"), 3),
             dlec = rep(list(character()), 3), dlec_without = rep(list(character()), 3),
             correlation_lec = list(c("HA", "HM", "HH")))
steps <- c(KA = "abc")
gt <- prepare.ed.level("gt", base, steps)
stopifnot(identical(gt$units$code, units$code), identical(gt$lec, lec),
          identical(gt$sources, base$sources),
          identical(gt$ht_specific_number_mu, gt$ht_specific_number_minor_types))
five <- prepare.ed.level("005", base, steps)
twenty <- prepare.ed.level("020", base, steps)
fifty <- prepare.ed.level("050", base, steps)
stopifnot(nrow(five$units) == 3, nrow(twenty$units) == 2, nrow(fifty$units) == 1,
          fifty$lec$KA == "ab", fifty$sources[1, "KA"] == "individual;major_type",
          fifty$units$ht == "A01",
          fifty$ht_specific_number_minor_types["A01", "KA"] /
            fifty$ht_specific_number_mu["A01", "KA"] == 2)
# Identical constituents aggregate identically at 020 and 050 for the same major type.
stopifnot(identical(as.character(twenty$lec$KA[1]), as.character(fifty$lec$KA[1])))
# Reference preparation is equivalent to the previous per-unit preparation.
for (data in list(gt, five, twenty, fifty)) {
 for (i in seq_len(nrow(data$units))) {
  ht <- data$units$ht[i]
  old <- sort(as.character(remove.common.bs(find.ht.steps(data$lec[data$units$ht == ht, "KA"]))))
  expected <- if (length(old)) add.border.bs(steps["KA"], old) else NULL
  stopifnot(identical(data$ht_specific_list[[i]][["KA"]], expected))
 }
}
# Component inventories describe the same rows that were actually exported.
dir <- tempfile();dir.create(dir)
args <- gt[setdiff(names(gt), "sources")]
do.call(write.ed.outputs, c(args, list(matrix_path = file.path(dir, "matrix.csv.gz"),
                                     decomposition_path = file.path(dir, "parts.csv.gz"),
                                     component_path = file.path(dir, "components.csv.gz"))))
stopifnot(nrow(read.csv(file.path(dir, "matrix.csv.gz"), row.names = 1)) == nrow(gt$units))
parts <- read.csv(file.path(dir, "parts.csv.gz"))
catalogue <- read.csv(file.path(dir, "components.csv.gz"))
stopifnot(sum(catalogue$row_count) == nrow(parts), setequal(catalogue$component, parts$component),
          catalogue$family[catalogue$component == "KA"] == "lec",
          catalogue$family[catalogue$component == "above_forest_line"] == "criterion")
unlink(dir, recursive = TRUE)
cat("All level preparation and component inventory tests passed\n")

# Hierarchy profiles use definitions, independent of minor-type variation ----
base$units$htg <- c("A", "A", "B")
major_definitions <- list(A01 = "KA_a", B01 = "KA_b")
group_definitions <- list(A = "KA_ab", B = "KA_c")
major_criteria <- criteria[!duplicated(criteria$ht), setdiff(names(criteria), "code")]
major <- prepare.ed.hierarchy("ht", base, major_definitions, group_definitions, major_criteria, steps)
groups <- prepare.ed.hierarchy("htg", base, major_definitions, group_definitions, major_criteria, steps)
stopifnot(identical(major$lec$KA, c("a", "b")),
          identical(groups$lec$KA, c("ab", "c")),
          all(lengths(major$hlec) == 0), all(lengths(groups$hlec) == 0),
          all(is.na(groups$criteria_data[, -(1:2)])))
changed <- base
changed$lec$KA <- c("c", "c", "a")
stopifnot(identical(major, prepare.ed.hierarchy("ht", changed, major_definitions,
                                             group_definitions, major_criteria, steps)))
cat("All hierarchy preparation tests passed\n")

# Defining ranges and known criteria contribute; hLEC terms do not ----
calculate_pair <- function(data) {
  arguments <- data[setdiff(names(data), "sources")]
  arguments$units <- as.matrix(arguments$units)
  arguments$lec <- as.matrix(arguments$lec)
  arguments$criteria_data <- cbind(code = 0, ht = 0,
                                    as.matrix(arguments$criteria_data[, -(1:2)]))
  do.call(compute.ed.ultimate, c(list(row = 1, col = 2, decompose = TRUE), arguments))
}
major_pair <- calculate_pair(major)
group_pair <- calculate_pair(groups)
stopifnot(major_pair$total == 3, group_pair$total == 2,
          !"KA" %in% names(major_pair$contributions),
          "defining_LEC:KA" %in% names(major_pair$contributions))
