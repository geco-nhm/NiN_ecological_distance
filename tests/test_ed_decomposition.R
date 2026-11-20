# Run from the repository root: Rscript tests/test_ed_decomposition.R
source("script/ed_functions.R")
source("script/ed_decomposition.R")

fixture <- function(lecs = c("KA", "UF"), between = FALSE) {
  ht <- if (between) c("A", "B") else c("A", "A")
  codes <- c("A-01", if (between) "B-01" else "A-02")
  units <- data.frame(code = codes, ht = ht)
  lec <- matrix(rep(c("a", "b"), length(lecs)), 2, dimnames = list(codes, lecs))
  steps <- setNames(rep(list(c("a", "b")), length(lecs)), lecs)
  hlec <- rep(list(lecs), 2)
  criteria <- data.frame(code = codes, ht = ht, deposits_present = 1,
                         vegetated = 1, above_forest_line = 0,
                         ssg_helophytes = 0, ssg_trees = 0, ssg_removed = 0)
  counts <- matrix(2, length(unique(ht)), length(lecs),
                    dimnames = list(unique(ht), lecs))
  list(units = units, lec = lec, ht_specific_list = list(steps, steps),
       hlec_dlec = hlec, hlec = hlec, dlec = list(character(), character()),
       dlec_without = list(character(), character()), criteria_data = criteria,
       correlation_lec = list(c("HA", "HM", "HH"), c("HR", "HH")),
       ht_specific_number_minor_types = counts, ht_specific_number_mu = counts)
}
explain <- function(x) {
  scalar <- do.call(compute.ed.ultimate, c(list(row = 1, col = 2), x))
  result <- do.call(compute.ed.ultimate, c(list(row = 1, col = 2), x, list(decompose = TRUE)))
  stopifnot(isTRUE(all.equal(result$total, scalar)),
            isTRUE(all.equal(sum(result$contributions), scalar)),
            !is.null(names(result$contributions)))
  result$contributions
}

x <- fixture()
stopifnot(identical(explain(x), c(KA = 1, UF = 1)))
x$ht_specific_number_minor_types[,] <- 4
stopifnot(identical(explain(x), c(KA = 2, UF = 2)))
x$lec[,] <- "a"
stopifnot(identical(explain(x), c(no_difference = 0)))
x$lec[,] <- NA_character_
stopifnot(identical(explain(x), c(no_difference = 0)))
x <- fixture("KA")
x$lec[, 1] <- c("A", "B")
x$ht_specific_list <- rep(list(list(KA = c("A", "B"))), 2)
stopifnot(identical(explain(x), c(KA = 1)))

# Overlapping correlation groups consume HH once, and retain other LECs.
x <- fixture(c("KA", "HA", "HM", "HH", "HR"))
z <- explain(x)
stopifnot(z[["correlated_LECs:HA+HM+HH"]] == 1,
          z[["correlated_LECs:HR"]] == 1, z[["KA"]] == 1, sum(z) == 3)
x <- fixture(c("HA", "HM"))
stopifnot(identical(explain(x), c("correlated_LECs:HA+HM" = 1)))

x <- fixture(between = TRUE)
x$criteria_data$above_forest_line <- c(0, 1)
x$criteria_data$ssg_helophytes <- c(0, 1)
z <- explain(x)
stopifnot(z[["above_forest_line"]] == 1, z[["ssg_helophytes"]] == 1)
x$criteria_data$above_forest_line <- c(NA, 1)
x$criteria_data$ssg_trees <- c(1, 0)
z <- explain(x)
stopifnot(!"above_forest_line" %in% names(z), !"ssg_helophytes" %in% names(z), z[["ssg_trees"]] == 1)
x$criteria_data$ssg_removed <- c(1, 0)
z <- explain(x)
stopifnot(!"ssg_trees" %in% names(z), z[["ssg_removed"]] == 1)
x <- fixture(between = TRUE)
x$dlec <- list("KA_a", "KA_b")
stopifnot(explain(x)[["defining_LEC:KA"]] == 2)
x$hlec_dlec <- list("KA", "UF")
x$hlec <- list("KA", "UF")
stopifnot(explain(x)[["no_shared_LECs"]] == 1)

# Defining-LEC attribution retains definitions and the original set-count penalty.
y <- fixture(between = TRUE)
y$dlec <- list(c("KA_ab", "TV_cde", "UF_a"), c("TV_fgh", "UF_a", "SS_jk"))
z <- explain(y)
stopifnot(z[["defining_LEC:KA"]] == 1, z[["defining_LEC:TV"]] == 2,
          z[["defining_LEC:SS"]] == 1, !"defining_LEC:UF" %in% names(z))
r <- do.call(compute.ed.ultimate, c(list(row = 1, col = 2), y, list(decompose = TRUE)))
d <- r$definitions
stopifnot(d$definition_a[d$component == "defining_LEC:KA"] == "KA_ab",
          d$definition_b[d$component == "defining_LEC:KA"] == "absent",
          d$definition_a[d$component == "defining_LEC:TV"] == "TV_cde",
          d$definition_b[d$component == "defining_LEC:TV"] == "TV_fgh")
reverse <- do.call(compute.ed.ultimate, c(list(row = 2, col = 1), y, list(decompose = TRUE)))
stopifnot(reverse$definitions$definition_a[reverse$definitions$component == "defining_LEC:KA"] == "absent")
# Test the correction explicitly even when no current workbook pair triggers it.
z <- decompose.defining.lecs("KA_ab", "KA_cd", adjusted_total = 0)
stopifnot(z$contributions[["defining_LEC:KA"]] == 2,
          z$contributions[["defining_LEC_correlation_adjustment"]] == -2,
          sum(z$contributions) == 0)
stopifnot(length(decompose.defining.lecs("KA_ab", "KA_ab", 0)$contributions) == 0)

# Round trip through the public export and lookup interface.
x <- fixture()
dir <- tempfile("ed-test-")
dir.create(dir)
matrix_path <- file.path(dir, "matrix.csv")
table_path <- file.path(dir, "table.csv")
do.call(write.ed.outputs, c(x, list(matrix_path = matrix_path, decomposition_path = table_path)))
table <- read.csv(table_path)
m <- as.matrix(read.csv(matrix_path, row.names = 1, check.names = FALSE))
stopifnot(isTRUE(all.equal(unname(m), matrix(c(0, 2, 2, 0), 2))))
r <- lookup.ed.decomposition(c("A-02", "A-01", "A-02", "A-01"),
                             c("A-01", "A-01", "A-01", "A-02"), table)
stopifnot(identical(unique(r$pair_id), 1:4),
          identical(as.numeric(tapply(r$contribution, r$pair_id, sum)), c(2, 0, 2, 2)),
          all(r$type_a[r$pair_id == 1] == "A-02"))
stopifnot(nrow(lookup.ed.decomposition(character(), character(), table)) == 0)
expect_error <- function(expr) stopifnot(inherits(tryCatch({force(expr); NULL}, error = identity), "error"))
expect_error(lookup.ed.decomposition("unknown", "A-01", table))
expect_error(lookup.ed.decomposition(c("A-01", "A-02"), "A-01", table))
expect_error(lookup.ed.decomposition(NA_character_, "A-01", table))
expect_error(lookup.ed.decomposition("A-01", "A-02", data.frame()))
# A failed calculation must not replace previously generated outputs.
checksums <- tools::md5sum(c(matrix_path, table_path))
bad <- x
bad$ht_specific_number_minor_types[,] <- Inf
expect_error(do.call(write.ed.outputs, c(bad, list(matrix_path = matrix_path, decomposition_path = table_path))))
stopifnot(identical(checksums, tools::md5sum(c(matrix_path, table_path))),
          length(list.files(dir)) == 2L)
if (.Platform$OS.type != "windows") {
  do.call(write.ed.outputs, c(x, list(matrix_path = matrix_path,
                                     decomposition_path = table_path, workers = 2L)))
  stopifnot(identical(read.csv(table_path), table))
}
expect_error(do.call(write.ed.outputs, c(x, list(matrix_path = matrix_path,
                                                decomposition_path = table_path, workers = Inf))))
# Compressed export remains available when explicitly requested.
gzip_path <- file.path(dir, "table.csv.gz")
do.call(write.ed.outputs, c(x, list(matrix_path = matrix_path, decomposition_path = gzip_path)))
stopifnot(identical(read.csv(gzip_path), table))
# Definitions survive export and paired lookup, with direction preserved.
do.call(write.ed.outputs, c(y, list(matrix_path = matrix_path, decomposition_path = table_path)))
r <- lookup.ed.decomposition(c("A-01", "B-01"), c("B-01", "A-01"), read.csv(table_path))
k <- r$component == "defining_LEC:KA"
stopifnot(identical(r$definition_a[k], c("KA_ab", "absent")),
          identical(r$definition_b[k], c("absent", "KA_ab")))
unlink(dir, recursive = TRUE)
cat("All decomposition tests passed\n")
