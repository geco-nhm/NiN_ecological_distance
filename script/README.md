This folder contains the R script for generating ecological distance matrices.

Run from the repository root with the R packages `readxl` and `pbapply` installed:

```sh
Rscript script/create_ed_matrix.R
```

The script reads `input/NiN_3.0.1_Master_Clean.xlsx` (the `Typer` sheet) and
`input/NiN3_categorical_variables.csv`, and writes both scales to
`output/ed_matrix_005.csv` and `output/ed_matrix_020.csv`, plus
`output/ed_decomposition_005.csv` and `output/ed_decomposition_020.csv`.
The function definitions live in `ed_functions.R`; exporting and paired-vector
lookup live in `ed_decomposition.R`.

Definitions ending in `(‒)` or `(-)` mean “and lower”. The script expands
these from `0` through the indicated letter, for example `SA_e(‒)` becomes
`SA_0abcde`, `TV_i(‒)` becomes `TV_0abcdefghi`, and `SSi(‒)` becomes
`SS_0abcdefghi`.

Each decomposition file contains `type_a`, `type_b`, `component`, `contribution`,
`definition_a`, and `definition_b`. Both pair directions are exported. Zero
contributions are omitted; a pair with no nonzero terms has one `no_difference`
row with contribution 0.

Defining-LEC differences are labelled by LEC, for example `defining_LEC:KA`.
The two definition columns show each type's defining range, or `absent` if the
LEC is only defining for the other type; they are missing for other components.
Different ranges for the same defining LEC count as two distinct definitions
(contribution 2), rather than a gradient distance. Any joint correction is a
negative `defining_LEC_correlation_adjustment` row. Include it when summing.
Totals are kept in the distance matrix, rather than repeated as table rows.
Contributions retain precision; the matrices round their totals to one decimal.

Read a table and explain corresponding elements of two vectors:

```r
source("script/ed_decomposition.R")
parts <- read.csv("output/ed_decomposition_005.csv")
explanation <- lookup.ed.decomposition(
  type_a = c("TA01-01", "TA01-01"),
  type_b = c("TA01-02", "TB01-01"),
  decomposition = parts
)
explanation
aggregate(contribution ~ pair_id, explanation, sum)
```

The returned `pair_id` identifies the original vector position, including repeated
pairs. Input vectors must have equal lengths and contain no missing codes; an
unknown pair raises an error. Use a table from the same mapping scale as the maps.

On Unix-like systems, set `NIN_WORKERS` to calculate batches in parallel:

```sh
NIN_WORKERS=8 Rscript script/create_ed_matrix.R
```

The default is one worker; use that default on Windows. The table is streamed in
batches to keep memory use bounded. Temporary output files are only installed
after a scale finishes calculating and every pair passes the additivity check.

Run the focused tests with `Rscript tests/test_ed_decomposition.R`. After generating
outputs, run `Rscript tests/check_generated_outputs.R` to check every exported pair.
