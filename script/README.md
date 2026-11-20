This folder contains the R script for generating ecological distance matrices.

Run from the repository root with the R packages `readxl` and `pbapply` installed:

```sh
Rscript script/create_ed_matrix.R
```

The script reads `input/NiN_3.0.1_Master_Clean.xlsx` (the `Typer` sheet) and
`input/NiN3_categorical_variables.csv`, and writes both scales to
`output/ed_matrix_005.csv` and `output/ed_matrix_020.csv`.

Definitions ending in `(‒)` or `(-)` mean “and lower”. The script expands
these from `0` through the indicated letter, for example `SA_e(‒)` becomes
`SA_0abcde`, `TV_i(‒)` becomes `TV_0abcdefghi`, and `SSi(‒)` becomes
`SS_0abcdefghi`.
