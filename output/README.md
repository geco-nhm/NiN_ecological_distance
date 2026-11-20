For practical analyst examples, see [Using the outputs](../USING_OUTPUTS.md).

## Outputs

This folder contains ecological distance matrices, decomposition tables,
reclassification profiles, and component inventories for the minor-type and mapping levels:

| Suffix | Level |
| --- | --- |
| `gt` | Minor types (908) |
| `005` | 1:5,000 mapping units (673) |
| `020` | 1:20,000 mapping units (405) |
| `050` | 1:50,000 mapping units (242) |

The files are named `ed_matrix_<level>.csv.gz`, `ed_decomposition_<level>.csv.gz`,
`type_profiles_<level>.csv.gz`, and `ed_components_<level>.csv.gz`. Inputs are
`input/NiN_3.0.1_Master_Clean.xlsx` and `input/NiN3_categorical_variables.csv`.
Distances in matrices are rounded to one decimal place.

The decomposition files explain every ordered type pair as additive
contributions. Columns are `type_a`, `type_b`, `component`, `contribution`,
`definition_a`, and `definition_b`. Defining-LEC differences are split by LEC
(e.g. `defining_LEC:KA`), with the two definitions or `absent` recorded alongside.
Any joint correlation correction is a separate negative adjustment; all signed
contributions must be summed. Nonzero terms retain precision; pairs without any
nonzero terms have a `no_difference` row. There are no total rows to double-count. See
`script/README.md` for lookup examples and `Documentation.md` for component rules.

The reclassification lookup files contain letter ranges, provenance, categorical values,
and both local major-type and shared numerical gradient positions. Choose the
reference in `reclassify.types()`; see `script/README.md` for examples.
`shared/lec_reference_steps.csv.gz` defines the shared basic-step ranks for all levels.
These ranks are ordinal positions, not physical measurements or total ED.

The current outputs incorporate the LC01-05 parsing repair: `[KA_w] VT_C` is
interpreted as separate tokens, with the excluded KA_w removed and VT_C retained.

Component inventories list the terms actually present at each level, grouped by
family, with their associated LECs and row counts. They support choosing the
components for future ht and htg calculations, which now use defining information only.

All generated CSVs are gzip-compressed. Files are organized in `output/005/`,
`output/020/`, `output/050/`, and `output/gt/`; the shared reference is in
`output/shared/`. R reads these files directly:

```r
parts <- read.csv("output/gt/ed_decomposition_gt.csv.gz")
```

Compression saves disk space and download bandwidth. Some spreadsheet and GIS
applications require extracting the CSV first. Reading a compressed file adds
some decompression work; loaded tables use the same memory as plain CSVs.

### Major types and major-type groups

`output/ht/` contains 124 major types; `output/htg/` contains six major-type
groups. Both folders contain a matrix, decomposition, profile, and component
inventory as `.csv.gz`. These calculations exclude minor-type hLECs. Major types
use their defining LECs and categorical criteria; groups use their defining LECs
with criteria unknown. The existing defining-set penalties and no-shared-LEC
rule apply. These are direct calculations, not averages of minor-type distances.

To generate only these levels, run:

```sh
Rscript script/create_ed_matrix.R --levels=ht,htg
```

The helper scripts use commented sections and named intermediate objects, following
the structure of `create_ed_matrix.R`.

### Hierarchy naming

`ht` means major type (hovedtype), and `htg` means major-type group
(hovedtypegruppe).
