For practical analyst examples, see [Using the outputs](../USING_OUTPUTS.md).

This folder contains the R script for generating ecological distance matrices.

Run from the repository root with the R packages `readxl` and `pbapply` installed:

```sh
Rscript script/create_ed_matrix.R
```

The script reads `input/NiN_3.0.1_Master_Clean.xlsx` (the `Typer` sheet) and
`input/NiN3_categorical_variables.csv`. It exports these levels:

| Suffix | Level |
| --- | --- |
| `005` | 1:5,000 mapping units (673 units) |
| `020` | 1:20,000 mapping units (405 units) |
| `050` | 1:50,000 mapping units (242 units) |
| `gt` | Minor types, retaining their input type codes (908 types) |

For each suffix `<level>`, it writes `output/<level>/ed_matrix_<level>.csv.gz`,
`output/<level>/ed_decomposition_<level>.csv.gz`, `output/<level>/type_profiles_<level>.csv.gz`,
and `output/<level>/ed_components_<level>.csv.gz`.
Level preparation lives in `ed_levels.R`. The function definitions live in
`ed_functions.R`; exporting and paired-vector
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
parts <- read.csv("output/005/ed_decomposition_005.csv.gz")
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
unknown pair raises an error. Use a table from the same mapping level as the maps. The same short code can
represent different ecological ranges at different levels; do not mix levels.

On Unix-like systems, set `NIN_WORKERS` to calculate batches in parallel:

```sh
NIN_WORKERS=8 Rscript script/create_ed_matrix.R
```

The default is one worker; use that default on Windows. The table is streamed in
batches to keep memory use bounded. Temporary output files are only installed
after a scale finishes calculating and every pair passes the additivity check.

Run the focused tests with `Rscript tests/test_ed_decomposition.R`. After generating
outputs, run `Rscript tests/check_generated_outputs.R` to check every exported pair.

Type profiles for reclassification are written to `output/<level>/type_profiles_<level>.csv.gz`.
Read the file for the mapping level of your
vector. Each type has a row for every LEC and categorical criterion, including
undefined values. `range` retains the basic-step letters; `source` identifies
`individual`, `major_type`, or `major_type_group`. Aggregated mapping units can
combine sources, separated by `;`. Categorical criteria have source
`major_type_criteria` and retain their numeric `value` (including meaningful NA).

Two numerical reference options are available for gradient LECs:

- `reference = "shared"` (default): mean of the 1-based ranks of the observed
  basic steps, ordered `0`, `a`, ..., `z`. Only observed steps receive ranks;
  gaps in the alphabet do not insert unobserved steps. The reference is based on
  the full minor-type profiles and is identical for all levels. Its
  explicit mapping is saved as `output/shared/lec_reference_steps.csv.gz`.
- `reference = "major_type"`: mean position using the type's own major-type
  adapted step partitions and the existing `convert.bs()` offsets. These
  positions are local to each major type. `major_type_steps` records the
  reference partitions separated by `|`. `major_type_adjustment` records the
  additional scale adjustment used in ED; it is not applied to the positions.
  For one gradient within a major type, the ED contribution is the absolute
  position difference multiplied by this adjustment, before correlation rules.

Neither reference gives a physical measurement or a complete reconstruction of
between-major-type ED. Factor levels retain letters and have no numeric position.
Status columns identify undefined ranges, factor values, or invalid references;
KA is explicitly treated as a gradient. Missing map cells remain missing, unknown
type codes raise an error, and vector order, names, and repeated labels are kept.

```r
source("script/type_profiles.R")
profiles <- read.csv("output/005/type_profiles_005.csv.gz")
types <- c("TA01-01", "TA01-02", "TB01-01", NA)

reclassify.types(types, "KA", profiles, reference = "shared")
reclassify.types(types, "KA", profiles, reference = "major_type")
reclassify.types(types, "KA", profiles, representation = "range")
reclassify.types(types, "above_forest_line", profiles)
```

Use `representation = "range"` for factor LECs; requesting numeric positions for
them returns NA. Criteria such as `above_forest_line` always return their input
numeric values in position mode, independent of the reference option. No pairwise
SSG suppression or other distance rules are applied to these type attributes.

To generate only the lookup files, without recalculating pairwise distances:

```sh
Rscript script/create_ed_matrix.R --profiles-only
```

Run `Rscript tests/test_type_profiles.R` for focused lookup tests and
`Rscript tests/check_generated_profiles.R` to check the generated tables.

The import repairs LC01-05's malformed `[KA_w] VT_C` separator and applies the
existing exclusion of `[KA_w]` as a whole token. LC01-05 consequently has VT_C
and no KA value. The Excel source itself is preserved. This correction affects
some distance/decomposition entries compared with the previous parser.

Select levels to generate, optionally without pairwise calculation:

```sh
NIN_WORKERS=8 Rscript script/create_ed_matrix.R --levels=050,gt
Rscript script/create_ed_matrix.R --profiles-only --levels=050,gt
```

All levels retain the existing ground/bottom-system scope of the input filter.
At `gt`, the input minor-type ranges and lists are used directly; the ED scale
adjustment is 1 for defined gradients. At mapping levels, minor types are grouped
using the workbook's scale column. Missing aggregation codes and codes containing
`*` or `#` are excluded. Major types without valid mapping units at a level are
omitted. References are calculated once per major type and LEC using the same
step-selection, overlap-removal, and border-step rules as previously.

The component inventory for each level lists the observed `component`, its
`family`, associated `lec_codes` (when applicable), and exported `row_count`.
Families are `lec`, `correlated_lecs`, `defining_lec`, `criterion`, `adjustment`,
`penalty`, and `zero`. These inventories are a basis for choosing which parts
should contribute to future major-type (`ht`) and major-type-group (`htg`)
distances. The ht and htg outputs use defining information only; see below. A joint correlation
adjustment cannot automatically be assigned to an arbitrary subset of LECs.

Run `Rscript tests/test_ed_levels.R` to check level preparation, exclusions, minor
references, and component inventories. The generated-output checks cover every
supported level.

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
(hovedtypegruppe). These replace the former major-type abbreviations identifiers in the R
code, criteria input header, output folders, and filenames. Profile fields using
the English name `major_type` retain their spelling. Generalisation conversion
lookups are not provided by this repository.
