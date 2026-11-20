# Using the ecological-distance outputs

This guide is for analysts using the generated files. You do not need to run the
matrix-generation script. Download or clone the repository and run the examples
from its root folder. The examples use base R and the supplied helper scripts;
no additional R packages are needed to read and use the outputs.

## Choose the classification level

| Folder | Classification | Number of types |
| --- | --- | ---: |
| `output/gt/` | Minor types (grunntyper) | 908 |
| `output/005/` | Mapping units at 1:5,000 | 673 |
| `output/020/` | Mapping units at 1:20,000 | 405 |
| `output/050/` | Mapping units at 1:50,000 | 242 |
| `output/ht/` | Major types (hovedtyper) | 124 |
| `output/htg/` | Major-type groups (hovedtypegrupper) | 6 |

Use the files matching your map's classification level. The same short code can
have different ecological meanings at different levels. The repository does not
provide conversion between classification levels or distances between levels.
The outputs retain the current ground/bottom-system scope; coverage and ecological
rules for aquatic systems have limitations described in [Documentation.md](Documentation.md).

Each level has four gzip-compressed CSV files:

| Filename | Use |
| --- | --- |
| `ed_matrix_<level>.csv.gz` | Total ecological distance between two type codes |
| `ed_decomposition_<level>.csv.gz` | Additive contributions explaining each pair's distance |
| `ed_components_<level>.csv.gz` | Available component names, families, LEC codes, and row counts |
| `type_profiles_<level>.csv.gz` | Each type's ecological ranges, positions, and criteria |

R reads `.csv.gz` directly. Some spreadsheet and GIS applications require
extracting the CSV first. Compressed files use less disk space; once loaded,
tables need the same memory as an uncompressed CSV. Load only the tables needed
for your analysis, especially for the large decomposition tables.

## Read the files

Choose one level for the following examples:

```r
level <- "020"
folder <- file.path("output", level)

ed <- as.matrix(read.csv(
  file.path(folder, paste0("ed_matrix_", level, ".csv.gz")),
  row.names = 1, check.names = FALSE
))
profiles <- read.csv(file.path(folder, paste0("type_profiles_", level, ".csv.gz")))
components <- read.csv(file.path(folder, paste0("ed_components_", level, ".csv.gz")))

# Inspect valid type codes and available ecological attributes.
head(rownames(ed))
unique(profiles$category)
components
```

The matrix's first column supplies its row names. `check.names = FALSE` keeps
type codes unchanged. Do not convert the matrix to numeric without preserving
its row and column names.

## Get distances between two vectors

For two maps, first align the observations: each vector position must refer to
the same location, polygon comparison, or sample. Spatial alignment and matching
are separate steps; the lookup uses only the supplied type codes.

This runnable example takes valid codes from the selected matrix. Replace the
two vectors with your map columns, expressed as character type codes.

```r
types_a <- rownames(ed)[c(1, 2, 1)]
types_b <- rownames(ed)[c(2, 3, 1)]

stopifnot(length(types_a) == length(types_b))
types_a <- as.character(types_a)
types_b <- as.character(types_b)

# Validate known labels; keep missing observations missing.
stopifnot(all(is.na(types_a) | types_a %in% rownames(ed)),
          all(is.na(types_b) | types_b %in% colnames(ed)))
valid <- !is.na(types_a) & !is.na(types_b)
distance <- rep(NA_real_, length(types_a))
distance[valid] <- ed[cbind(
  match(types_a[valid], rownames(ed)),
  match(types_b[valid], colnames(ed))
)]

data.frame(types_a, types_b, distance)
```

`distance` has one value per input position, including repeated pairs. Matrix
indexing with `cbind()` looks up corresponding pairs. Using
`ed[types_a, types_b]` instead selects all combinations and returns a submatrix.
Identical codes have distance zero; some different codes can also have zero
distance under the available ecological definitions. Matrix totals are rounded
to one decimal place.

## Explain the distance between each pair

```r
source("script/ed_decomposition.R")
parts <- read.csv(file.path(folder, paste0("ed_decomposition_", level, ".csv.gz")))

# The helper requires known, non-missing codes and equal vector lengths.
explanation <- lookup.ed.decomposition(
  types_a[valid], types_b[valid], decomposition = parts
)

# Translate pair_id back to positions in the original vectors if rows were missing.
explanation$input_position <- which(valid)[explanation$pair_id]
head(explanation)

# Sum every signed contribution to recover the unrounded total.
totals <- aggregate(contribution ~ pair_id, explanation, sum)
stopifnot(isTRUE(all.equal(
  round(totals$contribution, 1),
  unname(distance[which(valid)[totals$pair_id]])
)))
```

Skip this block if no observations have two known labels. `pair_id` identifies
positions within the vectors passed to the helper. Repeated pairs retain separate
IDs. An unknown pair produces an error.

Each explanation contains `type_a`, `type_b`, `component`, and `contribution`.
For defining LECs it also contains `definition_a` and `definition_b`, showing
which ranges differ (`absent` means no definition on that side).

| Component | Meaning |
| --- | --- |
| `KA`, `UF`, and other LEC codes | Adjusted ecological-distance contribution along that LEC |
| `defining_LEC:TV`, for example | Difference in defining LEC sets/ranges; this is a definition penalty, not a gradient position difference |
| `correlated_LECs:HM+HH`, for example | Joint contribution under the correlation rule |
| `above_forest_line` and other criterion names | Difference after the pairwise criterion rules |
| `defining_LEC_correlation_adjustment` | Joint negative correction, where present |
| `no_shared_LECs` | Additional penalty for no shared qualifying LECs |
| `no_difference` | Zero placeholder for a pair without nonzero contributions |

Signed contributions sum to the unrounded distance. Include negative corrections;
do not round contributions before summing. Correlated groups cannot be counted
again for their individual members. These terms explain the ecological difference
between labels, rather than establish why a mapper assigned a label.

At `ht` and `htg`, the calculation uses information defined at that hierarchy
level. Minor-type hLEC variation is excluded. Major types retain their criteria;
major-type-group criteria are unknown. These matrices are direct calculations,
rather than averages of their constituent minor-type distances.

## Get a distance vector for one component

Component names must match the inventory exactly. An ordinary `KA` contribution
and `defining_LEC:KA` are separate terms. First inspect what actually occurs:

```r
components[grepl("KA|forest|correlated", components$component), ]
```

To extract, for example, ordinary KA contributions while preserving input order:

```r
component <- "KA"
stopifnot(component %in% components$component)

component_distance <- rep(NA_real_, length(types_a))
component_distance[valid] <- 0
selected <- explanation[explanation$component == component, , drop = FALSE]
if (nrow(selected)) {
  contributions <- tapply(selected$contribution, selected$pair_id, sum)
  positions <- which(valid)[as.integer(names(contributions))]
  component_distance[positions] <- as.numeric(contributions)
}
component_distance
```

A component omitted for a known pair contributes zero. A missing observation
remains `NA`. The inventory includes only components observed somewhere at this
level; if a requested component is absent from the inventory, verify its name and
level before interpreting it as zero everywhere. A negative correction is a
signed term, so component vectors are not always standalone nonnegative distances.
Do not divide joint correlated contributions arbitrarily between individual LECs.

## Reclassify a type vector by an ecological attribute

This uses a single type vector, rather than a pair of maps:

```r
source("script/type_profiles.R")
types <- c(rownames(ed)[1:3], NA_character_)

# Gradient position on the shared reference.
ka_shared <- reclassify.types(types, "KA", profiles, reference = "shared")

# Position on each type's own major-type adapted reference.
ka_local <- reclassify.types(types, "KA", profiles, reference = "major_type")

# Basic-step range, retaining its letters.
ka_range <- reclassify.types(types, "KA", profiles, representation = "range")

# Criterion value, retaining 0/1/NA.
forest_line <- reclassify.types(types, "above_forest_line", profiles)
```

The helper preserves vector order, names, repeated codes, and missing values.
Unknown type codes or categories produce an error. For factor LECs, request
`representation = "range"`; numerical positions are missing. In position mode,
criteria return their original numeric values regardless of the reference option.
An undefined attribute stays missing; it is never assigned position zero.

The two gradient scales have different interpretations:

- **Shared:** the mean ordinal rank of the observed basic steps on a common
  reference for each LEC. Equal ranges get equal positions across major types.
  These ranks are descriptive, not physical measurements or calibrated ED units.
- **Major type:** the position on the type's own adapted step reference. These
  coordinates are local and cannot be compared directly across major types.
  `major_type_adjustment` is exported separately and is not applied to the position.

Neither scale replaces the full pairwise distance calculation. In particular,
subtracting shared KA positions does not reproduce the matrix's KA contribution.
Criterion profile values also precede pairwise SSG suppression and related rules.

Inspect missing positions and their provenance when needed:

```r
profiles[profiles$category == "KA",
         c("type", "range", "source", "position_shared", "shared_status",
           "position_major_type", "major_type_status", "major_type_adjustment")]

reference <- read.csv("output/shared/lec_reference_steps.csv.gz")
reference[reference$category == "KA", ]
```

`source` records where a range came from. Status values distinguish undefined
ranges, factors, and unavailable references. Coarse definitions extending beyond
the shared basic-step reference have status `outside_reference` and a missing
shared position; their original ranges remain available.

For ecological assumptions and calculation rules, see
[Documentation.md](Documentation.md). For generating outputs, see
[script/README.md](script/README.md).
