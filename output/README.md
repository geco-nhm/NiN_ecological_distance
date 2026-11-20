## Outputs

This folder contains the calculated ecological distance between all mapping units at the 1:5000 (`ed_matrix_005.csv`) and 1:20 000 scale (`ed_matrix_020.csv`). 

The current matrices were generated from `input/NiN_3.0.1_Master_Clean.xlsx`
and `input/NiN3_categorical_variables.csv`. They contain 673 mapping units
at 1:5000 and 405 at 1:20 000. Distances are rounded to one decimal place.

The companion files `ed_decomposition_005.csv` and
`ed_decomposition_020.csv` explain every ordered type pair as additive
contributions. Columns are `type_a`, `type_b`, `component`, `contribution`,
`definition_a`, and `definition_b`. Defining-LEC differences are split by LEC
(e.g. `defining_LEC:KA`), with the two definitions or `absent` recorded alongside.
Any joint correlation correction is a separate negative adjustment; all signed
contributions must be summed. Nonzero terms retain precision; pairs without any
nonzero terms have a `no_difference` row. There are no total rows to double-count. See
`script/README.md` for lookup examples and `Documentation.md` for component rules.

The reclassification lookup files `type_profiles_005.csv` and
`type_profiles_020.csv` contain letter ranges, provenance, categorical values,
and both local major-type and shared numerical gradient positions. Choose the
reference in `reclassify.types()`; see `script/README.md` for examples.
`lec_reference_steps.csv` defines the shared basic-step ranks for both scales.
These ranks are ordinal positions, not physical measurements or total ED.

The current outputs incorporate the LC01-05 parsing repair: `[KA_w] VT_C` is
interpreted as separate tokens, with the excluded KA_w removed and VT_C retained.
