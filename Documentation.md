
# Ecological Distance in NiN version 3

*Written by Adam E. Naas, Eva Lieungh, Amanda M.A. Folland*

Ecological Distance (ED), as estimated in this repository, is an approximation of the difference between two mapping units in terms of species composition, abiotic composition, and formative process. Its purpose is to give more information about differences where two assessments of the same area are not equal. Ecological Distance is given as numeric values between each mapping unit (ecosystem types) in 1:5000 and 1:20 000 scales in symmetrical matrices with mapping unit codes as row- and column names. 

An ED of 0 means that two mapping unit assignments are identical. Where they are different, ED is given as an integer number. There is no predefined maximum ED, but a maximal ED is where the compared mapping units have (almost) no species in common, vastly different substrates, and/or different governing processes. Note that the estimated ED should be interpreted with caution because it is an *approximation* of difference and therefore often, if not always, inaccurate. Note also that the method for calculating ED for NiN version 2 and 3 is quite different, and the resulting ED is not directly comparable even between types that are unchanged between the two versions.

This repository hosts scripts to calculate ED from an input dataset with columns for short codes for major-type groups, major-types, and minor types, ecological space, 1:5000 units, 1:20 000 units, and 1:50 000 units. The output is a matrix of ED between mapping units, under `../output`. To use ED in analyses of e.g. rasterised maps, make pairwise comparisons using the mapping unit from each map as a 'coordinate' in the matrix to find pixel-wise ED between two maps. An example script to get the ED in a point dataset is provided under `../script`.

## Estimating Ecological Distance (ED)

The NiN ecosystem typology (Halvorsen et al. 2020) is hierarchical, with three nested levels (major-type groups, major types, and minor types). In addition, mapping units are defined by aggregating minor types (within, but not across, major types) to be suitable for mapping at scales 1:500, 1:5000, 1:20 000 and 1:50 000. Ecological distance was first operationalised for NiN version 2 in Eriksen et al. (2018), and scripted and revised in Naas et al. (2023).

### Major-type groups, major types, and minor types

NiN version 3 includes eight major-type groups (MTGs): terrestrial non-wetland (T), wetland (V), snow and ice systems (I), limnic (L), marine (M), and river (O) bottom systems, and limnic (F), and marine (S) water body systems. This repository so far only covers ED for the terrestrial systems (T, V, and I) thoroughly. Marine, limnic and river bottom (L, O, and M) systems are also included, but criteria to define the ED between them properly are not considered. Updating the ED for these major-type groups will require some extra data curation and decisions about ecological distance.

Within major-type groups (e.g. terrestrial and wetland ecosystems), major types (MTs) are defined by a set of principles, e.g. separating ecosystems differing in type of disturbance or in dominance of ecosystem-engineering species groups (e.g. trees, helophytes). 

Within major types, minor types exist in a theoretical ecological space along local complex environmental gradients (LECs). Within major types, each minor type spans an interval of standardised size along each main local complex environmental gradient (LEC; e.g. soil moisture gradient, lime richness gradient). This interval comprises a standardised amount of species compositional turnover, set at 25% change in species composition. This amount of turnover is defined as one ED unit. 

LECs are divided into basic steps that are aggregated into major-type adapted steps within each major type for each spatial scale.The mapping units’ positions along major-type adapted steps can be translated into integer values if the mapping unit is fully contained within one major-type adapted step or decimal values if the mapping units span several major-type adapted steps. To find the position along major-type adapted steps for a mapping unit within another major type becomes more complicated. In those cases, the mapping units’ sometimes span a major-type adapted step only partly or not overlap with any major-type adapted step at the extreme parts of the LECs. To tackle the first situation, the major-type adapted step is estimated based on the basic steps. The second situation is tackled by adding two bordering major-type adapted steps by aggregating basic steps at both of the extreme ends of the LEC.

Many ecosystems are defined and differentiated based on principles other than species composition, or have no or unpredictable species occurrence. The lack of a usable species composition can stem from natural or anthropogenic disturbances, and result in substrates with different potential for succession and ecosystem composition and function. For these types, defining principles are used to estimate ED instead of, or in addition to, ED from species compositional turnover.

### Calculating ED 

In practice, ED is calculated based on two datasets. The main dataset contains rows for each major-type group, major type, and minor type and columns for short code, ecological space, 1:5000 unit, 1:20 000 unit, and 1:50 000 unit. The other dataset contains rows for each minor type and columns for principles other than ecological space that differentiate the ecosystem types (e.g., structuring species groups, vegetation presence, above/below forest line). These are the general rules for calculation:

When no local environmental complex gradients (LECs) are shared, add 1 ED (bLEC/“bLKM” excepted).
Every major-type adapted step along a factor LEC gives an ED of 1. Between two mapping units, ED is calculated along each LEC by placing the mapping units within the first mapping unit’s major type to find their values with respect to major-type adapted steps for the first mapping unit’s major type. Then, the absolute difference between the two values is calculated. Next, the mapping units are placed within the second mapping unit’s major type to find their values with respect to major-type adapted steps for the second mapping unit’s major type. Then, the absolute difference between the values is calculated. The resulting ED is given as the mean of the two absolute differences.
The LEC-based ED is scaled by a factor obtained by dividing the gradient length (i.e., the number of major-type adapted steps) for the major type at the minor-type level by the gradient length for the major type at the desired spatial scale (e.g., 1:5000).
Differing presence/absence of a structuring species group adds an ED of 1.
Difference in removal of structuring species groups gives an ED of 1. In that case the above rule (#4) is not taken into account.
Position relative to the forest line differs by 1 ED. No difference if one mapping unit can be found both below and above.
Difference in presence/absence of substrate is given an ED of 1.
Difference in presence/absence of vegetation is given an ED of 1.
The ED between major-type groups are defined by LECs.

Each Major-type group can be defined by the LECs and intervals in the table below (LEC_interval). For example, terrestrial non-wetland and wetland systems are separated by their position along the “VM” LEC Duration of period without inundation, where non-wetland major types span basic steps 0abc, and wetland major types span the z step.

|  |
| -- |
|T = SV_0abcdefg, TV_y, VM_0abc, SA_0ab|
|V = SV_0abcdefg, TV_cdefghijk, VM_z, SA_0ab|
|I = SV_y, TV_y, VM_0abc, SA_0ab|
|L = TV_0ab, SA_0ab|
|M = TV_0ab, SA_cdefghz|
|O = TV_0ab, SA_0ab|

Each major type has its own set of LECs. The total compositional turnover along the LECs within the major type is used to differentiate major (hLEC; > 2 ED) and significant (bLEC; 1-2 ED) LECs. Within major types, ED is only taken into account for major LECs. Differences with respect to LECs and principles are only computed where they are relevant for both mapping units.

Several LECs (for example HH, HM, HA, and VF, TV, VM) are strongly correlated and could cause an inflation of ED with the default calculation method. In the group of correlated LECs, we only consider the ED for the LEC that gives the maximum value. In the current version of ED calculation, this correction has only been done for the HA, HM, and HH LECs, and for HR and HH. To change this, modify the cor_lec object in the create_ed_matrix.R script. 


### Explaining a distance

The decomposition tables record the additive terms used by the existing distance
calculation. They explain the ecological dimensions of a difference; they do not
establish why an observer assigned a particular map label.

- LEC codes such as `KA` give their adjusted distance contributions, including
  averaging across the two major-type perspectives.
- Categorical criteria use their column names, such as `above_forest_line` and
  `ssg_helophytes`. Contributions are recorded after the existing SSG cap/removal
  and missing-value rules. An omitted criterion has no contribution to that pair.
- `correlated_LECs:HA+HM+HH`, for example, is the maximum contribution of the listed
  LECs under the existing correlation rule. It is one group contribution; it must
  not be added again for each member. Overlapping groups follow the existing
  order, which assigns shared LECs to the first applicable group.
- `defining_LEC:KA`, for example, identifies KA's contribution to the defining-LEC
  set penalty. A definition in only one type contributes 1; different ranges for
  the same LEC count as two distinct definitions and contribute 2. Identical
  definitions contribute 0 and are omitted. This counts differing definitions;
  gradient separation is recorded separately under the ordinary LEC code.
  `definition_a` and `definition_b` show the definitions for `type_a` and `type_b`,
  with `absent` for a missing defining LEC. Multiple definitions for one LEC are
  separated by `; `. These fields are missing for other component categories.
- `defining_LEC_correlation_adjustment` records any existing joint correction as
  a negative contribution. It must be included when summing the decomposition;
  it is not arbitrarily assigned to a single LEC. The per-LEC defining terms plus
  this adjustment replace the former `defining_LEC_difference` entry without
  changing the original penalty.
- `no_shared_LECs` is the additional unit when the two types share no qualifying
  LECs. `no_difference` is a zero-valued placeholder when no nonzero terms remain.

Signed contributions sum to the unrounded ecological distance. The matrix stores
that total rounded to one decimal place; independently rounding each contribution
before summing can give a different result. The long tables contain every ordered
pair, including self-pairs, and no separate total rows. Both scales are exported
as plain CSV files that can be read directly with R's `read.csv()`.

See `script/README.md` for paired-vector lookup examples.

### Reclassifying types by ecological attributes

The type-profile tables provide each mapping unit's effective LEC ranges and
categorical criteria. Individual-type ranges take precedence. Missing ranges are
filled from the major type's defining LECs, followed by the major-type group's
ranges. Provenance follows that actual fill order and is combined when minor types
are aggregated. A missing LEC remains undefined; it is not assigned position zero.

For gradient LECs, `position_major_type` uses the same `convert.bs()` positions as
ED, on the type's own adapted step reference. These local coordinates are not
comparable across major types. They do not include `major_type_adjustment`, which
is exported separately. The shared option, `position_shared`, averages the ordinal
ranks of the observed basic steps on one reference per LEC, common to both mapping
scales. The reference orders `0`, then `a` through `z`, skips unobserved steps, and
assigns ranks starting at 1. Equal ranges therefore have equal shared positions,
regardless of major type. These are descriptive ranks, not calibrated environmental
measurements or a replacement for the pairwise ecological-distance calculation.

Factor LECs retain their letter levels and do not receive numerical positions.
Categorical criteria retain the original 0/1/NA values. In particular, forest-line
NA is preserved; it must not be silently reclassified to below-forest-line 0.

The source row LC01-05 has `[KA_w] VT_C`, with a missing comma. The import repairs
the separator and applies the intended KA_w exclusion, leaving VT_C and undefined
KA. The previous parser incorrectly joined these tokens and assigned C to KA,
which also incorrectly triggered its uppercase-factor rule. The erroneous C
propagated into KA reference steps for seven major types through the existing
border-step logic. Repairing the source changes 5,120 unordered distances at
1:5000 and 2,252 at 1:20 000; all involve LC01 or a formerly contaminated KA
reference. The regenerated outputs incorporate this correction; KA remains a
gradient.

See `script/README.md` for choosing the reference and reclassifying a vector.

### References

Halvorsen, R., Skarpaas, O., Bryn, A., Bratli, H., Erikstad, L., Simensen, T., & Lieungh, E. (2020). Towards a systematics of ecodiversity: The EcoSyst framework. *Global Ecology and Biogeography*, 29(11), 1887–1906. <https://doi.org/10.1111/geb.13164>

Naas, A.E., Halvorsen, R., Horvath, P., Wollan, A.K., Bratli, H., Brynildsrud, K., et al. (2023). What explains inconsistencies in field-based ecosystem mapping? *Applied Vegetation Science*, 26, e12715. <https://doi.org/10.1111/avsc.12715>

Eriksen, E.L., Ullerud, H.A., Halvorsen, R., Aune, S., Bratli, H., Horvath, P., Volden, I.K., Wollan, A.K., & Bryn, A. (2018). Point of view: Error estimation in field assignment of land-cover types. *Phytocoenologia*, 49(2), 135–148. <https://doi.org/10.1127/phyto/2018/0293>

