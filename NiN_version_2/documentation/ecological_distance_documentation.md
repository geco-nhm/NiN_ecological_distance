# Ecological Distance for NiN version 2

Ecological Distance (ED), as estimated in this repository, is an approximation of the difference between two mapping units in terms of species composition, abiotic composition, and formative process. Its purpose is to give more information about differences where two assessments of the same area are not equal. Ecological Distance is given as integer values between each mapping unit (ecosystem types) in 1:5000 and 1:20 000 scales in symmetrical matrices with mapping unit codes as row- and column names. Estimated ED that is not an integer value has been rounded down.

An ED of 0 means that two mapping unit assignments are identical. Where they are different, ED is given as an integer number. There is no predefined maximum ED, but large ED is where the compared mapping units have (almost) no species in common, vastly different substrates, and/or different governing processes. Note that the estimated ED should be interpreted with caution because it is an *approximation* of difference and therefore often, if not always, inaccurate. Note also that the method for calculating ED for NiN version 2 and 3 is quite different, and the resulting ED is not directly comparable even between types that are unchanged between the two versions.

This repository hosts scripts to calculate ED from input data under /excel_files. The output is a scale-specific matrix of ED between mapping units, under `../matrices`. To use ED in analyses of e.g. rasterised maps, make pairwise comparisons using the mapping unit from each map as a 'coordinate' in the matrix to find pixel-wise ED between two maps. An example script to get the ecological distance in a point data set is provided under `../script`.

## Estimating Ecological Distance (ED)

The NiN ecosystem typology (Halvorsen et al. 2020) is hierarchical, with three nested levels (major-type groups, major types, and minor types). In addition, mapping units are defined by aggregating minor types (within, but not across, major types) to be suitable for mapping at scales 1:500, 1:5000, 1:10 000, 1:20 000 and 1:50 000. Ecological distance was first operationalised for NiN version 2 in Eriksen et al. (2018), and scripted and revised in Naas et al. (2023).

### Major-type groups, major types, and minor types

NiN version 2 includes several major-type groups (MTGs): terrestrial non-wetland (T) and wetland (V) systems, limnic bottom systems (L), marine bottom systems (M), and snow and ice systems (I). This repository so far only covers ED for the terrestrial systems (T and V) thoroughly. Updating the ED for additional major-type groups will require some extra data curation and decisions about ecological distance.

Within major-type groups (e.g. terrestrial and wetland ecosystems), major types (MTs) are defined by a set of principles, e.g. separating ecosystems differing in type of disturbance or in dominance of ecosystem-engineering species groups (e.g. trees, helophytes). Major types are also divided into three categories based on the level of human (anthropogenic) influence (natural, semi-natural, or strongly modified). 

Within major types, minor types exist in a theoretical ecological space along local complex environmental gradients (LECs). Within major types, each minor type spans an interval of standardised size along each main local complex environmental gradient (LEC; e.g. soil moisture gradient, lime richness gradient). This interval comprises a standardised amount of species compositional turnover, set at 25% change in species composition. This amount of turnover is defined as one ED unit. 

Many ecosystems are defined and differentiated based on principles other than species composition, or have no or unpredictable species occurrence. The lack of a usable species composition can stem from natural or anthropogenic disturbances, and result in substrates with different potential for succession and ecosystem composition and function. For these types, defining principles are used to estimate ED instead of, or in addition to, ED from species compositional turnover.

### Calculating ED 

In practice, ED is calculated based on several datasets (under `/excel_files`). All MTs need a value (or NA) for all LECs and MT-defining properties (structuring species groups, soil presence, above/below forest line). These are the general rules for calculation:

- Within Major Types, an ED of 1 is added for each MT-specific step along major local environmental complex gradients (LECs).
- Across Major Types, an ED of 0.5 is added for each basic step along each major LEC and shared defining LEC.
- Every major-type adapted step along a factor LEC gives an ED of 1.
- Differing presence/absence of a structuring species group (e.g.trees) is given an ED of 2.
- Position relative to forest line (only below=0, only above=1)
- Presence of soil (yes=0, no=2)
- Anthropogenic influence (natural=0, semi-natural=2, strongly modified=4)
- Between semi-natural units, 1 ED is added to differentiate between semi-natural major-type units separated based on the local complex environmental factor Semi-natural ground without signs of land use, influenced by anthropogenic influence (MX) or Semi-natural land use regime (HR).
- Different Major-Type Group adds 2 ED (but only 1 ED for terrestrial mapping units if they are influenced by spring water, water inundation, or water disturbance).

The mapping units are also given values for major-type-adapted steps along defining LCEs (dLCEs). The values are given in the Excel files sLKM5 for 1:5000 units and sLKM20 for 1:20 000 units. The difference according to the dLCEs is added to the ED between mapping units from different major types.

When calculating the ED between two mapping units belonging to different major types, the mapping unit’s values for each LCE are given by the basic step they occur on. The values are given in the Excel files bT5 for 1:5000 units and bT20 for 1:20 000 units. Basic steps are converted to numbers from their original lowercase letter format, so that a=1, b=2, c=3, and so on. ED between two mapping units from different major types is added as the sum of the absolute differences, weighted by one half, for all LCEs relevant to both mapping units (i.e. it has a value of more than zero for the basic step).

## References

Halvorsen, R., Skarpaas, O., Bryn, A., Bratli, H., Erikstad, L., Simensen, T., & Lieungh, E. (2020). Towards a systematics of ecodiversity: The EcoSyst framework. *Global Ecology and Biogeography*, 29(11), 1887–1906. <https://doi.org/10.1111/geb.13164>

Naas, A.E., Halvorsen, R., Horvath, P., Wollan, A.K., Bratli, H., Brynildsrud, K., et al. (2023). What explains inconsistencies in field-based ecosystem mapping? *Applied Vegetation Science*, 26, e12715. <https://doi.org/10.1111/avsc.12715>

Eriksen, E.L., Ullerud, H.A., Halvorsen, R., Aune, S., Bratli, H., Horvath, P., Volden, I.K., Wollan, A.K., & Bryn, A. (2018). Point of view: Error estimation in field assignment of land-cover types. *Phytocoenologia*, 49(2), 135–148. <https://doi.org/10.1127/phyto/2018/0293>
