# Ecological Distance

The NiN ecosystem typology (Halvorsen et al. 2020) is hierarchical, with three nested levels (major-type groups, major types, and minor types). In addition, mapping units are defined by merging minor types (within, but not across, major types) to be suitable for mapping at scales 1:5000, 1:10 000, and 1:20 000.

Within major-type groups (e.g. Terrestrial and Wetland ecosystems), major types are defined by a set of principles, e.g. separating ecosystems differing in type of disturbance or in dominance of ecosystem-engineering species groups (e.g. trees, helophytes). Within major types, each minor type spans an interval of standardised size along each main local complex environmental gradient (e.g. soil moisture gradient, lime richness gradient). This interval comprises a standardised amount of species compositional turnover (25% change in species composition, defined as one ecological distance unit (EDU).

## Within major types

Within major types, mapping units exist in an ecological space along local complex environmental gradients (LCEs). Their values for each relevant LCE are given by the major type-adapted steps they occur on. The values are given in the Excel files HT5 for 1:5000 units and HT20 for 1:20 000 units. If a mapping unit occurs on more than one step it, is given the value of the midpoint between the steps. The Ecological Distance (ED) between two mapping units within a major type is given as the sum of the absolute differences for all LCEs relevant to that major type.

## Between major types

*Gradients*

When calculating the ED between two mapping units belonging to different major types, the mapping unit’s values for each  LCE are given by the basic step they occur on. The values are given in the Excel files bT5 for 1:5000 units and bT20 for 1:20 000 units. Basic steps are in NiN given as letters, but are converted to numbers, so that a=1, b=2, and so on. ED between two mapping units from different major types is added as the sum of the absolute differences, weighted by one half, for all LCEs relevant to both mapping units (i.e. it has a value of more than zero for the basic step).

The mapping units are also given values for major-type-adapted steps along defining LCEs (dLCEs). The values are given in the Excel files sLKM5 for 1:5000 units and sLKM20 for 1:20 000 units. The difference according to the dLCEs is added to the ED between mapping units from different major types. Firm ground mapping unit’s values for the dLCEs drought duration (TV), water disturbance intensity (VF), and spring water influence (KI) are subtracted from the ED to wetland, freshwater and marine mapping units. 

*Principles*

In addition to LCEs, a set of principles is used to calculate the ED between mapping units form different major types. The mapping units are assigned values according to their characteristics (based on principles). ED between the mapping units from different major types is added as the difference between the values for the characteristics. The characteristics and their values include:

- Position relative to forest line (only below=0, only above=1, can be both=0.5)
- Presence of soil (yes=0, no=2)
- Anthropogenic influence (natural=0, semi-natural=2, strongly modified=4)

Two EDU are added between mapping units from different major type groups, and between mapping units with different ecosystem engineering groups (or absence of such groups). Between semi-natural units, 1 EDU is added to differentiate between semi-natural major-type units separated based on the local complex environmental factor Semi-natural ground without signs of land use, influenced by anthropogenic influence (MX) or Semi-natural land use regime (HR).

The same rules of calculating ED applies to all spatial scales. ED that is not an integer value is rounded down (i.e. between mapping units that only occur below the forest line and those occurring bot below and above the ED is zero).

*Criteria*

The script also contains code for generating an array of confusion matrices containing the violated gradients or principles for each combination of mapping units. For instance, Lime-poor exposed bare rock and Lime-poor submesic to subxeric forest are separated based on the violation of two criteria, i.e., presence of soil and the presence of trees. The matrix does not differentiate between magnitudes of violation, e.g. if two mapping units are separated by two or three steps along the richness gradient. This matrix can thus be used to identify the number of times a set of criteria has been violated in comparison of a map pair.

### References

Halvorsen, R., Skarpaas, O., Bryn, A., Bratli, H., Erikstad, L., Simensen, T., & Lieungh, E. (2020). Towards a systematics of ecodiversity: The EcoSyst framework. Global Ecology and Biogeography, 29(11), 1887–1906. https://doi.org/10.1111/geb.13164

