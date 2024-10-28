# Ecological Distance

*Written by Adam E. Naas, Eva Lieungh, Amanda M.A. Folland*

Ecological distance, as estimated in this repository, is an approximation of the difference between two mapping units in terms of species composition, abiotic composition, and formative process. Its purpose is to give more information about differences where two assessments of the same area are not equal. Ecological Distance is given as integer values between each mapping unit (ecosystem types) in 1:5000 and 1:20 000 scales in symmetrical matrices with mapping unit codes as row- and column names. The same rules of calculating ED apply to all spatial scales. Estimated ED that is not an integer value is rounded down. 

An ecological distance of 0 means that two assessments/maps are identical. Where maps are different, ecological distance is given as an integer number. There is no pre-defined maximum ED value, but a maximal ED value is where the compared mapping units have (almost) no species in common, vastly different substrates, and different governing processes. The estimated ecological distance is often, if not always, inaccurate, and should be interpreted with caution. 

This repository hosts scripts to calculate ecological distance from an input data set with minor types or mapping units as rows and variables as columns. The output is a matrix of ecological distance between mapping units, under `../output`. To use ecological distance in analyses of e.g. rasterised maps, make pairwise comparisons using the mapping unit from each map as a 'coordinate' in the matrix to find pixel-wise ecological distance value between two maps.

## Estimating Ecological Distance (ED)

The NiN ecosystem typology (Halvorsen et al. 2020) is hierarchical, with three nested levels (major-type groups, major types, and minor types). In addition, mapping units are defined by merging minor types (within, but not across, major types) to be suitable for mapping at scales 1:5000, 1:10 000, and 1:20 000.

NiN version 3 includes seven major-type groups: terrestrial non-wetland (1) and wetland (2) systems, limnic bottoms (3) and water bodies (4), marine bottoms (5) and water bodies (6), and snow and ice systems (7). This repository so far only covers ecological distance for the terrestrial systems. It should be possible to extend the calculations of ecological distance to the other major-type groups with the same general method, but this will require some extra data curation and decisions about ecological distance across major-type groups and within each of the other major-type groups.

Within major-type groups (e.g. Terrestrial and Wetland ecosystems), major types are defined by a set of principles, e.g. separating ecosystems differing in type of disturbance or in dominance of ecosystem-engineering species groups (e.g. trees, helophytes). Within major types, each minor type spans an interval of standardised size along each main local complex environmental gradient (e.g. soil moisture gradient, lime richness gradient). This interval comprises a standardised amount of species compositional turnover, set at 25% change in species composition. This amount of turnover is defined as one ecological distance unit. Many ecosystems are defined and differentiated based on principles other than species composition, or have no or unpredictable species occurrence. The lack of a usable species composition can stem from natural or anthropogenic disturbances, and result in substrates with different potential for succession and ecosystem composition and function. For these types, defining principles are used to estimate ecological distance.

## Within major types

Within major types, mapping units exist in a theoretical ecological space along local complex environmental gradients (LCEs). Their values for each relevant LCE are given by the major type-adapted steps they occur on (listed in the `../data/input_units_attributes.csv ` file). If a mapping unit occurs on more than one step, it is given the value of the midpoint between the steps. The Ecological Distance between two mapping units within a major type is given as the sum of the absolute differences for all LCEs relevant to that major type.

## Between major types

*Gradients*

When calculating the ED between two mapping units belonging to different major types, the mapping unit’s values for each LCE are given by the basic step they occur on. The values are given in the .csv files <add name here> for 1:5000 units and <add name here> for 1:20 000 units. Basic steps are converted to numbers from their original letter format, so that a=1, b=2, c=3, and so on. ED between two mapping units from different major types is added as the sum of the absolute differences, weighted by one half, for all LCEs relevant to both mapping units (i.e. it has a value of more than zero for the basic step).

The mapping units are also given values for major-type-adapted steps along defining LCEs (dLCEs). The difference according to the dLCEs is added to the ED between mapping units from different major types. To account for the transition between dry land and wetlands and limnic or marine ecosystems, firm ground mapping units' values for the dLCEs drought duration (TV), water disturbance intensity (VF), and spring water influence (KI) are subtracted from the ED to wetland, freshwater and marine mapping units. 

*Principles*

In addition to LCEs, a set of principles is used to calculate the ED between mapping units form different major types. The mapping units are assigned values according to their characteristics. ED between the mapping units from different major types is added as the difference between the values for the characteristics. The characteristics and their values include:

- Position relative to forest line (only below=0, only above=1, can be both=0.5)
- Presence of soil (yes=0, no=2)
- Anthropogenic influence (weakly modified=0, clearly modified=2, strongly modified=4)

Two ED units are added between mapping units from different major type groups, and between mapping units with different ecosystem engineering groups (or absence of such groups). Between clearly modified (semi-natural) units, 1 ED unit is added to differentiate between major-type units separated based on the local complex environmental factor Semi-natural ground without signs of land use, influenced by anthropogenic influence (MX) or Semi-natural land use regime (HR).

*Criteria*

The scripts also contain code for generating an array of confusion matrices containing the violated gradients or principles for each combination of mapping units. For instance, Lime-poor exposed bare rock and Lime-poor submesic to subxeric forest are separated based on the violation of two criteria, i.e., presence of soil and the presence of trees. The matrix does not differentiate between magnitudes of violation, e.g. if two mapping units are separated by two or three steps along the richness gradient. This matrix can thus be used to identify the number of times a set of criteria has been violated in comparison of a map pair.

### References

Halvorsen, R., Skarpaas, O., Bryn, A., Bratli, H., Erikstad, L., Simensen, T., & Lieungh, E. (2020). Towards a systematics of ecodiversity: The EcoSyst framework. Global Ecology and Biogeography, 29(11), 1887–1906. https://doi.org/10.1111/geb.13164
