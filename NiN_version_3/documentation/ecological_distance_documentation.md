# Ecological Distance

*Written by Adam E. Naas, Eva Lieungh, Amanda M.A. Folland*

Ecological distance, as estimated in this repository, is an approximation of the difference between two mapping units in terms of species composition, abiotic composition, and formative process. Its purpose is to give more information about differences where two assessments of the same area are not equal. Ecological Distance is given as integer values between each mapping unit (ecosystem types) in 1:5000 and 1:20 000 scales in symmetrical matrices with mapping unit codes as row- and column names. The same rules of calculating ED apply to all spatial scales. Estimated ED that is not an integer value is rounded down. 

An ecological distance of 0 means that two assessments/maps are identical. Where maps are different, ecological distance is given as an integer number. There is no pre-defined maximum ED value, but a maximal ED value is where the compared mapping units have (almost) no species in common, vastly different substrates, and different governing processes. The estimated ecological distance is often, if not always, inaccurate, and should be interpreted with caution. 

This repository hosts scripts to calculate ecological distance from an input data set with minor types or mapping units as rows and variables as columns. The output is a matrix of ecological distance between mapping units, under `../output`. To use ecological distance in analyses of e.g. rasterised maps, make pairwise comparisons using the mapping unit from each map as a 'coordinate' in the matrix to find pixel-wise ecological distance value between two maps.

## Estimating Ecological Distance (ED)

The NiN ecosystem typology (Halvorsen et al. 2020) is hierarchical, with three nested levels (major-type groups, major types, and minor types). In addition, mapping units are defined by merging minor types (within, but not across, major types) to be suitable for mapping at scales 1:5000, 1:10 000, and 1:20 000.

NiN version 3 includes nine major-type groups: terrestrial non-wetland (T) and wetland (V) systems, limnic bottoms (L), and water bodies (F), rivers beds (O) and water bodies (), marine bottoms (M) and water bodies (H), and snow and ice systems (I). This repository so far only covers ecological distance for the terrestrial systems (T and V). It should be possible to extend the calculations of ecological distance to the other major-type groups with the same general method, but this will require some extra data curation and decisions about ecological distance.

Within major-type groups (e.g. Terrestrial and Wetland ecosystems), major types are defined by a set of principles, e.g. separating ecosystems differing in type of disturbance or in dominance of ecosystem-engineering species groups (e.g. trees, helophytes). Within major types, each minor type spans an interval of standardised size along each main local complex environmental gradient (e.g. soil moisture gradient, lime richness gradient). This interval comprises a standardised amount of species compositional turnover, set at 25% change in species composition. This amount of turnover is defined as one ecological distance unit. Many ecosystems are defined and differentiated based on principles other than species composition, or have no or unpredictable species occurrence. The lack of a usable species composition can stem from natural or anthropogenic disturbances, and result in substrates with different potential for succession and ecosystem composition and function. For these types, defining principles are used to estimate ecological distance.

In practice, ED is calculated based on several datasets. The main data set lists all MTs as rows, and all LECs defining at least one MT as columns. All MTs need a value (or NA?) for all LECs and MT-defning properties (structuring species groups, vegetation presence, above/below forest line). 

## Within major types

Within major types, mapping units exist in a theoretical ecological space along local complex environmental gradients (LECs). Their values for each relevant LEC are given by the major type-adapted steps they occur on (listed in the `../data/input_units_attributes.csv ` file). If a mapping unit occurs on more than one step, it is given the value of the midpoint between the steps. The Ecological Distance between two mapping units within a major type is given as the sum of the absolute differences for all LECs relevant to that major type.

## Between major types

*Gradients*

When calculating the ED between two mapping units belonging to different major types, the mapping unit’s values for each LEC are given by the basic step they occur on. The values are given in the .csv files <add name here> for 1:5000 units and <add name here> for 1:20 000 units. Basic steps are converted to numbers from their original lowercase letter format, so that a=1, b=2, c=3, and so on. 

Major-type groups (MTGs): terrestrial non-wetland (T) and wetland (V) systems, limnic bottoms (L), rivers (O), and water bodies (F), marine bottoms (M) and water bodies (H), and snow and ice systems (I). 
The ED between major-type groups must be defined by LECs. For example, terrestrial non-wetland and wetland systems are separated by their position along the “VM” LEC Duration of period without inundation, where non-wetland major types span basic steps 0abc, and wetland major types span the z step. Similarly, each MTG can be defined by these LECs and intervals (LEC_interval):

|  |
| -- |
|T = SV_0abcdefg, TV_y, VM_0abc, SA_0ab|
|V = SV_0abcdefg, TV_y, VM_z, SA_0ab|
|L = SV_0abcdefg, TV_0abcdefghijkl,VM_0abc, SA_0ab|
|M = SV_0abcdefg, TV_0abcdefghijkl, VM_0abc, SA_cdefghz|
|I = SV_y, TV_y, VM_0abc, SA_0ab|

To deal with different gradient lenghts (merging of basic steps to major-type-adapted steps) between major types, we check where the midpoint is contained within the compared major types. The average number of MT-specific steps is the added ED units.

Major types are also divided into three categories based on the level of human (anthropogenic) influence, namely weakly, clearly, and strongly modified MTs. For calculating ED in NiN version 3, we replace the previous rule of adding 2 ED between each category of anthropogenic influence (then termed natural/semi-natural/strongly-modified) with factor variables based on processes. If necessary, we can potentially modify the ED post-hoc with an ifelse statement (if ED < 2(?), add 1, else continue). This might be necessary if some types end up with too small ED across anthropogenic influence categories. For agricultural MTs, several LECs (HH,HM,HA,?) are correlated and could cause an inflation of ED with the default calculation method. We solve this by only considering the HH LEC between major types, and only adding ED along additional gradients where the difference is bigger than the 'normal' variation within these MTs.

- An ED of 1 is added for each MT-specific step along defining local environmental complex gradients (LECs) between major types.
- MTs differing in a factor LEC are given an ED of 1.
- Differing presence/absence of a structuring species group is given an ED of 1.
- Position relative to the forest line differs by 1 ED: Below, above, can be found both below and above (0,5)
- When no LECs are shared by the compared MTs, add 1 ED.
- Three MT-level substrate categories differ by 1 ED: No substrate (suitable for vegetation to colonise), 
deposits present, vegetation present


### References

Halvorsen, R., Skarpaas, O., Bryn, A., Bratli, H., Erikstad, L., Simensen, T., & Lieungh, E. (2020). Towards a systematics of ecodiversity: The EcoSyst framework. Global Ecology and Biogeography, 29(11), 1887–1906. https://doi.org/10.1111/geb.13164
