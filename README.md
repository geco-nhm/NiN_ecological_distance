# Ecological distance - a quantitative measure of difference between "Nature in Norway" mapping units

Ecological distance is a useful measure for assessing differences between e.g. two maps of the same area. This repository contains measures of ecological distance between NiN types, as well as scripts and source files for reproducing the calculations. It is split into two parallel folders for NiN versions 2 and 3. Within each version, you will find:

| Directory | Contents |
| --------- | -------- |
| Documentation | Textual documentation of the rules for calculating Ecological Distance (ED) |
| Input | Input data used to define the ecological space for calculating ED |
| Output | Output files, including processed data and the finished ED matrices for scales 1:5000 and 1:20000 |

Note that NiN version 2 and 3 differ in many aspects, including the number of types (and mapping units), and ED is calculated in different ways. ED between the two versions should therefore *not* be compared directly. 

Ecological distance, as defined in this repository, is based on the structure of the ["Nature in Norway" (version 2)]( https://doi.org/10.1111/geb.13164) system. The method further develops the concept and estimation of ecological distance from [Eriksen et al. (2019)](https://doi.org/10.1127/phyto/2018/0293) and [Naas et al. (2023)](https://doi.org/10.1111/avsc.12715).

Main developer: [Adam E. Naas](https://www.nhm.uio.no/?vrtx=person-view&uid=adamen)
