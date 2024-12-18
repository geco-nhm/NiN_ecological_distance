# Ecological distance - a quantitative measure of difference between "Nature in Norway" mapping units

Main developer: [Adam E. Naas](https://www.nhm.uio.no/?vrtx=person-view&uid=adamen)

Ecological distance is a useful measure for assessing differences between e.g. two maps of the same area. This repository contains measures of ecological distance between NiN types, as well as scripts and source files for reproducing the calculations. See Documentation.md for an explanation of what ED is, and how it is calculated.

Ecological distance estimation and documentation for NiN version 2 is stored under `/NiN_version_2`. The rest of the repository is for NiN version 3. Note that NiN version 2 and 3 differ in many aspects, including the number of types (and mapping units), and ED is calculated in different ways. ED between the two versions should therefore *not* be compared directly. 

| Directory or file name | Contents |
| ---------------------- | -------- |
| /NiN_version_2 | Estimation of Ecological Distance for NiN version 2, with its own documentation and folder structure. |
| /input | Input data used to define the ecological space for calculating ED |
| /output | Output files, the finished ED matrices |
| /script | R scripts to (re)produce the output, including functions to create input data and calculate ED.|
| Documentation.md | Textual descrtiption of what ED is and principles for how it is calculated. |
| CITATION.cff | Provides GitHub with a suggested citation, see button --> |
| .gitignore | local files for Git to ignore when committing and uploading changes |
| .zenodo.json | Provides Zenodo with metadata about this repository, notably creators. This will overwrite the default method of fetching GitGub users from the repository.|
| README.md | This file, providing the repos 'front page' |

Ecological distance, as defined in this repository, is based on the structure of the ["Nature in Norway" system](https://doi.org/10.1111/geb.13164). The method further develops the concept and estimation of ecological distance from [Eriksen et al. (2019)](https://doi.org/10.1127/phyto/2018/0293) and [Naas et al. (2023)](https://doi.org/10.1111/avsc.12715).
