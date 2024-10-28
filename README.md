# Ecological distance - a quantitative measure of difference between "Nature in Norway" types

This repository contains spreadsheets with measures of ecological distance between NiN types, as well as scripts and source files for reproducing the calculations. The most notable product in this repository is `matrices/ED5000.csv` (within each NiN version directory), which contains the ecological distance between each of the terrestrial and one Limnic mapping units in 1:5000 scale. 

This measure of ecological distance is based on the structure of the ["Nature in Norway" (version 2)]( https://doi.org/10.1111/geb.13164) system. The method further develops the concept and estimation og ecological distance from [Eriksen et al. (2019)](https://doi.org/10.1127/phyto/2018/0293).

Ecological distance is a useful measure for assessing differences between e.g. two maps of the same area.

Main developer: [Adam E. Naas](https://www.nhm.uio.no/?vrtx=person-view&uid=adamen)

The folder "documentation" contains a short description of what ecological distance is and how it is calculated. The scripts in the "script"-folder use excel sheets in the "excel_files"-folder to derive ecological distance matrices and arrays of matrices with criteria violations contained in "matrices". All matrices and arrays exist for the 1:5000 level, 1:20 000 level, major-type level and major-type group level.
