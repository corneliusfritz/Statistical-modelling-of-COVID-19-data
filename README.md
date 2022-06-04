Overview
------------

This is the accompanying code repository for the paper Fritz et al. (2022) - ''Statistical modelling of COVID-19 data: Putting Generalized Additive Models to work''.

The code folder contains the R code for all three applications in the paper.

As it is not possible to publish the individual data provided by the
Bavarian State Office for Health and Food Safety (LGL) used for Sections 3 and
4, we construct a dataset on individual level based on publicly
available data on infections and hospitalisations by the Robert-Koch-Institut
(RKI, https://github.com/robert-koch-institut). Thus, results for these
two sections may differ from the results shown in the manuscript. In contrast
to the manuscript, the modelling of hospitalisations is based on the delay
between the reporting of an infection and the reporting of the hospitalisation.
The daily distribution of delays is based on preprocessed data by the
Hospitalization Nowcast Hub
(https://github.com/KITmetricslab/hospitalization-nowcast-hub).


Folder structure:

- Hospitalisation: Analysis of section 4
- ICU Occupancy: Analysis of Section 3
- Infections between age groups: Analysis of Section 5
- Simulated data: Construction of individual data based on publicly available
RKI data

