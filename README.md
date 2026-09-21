# MS and hormones: Hair hormones and male services in wild vervet monkeys

This repository contains the R code used for the analyses presented in:

*Long-term endocrine profiles of male competitive status and service provision*.
**Granell Ruiz, M., van de Waal, E., van Schaik, C. P., & Bshary, R.**  

The study investigates whether long-term testosterone and cortisol concentrations in wild male vervet monkeys (*Chlorocebus pygerythrus*) are associated with male competitive status and participation in male services.

Data were collected as part of the [iNkawu Vervet Project](https://inkawuvervetproject.weebly.com/) at Mawana Game Reserve, KwaZulu-Natal, South Africa.

## Repository structure

### `Scripts/`

**`MSIndex_hair_hormones.R`**  
Calculates individual participation in four male services during the three months associated with each hair sample:

- alarm calling
- between-group conflict (BGC) participation
- sentinelling
- leading group crossings

Service measures account for the opportunities available to each male and changes in group membership during the sampling period.

**`Combine_dataset.R`**  
Combines the hair hormone data with male-service measures, dominance information, social integration and demographic variables to produce the dataset used in the statistical analyses.

Competitive status is calculated by combining a male's dominance rank with his recent rank trajectory.

**`Tanalysis.R`**  
Statistical analyses of hair testosterone concentrations. Fits the baseline model of testosterone and competitive status and subsequently tests whether each male service explains additional variation in testosterone.

**`Canalysis.R`**  
Statistical analyses of hair cortisol concentrations. Fits the baseline model of cortisol and competitive status and subsequently tests whether each male service explains additional variation in cortisol.

**`Plots.R`**  
Produces the main and supplementary visualisations used in the manuscript, including hormone–competitive status relationships, male-service effects and correlations among study variables.

## Analysis workflow

The repository includes the final analysis dataset (`HairHormonesMSdf.csv`), allowing the statistical analyses and figures reported in the manuscript to be reproduced without access to the long-term iNkawu Vervet Project database.

The main scripts required to reproduce the analyses are:

1. `Tanalysis.R` – testosterone analyses.
2. `Canalysis.R` – cortisol analyses.
3. `Plots.R` – manuscript figures and supplementary visualisations.

`Combine_dataset.R` documents how the final analysis dataset was assembled from the hair hormone data, male-service measures, dominance information, social integration and demographic data.

`MSIndex_hair_hormones.R` is provided for transparency and documents how the male-service measures were calculated from the long-term behavioural data. The underlying long-term behavioural datasets are not included in this repository, so this script is not intended to be run independently from the public repository.

Male services were quantified over the sampling period associated with each hair-hormone sample. Dominance rank was calculated using Elo ratings based on agonistic interactions, and competitive status incorporated both dominance rank and recent rank trajectory.

Testosterone and cortisol concentrations were log-transformed before analysis. Linear mixed-effects models included male identity as a random effect. Male services were added separately to the corresponding baseline hormone model and compared with the baseline model.

See the manuscript for the complete description of data collection, variable definitions and statistical analyses.

## Data availability

The repository contains the analysis code associated with the manuscript. Some source data derive from the long-term iNkawu Vervet Project database and are therefore not included directly in this repository.

Consequently, some scripts contain paths to the original project data and are provided primarily to document the processing and analysis workflow.

## Software

Analyses were conducted in R. Principal packages used across the scripts include:

`dplyr`, `tidyr`, `lubridate`, `purrr`, `lme4`, `DHARMa`, `emmeans`, `effects`, `ggplot2`, `ggpubr`, `patchwork`, and `sjPlot`.