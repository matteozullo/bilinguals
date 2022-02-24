# Replication code for "Complementaries of Language and Non-Language Skills: Accumulation and Returns Effects"

This is the code repository for the Bilinguals Project over the span of 15 years: from 2005 to 2019. The R-code consists of 4 parts. The first file is a sparce principal component analysis (SPCA). The second and third files consist of data processing script and three-way matching algorithm. The fourth file includes regression adjustments and Oaxaca-Blinder wage gap decomposition. The data is retrived from IPUMS USA [doi:10.18128/D010.V8.0] and ONET [doi:10.26209/MJ1461305].


## Installation

The R scripts are written in R 4.0.2 with the following R packages used for the analysis:

### Data handling and visualisation
- 'tidyverse', including 'readr' and 'readxl' (read data), 'tibble' (update dataframes), 'dplyr' (manipulate data), 'ggplot2' (create graphics)
- 'ggalt' (extra coordinate systems, 'geoms', statistical transformations, scales and fonts for 'ggplot2')
- 'fastDummies' (dummies from categorical variables)
- 'ggsci' (color palettes for 'ggplot2')
- 'scales' (breaks and labels for axes and legends)

### Sparce principal component analysis
- 'rospca' (robust sparse PCA using the ROSPCA algorithm)
- 'GPArotation' (gradient projection algorithm rotation for factor analysis)
- 'psych' (multivariate analysis and scale construction using factor analysis and principal component analysis)
- 'psychTools' (support functions for 'psych')
- 'psy' (Cronbach's coefficient alpha)

### Regression analysis and wage gap decomposition
- 'lmtest' (testing linear regression models)
- 'sandwich' (robust and cluster errors)
- 'multiwayvcov' (multi-way clustering)
- 'Hmisc' (miscellaneous functions for data analysis)
- 'oaxaca' (Oaxaca-Blinder wage gap decomposition)
- 'stargazer' (convert output to LaTeX code, HTML code and ASCII text)
- 'dineq' (decomposition of income inequality, including 'rif' function)


## Data Analysis

The R code `bilinguals_data_processing.R` cleans and modifies the data by assigning labels, trimming variables, and merging the dataset from IPUMS with the output from SPCA.


First,  implements clea


the following algorithms to reduce self-selection bias for the estimated treatment effect: propensity score matching, genetic matching, and matching frontier. The matching variables include property age and size in square footage, number of beds and baths, baseline electricity consumption (average consumption over the pre-treatment period), and property market value. These covariates are used to construct counterfactual to the treated units out of properties that did not receive HUD funding. 

The R code `bilinguals_threeway_matching.R` evaluates the overall treatment effect of HUD funding, along with the treatment effect per program and per project, on the monthly electricity consumption (logged normalized by sqft.) from 2004 to 2019. Regression adjustments include time and group fixed effects. The model is implemented to the initial dataset as well as to the datasets after matching, standard errors are clustered at the property ID level. The script also contains the code for the visualizations except the Albany map.

`bilinguals_SPCA.R`

`bilinguals_regression_analysis.R`

Finally, `EPS.eps` executes the mapping of skills into skill groups, namely cognitive, manual and interpersonal skills.
