# Replication code for ""Complementaries of Language and Non-Language Skills: Accumulation and Returns Effects"

This is the code repository for the Bilinguals Project over the span of 15 years: from 2005 to 2019. The R-code consists of 4 parts. The first file is a data processing script. The second and third files consist of three-way matching algorithm and sparce PCA analysis. The fourth file includes regression adjustments and Oaxaca-Blinder wage gap decomposition. The data is retrived from IPUMS USA.


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

First, the R code `Data_analysis.R` implements the following algorithms to reduce self-selection bias for the estimated treatment effect: propensity score matching, genetic matching, and matching frontier. The matching variables include property age and size in square footage, number of beds and baths, baseline electricity consumption (average consumption over the pre-treatment period), and property market value. These covariates are used to construct counterfactual to the treated units out of properties that did not receive HUD funding. 

The R code `Data_analysis.R` evaluates the overall treatment effect of HUD funding, along with the treatment effect per program and per project, on the monthly electricity consumption (logged normalized by sqft.) from 2004 to 2019. Regression adjustments include time and group fixed effects. The model is implemented to the initial dataset as well as to the datasets after matching, standard errors are clustered at the property ID level. The script also contains the code for the visualizations except the Albany map.

Finally, `EPS.svg` executes the mapping of skills into skill groups, namely cognitive, manual and interpersonal skills.
