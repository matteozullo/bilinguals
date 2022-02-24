# Replication code for "Complementaries of Language and Non-Language Skills: Accumulation and Returns Effects"

This is the code repository for the Bilinguals Project over the span of 15 years: from 2005 to 2019. The R-code consists of 4 parts. The first file is a sparce principal component analysis (SPCA). The second and third files consist of data processing script and three-way matching algorithm. The fourth file includes regression adjustments and Oaxaca-Blinder wage gap decomposition. The data is retrived from 
Integrated Public Use Microdata Series (IPUMS) [doi:10.18128/D010.V8.0]([doi:10.18128/D010.V8.0) and Occupation Information Network (ONET) [doi:10.26209/MJ1461305](doi:10.26209/MJ1461305).


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

First, `bilinguals_SPCA.R` script implements sparce PCA to reduce down the 35 non-language skills acquired from ONET. The procedure identifies three main components (sparse loadings and scores) that were interpreted as representative of the cognitive, manual, and interpersonal dimensions of each job. The result was validated by exploratory factor analysis (EFA).

Second, the R code `bilinguals_data_processing.R` cleans and modifies the data from IPUMS USA by assigning labels, trimming variables, and merging the dataset from IPUMS with the output from SPCA.

Third, the script `bilinguals_threeway_matching.R` implements three-way exact matching of the late learner and bilingual group, which pairs each language learner to possible monolingual counterparts with exactly the same values on selected covariates to reduce the sample space and the bias in the data. the matching is followed with the visualization of the resulting bias reduction. 

Finally, `bilinguals_regression_analysis.R` evaluates the wage gap between monolinguals and bilinguals and late learner and then decomposes the difference into endowments and returns effects for the matched sample. Regression adjustments include three types of skills, demographic characteristics, time and state fixed effects. To investigate distributional differences in returns to language skills, the script runs and visualize the regression analysis using recentered influence function (RIF) transforms of the dependent variable at each percentile. The distributional effects of skill accumulations are studied by employing endowments and returns estimates from unconditional quantile regressions in the spirit of Oaxaca-Blinder wage gap decomposition.

Finally, `EPS.eps` executes the mapping of skills into skill groups, namely cognitive, manual and interpersonal skills.
