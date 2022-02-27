# Replication code for "Complementaries of Language and Non-Language Skills: Accumulation and Returns Effects"

This is the code repository for the Bilinguals Project. The R-code consists of four files: i) the Sparse Principal Component Analysis (SPCA); ii) preprocessing file for the Census data; iii) the three-way matching implementation; and iv) regression analysis and decomposition.

## Data sources
Data is obtained from two data sources:
- Census data: from Integrated Public Use Microdata Series (IPUMS) [doi:10.18128/D010.V8.0]([doi:10.18128/D010.V8.0). The following variables have been obtained for years 2005-2019: YEAR, SAMPLE, SERIAL, CBSERIAL, HHWT, CLUSTER, REGION, STATEFIP, METRO, STRATA, GQ, PERNUM, PERWT, SEX, AGE, MARST, BIRTHYR, RACE, RACED, HISPAN, HISPAND, BPL, BPLD, CITIZEN, YRIMMIG, YRSUSA1, LANGUAGE, LANGUAGED, SPEAKENG, EDUC, EDUCD, EMPSTAT, EMPSTATD, LABFORCE, CLASSWKR, CLASSWKRD, OCC2010, IND1990, WKSWORK2,	UHRSWORK, INCTOT, INCWAGE, INCOTHER, POVERTY, PWSTATE2. A detailed description of the variables is provided in the Census codebook ([here](https://usa.ipums.org/usa-action/variables/group)).
- Skill codes: from Occupation Information Network (O\*NET) [doi:10.26209/MJ1461305](doi:10.26209/MJ1461305).

## Installation

The R scripts are written in R 4.0.2 and use the packages listed below. To install the packages via Terminal/Command Line, navigate to the folder with all Rscripts and data files and open R, then specify the CRAN mirror using 'chooseCRANmirror(graphics=FALSE, ind=76)'. To install packages, run the following line 'install.packages(c("readr","tidyverse","readxl","psych","psychTools","GPArotation","rospca","dplyr","plot3D", "writexl","psy","fastDummies","ggalt","ggsci","scales","multiwayvcov","lmtest","Hmisc","sandwich","oaxaca","stargazer","dineq"))

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


## Implementation

- The original file downloaded from the IPUMS website is vary large (~16 GB) and the preprocessing takes an excess of 20 minutes to run. The output file *bilinguals_processed.csv* (~6.2 GB) includes the processed data which performs the following transformation of the dependent variable: monetary values are discounted to 2005 dollars and logged (*lnwage2005*). 
- The SPCA might be performed by supplying the input file *Skills_2010.xlsx* to the *bilinguals_SPCA.R* script. The script implements the SPCA algorithm reduces down the 35 skills obtained from O\*NET into three main components (sparse loadings and scores)
- 
-  
-    that were interpreted as representative of the cognitive, manual, and interpersonal dimensions of jobs. The finding was validated through exploratory factor analysis (EFA).
- 
- Launch a command line session and ``cd`` into the folder containing the input files. Then, run the script like:

```
$ Rscript Skills_2010.xlsx
```
- 
- The *bilinguals_threeway_matching.R* merges the *bilinguals_processed.csv* and *Skills_2010.xlsx* files and performs the three-way matching of bilinguals, late learners, and monolinguals. The script outputs the *bilinguals_matched.csv* file.
- 




Second, the R code `bilinguals_data_processing.R` cleans and modifies the data from IPUMS USA by assigning labels, trimming variables, and merging the dataset from IPUMS with the output from SPCA.

Third, the script `bilinguals_threeway_matching.R` implements three-way exact matching of the late learner and bilingual group, which pairs each language learner to possible monolingual counterparts with exactly the same values on selected covariates to reduce the sample space and the bias in the data. the matching is followed with the visualization of the resulting bias reduction. 

Finally, `bilinguals_regression_analysis.R` evaluates the wage gap between monolinguals and bilinguals and late learner and then decomposes the difference into endowments and returns effects for the matched sample. Regression adjustments include three types of skills, demographic characteristics, time and state fixed effects. To investigate distributional differences in returns to language skills, the script runs and visualize the regression analysis using recentered influence function (RIF) transforms of the dependent variable at each percentile. The distributional effects of skill accumulations are studied by employing endowments and returns estimates from unconditional quantile regressions in the spirit of Oaxaca-Blinder wage gap decomposition.

Finally, `EPS.eps` executes the mapping of skills into skill groups, namely cognitive, manual and interpersonal skills.
