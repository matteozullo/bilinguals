###################################################################################################
# Authors: Olga Churkina, Luisa Nazareno, Matteo Zullo
# Title: Complementaries of Language and Non-Language Skills
# Error reports and other questions: ochurkina@gatech.edu
###################################################################################################

library(tidyverse)
library(psych)
library(psychTools)
library(GPArotation)
library(rospca)
library(writexl)
library(psy)

###################################################################################################
# Load and merge data
###################################################################################################

# merge skills data with crosswalks to create a final spreadsheet of 427 Census occupations
# census_skills <- read_excel("Skills.xlsx", sheet = "Skills_final") # start for merging crosswalks
# onet_census_2010 <- read_excel("Crosswalks.xlsx", sheet="ONET_IPUMS_CENSUS_2010")
# census_skills <- merge(onet_census_2010,census_skills,by="ONET",all.x = TRUE,all.y = TRUE) # merging crosswalks
# write_xlsx(census_skills, "Skills_2010.xlsx") # creating final documents

# Cobb-Douglas skill importance (2/3) & level (1/3)
skills <- read_excel("Skills_2010.xlsx", 
                     sheet = "CD_for_R")

###################################################################################################
# Exploratory Factor Analysis (EFA) 
###################################################################################################

# EFA for number of groups
efa_skills <- fa.parallel(skills[,c(-1)], fm = "minres", fa = "fa") 

# cognitive/manual/interpersonal loadings
factors_skills_3 <- fa(skills[,c(-1)],nfactors = 3,rotate = "varimax", fm = "minres")
print(factors_skills_3$loadings,cutoff = 0.4)

## graphical representation of "cognitive/manual/interpersonal"
fa.diagram(factors_skills_3)

###################################################################################################
# Sparse Principal Component Method (SPCA) 
###################################################################################################

# SPCA for reduced linear combinations
skills_spca <- selectLambda(skills[,c(-1)],3,method = "SCoTLASS", lmin=1) 
skills_spca_eigen <- skills_spca$fit$eigenvalues
skills_spca_eigen

# loadings on the principal component = weights for each original variable
skills_loadings <- round(skills_spca$loadings,2) 
skills_loadings

# original data in a rotated coordinate system (OCC -> skills)
skills_scores <- skills_spca$fit$scores 
skills_scores[1:10,]

# by skill types

cognitive_scores <- skills_scores[,1]
skills$cognitive_scores <- cognitive_scores
skills <- skills %>%
  mutate(cognitive = percent_rank(cognitive_scores))

manual_scores <- skills_scores[,2]
skills$manual_scores <- manual_scores
skills <- skills %>%
  mutate(manual = percent_rank(manual_scores))

interpersonal_scores <- skills_scores[,3]
skills$interpersonal_scores <- interpersonal_scores
skills <- skills %>%
  mutate(interpersonal = percent_rank(interpersonal_scores))

###################################################################################################
# Form skill tables 
###################################################################################################

### cognitive skills only

# Cronbach alpha for cog skills
alpha_cog <- cronbach(skills[,c("b1","b3","b5","b7","b8","b10","c1","sy1","sy2","sy3","t6","t7","t10")])
alpha_cog

# SPCA for cog skills
skills_spca_cognitive <- selectLambda(skills[,c("b1","b3","b5","b7","b8","b10",
                                          "c1","sy1","sy2","sy3","t6","t7","t10")],1,method = "SCoTLASS", lmin=1)
skills_spca_eigen_cognitive <- skills_spca_cognitive$fit$eigenvalues
skills_spca_eigen_cognitive

# loadings on the principal component
skills_loadings_cognitive <- round(skills_spca_cognitive$loadings,2)
skills_loadings_cognitive

# original data in a rotated coordinate system (OCC -> skills)
skills_scores_cognitive <- skills_spca_cognitive$fit$scores
skills_scores_cognitive[1:10,]

### manual skills only

# Cronbach alpha for man skills
alpha_man <- cronbach(skills[,c("t1","t2","t3","t4","t5","t8","t9","t11")])
alpha_man

# SPCA for man skills
skills_spca_manual <- selectLambda(skills[,c("t1","t2","t3","t4","t5","t8","t9","t11")],1,method = "SCoTLASS", lmin=1)
skills_spca_eigen_manual <- skills_spca_manual$fit$eigenvalues
skills_spca_eigen_manual

# loadings on the principal component
skills_loadings_manual <- round(skills_spca_manual$loadings,2)
skills_loadings_manual

# original data in a rotated coordinate system (OCC -> skills)
skills_scores_manual<- skills_spca_manual$fit$scores
skills_scores_manual[1:10,]

### interpersonal skills only

# Cronbach alpha for int skills
alpha_int <- cronbach(skills[,c("b2","b4","b6","b9","m1","m2","m3","m4","so1","so2","so3","so4","so5","so6")])
alpha_int

# SPCA for int skills
skills_spca_interpersonal <- selectLambda(skills[,c("b2","b4","b6","b9","m1","m2","m3","m4",
                                                    "so1","so2","so3","so4","so5","so6")],1,method = "SCoTLASS", lmin=1)
skills_spca_eigen_interpersonal <- skills_spca_interpersonal$fit$eigenvalues
skills_spca_eigen_interpersonal

# loadings on the principal component
skills_loadings_interpersonal <- round(skills_spca_interpersonal$loadings,2)
skills_loadings_interpersonal

# original data in a rotated coordinate system (OCC -> skills)
skills_scores_interpersonal <-skills_spca_interpersonal$fit$scores
skills_scores_interpersonal[1:10,]

###################################################################################################
# Sensitivity Check 
###################################################################################################

### STEM (cognitive subgroup)
alpha_stem <- cronbach(skills[,c("b5","b8","c1","t10")])

# SPCA
skills_spca_stem <- selectLambda(skills[,c("b5","b8","c1","t10")],1,method = "SCoTLASS", lmin=1)
skills_spca_eigen_stem <- skills_spca_stem$fit$eigenvalues

# loadings on the principal component
skills_loadings_stem <- round(skills_spca_stem$loadings,2)
skills_loadings_stem
skills_scores_stem <- skills_spca_stem$fit$scores
skills_scores_stem[1:10,]


### Non-STEM (cognitive subgroup)
alpha_non_stem<- cronbach(skills[,c("b1","b3","b7","b10","t6","t7")])
skills_spca_non_stem <- selectLambda(skills[,c("b1","b3","b7","b10","t6","t7")],1,method = "SCoTLASS", lmin=1)

# SPCA
skills_spca_eigen_non_stem <- skills_spca_non_stem$fit$eigenvalues
skills_spca_eigen_non_stem

# loadings on the principal component
skills_loadings_non_stem <- round(skills_spca_non_stem$loadings,2)
skills_loadings_non_stem
skills_scores_non_stem<- skills_spca_non_stem$fit$scores
skills_scores_non_stem[1:10,]


### system (cognitive subgroup)
alpha_sys <- cronbach(skills[,c("sy1","sy2","sy3")])

#SPCA
skills_spca_sys <- selectLambda(skills[,c("sy1","sy2","sy3")],1,method = "SCoTLASS", lmin=1)
skills_spca_eigen_sys <- skills_spca_sys$fit$eigenvalues

# loadings on the principal component
skills_loadings_sys <- round(skills_spca_sys$loadings,2)
skills_loadings_sys

skills_scores_sys <-skills_spca_sys$fit$scores
skills_scores_sys[1:10,]


### social (interpersonal subgroup)
alpha_soc<- cronbach(skills[,c("b9","so1","so2","so3","so4","so5","so6")])

# SPCA
skills_spca_soc <- selectLambda(skills[,c("b9","so1","so2","so3","so4","so5","so6")],1,method = "SCoTLASS", lmin=1)
skills_spca_eigen_soc <- skills_spca_soc$fit$eigenvalues

# loadings on the principal component
skills_loadings_soc <- round(skills_spca_soc$loadings,2)
skills_loadings_soc
skills_scores_soc <-skills_spca_soc$fit$scores
skills_scores_soc[1:10,]


### monitoring (cognitive subgroup)
alpha_mon<- cronbach(skills[,c("b2","b4","b6","m1","m2","m3","m4")])

# SPCA
skills_spca_mon <- selectLambda(skills[,c("b2","b4","b6","m1","m2","m3","m4")],1,method = "SCoTLASS", lmin=1)
skills_spca_eigen_mon <- skills_spca_mon$fit$eigenvalues

# loadings on the principal component
skills_loadings_mon <- round(skills_spca_mon$loadings,2)
skills_loadings_mon
skills_scores_mon <-skills_spca_mon$fit$scores
skills_scores_mon[1:10,]


### by subgroup

cog_f <- skills_scores_cognitive
skills$cog_f <- cog_f
skills <- skills %>%
  mutate(pcog = percent_rank(cog_f))

man_f <- skills_scores_manual
skills$man_f <- man_f
skills <- skills %>%
  mutate(pman = percent_rank(man_f))

int_f <- skills_scores_interpersonal
skills$int_fl <- int_f
skills <- skills %>%
  mutate(pint = percent_rank(int_f))

stem_f <- skills_scores_stem
skills$stem_f <- stem_f
skills <- skills %>%
  mutate(pstem = percent_rank(stem_f))

non_stem_f <- skills_scores_non_stem
skills$non_stem_f <- non_stem_f
skills <- skills %>%
  mutate(pnon_stem = percent_rank(non_stem_f))

sys_f <- skills_scores_sys
skills$sys_f <- sys_f
skills <- skills %>%
  mutate(psys = percent_rank(sys_f))

soc_f <- skills_scores_soc
skills$soc_f <- soc_f
skills <- skills %>%
  mutate(psoc = percent_rank(soc_f))

mon_f <- skills_scores_mon
skills$mon_f <- mon_f
skills <- skills %>%
  mutate(pmon = percent_rank(mon_f))

# combine vectors
write_xlsx(skills, "skills_factors.xlsx") # creating final documents
