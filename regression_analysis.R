###################################################################################################
# Authors: Olga Churkina, Luisa Nazareno, Matteo Zullo
# Title: Complementaries of Language and Non-Language Skills
# Error reports and other questions: mzullo@gatech.edu
###################################################################################################

# install packages
packages <- c("tidyverse","multiwayvcov","lmtest","Hmisc","sandwich","oaxaca","stargazer","dineq")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))

# e.g., bilingual-L2
# endowments: 2.811176e-02
# dollars: -499.38551
# The cog endowments effect is (+), which means that the gap closes. Not only it closes, but l2learner would be actually making more. How much? Around 500 more
# e.g. -7.563687e-03	570.30936
# The man endowment effect is (-), which means that l2 learner would be making 570 less than bilingual, which is larger than initial gap.

###################################################################################################
# Define functions
###################################################################################################

#' Read the bilinguals file and create logs of earnings and cohort variables.
#' 
#' @param datafile A string holding the name of input data.
#' @return tibble dataframe with the new columns.
#' @examples
#' read.bilinguals(data)

read_bilinguals <- function(datafile){
  data <- read_csv(
    datafile,
    na = ".",
    col_types = cols(
      .default = col_character(),
      "year" = col_factor(),
      "statefip" = col_factor(),
      "langstatus" = col_factor(),
      "age" = col_integer(),
      "perwt" = col_integer(),
      "yrimmig" = col_integer(),
      "uhrswork" = col_integer(),
      "incwage2005adj" = col_double(),
      "cognitive" = col_double(),
      "manual" = col_double(),
      "interpersonal" = col_double(),
      "cohort" = col_factor()
    )
  )
  
  # add cohort time trend
  data <- data %>% mutate(cohort_trend = as.integer(cohort)-min(as.integer(cohort)))
  
  # create log earnings
  data <- data %>% mutate(lnincwage2005adj = log(incwage2005adj))
  
  return(data)
}


#' Perform a twofold Oaxaca decomposition.
#' 
#' @param data A dataframe containing the input data.
#' @param y A string holding the name of the dependent variables.
#' @param X A vector holding the names of the independent variables.
#' @param groupvar A string holding the name of the variable holding the groups.
#' @param groups A vector holding the names of the two groups.
#' @param logged A boolean value indicating that the dependent variable is logged.
#' @return list with the difference between groups and the effects.
#' @examples
#' oaxaca_twofold(bilinguals, "incwage2005adj", c("ind", "uhrswork"), "langstatus", c('bilingual','monolingual'), logged = TRUE)

oaxaca_twofold <- function(data, y, X, groupvar, groups, logged = TRUE){
  
  # group means
  mean1 <- mean(as.matrix(data[data[[groupvar]] == groups[1], ][,y]))
  mean2 <- mean(as.matrix(data[data[[groupvar]] == groups[2], ][,y]))
  
  # set advantaged group 
  if (mean1-mean2 >0) {
    grp.max <- groups[1]
    grp.min <- groups[2]
  } else {
    grp.max <- groups[2]
    grp.min <- groups[1]
  }
  
  # regression formula
  oaxaca.formula <- as.formula(paste(paste(y,"~"), paste(X, collapse="+")))
  
  # run regressions
  reg.max <- lm(oaxaca.formula, data[data[[groupvar]] == grp.max, ])
  reg.min <- lm(oaxaca.formula, data[data[[groupvar]] == grp.min, ])
  
  # coefficents
  coef.max <- reg.max$coefficients
  coef.min <- reg.min$coefficients
  
  # endowments
  endow.max <- colMeans(model.matrix(reg.max))
  endow.min <- colMeans(model.matrix(reg.min))
  
  # differential
  y.max <- mean(reg.max$fitted.values)
  y.min <- mean(reg.min$fitted.values)
  y.diff <- y.max-y.min
  
  # coefficients effects
  coef <- endow.min*(coef.max-coef.min)
  
  # endowments effects
  endow <- coef.max*(endow.max-endow.min)
  
  # coefficients and endowments effect (% total)
  endow.perctot <- endow/y.diff * 100
  coef.perctot <- coef/y.diff * 100
  
  if (logged == TRUE) {
    # exponentiate terms
    y.diff.exp <- exp(y.max)-exp(y.min)
    endow.exp <- (y.diff-endow)*exp(y.max)
    coef.exp <- (y.diff-coef)*exp(y.max)
    # save results
    df <- cbind(endow,-endow.exp,endow.perctot,coef,-coef.exp,coef.perctot)
    colnames(df) <- c("endowments_log","endowments_abs","endowments_perctot","coefficients_log", "coefficients_abs", "coefficients_perctot")    
  } else {
    # save results
    df <- cbind(endow,endow.perctot,coef,coef.perctot)
    colnames(df) <- c("endowments","endowments_perctot","coefficients", "coefficients_perctot")
  }
  
  if (logged == TRUE) {
    out <- list(ydiff=-y.diff.exp, twofold=df)
  } else {
    out <- list(ydiff=-y.diff, twofold=df)
  }
  
  return(out)
}


#' Run a model and cluster standard errors by state.
#' 
#' @param data A dataframe holding the input data.
#' @param y A string holding the name of the dependent variable.
#' @param x A vector holding the names of the independent variables.
#' @param weights A string holding the name of the weighting variable.
#' @return list with the linear fit and output with clustering.
#' @examples
#' lm.wage2005(bilinguals, "lnincwage2005", c("age","educlevel"), weights = NULL)
lm.wage2005 <- function(data, y, X, weights = NULL) {
  lm.formula <- as.formula(paste(paste(y,"~"), paste(X, collapse="+")))
  
  # fit model
  lm.fit <- lm(lm.formula, weights = NULL, data)
  
  # cluster standard errors
  lm.clustered <- coeftest(lm.fit, vcov = vcovHC, cluster = ~ statefip)
  
  # add 95% CIs
  CI95_lwr <- lm.clustered[,1] - qnorm(1-0.05/2, lower.tail=TRUE)*lm.clustered[,2]
  CI95_upr <- lm.clustered[,1] + qnorm(1-0.05/2, lower.tail=TRUE)*lm.clustered[,2]
  lm.clustered <- cbind(lm.clustered,CI95_lwr,CI95_upr)
  
  out <- list(fit=lm.fit, clustered=lm.clustered)
  return(out)
}

###################################################################################################
# Load data
###################################################################################################

bilinguals <- read_bilinguals("bilinguals_matched.csv")

# create log earnings RIFs
bilinguals$lnincwage2005adj.pct10 <- rif(x=bilinguals$lnincwage2005adj, weights = NULL, method="quantile", quantile=0.1)
bilinguals$lnincwage2005adj.pct25 <- rif(x=bilinguals$lnincwage2005adj, weights = NULL, method="quantile", quantile=0.25)
bilinguals$lnincwage2005adj.pct50 <- rif(x=bilinguals$lnincwage2005adj, weights = NULL, method="quantile", quantile=0.50)
bilinguals$lnincwage2005adj.pct75 <- rif(x=bilinguals$lnincwage2005adj, weights = NULL, method="quantile", quantile=0.75)
bilinguals$lnincwage2005adj.pct90 <- rif(x=bilinguals$lnincwage2005adj, weights = NULL, method="quantile", quantile=0.9)

# regression parameters
x1 <- c("langstatus","year","sex", "age5cat", "race6", "marstatus", "educlevel", "cohort")
x2 <- c("langstatus","uhrswork","year","statefip","sex", "age5cat", "race6", "marstatus", "educlevel", "cohort")
x3 <- c("langstatus","uhrswork","year","statefip","ind","sex", "age5cat", "race6", "marstatus", "educlevel", "cohort")
x4 <- c("langstatus","cognitive","manual","interpersonal","uhrswork","year","statefip","ind","sex", "age5cat", "race6", "marstatus", "educlevel","cohort")
x.oaxaca <- c("cognitive","manual","interpersonal","uhrswork","year","statefip","ind","sex", "age5cat", "race6", "marstatus", "educlevel", "cohort")

# regressions
model1 <- lm.wage2005(bilinguals, "lnincwage2005adj", x1, weights = NULL)
model2 <- lm.wage2005(bilinguals, "lnincwage2005adj", x2, weights = NULL)
model3 <- lm.wage2005(bilinguals, "lnincwage2005adj", x3, weights = NULL)
model4 <- lm.wage2005(bilinguals, "lnincwage2005adj", x4, weights = NULL)

# unconditional quantile regressions
model4.pct10 <- lm.wage2005(bilinguals, "lnincwage2005adj.pct10", x4, weights = NULL)
model4.pct25 <- lm.wage2005(bilinguals, "lnincwage2005adj.pct25", x4, weights = NULL)
model4.pct50 <- lm.wage2005(bilinguals, "lnincwage2005adj.pct50", x4, weights = NULL)
model4.pct75 <- lm.wage2005(bilinguals, "lnincwage2005adj.pct75", x4, weights = NULL)
model4.pct90 <- lm.wage2005(bilinguals, "lnincwage2005adj.pct90", x4, weights = NULL)

# decompositions - bilingual and monolingual
bimono <- c('bilingual','monolingual')
oaxaca.bimono <- oaxaca_twofold(bilinguals, "lnincwage2005adj", x.oaxaca, "langstatus", bimono)
oaxaca.bimono.pct10 <- oaxaca_twofold(bilinguals, "lnincwage2005adj.pct10", x.oaxaca, "langstatus", bimono)
oaxaca.bimono.pct25 <- oaxaca_twofold(bilinguals, "lnincwage2005adj.pct25", x.oaxaca, "langstatus", bimono)
oaxaca.bimono.pct50 <- oaxaca_twofold(bilinguals, "lnincwage2005adj.pct50", x.oaxaca, "langstatus", bimono)
oaxaca.bimono.pct75 <- oaxaca_twofold(bilinguals, "lnincwage2005adj.pct75", x.oaxaca, "langstatus", bimono)
oaxaca.bimono.pct90 <- oaxaca_twofold(bilinguals, "lnincwage2005adj.pct90", x.oaxaca, "langstatus", bimono)

# decompositions - l2learner and monolingual
latemono <- c('l2learner','monolingual')
oaxaca.latemono <- oaxaca_twofold(bilinguals, "lnincwage2005adj", x.oaxaca, "langstatus", latemono)
oaxaca.latemono.pct10 <- oaxaca_twofold(bilinguals, "lnincwage2005adj.pct10", x.oaxaca, "langstatus", latemono)
oaxaca.latemono.pct25 <- oaxaca_twofold(bilinguals, "lnincwage2005adj.pct25", x.oaxaca, "langstatus", latemono)
oaxaca.latemono.pct50 <- oaxaca_twofold(bilinguals, "lnincwage2005adj.pct50", x.oaxaca, "langstatus", latemono)
oaxaca.latemono.pct75 <- oaxaca_twofold(bilinguals, "lnincwage2005adj.pct75", x.oaxaca, "langstatus", latemono)
oaxaca.latemono.pct90 <- oaxaca_twofold(bilinguals, "lnincwage2005adj.pct90", x.oaxaca, "langstatus", latemono)

# output models
stargazer(model1$fit, model2$fit, model3$fit, model4$fit,
          # output
          type = "latex", out = "regs.tex",
          # display
          style = "jpam", align=TRUE,  multicolumn = TRUE, digits = 3, single.row=TRUE,
          # column labels
          model.numbers=FALSE, column.labels = c("(1)", "(2)","(3)","(4)"),
          # dependent variable
          dep.var.labels.include = FALSE, dep.var.labels = NULL,
          # independent variables
          covariate.labels = c("Constant","Bilingual","L2 Learner","Cognitive","Manual","Interpersonal","Hours worked"),
          # errors
          #se = starprep(model1$fit, model2$fit, model3$fit, model4$fit, clusters = interaction(bilinguals$statefip,bilinguals$year), se_type = "stata"),
          se=list(model1$clustered[,2],model2$clustered[,2],model3$clustered[,2],model4$clustered[,2]),
          # statistics
          omit.stat=c("LL","ser","f","rsq"), intercept.bottom = FALSE,
          # fixed effects
          omit = c("state","ind","sex","age", "race", "marstatus", "educ", "cohort"), omit.labels = c("State","Industry","Sex","Age","Race","Marital","Education","Cohort")
)

# output RIF regression models
stargazer(model4.pct10$fit, model4.pct25$fit, model4.pct50$fit, model4.pct75$fit, model4.pct90$fit,
          # output
          type = "latex", out = "RIF.tex",
          # display
          style = "jpam", align=TRUE,  multicolumn = TRUE, digits = 3, single.row=TRUE,
          # column labels
          model.numbers=FALSE, column.labels = c("10 pct.", "25 pct.","50 pct.","75 pct.","90 pct."),
          # dependent variable
          dep.var.labels.include = FALSE, dep.var.labels = NULL,
          # independent variables
          covariate.labels = c("Cognitive","Manual","Interpersonal"),
          # errors
          ci.custom = list(
            coefci(model4.pct10$fit, vcov = vcovCL, cluster = ~ statefip + year),
            coefci(model4.pct25$fit, vcov = vcovCL, cluster = ~ statefip + year),
            coefci(model4.pct50$fit, vcov = vcovCL, cluster = ~ statefip + year),
            coefci(model4.pct75$fit, vcov = vcovCL, cluster = ~ statefip + year),
            coefci(model4.pct90$fit, vcov = vcovCL, cluster = ~ statefip + year)),
          # statistics
          omit.stat=c("LL","ser","f","rsq","adj.rsq","n"),
          # fixed effects
          omit = c("Constant","state","year","ind","lang","uhrswork","cohort")
)


# output decompositions

# create bilingual-monolingual table
oaxaca.bimono.tabout <- cbind(
  oaxaca.bimono$twofold[,c('endowments_abs','coefficients_abs'), drop=FALSE],
  oaxaca.bimono.pct10$twofold[,c('endowments_abs','coefficients_abs'), drop=FALSE],
  oaxaca.bimono.pct25$twofold[,c('endowments_abs','coefficients_abs'), drop=FALSE],
  oaxaca.bimono.pct50$twofold[,c('endowments_abs','coefficients_abs'), drop=FALSE],
  oaxaca.bimono.pct75$twofold[,c('endowments_abs','coefficients_abs'), drop=FALSE],
  oaxaca.bimono.pct90$twofold[,c('endowments_abs','coefficients_abs'), drop=FALSE]
)
difference <- c(oaxaca.bimono$ydiff,oaxaca.bimono.pct10$ydiff,oaxaca.bimono.pct25$ydiff,oaxaca.bimono.pct50$ydiff,oaxaca.bimono.pct75$ydiff,oaxaca.bimono.pct90$ydiff)
oaxaca.bimono.tabout <- rbind(difference,oaxaca.bimono.tabout)
rownames(oaxaca.bimono.tabout) <- c('Difference','Cognitive','Manual','Interpersonal')

# create l2learner-monolingual table
oaxaca.latemono.tabout <- cbind(
  oaxaca.latemono$twofold[,c('endowments_abs','coefficients_abs'), drop=FALSE],
  oaxaca.latemono.pct10$twofold[,c('endowments_abs','coefficients_abs'), drop=FALSE],
  oaxaca.latemono.pct25$twofold[,c('endowments_abs','coefficients_abs'), drop=FALSE],
  oaxaca.latemono.pct50$twofold[,c('endowments_abs','coefficients_abs'), drop=FALSE],
  oaxaca.latemono.pct75$twofold[,c('endowments_abs','coefficients_abs'), drop=FALSE],
  oaxaca.latemono.pct90$twofold[,c('endowments_abs','coefficients_abs'), drop=FALSE]
)
difference <- c(oaxaca.latemono$ydiff,oaxaca.latemono.pct10$ydiff,oaxaca.latemono.pct25$ydiff,oaxaca.latemono.pct50$ydiff,oaxaca.latemono.pct75$ydiff,oaxaca.latemono.pct90$ydiff)
oaxaca.latemono.tabout <- rbind(difference,oaxaca.latemono.tabout)
rownames(oaxaca.latemono.tabout) <- c('Difference','Cognitive','Manual','Interpersonal')

# fix output tables
rm.words <- c('state','year','ind','uhrswork','Intercept',"cohort")
oaxaca.bimono.tabout = oaxaca.bimono.tabout[!grepl(paste(rm.words, collapse='|'), row.names(oaxaca.bimono.tabout)),]
oaxaca.latemono.tabout = oaxaca.latemono.tabout[!grepl(paste(rm.words, collapse='|'), row.names(oaxaca.latemono.tabout)),]

# output decompositions
stargazer(oaxaca.bimono.tabout,
          # output
          type = "latex", dep.var.labels=c("aj","gd"), out = "oaxaca-bimono.tex",
          # display
          style = "jpam", align=FALSE,  multicolumn = TRUE, digits = 0, single.row=FALSE,
          # omit
          omit = c("state","year","ind")
)

stargazer(oaxaca.latemono.tabout,
          # output
          type = "latex", dep.var.labels=c("aj","gd"), out = "oaxaca-latemono.tex",
          # display
          style = "jpam", align=FALSE,  multicolumn = TRUE, digits = 0, single.row=FALSE,
          # omit
          omit = c("state","year","ind")
)

###################################################################################################
# Sensitivity Analysis
###################################################################################################

bilinguals.cohort <- read_bilinguals("bilinguals_matched-nocohort.csv")
bilinguals.spanish <- read_bilinguals("bilinguals_spanish1.csv")
bilinguals.asian <- read_bilinguals("bilinguals_asian1.csv")
bilinguals.chinese <- read_bilinguals("bilinguals_chinese2.csv")
bilinguals.hindi <- read_bilinguals("bilinguals_hindi1.csv")
bilinguals.european <- read_bilinguals("bilinguals_european1.csv")

# cohort
x4.cohort <- c("langstatus","cognitive","manual","interpersonal","uhrswork","year","statefip","ind","sex", "age5cat", "race6", "marstatus", "educlevel","cohort_trend","langstatus:cohort_trend")
model4.cohort <- lm.wage2005(bilinguals.cohort, "lnincwage2005adj", x4.cohort, weights = NULL)

# language subgroups
model4.spanish <- lm.wage2005(bilinguals.spanish, "lnincwage2005adj", x4, weights = NULL)
model4.asian <- lm.wage2005(bilinguals.asian, "lnincwage2005adj", x4, weights = NULL)
model4.chinese <- lm.wage2005(bilinguals.chinese, "lnincwage2005adj", x4, weights = NULL)
model4.hindi <- lm.wage2005(bilinguals.hindi, "lnincwage2005adj", x4, weights = NULL)
model4.european <- lm.wage2005(bilinguals.european, "lnincwage2005adj", x4, weights = NULL)

stargazer(model4.spanish$fit, model4.asian$fit, model4.chinese$fit, model4.hindi$fit,model4.european$fit,model4.cohort$fit,
          # output
          type = "latex", out = "regs-langgrp.tex",
          # display
          style = "jpam", align=TRUE,  multicolumn = TRUE, digits = 3, single.row=TRUE,
          # column labels
          model.numbers=FALSE, column.labels = c("Spanish", "Asian","Chinese","Hindi","European","Cohort"),
          # dependent variable
          dep.var.labels.include = FALSE, dep.var.labels = NULL,
          # independent variables
          covariate.labels = c("Constant","Bilingual","L2 Learner","Cognitive","Manual","Interpersonal","Hours worked"),
          # errors
          #se = starprep(model1$fit, model2$fit, model3$fit, model4$fit, clusters = interaction(bilinguals$statefip,bilinguals$year), se_type = "stata"),
          se=list(model4.spanish$clustered[,2],model4.asian$clustered[,2],model4.chinese$clustered[,2],model4.hindi$clustered[,2],model4.european$clustered[,2],model4.cohort$clustered[,2]),
          # statistics
          omit.stat=c("LL","ser","f","rsq"), intercept.bottom = FALSE,
          # fixed effects
          omit = c("year","state","ind","sex","age", "race", "marstatus", "educ","cohort"), omit.labels = c("year","State","Industry","Sex","Age","Race","Marital","Education","Cohort")
)
