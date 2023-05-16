###################################################################################################
# Authors: Olga Churkina, Luisa Nazareno, Matteo Zullo
# Title: Complementaries of Language and Non-Language Skills
# Error reports and other questions: mzullo@gatech.edu & ochurkina@gatech.edu
###################################################################################################

# install packages
# packages <- c("tidyverse","multiwayvcov","lmtest","Hmisc","sandwich","oaxaca","stargazer","dineq","ggsci","ggplot2")

# installed_packages <- packages %in% rownames(installed.packages())
# if (any(installed_packages == FALSE)) {
#   install.packages(packages[!installed_packages])
# }

# load packages
# invisible(lapply(packages, library, character.only = TRUE))

library(tidyverse)
library(multiwayvcov)
library(lmtest)
library(Hmisc)
library(sandwich)
library(oaxaca)
library(stargazer)
library(dineq)
library(ggsci)
library(ggplot2)

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
    endow.exp <- endow.perctot*exp(y.diff)
    coef.exp <- coef.perctot*exp(y.diff)
    # save results
    df <- cbind(endow,endow.exp,endow.perctot,coef,coef.exp,coef.perctot)
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
x2 <- c("langstatus","uhrswork","year","lqnonenglish","sex", "age5cat", "race6", "marstatus", "educlevel", "cohort")
x3 <- c("langstatus","uhrswork","year","ind","lqnonenglish","sex", "age5cat", "race6", "marstatus", "educlevel", "cohort")
x4 <- c("langstatus","cognitive","manual","interpersonal","uhrswork","year","ind","lqnonenglish","sex", "age5cat", "race6", "marstatus", "educlevel","cohort")
x.oaxaca <- c("cognitive","manual","interpersonal","uhrswork","year","ind","lqnonenglish","sex", "age5cat", "race6", "marstatus", "educlevel", "cohort")

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
x4.cohort <- c("langstatus","cognitive","manual","interpersonal","uhrswork","year","ind","lqnonenglish","sex", "age5cat", "race6", "marstatus", "educlevel","cohort_trend","langstatus:cohort_trend")
model4.cohort <- lm.wage2005(bilinguals.cohort, "lnincwage2005adj", x4.cohort, weights = NULL)

# language subgroups
x4.spanish <- c("langstatus","cognitive","manual","interpersonal","uhrswork","year","ind","lqspanish","sex", "age5cat", "race6", "marstatus", "educlevel","cohort")
model4.spanish <- lm.wage2005(bilinguals.spanish, "lnincwage2005adj", x4.spanish, weights = NULL)
x4.asian <- c("langstatus","cognitive","manual","interpersonal","uhrswork","year","ind","lqasian","sex", "age5cat", "race6", "marstatus", "educlevel","cohort")
model4.asian <- lm.wage2005(bilinguals.asian, "lnincwage2005adj", x4.asian, weights = NULL)
x4.chinese <- c("langstatus","cognitive","manual","interpersonal","uhrswork","year","ind","lqchinese","sex", "age5cat", "race6", "marstatus", "educlevel","cohort")
model4.chinese <- lm.wage2005(bilinguals.chinese, "lnincwage2005adj", x4.chinese, weights = NULL)
x4.hindi <- c("langstatus","cognitive","manual","interpersonal","uhrswork","year","ind","lqhindi","sex", "age5cat", "race6", "marstatus", "educlevel","cohort")
model4.hindi <- lm.wage2005(bilinguals.hindi, "lnincwage2005adj", x4.hindi, weights = NULL)
x4.european <- c("langstatus","cognitive","manual","interpersonal","uhrswork","year","ind","lqeuropean","sex", "age5cat", "race6", "marstatus", "educlevel","cohort")
model4.european <- lm.wage2005(bilinguals.european, "lnincwage2005adj", x4.european, weights = NULL)

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



###################################################################################################
# Figures
###################################################################################################

## Unconditional Quantile Regression coefficients ##

# initialize vectors
BI = NULL
L2 <- NULL

# for each percentile, run RIF regression and save betas
for (PCT in seq(0.01, 1, by=0.01)){

  # create RIF
  bilinguals$lnincwage2005adj.pct <- rif(x=bilinguals$lnincwage2005adj, weights = NULL, method="quantile", quantile=PCT)

  # run RIF regression
  model4.pct <- lm.wage2005(bilinguals, "lnincwage2005adj.pct", x4, weights = NULL)

  # append to matrices
  BI <- rbind(BI, c(model4.pct$clustered["langstatusbilingual",],PCT))
  L2 <- rbind(L2, c(model4.pct$clustered["langstatusl2learner",],PCT))
}

# combine into single dataframe
df.plot.betas <- rbind(cbind(BI,'Bilingual'),cbind(L2,'L2'))
colnames(df.plot.betas) <- c('Estimate','SE','t','pval','CI95_lwr','CI95_upr','Percentile',"Language")

df.plot.betas <- as_tibble(df.plot.betas) %>% mutate(
  Language = as.factor(Language),
  Percentile = as.numeric(Percentile)*100,
  Estimate = as.numeric(Estimate),
  CI95_lwr = as.numeric(CI95_lwr),
  CI95_upr = as.numeric(CI95_upr)
  ) %>% filter(Percentile > 9 & Percentile < 91)

# plot betas
plot.betas <- ggplot(df.plot.betas, aes(x = Percentile, y = Estimate, color = Language)) +
  geom_line(aes(size = Language), alpha = 1, lwd=1) +
  scale_color_manual(labels = c("Bilingual", "Late learner"), values = c("#800000","#155F83")) +
  geom_ribbon(aes(ymin = CI95_lwr, ymax = CI95_upr, , fill = Language), alpha = 0.1, color = NA, show.legend = FALSE) +
  scale_fill_manual(labels = c("Bilingual", "Late "), values = c("#800000","#155F83")) +
  theme_classic() +
  theme(
    text=element_text(size=14),
    legend.title = element_blank(), legend.position = "bottom", legend.justification=c(0.5,1), 
    panel.grid.major = element_line(size = 0.5, linetype = 4),
    panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(min(df.plot.betas$CI95_lwr),max(df.plot.betas$CI95_upr))) +
  scale_x_continuous(breaks = seq(10, 90, by=20), labels = seq(10, 90, by=20)) +
    ylab(expression("Coefficient estimate")) +
    xlab("Percentile")

# save plots
ggsave(plot.betas, filename = "betas.png", type = "cairo", dpi = 300)
ggsave("betas.eps", device=cairo_ps)
ggsave(plot.betas, filename = "betas.tiff", device = "tiff", dpi = 300)


## Unconditional Quantile Regression decompositions ##

# initialize vectors
diff.BI <- NULL
diff.L2 <- NULL
cog.BI.endow <- NULL
cog.L2.endow <- NULL
man.BI.endow <- NULL
man.L2.endow <- NULL 
int.BI.endow <- NULL
int.L2.endow <- NULL
cog.BI.coef <- NULL
cog.L2.coef <- NULL
man.BI.coef <- NULL
man.L2.coef <- NULL
int.BI.coef <- NULL
int.L2.coef <- NULL

# initialize parameters
bimono <- c('bilingual','monolingual')
latemono <- c('l2learner','monolingual')

# for each percentile, run RIF regression and decompose
for (PCT in seq(0.01, 1, by=0.01)){

  # create RIF variable
  bilinguals$lnincwage2005adj.pct <- rif(x=bilinguals$lnincwage2005adj, weights = NULL, method="quantile", quantile=PCT)

  # run RIF regressions
  oaxaca.bimono <- oaxaca_twofold(bilinguals, "lnincwage2005adj.pct", x.oaxaca, "langstatus", bimono)
  oaxaca.latemono <- oaxaca_twofold(bilinguals, "lnincwage2005adj.pct", x.oaxaca, "langstatus", latemono)

  # append differences
  diff.BI <- rbind(diff.BI, c(oaxaca.bimono$ydiff, PCT))
  diff.L2 <- rbind(diff.L2, c(oaxaca.latemono$ydiff, PCT))

  # append endowments
  cog.BI.endow <- rbind(cog.BI.endow, c(oaxaca.bimono$twofold['cognitive','endowments_abs'],PCT))
  cog.L2.endow <- rbind(cog.L2.endow, c(oaxaca.latemono$twofold['cognitive','endowments_abs'],PCT))
  man.BI.endow <- rbind(man.BI.endow, c(oaxaca.bimono$twofold['manual','endowments_abs'],PCT))
  man.L2.endow <- rbind(man.L2.endow, c(oaxaca.latemono$twofold['manual','endowments_abs'],PCT))  
  int.BI.endow <- rbind(int.BI.endow, c(oaxaca.bimono$twofold['interpersonal','endowments_abs'],PCT))
  int.L2.endow <- rbind(int.L2.endow, c(oaxaca.latemono$twofold['interpersonal','endowments_abs'],PCT))

  # append coefficients
  cog.BI.coef <- rbind(cog.BI.coef, c(oaxaca.bimono$twofold['cognitive','coefficients_abs'],PCT))
  cog.L2.coef <- rbind(cog.L2.coef, c(oaxaca.latemono$twofold['cognitive','coefficients_abs'],PCT))
  man.BI.coef <- rbind(man.BI.coef, c(oaxaca.bimono$twofold['manual','coefficients_abs'],PCT))
  man.L2.coef <- rbind(man.L2.coef, c(oaxaca.latemono$twofold['manual','coefficients_abs'],PCT))  
  int.BI.coef <- rbind(int.BI.coef, c(oaxaca.bimono$twofold['interpersonal','coefficients_abs'],PCT))
  int.L2.coef <- rbind(int.L2.coef, c(oaxaca.latemono$twofold['interpersonal','coefficients_abs'],PCT))

}

# combine skill-group dataframes
cog.BI <- cbind(rbind(cbind(cog.BI.endow,'Endowments'),cbind(diff.BI,'Difference'),cbind(cog.BI.coef,'Coefficients')),'Cognitive','Bilingual')
man.BI <- cbind(rbind(cbind(man.BI.endow,'Endowments'),cbind(diff.BI,'Difference'),cbind(man.BI.coef,'Coefficients')),'Manual','Bilingual')
int.BI <- cbind(rbind(cbind(int.BI.endow,'Endowments'),cbind(diff.BI,'Difference'),cbind(int.BI.coef,'Coefficients')),'Interpersonal','Bilingual')
cog.L2 <- cbind(rbind(cbind(cog.L2.endow,'Endowments'),cbind(diff.L2,'Difference'),cbind(cog.L2.coef,'Coefficients')),'Cognitive','L2')
man.L2 <- cbind(rbind(cbind(man.L2.endow,'Endowments'),cbind(diff.L2,'Difference'),cbind(man.L2.coef,'Coefficients')),'Manual','L2')
int.L2 <- cbind(rbind(cbind(int.L2.endow,'Endowments'),cbind(diff.L2,'Difference'),cbind(int.L2.coef,'Coefficients')),'Interpersonal','L2')

# combine into single dataframe
df.plot.oaxaca <- rbind(cog.BI,man.BI,int.BI,cog.L2,man.L2,int.L2)
# rename columns
colnames(df.plot.oaxaca) <- c('Estimate','Percentile','Effect','Skill','Language')

df.plot.oaxaca <- as_tibble(df.plot.oaxaca) %>% mutate(
  Language = as.factor(Language),
  Skill = as.factor(Skill),
  Effect = as.factor(Effect),
  Percentile = as.numeric(Percentile)*100,
  Estimate = as.numeric(Estimate)
  ) %>% filter(Percentile > 9 & Percentile < 91)
  
plot.oaxaca <- ggplot(df.plot.oaxaca, aes(x = Percentile, y = Estimate, color = Effect, linetype = Effect)) + 
  geom_line(aes(size = Effect), alpha = 1) +
  geom_point(data = subset(df.plot.oaxaca[seq(1, nrow(df.plot.oaxaca), 2), ], Effect == "Difference"),size=3, shape=25, alpha =0.3) +
  scale_linetype_manual(values = c("solid", "longdash","solid")) +
  scale_size_manual(values = c(1,0.0,1)) +
  scale_colour_manual(values = c("#155F83","#000000","#800000")) +
  theme_classic() +
    theme(
    text=element_text(size=14),
    legend.title = element_blank(), legend.position = "bottom", 
    panel.grid.major.y = element_line(size = 0.5, linetype = "solid"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=1)
    ) +
  #scale_y_continuous(limits = c(min(df.plot.oaxaca$Estimate),max(df.plot.oaxaca$Estimate))) +
  scale_y_continuous(breaks = c(0,-1000,-2000,-3000), labels = c(0,-1000,-2000,-3000)) +
  scale_x_continuous(breaks = seq(10, 90, by=20), labels = seq(10, 90, by=20)) +
  facet_grid(Skill ~ Language, labeller = labeller(Language = c(Bilingual = "Bilingual", L2 = "Late learner"))) +
  # override legend
  guides(size = guide_legend(override.aes = list(shape = c(NA,24,NA), color = c("#155F83","#000000","#800000")))) +
    ylab("Total difference (USD)") +
    xlab("Percentile")

# save plots
ggsave(plot.oaxaca, filename = "oaxacas.png", type = "cairo", dpi = 300)
ggsave("oaxacas.eps", device=cairo_ps)
ggsave(plot.oaxaca, filename = "oaxacas.tiff", device = "tiff", dpi = 300)
