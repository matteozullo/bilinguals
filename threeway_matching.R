###################################################################################################
# Authors: Olga Churkina, Luisa Nazareno, Matteo Zullo
# Title: The Labor Market Outcomes of Bilinguals in the United States
# Error reports and other questions: mzullo@gatech.edu & ochurkina@gatech.edu
###################################################################################################

library(tidyverse)
library(fastDummies)
library(ggalt)
library(ggsci)
library(scales)

###################################################################################################
# Load data
###################################################################################################

bilinguals <- read_csv("bilinguals_processed.csv",
                       na = ".",
                       col_types = cols(
                         .default = col_character(),
                         "year" = col_integer(),
                         "age" = col_integer(),
                         "perwt" = col_integer(),
                         "birthyr" = col_integer(),
                         "yrimmig" = col_integer(),
                         "uhrswork" = col_integer(),
                         "incwage2005adj" = col_double(),
                         "cognitive" = col_double(),
                         "manual" = col_double(),
                         "interpersonal" = col_double()
                         )) #13,196,871

# skills
skills <- read_excel("skills_factors.xlsx")
names(skills)
names(skills)[1] <- "occ2010_code"
skills$occ2010_code <- as.integer(skills$occ2010_code)
bilinguals <- left_join(bilinguals,
                        skills[c("occ2010_code","cognitive","manual","interpersonal","pcog","pman","pint","pstem","pnon_stem","psys","psoc","pmon")],
                        by="occ2010_code")

# drop NAs
bilinguals <- bilinguals %>% filter(!is.na(cognitive)) #12,865,608

# recode NAs to "other"
bilinguals <- bilinguals %>% mutate(language_grp2 = if_else(language_grp2 == "NA","other",language_grp2))

###################################################################################################
# Function Definitions
###################################################################################################

#' Flags common rows on selected variables.
#' 
#' @param data.x A tibble dataframe containing the first set of observations.
#' @param data.y A tibble dataframe containing the second set of observations.
#' @param vars A list of variables on which co-occurrences must be evaluated.
#' @param dummyvar A string holding the name of the dummy variable.
#' @return tibble dataframe with the dummy variable.
#' @examples
#' match.check(data1, data2, c("age","weight","height"), "flag")
match.check <- function(data.x,data.y,vars,dummyvar){
  data.x <- within(
    data.x,
    check <- ifelse(
      # match on target variables
      is.na(match(do.call(paste, c(data.x[vars], sep = "")), do.call(paste, c(data.y[vars], sep = "")))),
      # code 1 if match, else 0
      0, 1))
  
  # rename dummy variable
  names(data.x)[names(data.x) == 'check'] <- dummyvar
  return(data.x)
}

#' Perform exact matching for three groups.
#' 
#' @param data A tibble dataframe containing the observations.
#' @param groupvar A string holding the group variable name.
#' @param groupvals A list of strings holding the group labels; larger group must be position 1.
#' @param matchvars A list of strings holding the matching variables.
#' @return tibble dataframe with the matched sample.
#' @examples
#' match.threeway(data, ethnicity, c("white","black","latino"), c("age","education"))
match.threeway <- function(data,groupvar,groupvals,matchvars){
  
  require(tidyverse)
  
  # control group
  grp0 <- data %>%
    filter(!!as.symbol(groupvar) == groupvals[1])
  
  # treatment group 1
  grp1 <- data %>%
    filter(!!as.symbol(groupvar) == groupvals[2])
  
  # treatment group 2
  grp2 <- data %>%
    filter(!!as.symbol(groupvar) == groupvals[3])
  
  # check that the other datasets contain observation
  grp0 <- match.check(grp0, grp1, matchvars, "check1")
  grp0 <- match.check(grp0, grp2, matchvars, "check2")
  grp1 <- match.check(grp1, grp0, matchvars, "check1")
  grp1 <- match.check(grp1, grp2, matchvars, "check2")
  grp2 <- match.check(grp2, grp0, matchvars, "check1")
  grp2 <- match.check(grp2, grp1, matchvars, "check2")
  
  # sort by matching variables
  grp0 <- grp0 %>% filter(check1 == 1, check2 == 1) %>% select(-one_of("check1","check2")) %>% arrange_at(matchvars)
  grp1 <- grp1 %>% filter(check1 == 1, check2 == 1) %>% select(-one_of("check1","check2")) %>% arrange_at(matchvars)
  grp2 <- grp2 %>% filter(check1 == 1, check2 == 1) %>% select(-one_of("check1","check2")) %>% arrange_at(matchvars)
  
  # save values (variables other than matching)
  grp0.full <- grp0 %>% select(-one_of(matchvars))
  grp1.full <- grp1 %>% select(-one_of(matchvars))
  grp2.full <- grp2 %>% select(-one_of(matchvars))
  
  # save unique values (matching variables)
  grp0.unique <- grp0 %>% group_by_at(matchvars) %>% mutate(n0 = n()) %>% select_at(c(matchvars,"n0")) %>% slice(1)
  grp1.unique <- grp1 %>% group_by_at(matchvars) %>% mutate(n1 = n()) %>% select_at(c(matchvars,"n1")) %>% slice(1)
  grp2.unique <- grp2 %>% group_by_at(matchvars) %>% mutate(n2 = n()) %>% select_at(c(matchvars,"n2")) %>% slice(1)
  
  # add counters to control group dataset
  grp0.unique <- grp0.unique %>% bind_cols(grp1.unique %>% ungroup() %>% select(n1))
  grp0.unique <- grp0.unique %>% bind_cols(grp2.unique %>% ungroup() %>% select(n2))
  
  # expand back to include values of variables other than matching
  grp0.unique <- grp0.unique %>% mutate(reps = n0) %>% uncount(reps) %>% bind_cols(grp0.full)
  grp1.unique <- grp1.unique %>% uncount(n1) %>% bind_cols(grp1.full)
  grp2.unique <- grp2.unique %>% uncount(n2) %>% bind_cols(grp2.full)
  
  # keep as many controls as sum of treatments by match group
  grp0.unique <- grp0.unique[sample(1:nrow(grp0.unique)), ]  # shuffle data
  grp0.match <- grp0.unique %>% mutate(ID = row_number()) %>% filter(ID-(n1 + n2)*2<1) %>% select(-one_of("n0","n1","n2"))
  
  # bind three dataframes
  match <- bind_rows(grp0.match,grp1.unique,grp2.unique) %>% ungroup() %>% select(-ID)
  
  # reorder variables as per source
  match <- match %>% select_at(colnames(data))
  
  return(match)
}

#' Perform exact matching for three groups with stratification by time.
#' 
#' @param data A tibble dataframe containing the observations.
#' @param timevar A string holding the time variable name.
#' @param groupvar A string holding the group variable name.
#' @param groupvals A list of strings holding the group labels (larger group must be position 1).
#' @param matchvars A list of strings holding the matching variables.
#' @return tibble dataframe with the matched sample.
#' @examples
#' match.threeway(data,"year",ethnicity, c("white","black","latino"), c("age","education"))
match.threeway.time <- function(data,timevar,groupvar,groupvals,matchvars) {
  
  require(tidyverse)
  
  # extract unique years
  time <- pull(distinct(data, !!as.symbol(timevar)))
  
  # for each time-period
  for(t in time){
    
    data.t <- data %>% filter(!!as.symbol(timevar) == t)
    
    # if first iteration, # initialize dataframe 
    if (which(t == time) == 1){
      matched <- match.threeway(data.t, groupvar, groupvals, matchvars)
    } else {
      # else, append to dataframe
      matched <- bind_rows(matched, match.threeway(data.t, groupvar, groupvals, matchvars))
    }
  }
  return(matched)
}

###################################################################################################
# Perform Matching
###################################################################################################

groupvar <- "langstatus"
groupvals <- c("monolingual","bilingual","l2learner")
matchvars <- c("sex","race6","marstatus","educlevel","cohort","age5cat")

bilinguals_matched <- match.threeway.time(bilinguals,"year",groupvar,groupvals,matchvars)
write.csv(bilinguals_matched,"bilinguals_matched.csv") #818,987
#write.csv(bilinguals, "bilinguals_matched_nocohort.csv", row.names = FALSE) # matching without cohort

###################################################################################################
# Plot Bias Reduction
###################################################################################################

# create dummies for matching variables
bilinguals <- bilinguals %>% dummy_cols(select_columns = c("sex","race6","marstatus","educlevel","cohort","age5cat"))
bilinguals_matched <- bilinguals_matched %>% dummy_cols(select_columns = c("sex","race6","marstatus","educlevel","cohort","age5cat"))

# covariate means and standard deviations of monolinguals
mean_mono_bef <- bilinguals %>%
  filter(langstatus=='monolingual') %>%
  summarise_if(is.numeric, mean)
sd_mono_bef <- bilinguals %>%
  filter(langstatus=='monolingual') %>%
  summarise_if(is.numeric, sd)
mean_mono_aft <- bilinguals_matched %>%
  filter(langstatus=='monolingual') %>%
  summarise_if(is.numeric, mean)
sd_mono_aft <- bilinguals_matched %>%
  filter(langstatus=='monolingual') %>%
  summarise_if(is.numeric, sd)

# covariate means and standard deviations of bilinguals
mean_bi_bef <- bilinguals %>%
  filter(langstatus=='bilingual') %>%
  summarise_if(is.numeric, mean)
sd_bi_bef <- bilinguals %>%
  filter(langstatus=='bilingual') %>%
  summarise_if(is.numeric, sd)
mean_bi_aft <- bilinguals_matched %>%
  filter(langstatus=='bilingual') %>%
  summarise_if(is.numeric, mean)
sd_bi_aft <- bilinguals_matched %>%
  filter(langstatus=='bilingual') %>%
  summarise_if(is.numeric, sd)

# covariate means and standard deviations of late learners
mean_late_bef <- bilinguals %>%
  filter(langstatus=='l2learner') %>%
  summarise_if(is.numeric, mean)
sd_late_bef <- bilinguals %>%
  filter(langstatus=='l2learner') %>%
  summarise_if(is.numeric, sd)
mean_late_aft <- bilinguals_matched %>%
  filter(langstatus=='l2learner') %>%
  summarise_if(is.numeric, mean)
sd_late_aft <- bilinguals_matched %>%
  filter(langstatus=='l2learner') %>%
  summarise_if(is.numeric, sd)

# average variance
avg_bi_bef <- sqrt((sd_bi_bef^2 + sd_mono_bef^2)/2)
avg_bi_aft <- sqrt((sd_bi_aft^2 + sd_mono_aft^2)/2)
avg_late_bef <- sqrt((sd_late_bef^2 + sd_mono_bef^2)/2)
avg_late_aft <- sqrt((sd_late_aft^2 + sd_mono_aft^2)/2)

# standardized percent differences
std_bi_dif_bef <- 100*(mean_bi_bef[11:49]-mean_mono_bef[11:49])/avg_bi_bef[11:49]
std_bi_dif_aft <- 100*(mean_bi_aft[8:44]-mean_mono_aft[8:44])/avg_bi_aft[8:44]
std_late_dif_bef <- 100*(mean_late_bef[11:49]-mean_mono_bef[11:49])/avg_late_bef[11:49]
std_late_dif_aft <- 100*(mean_late_aft[8:44]-mean_mono_aft[8:44])/avg_late_aft[8:44]

# assign variable names
names <- c("Sex", "Race", "Marital Status", "Education Level", "Cohort", "Age Categories (5 years)")
covariates <- factor(names, ordered = TRUE, levels = rev(names))

### bilingials vs. monolinguals

# create vectors and data frame
before_bi <- c(sum(std_bi_dif_bef[1:2])/2,sum(std_bi_dif_bef[3:8])/6,sum(std_bi_dif_bef[9:12])/4,
            sum(std_bi_dif_bef[13:15])/3,sum(std_bi_dif_bef[18:25])/8,sum(std_bi_dif_bef[26:39])/14)
after_bi <- c(sum(std_bi_dif_aft[1:2])/2,sum(std_bi_dif_aft[3:8])/6,sum(std_bi_dif_aft[9:12])/4,
           sum(std_bi_dif_aft[13:15])/3,sum(std_bi_dif_aft[16:23])/8,sum(std_bi_dif_aft[24:37])/14)

bi_bias_df <- data.frame('Covariates' = covariates,
                          'Before' = before_bi,
                          'After' = after_bi)
# plot bias reduction
gg_bi <- ggplot(bi_bias_df, aes(x=After, xend=Before, y=Covariates, group=Covariates)) +
  geom_vline(xintercept = 0) +
  scale_color_uchicago() +
  xlim(-3,5) +
  geom_dumbbell(colour='#800000', 
                size=2, 
                dot_guide=TRUE, dot_guide_size=0.25) + 
  labs(x="", y="", title="") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#ffffff"),
        panel.background=element_rect(fill="#ffffff"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_line(linetype=2),
        panel.grid.major.x=element_blank(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(gg_bi)

### late learners vs. monolinguals bias reduction

# create vectors and data frame
before_late <- c(sum(std_late_dif_bef[1:2])/2,sum(std_late_dif_bef[3:8])/6,sum(std_late_dif_bef[9:12])/4,
               sum(std_late_dif_bef[13:15])/3,sum(std_late_dif_bef[18:25])/8,sum(std_late_dif_bef[26:39])/14)
after_late <- c(sum(std_late_dif_aft[1:2])/2,sum(std_late_dif_aft[3:8])/6,sum(std_late_dif_aft[9:12])/4,
              sum(std_late_dif_aft[13:15])/3,sum(std_late_dif_aft[16:23])/8,sum(std_late_dif_aft[24:37])/14)

late_bias_df <- data.frame('Covariates' = covariates,
                         'Before' = before_late,
                         'After' = after_late)
# plot bias reduction 
gg_late <- ggplot(late_bias_df, aes(x=After, xend=Before, y=Covariates, group=Covariates)) +
  geom_vline(xintercept = 0) +
  scale_color_uchicago() +
  xlim(-3,5) +
  geom_dumbbell(colour = '#155F83',
                size=2, 
                dot_guide=TRUE, dot_guide_size=0.25) + 
  labs(x="", y="", title="") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#ffffff"),
        panel.background=element_rect(fill="#ffffff"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_line(linetype=2),
        panel.grid.major.x=element_blank(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(gg_late)
