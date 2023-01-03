###################################################################################################
# Authors: Olga Churkina, Luisa Nazareno, Matteo Zullo
# Title: Complementaries of Language and Non-Language Skills
# Error reports and other questions: mzullo@gatech.edu
###################################################################################################

library(tidyverse)

###################################################################################################
# Load data
###################################################################################################

# Census data
bilinguals_full <- read_csv("data.csv",
                            na = ".",
                            col_types = cols(
                              .default = col_character(),
                              "year" = col_integer(),
                              "age" = col_integer(),
                              "perwt" = col_integer(),
                              "birthyr" = col_integer(),
                              "yrimmig" = col_integer(),
                              "uhrswork" = col_integer(),
                              "incwage" = col_integer()
                            ))

# languages
languages <- read_csv("languages.csv",
                      na = "",
                      col_types = cols(
                        .default = col_character()
                      ))

# industry data
industries <- read_csv("ind_labels.csv", col_types = cols(
  "ind1990_code" = col_integer(),
  "ind1990" = col_character()
))

# occupation data
occupations <- read_csv("occ_labels.csv", col_types = cols(
  "occ2010_code" = col_integer(),
  "occ2010" = col_character()
))

# lowercase labels
industries <- industries %>% mutate(ind1990 = tolower(ind1990))
occupations <- occupations %>% mutate(occ2010 = tolower(occ2010))

# left join to include numeric codes for industries and occupations in the main dataset
bilinguals <- left_join(bilinguals_full, occupations, by = "occ2010")
bilinguals <- left_join(bilinguals, industries, by = "ind1990")

###################################################################################################
# Set global parameters
###################################################################################################

### define variables

# years (keep)
year.keep <- c(
  2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019
)

# Di Nardo-Card wage in 2005 prices 
incwage.low <- 5381.008

# inflation rate
incwage.defl <- c(
  1,0.9683,0.9426,0.9074,0.9109,0.8957,0.8687,0.8511,0.8382,0.8253,0.8242,0.8136,0.7960,0.7773,0.7639
)

# Year of entry (early cutoff)
AgeEntryYoung <- 6

# Year of entry (late cutoff)
AgeEntryOld <- 16

# Language English
Eng <- "english"

### remove / keep cases

# employment
empstat.rm <- c("n/a","not in labor force")

# weeks worked
wkswork2.rm <- c("n/a")

# occupation
occ.rm <- c(9800,9810,9820,9830)

# industry
ind.rm <- c(0,940,941,942,950,951,952,960,992,999)

# speak English - remove non-English speakers
speakeng.rm  <- c("n/a (blank)","does not speak english","yes, but not well")

# place of origin - keep mainland US
bpl.rm <- c("american samoa","guam","puerto rico","u.s. virgin islands","other us possessions")

bpl.US <- c(
  "alabama","alaska","arizona","arkansas","california", "colorado","connecticut","delaware",
  "district of columbia", "florida", "georgia","hawaii","idaho","illinois","indiana","iowa",
  "kansas","kentucky","louisiana","maine", "maryland","massachusetts","michigan","minnesota",
  "mississippi","missouri","montana","nebraska","nevada","new hampshire","new jersey",
  "new mexico","new york","north carolina","north dakota", "ohio","oklahoma","oregon",
  "pennsylvania","rhode island", "south carolina","south dakota","tennessee","texas",
  "utah","vermont","virginia","washington","west virginia","wisconsin","wyoming"
)

# unemployed
empstat.unemp <- c("unemployed")

###################################################################################################
# Define Labels
###################################################################################################

# occupation
occ.lab <- list(
  "Management in Business, Science, and Arts" = c(10:430),
  "Business Operations Specialists" = c(500:730),
  "Financial Specialists"= c(800:950),
  "Computer and Mathematical" = c(1000:1240),
  "Architecture and Engineering" = c(1300:1540),
  "Technicians" = c(1550:1560),
  "Life, Physical, and Social Science" = c(1600:1980),
  "Community and Social Services" = c(2000:2060),
  "Legal" = c(2100:2150),
  "Education, Training, and Library" = c(2200:2550),
  "Arts, Design, Entertainment, Sports, and Media" = c(2600:2920),
  "Healthcare Practitioners and Technicians" = c(3000:3540),
  "Healthcare Support" = c(3600:3650),
  "Protective Service" = c(3700:3950),
  "Food Preparation and Serving" = c(4000:4150),
  "Building and Grounds Cleaning and Maintenance" = c(4200:4250),
  "Personal Care and Service" = c(4300:4650),
  "Sales and Related" = c(4700:4965),
  "Office and Administrative Support" = c(5000:5940),
  "Farming, Fisheries, and Forestry" = c(6005:6130),
  "Construction" = c(6200:6765),
  "Extraction" = c(6800:6940),
  "Installation, Maintenance, and Repair" = c(7000:7630),
  "Production" = c(7700:8965),
  "Transportation and Material Moving" = c(9000:9750)
)

# industry
ind.lab <- list(
  "Agriculture, forestry, and fisheries" = c(10:32),
  "Mining" = c(40:50),
  "Construction"= c(60),
  "Manufacturing" = c(100:392),
  "Transportation" = c(400:432),
  "Communications" = c(440:442),
  "Utilities and sanitary services" = c(450:472),
  "Wholesale" = c(500:571),
  "Retail" = c(580:691),
  "Fire" = c(700:712),
  "Business and repair" = c(721:760),
  "Personal services" = c(761:791),
  "Entertainment and recreation services" = c(800:810),
  "Professional and related services" = c(812:893),
  "Public administration" = c(900:932)
)

# marital status
marstatus.lab <- list(
  "Single" = c("never married/single"),
  "Widowed"  = c("widowed"),
  "Separated"  = c("separated","divorced"),
  "Married" = c("married, spouse present","married, spouse absent")
)

# education
educlevel.lab <- list(
  "Less than High School" = c("n/a or no schooling","nursery school to grade 4", "grade 5, 6, 7, or 8","grade 9","grade 10","grade 11"),
  "High School graduate"  = c("grade 12","1 year of college","2 years of college","3 years of college"),
  "College graduate"  = c("4 years of college","5+ years of college")
)

# metroarea
metroarea.lab <- list(
  "Mixed" = c("metropolitan status indeterminable (mixed)"),
  "Rural" = c("not in metropolitan area"),
  "Metro" = c("in metropolitan area: central/principal city status indeterminable (mixed)",
              "in metropolitan area: in central/principal city",
              "in metropolitan area: not in central/principal city")
)

cohort.lab <- list(
  "1910" = c(1910:1919),
  "1920" = c(1920:1929),
  "1930" = c(1930:1939),
  "1940" = c(1940:1949),
  "1950" = c(1950:1959),
  "1960" = c(1960:1969),
  "1970" = c(1970:1979),
  "1980" = c(1980:1989),
  "1990" = c(1990:1999),
  "2000" = c(2000:2009),
  "2010" = c(2010:2019)
)

lang.lab <- list(
  "european" = c("german","yiddish, jewish","dutch","swedish","danish","norwegian","icelandic","italian","french",
                 "portuguese","rumanian","celtic","greek","albanian","russian","ukrainian, ruthenian, little russian","czech",
                 "polish","slovak","serbo-croatian, yugoslavian, slavonian","slovene","lithuanian","other balto-slavic"),
  "spanish" = c("spanish"),
  "asian" = c("chinese","tibetan","burmese, lisu, lolo","kachin","thai, siamese, lao","japanese","korean","vietnamese",
              " other east/southeast asian","indonesian"," other malayan","filipino, tagalog","micronesian, polynesian","hawaiian"),
  "hindi" = c("hindi and related")
)

# race
race.lab <- list(
  "White" = c("white"),
  "Black"  = c("black/african american/negro"),
  "Latino"  = c("white","black/african american/negro","american indian or alaska native", "chinese",
                "japanese","other asian or pacific islander","other race, nec","two major races",
                "three or more major races"),
  "Indian" = c("american indian or alaska native"),
  "Asian" = c("chinese","japanese","other asian or pacific islander"),
  "Other" = c("other race, nec","two major races","three or more major races")
)

# hispanic
hispan.lab <- list(
  "White" = c("not hispanic"),
  "Black"  = c("not hispanic"),
  "Latino"  = c("mexican","puerto rican","cuban","other"),
  "Indian" = c("not hispanic"),
  "Asian" = c("not hispanic"),
  "Other" = c("not hispanic")
)

###################################################################################################
# Clean & modify data
###################################################################################################

bilinguals <- bilinguals %>%
  
  # exclude rows with NAs on at least one of the columns
  filter_at(
    vars(year, bpl, statefip, language, incwage, occ2010_code, ind1990_code, marst, educ, metro),all_vars(!is.na(.))
  )  %>% 
  
  # filter observations
  filter(
    year %in% year.keep,  # keep years
    !empstat %in% empstat.rm,  # keep employed and unemployed
    !occ2010_code %in% occ.rm,  # remove military and unemployed for 5 yrs or never worked
    !ind1990_code %in% ind.rm,  # remove military
    !speakeng %in% speakeng.rm, # keep speakers of English only
    !wkswork2 %in% wkswork2.rm, # remone n/a
    !bpl %in% bpl.rm # keep mainland Americans
  )  %>% #18775237
  
  # add income variables (low income, deflator)
  group_by(year) %>% 
  mutate(incwage_defl = incwage.defl[which(year.keep == as.name(year))]) %>% 
  ungroup() %>%
  # create standardized income variable (base year: 2005)
  mutate(incwage2005 = incwage*incwage_defl) %>%
  # keep observations if above income low
  filter(incwage2005 > incwage.low) %>%
  # adjust wage/week
  mutate(
    weeks = case_when(
      wkswork2=="1-13 weeks" ~ 7,
      wkswork2=="14-26 weeks" ~ 20,
      wkswork2=="27-39 weeks" ~ 33,
      wkswork2=="40-47 weeks" ~ 43.5,
      wkswork2=="48-49 weeks" ~ 48.5,
      wkswork2=="50-52 weeks" ~ 51),
    incwage2005adj = 52*incwage2005/weeks) %>%
  
  select(-c(incwage_defl)) %>% # take out the deflator variable -> 15718852

    # create variables
  mutate(
    # unemployed dummy
    unemp = ifelse(empstat %in% empstat.unemp, 1,0),
    # birthplace US dummy
    US = ifelse(bpl %in% bpl.US, 1, 0),
    # age of entry
    ageentry = ifelse(yrimmig>0, age-(year-yrimmig), NA),
    
    # age categories
    agecat = case_when(
        age %in% c(16:24) ~ "16-24",
        age %in% c(25:34) ~ "25-34",
        age %in% c(35:44) ~ "35-44",
        age %in% c(45:54) ~ "45-54",
        age %in% c(55:64) ~ "55-64",
        age %in% c(65:74) ~ "65-74",
        age %in% c(75:84) ~ "75-84",
        age %in% c(85:99) ~ "85-99"), 
    
    age5cat = case_when(
      age>=16 & age<20 ~ 1,
      age>=20 & age<25 ~ 2,
      age>=25 & age<30 ~ 3,
      age>=30 & age<35 ~ 4,
      age>=35 & age<40 ~ 5,
      age>=40 & age<45 ~ 6,
      age>=45 & age<50 ~ 7,
      age>=50 & age<55 ~ 8,
      age>=55 & age<60 ~ 9,
      age>=60 & age<65 ~ 10,
      age>=65 & age<70 ~ 11,
      age>=70 & age<75 ~ 12,
      age>=75 & age<80 ~ 13,
      age>=80 & age<85 ~ 14,
      age>=85 ~ 15),
  
    # language variables - does not include people speaking English but born outside of the US + ones whose year of entry is not known
    langstatus = case_when(
      US==1 & language==Eng ~ "monolingual",
      language!=Eng & ageentry<=AgeEntryYoung ~ "bilingual",
      language!=Eng & ageentry>AgeEntryYoung & ageentry <= AgeEntryOld ~ "l2learner",
      language!=Eng & ageentry>AgeEntryOld ~ "oldl2learner"
    )) %>%
  
  
  # filter observations
  filter(
    is.na(ageentry) | ageentry >=-1, #-1 accounts for months
    !is.na(langstatus),
    langstatus!="oldl2learner"
    )  %>% 

# remove top 5% income earners
group_by(year, statefip) %>% 
  mutate(incwage.pct = percent_rank(incwage2005)) %>% 
  ungroup() %>%
  filter(incwage.pct < 0.995) #13196871


### language subgroups

bilinguals <- left_join(bilinguals,languages, by = "language")
bilinguals <- bilinguals %>% mutate(language_grp=ifelse(is.na(language_grp),"other",language_grp))

###################################################################################################
# Assign labels
###################################################################################################

bilinguals <- bilinguals %>%
  mutate(
    occ=NA,
    ind=NA,
    marstatus=NA,
    educlevel=NA,
    metroarea=NA,
    cohort=NA,
    race6=NA
  )

# occupation
for (j in 1:length(occ.lab)){
  bilinguals <- mutate(bilinguals, occ := ifelse(occ2010_code  %in%  occ.lab[[j]], labels(occ.lab[j]), occ))
}

# industry
for (j in 1:length(ind.lab)){
  bilinguals <- mutate(bilinguals, ind := ifelse(ind1990_code  %in%  ind.lab[[j]], labels(ind.lab[j]), ind))
}

# marital status
for (j in 1:length(marstatus.lab)){
  bilinguals <- mutate(bilinguals, marstatus := ifelse(marst  %in%  marstatus.lab[[j]], labels(marstatus.lab[j]), marstatus))
}

# educational level
for (j in 1:length(educlevel.lab)){
  bilinguals <- mutate(bilinguals, educlevel := ifelse(educ %in%  educlevel.lab[[j]], labels(educlevel.lab[j]), educlevel))
}

# metroarea
for (j in 1:length(metroarea.lab)){
  bilinguals <- mutate(bilinguals, metroarea := ifelse(metro %in%  metroarea.lab[[j]], labels(metroarea.lab[j]), metroarea))
}

# cohort
for (j in 1:length(cohort.lab)){
  bilinguals <- mutate(bilinguals, cohort := ifelse(birthyr %in%  cohort.lab[[j]], labels(cohort.lab[j]), cohort))
}

# race
for (j in 1:length(race.lab)){
  bilinguals <- mutate(bilinguals, race6 = ifelse(race  %in%  race.lab[[j]] & hispan %in% hispan.lab[[j]], labels(race.lab[j]), race6)) 
}

### save datafile
write.csv(bilinguals, "bilinguals_processed.csv") 
