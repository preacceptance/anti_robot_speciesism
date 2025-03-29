## clear workspace
rm(list = ls()) 

## install packages
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('tidyverse', "dplyr", "Hmisc")

## ================================================================================================================
##                                                  PRE-PROCESSING                 
## ================================================================================================================

## read in data
# set working directory to current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
d <- read_csv('data.csv')
d <- d[-c(1,2),]

d |>
  mutate_if(all.is.numeric, as.numeric) -> d

## Attention Check
d |>
  filter(att_1 == 2 & att_2 == 5) -> d

nrow(d)

## Comp Check
d |>
  filter(comp == 2) -> d

nrow(d)


## ================================================================================================================
##                                                 DEMOGRAPHICS                 
## ================================================================================================================

prop.table(table(d[d$gender == 1 | d$gender == 2,]$gender))[[1]]
mean(as.numeric(d$age), na.rm = T)

## ================================================================================================================
##                                                 ANALYSIS                
## ================================================================================================================

colnames(d)

# Intangible
t.test(d$consciousness_1, mu = 50, alternative = "less")
t.test(d$soul_1, mu = 50, alternative = "less")
t.test(d$meaning_1, mu = 50, alternative = "less")
t.test(d$empathy_1, mu = 50, alternative = "less")
t.test(d$well_being_1, mu = 50, alternative = "less")

# Tangible
t.test(d$growth_1, mu = 50, alternative = "less")
t.test(d$harmed_1, mu = 50, alternative = "less")
t.test(d$wear_tear_1, mu = 50, alternative = "less")

