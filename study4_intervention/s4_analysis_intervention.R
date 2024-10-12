rm(list = ls())

library(effsize)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
c <- read.csv ("s4_data_intervention.csv", header=T, sep=",")

## Demographics
mean(c$age, na.rm=T)
prop.table(table(c$gender))

## Recode Condition
c$cond[c$FL_31_DO_yes==1] <- "conscious"
c$cond[c$FL_31_DO_no==1] <- "not"

## T-tests based on Table 1
t.test(c[c$cond != "conscious",]$engaging_1, c[c$cond == "conscious",]$engaging_1)
t.test(c[c$cond != "conscious",]$convincing_1, c[c$cond == "conscious",]$convincing_1)
t.test(c[c$cond != "conscious",]$knowledgeable_1, c[c$cond == "conscious",]$knowledgeable_1)
t.test(c[c$cond != "conscious",]$MC_1, c[c$cond == "conscious",]$MC_1)
t.test(c[c$cond != "conscious",]$store_1, c[c$cond == "conscious",]$store_1)
t.test(c[c$cond != "conscious",]$restaurant_1, c[c$cond == "conscious",]$restaurant_1)
t.test(c[c$cond != "conscious",]$companion_1, c[c$cond == "conscious",]$companion_1)

## Chi-squared test
prop.table(table(c[c$cond != "conscious",]$conseq))[[1]]
prop.table(table(c[c$cond == "conscious",]$conseq))[[1]]
chisq.test(table(c$conseq))

t.test(c[c$cond != "conscious",]$safety_1, c[c$cond == "conscious",]$safety_1)
t.test(c[c$cond != "conscious",]$job_1, c[c$cond == "conscious",]$job_1)
t.test(c[c$cond != "conscious",]$distinctiveness_1, c[c$cond == "conscious",]$distinctiveness_1)

## t-test with Effect Size
t.test(c[c$cond != "conscious",]$restaurant_1, c[c$cond == "conscious",]$restaurant_1)
cohen.d(c[c$cond != "conscious",]$restaurant_1, c[c$cond == "conscious",]$restaurant_1)

t.test(c[c$cond != "conscious",]$companion_1, c[c$cond == "conscious",]$companion_1)
cohen.d(c[c$cond != "conscious",]$companion_1, c[c$cond == "conscious",]$companion_1)
