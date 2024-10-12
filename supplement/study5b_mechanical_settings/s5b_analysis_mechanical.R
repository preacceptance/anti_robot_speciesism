rm(list=ls())

library(tidyverse)
library(psych)
library(effsize)
library(ltm)
library(sjstats)
library(rstatix)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
h <- read.csv ("s5b_data_mechanical.csv", header=T, sep=",")

demographic <- read_csv("5b demos.csv")
mean(as.numeric(demographic$Age), na.rm = T)
prop.table(table(demographic$Sex))

## Code conditions
h$huro[h$roMa.1==2|h$roNu.1==2] <- "robot"
h$huro[h$huMa.1==1|h$huNu.1==1] <- "human"

h$job[h$roMa.1==2|h$huMa.1==1] <- "machine"
h$job[h$roNu.1==2|h$huNu.1==1] <- "nurse"

## r-value for comfort_1 and likely_1
alpha(h[,c("comfort_1", "likely_1")], check.keys = TRUE)
## Average measures
h$dv <- (h$comfort_1 + h$likely_1)/2

## ANOVA analysis
a <- aov(dv ~ as.factor(huro) * as.factor(job), data = h)
summary(a)
eta_squared(a)

## t-tests
### Robot v. Human
t.test(h[h$huro == "robot",]$dv, h[h$huro == "human",]$dv)
cohen.d(h[h$huro == "robot",]$dv, h[h$huro == "human",]$dv)

### Mechanic v. Nurse
t.test(h[h$job == "machine",]$dv, h[h$job == "nurse",]$dv)
cohen.d(h[h$job == "machine",]$dv, h[h$job == "nurse",]$dv)

## Proportion of participants rating that it is easy to imagine
prop.table(table(h$easy_1 >= 5))
prop.table(table(h$easy_1 >= 7))
prop.table(table(h$easy_1 >= 8))
