rm(list=ls())

library(tidyverse)
library(effsize)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
h <- read.csv ("s5c_data_robotBenefits.csv", header=T, sep=",")

demographics <- read.csv("5c demos.csv")
mean(as.numeric(demographics$Age), na.rm = T)
prop.table(table(demographics[demographics$Sex != "CONSENT_REVOKED",]$Sex))

## Recoding Condition
h$group[h$human.1==1] <- "human"
h$group[h$bot.1==1] <- "robot"
h$group[h$benefits.1==1] <- "robotB"

table(h$group)

## Average measures
h$dv<-(h$comfort_1+h$likely_1)/2

## t-tests
### Human v. Robot
t.test(h[h$group == "human",]$dv, h[h$group == "robot",]$dv)
cohen.d(h[h$group == "human",]$dv, h[h$group == "robot",]$dv)

### Robot v. Robot with benefits
t.test(h[h$group == "robot",]$dv, h[h$group == "robotB",]$dv)
cohen.d(h[h$group == "robot",]$dv, h[h$group == "robotB",]$dv)
