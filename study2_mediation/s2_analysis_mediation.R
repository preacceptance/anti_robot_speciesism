rm(list = ls())

library(tidyverse)
library(psych)
library(effsize)
library(boot)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
p <- read.csv ("s2_data_mediation.csv", header=T, sep=",")

## r value between soul_1 and conscious_1
alpha(p[c(19:20)], check.keys = TRUE)

## Average the measures
p$int<-(p$soul_1+p$conscious_1)/2

## Create variable for condition (I=imperfect, p=perfect, h=human)
p$cond[p$I==1]<-"I"
p$cond[p$P==1]<-"P"
p$cond[p$H==1]<-"H"

table(p$cond)

## t-tests
### Compare Cashier Comfort between the three conditions
t.test(p[p$cond == "I",]$cashier_1 ,p[p$cond == "P",]$cashier_1)
cohen.d(p[p$cond == "I",]$cashier_1 ,p[p$cond == "P",]$cashier_1)

t.test(p[p$cond == "P",]$cashier_1 ,p[p$cond == "H",]$cashier_1)
cohen.d(p[p$cond == "P",]$cashier_1 ,p[p$cond == "H",]$cashier_1)

## Compare intangible qualities between the three conditions
t.test(p[p$cond == "I",]$int ,p[p$cond == "P",]$int)
cohen.d(p[p$cond == "I",]$int ,p[p$cond == "P",]$int, na.rm=T)

t.test(p[p$cond == "P",]$int ,p[p$cond == "H",]$int)
cohen.d(p[p$cond == "P",]$int ,p[p$cond == "H",]$int, na.rm=T)

## Correlation test between comfort and intangible qualities
### All condition
cor.test(p$cashier_1, p$int)
### Exclude "Human" condition
cor.test(p[p$cond != "H",]$cashier_1, p[p$cond != "H",]$int)

## Simple Mediation Analysis
model.name <- function(d, i){q <- d[i,]
return((lm(q$int ~ q$cond)$coef[2])*
         (lm(q$cashier_1 ~ q$int + q$cond)$coef[2]))}

ind <- boot(p, model.name, R=1000); ind
boot.ci(ind, type = "bca", conf = 0.95)

## Exclude participants that noticed it was a robot
p <- subset(p, exclude==0)

## t-test post exclusion
t.test(p[p$cond == "P",]$cashier_1 ,p[p$cond == "H",]$cashier_1)
cohen.d(p[p$cond == "P",]$cashier_1 ,p[p$cond == "H",]$cashier_1, na.rm=T)

t.test(p[p$cond == "P",]$cashier_1 ,p[p$cond == "I",]$cashier_1)
cohen.d(p[p$cond == "P",]$cashier_1 ,p[p$cond == "I",]$cashier_1, na.rm=T)
