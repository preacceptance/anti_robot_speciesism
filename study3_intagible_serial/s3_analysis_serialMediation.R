rm(list = ls())

library(tidyverse)
library(psych)
library(effsize)
library(ltm)
library(boot)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
p <- read.csv ("s3_data_serialMediation.csv", header=T, sep=",")

table(p$condition)

## Cronbach Alphas for the measures
cronbach.alpha(p[,c("comfort_1", "likely_1", "service_1")])
cronbach.alpha(p[,c("organism_1", "bio_1")])
cronbach.alpha(p[,c("soul_1", "conscious_1", "empathy_1", "meaning_1")], na.rm = T)

## Average the measures
p$int <- (p$soul_1+p$conscious_1+p$empathy_1+p$meaning_1)/4
p$dv <- (p$comfort_1 + p$likely_1 + p$service_1)/3
p$bio <- (p$organism_1+p$bio_1)/2

## T-test
### Service Quality
t.test(p[p$condition == "robot",]$dv, p[p$condition == "person",]$dv)
cohen.d(p[p$condition == "robot",]$dv, p[p$condition == "person",]$dv)

### Intuitions in Biology
t.test(p[p$condition == "robot",]$bio, p[p$condition == "person",]$bio)
cohen.d(p[p$condition == "robot",]$bio, p[p$condition == "person",]$bio)

### Intangible Qualities
t.test(p[p$condition == "robot",]$int, p[p$condition == "person",]$int)
cohen.d(p[p$condition == "robot",]$int, p[p$condition == "person",]$int, na.rm = T)

## Regression Analysis
### Intangible Qualities predicted by Intuitions in Biology
summary(lm(int ~ bio, p))
### Service Quality predicted by Intangible Qualities
summary(lm(dv ~ int, p))

## Serial Mediation
model.name <- function(d, i){q <- d[i,]

return((lm(q$bio ~ q$condition)$coef[2])*
         (lm(q$int ~ q$bio + q$condition)$coef[2])*
         (lm(q$dv ~ q$int + q$bio + q$condition)$coef[2]))}

ind <- boot(p, model.name, R=1000); ind
boot.ci(ind, type = "bca", conf = 0.95)

## Regression Analysis (2)
### Service Quality predicted by condition
summary(lm(dv ~ condition, p))
### with mediators as covariates
summary(lm(dv ~ condition + int + bio, p))
### Service Quality predicted by Intuitions in Biology
summary(lm(dv ~ bio, p))








