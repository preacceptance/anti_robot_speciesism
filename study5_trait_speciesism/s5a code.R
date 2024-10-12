rm(list = ls())

library(tidyverse)
library(Hmisc)
library(interactions)
library(sandwich)
library(ltm)
library(effsize)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
d <- read_csv("s5a data.csv")
d <- d[-c(1,2),]


p <- read_csv("s5a data part 2.csv")
p <- p[-c(1,2),]

names(d)

d %>%
  filter(Q1 == 2, Q2 == 1) %>%
  mutate(
    s1 = as.numeric(s1),
    s2 = as.numeric(s2),
    s3 = as.numeric(s3),
    s4r = 8 - as.numeric(s4r),
    mod = (s1 + s2 + s3 + s4r) / 4
  ) -> d

cronbach.alpha(d[,c("s1",'s2','s3','s4r')])

d <- d[,c("PROLIFIC_PID","mod","s1",'s2','s3','s4r')]

p %>%
  filter(Q2 == 2) %>%
  left_join(d, by = "PROLIFIC_PID") %>%
  mutate_if(all.is.numeric, as.numeric) -> p

s <- p %>%
  mutate(
    cond = scenarios_DO,
    dv = case_when(
      cond == "surprise" ~ (comf2_1 + likely2_1 + recommend2_1)/3,
      cond != "surprise" ~ (comfort_1 + likely_1 + recommend_1)/3
    )
  ) 

cronbach.alpha(s[,c("likely_1", "recommend_1", "comfort_1")])

s %>%
  filter(!(is.na(s$P) & is.na(s$H) & is.na(s$surprise)), # Remove if responded to none of the three items
         ResponseId != "R_XnpqkIlmsKFmJQl" # dropped the repeated participant 
         ) -> s 
   

## Ease
s$ease <- (s$easy_1 + s$confident_1)/2

t.test(s[s$cond == "P",]$ease, s[s$cond == "H",]$ease) 
cohen.d(s[s$cond == "P",]$ease, s[s$cond == "H",]$ease)

t.test(s[s$cond == "H",]$ease, s[s$cond == "surprise",]$ease)
cohen.d(s[s$cond == "H",]$ease, s[s$cond == "surprise",]$ease, na.rm=TRUE)

t.test(s[s$cond == "surprise",]$ease, s[s$cond == "P",]$ease)

## Service Evaluations
t.test(s[s$cond == "P",]$dv, s[s$cond == "H",]$dv) 
cohen.d(s[s$cond == "P",]$dv, s[s$cond == "H",]$dv)

t.test(s[s$cond == "H",]$dv, s[s$cond == "surprise",]$dv)
cohen.d(s[s$cond == "H",]$dv, s[s$cond == "surprise",]$dv)

t.test(s[s$cond == "surprise",]$dv, s[s$cond == "P",]$dv)
cohen.d(s[s$cond == "surprise",]$dv, s[s$cond == "P",]$dv)

## Interaction Regression
s$cond <- relevel(as.factor(s$cond), ref = "H")
summary(lm(dv ~ cond * mod, s))


# floodlight
s2<-subset(s, cond!="surprise")
s2$cond2[s2$cond=="H"]<-0
s2$cond2[s2$cond=="P"]<-1

m1 <- lm(dv ~ cond2 * mod, data = s2)
summary(m1)
sim_slopes(m1, pred = cond2, modx = mod, jnplot = TRUE)

summary(lm(dv ~ cond * mod, s))

## Regressions in Service Eval
lo <- subset(s2, mod < 4.38)
summary(lm(dv ~ cond2, lo))

hi <- subset(s2, mod > 4.38)
summary(lm(dv ~ cond2, hi)) # Beta = 1.27 here

