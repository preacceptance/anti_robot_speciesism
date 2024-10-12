rm(list = ls()) 

library(psych)
library(effsize)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
n <- read.csv ("s1_data_effect.csv", header=T, sep=",")

## Demographics
table(n$gender)
mean(n$age, na.rm=T)

## r value between the ratings
alpha(n [c(18:20)], check.keys = TRUE)

## Average the ratings
n$perfect<-10-(n$perfect_1 + n$perfect_2 + n$perfect_3)/3
n$imperfect<-10-(n$imperfect_1 + n$imperfect_2 + n$imperfect_3)/3
n$human<-10-(n$human_1 + n$human_2 + n$human_3)/3

## t-tests of differences in means
t.test(n$perfect, n$imperfect)
cohen.d(n$perfect, n$imperfect, na.rm = T)

t.test(n$perfect, n$human)
cohen.d(n$perfect, n$human, na.rm = T)

