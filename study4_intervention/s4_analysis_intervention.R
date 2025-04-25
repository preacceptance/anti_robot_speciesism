rm(list = ls())

library(effsize)
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
c <- read.csv ("s4_data_intervention.csv", header=T, sep=",")

## Demographics
mean(c$age, na.rm=T)
prop.table(table(c$gender))

## Recode Condition
c$cond[c$FL_31_DO_yes==1] <- "conscious"
c$cond[c$FL_31_DO_no==1] <- "not"

variables <- c("engaging_1", "convincing_1", "knowledgeable_1", "MC_1", 
               "store_1", "restaurant_1", "companion_1", "safety_1", 
               "job_1", "distinctiveness_1")

c[, variables] |> drop_na() |> cor()

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


variables <- c("engaging_1", "convincing_1", "knowledgeable_1", "MC_1", 
                "store_1", "restaurant_1", "companion_1", "safety_1", 
                "job_1", "distinctiveness_1", "conseq")

# Calculate the standard deviation for each variable within each condition
sd_summary <- c %>%
  select(cond, all_of(variables)) %>%
  pivot_longer(cols = all_of(variables), names_to = "variable", values_to = "value") %>%
  group_by(cond, variable) %>%
  summarise(sd = sd(value, na.rm = TRUE), .groups = "drop")

sd_summary
