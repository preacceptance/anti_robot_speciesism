#====================================================================================
#                         Fooling Humans with Robots
#====================================================================================

## clear workspace
rm(list = ls()) 
options(download.file.method="libcurl")

library(tidyverse)

#====================================================================================
#                               PRE-PROCESSING
#====================================================================================

# import data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
df <- read.csv('data.csv') 

df <- df[-c(1,2),]

df |>
  filter(att_1 == "Paul", att_2 == "Purple") -> df

# Num participants
nrow(df)

df |>
  filter(comp == "The course is on Mastering Online Income") -> df

## Final Sample
nrow(df)

## Gender
prop.table(table(df$gender)) * 100
## Age
mean(as.numeric(df$age))

#====================================================================================
#                                 ANALYSIS
#====================================================================================

mention_bot <- function(x) grepl("robot|\\bbot\\b|humanoid|\\bai\\b|computer|generated", x, ignore.case = TRUE)

df |>
  mutate(
    detected_bot = mention_bot(q2)
  ) -> df

# Chi-Square Test
## Detected AI
table(df$detected_bot)
prop.table(table(df$detected_bot))
chisq.test(table(df$detected_bot))

## Interest
table(df$interest_1)
prop.table(table(df$interest_1))
chisq.test(table(df$interest_1))

## Meet
table(df$meeting)
prop.table(table(df$meeting))
chisq.test(table(df$meeting))

# end ============================================#
