#====================================================================================
#                         Fooling Humans Experiment
#====================================================================================

library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
df <- read_csv("data.csv")
df <- df[-c(1,2),]

#====================================================================================
#                               PRE-PROCESSING
#====================================================================================

df |>
  filter(att_1 == "Paul", att_2 == "Purple") -> df

# Num participants
nrow(df)

df |>
  filter(comp == "You were asked to watch a video") -> df

## Final Sample
nrow(df)

## Gender
prop.table(table(df$gender)) * 100
## Age
mean(as.numeric(df$age))

#====================================================================================
#                                 ANALYSIS
#====================================================================================

mention_bot <- function(x) grepl("robot|\\bbot\\b|humanoid|\\bai\\b", x, ignore.case = TRUE)

df |>
  mutate(
    first_funnel = mention_bot(text_explain),
    second_funnel = ifelse(first_funnel, TRUE, mention_bot(q1)),
    third_funnel = ifelse(second_funnel, TRUE, mention_bot(q2)),
    last_funnel = ifelse(q3 == "Yes", TRUE, FALSE)
  ) -> df

# Chi-Square Test

# TRUE == Mentioned Bot, FALSE == otherwise
## First Funnel
table(df$first_funnel)
table(df$first_funnel)[2]/sum(table(df$first_funnel))
chisq.test(table(df$first_funnel))

## Second Funnel
table(df$second_funnel)
table(df$second_funnel)[2]/sum(table(df$second_funnel))
chisq.test(table(df$second_funnel))

## Third Funnel
table(df$third_funnel)
table(df$third_funnel)[2]/sum(table(df$third_funnel))
chisq.test(table(df$third_funnel))
table(df$third_funnel, df$Group)

## Last Funnel
table(df$last_funnel)
table(df$last_funnel)[2]/sum(table(df$last_funnel))
chisq.test(table(df$last_funnel))
table(df$last_funnel, df$Group)

## end ===================================================================#
