rm(list = ls()) 

library(psych)
library(effsize)
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory


# Specify the path to your folder
folder_path <- "../"

# Find all CSV files recursively
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)

# Columns to anonymize
cols_to_anonymize <- c('IPAddress', 'LocationLatitude', 'LocationLongitude')

# Loop through each file
for (file in file_list) {
  # Read the CSV file
  data <- read.csv(file, stringsAsFactors = FALSE)
  
  # Check if the columns exist in the data
  cols_exist <- intersect(cols_to_anonymize, names(data))
  
  # Replace specified columns with "Anonymized"
  if (length(cols_exist) > 0) {
    data[cols_exist] <- "Anonymized"
  }
  
  # Write the modified data back to the same file
  write.csv(data, file = file, row.names = FALSE)
}




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

sd(n$perfect, na.rm = T)
sd(n$imperfect, na.rm = T)
sd(n$human, na.rm = T)

p <- n |> drop_na(perfect) |> select( perfect_1, perfect_2, perfect_3)
i <- n |> drop_na(imperfect) |> select( imperfect_1, imperfect_2, imperfect_3)
h <- n |> drop_na(human) |> select( human_1, human_2, human_3)

colnames(p) <- c( "creeped out", "uneasy", "unnerved" )
colnames(i) <- colnames(p)
colnames(h) <- colnames(p)

d <- rbind(p,i,h) 

cor(d)
