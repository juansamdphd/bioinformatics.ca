# Assignment #2 ####

#==================================#
# Author: Juan C. Sanchez-Arias
# Using bioinformatics.ca Introduction to R Workshop materials
# and Assignment #1 master script.
# =================================#

## set working directory
setwd("~/GitHub/bioinformatics.ca/intro_to_r_workshop")

## read in Assignment_data2.txt data ####
df <- read.delim("Assignment_data2.txt", header = TRUE, sep = "", as.is = TRUE)

## inspect your data ####
str(df)
head(df)

## recode factors in your data ####

## malignant = 0,1 c("Benign","Malignant")
df$malignant_recoded <- factor(df$malignant, levels = c(0, 1),
                               labels = c("Benign","Malignant"))

## sex = 0,1 c("Male", "Female") ####
df$sex_recoded <- factor(df$sex, levels = c(0, 1),
                         labels = c("Male", "Female"))

## city = 0,1,2,3,4 c("Toronto","Addis Ababa","Paris","Sao Pauo","Manila")
df$city_recoded <- factor(df$city, levels = c(0,1,2,3,4),
                          labels = c("Toronto","Addis Ababa","Paris","Sao Pauo","Manila"))

## This data simulates data from a community skin cancer biopsy program. 
## Each city collected data on the people it successfully screened in the first month of 
## Operations and whether those individuals' biopsies came back as 
## Benign or Malignant

## First we'd like to know the malignancy rates overall
  ## use the table() function to find this

## Do malignancy rates differ signficantly by sex? 

## Do malignancy rates differ significantly by city? 

## Melt your <malignancy x city> table using reshape 
## and use ggplot to create barplots of malignancy rates by city. 
## Don't forget to make your ggplot look pretty!



