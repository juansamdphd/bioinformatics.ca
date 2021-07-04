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

## First we'd like to know the malignancy rates overall ####
  ## use the table() function to find this
table(df$malignant_recoded, df$city_recoded)

barplot(table(df$malignant_recoded, df$city_recoded),
        beside = TRUE,
        col = (c("white", "grey")),
        ylim = c(0, 250),
        cex.names = 1.5,
        cex.axis = 1.5)
legend(x = 12, 250,
       legend = levels(df$malignant_recoded),
       fill = c("white", "grey"),
       cex = 1.2)

## Do malignancy rates differ signficantly by sex?
chisq.test(table(df$malignant_recoded, df$sex_recoded))

## Do malignancy rates differ significantly by city? 
chisq.test(table(df$malignant_recoded, df$city_recoded))

## Melt your <malignancy x city> table using reshape ####
## and use ggplot to create barplots of malignancy rates by city. 
## Don't forget to make your ggplot look pretty!
library(reshape)
library(ggplot2)

df_mlt <- melt(data = table(df$malignant_recoded, df$city_recoded),
               id.vars = c("Toronto", "Addis Ababa", "Paris", "Sao Pauo", "Manila"),
               measure.vars = c("Benign", "Malignant"),
               variable_name = "Count")

names(df_mlt) <- c("Malignancy", "City", "Count")

ggplot(data = df_mlt, aes(x = City, y = Count, fill = Malignancy)) +
  geom_col(position = "dodge", colour = "black") +
  theme_light(base_size = 14) +
  labs(title = "Skin cancer biopsies results from 5 cities") +
  scale_fill_manual("Pathology", values = c("white", "grey")) +
  ylab(label = "Total count") +
  xlab(label = element_blank())
