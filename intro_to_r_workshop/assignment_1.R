# Assignment #1 #####
# Practice on your own!
# 
# Assignment_data1.csv
# 
# Set your working directory
# 
# Read in your data
# 
# Inspect your data frame
# 
# Create a "responder" factor
# 
  # 2 levels (0,1)
# 
  # 2 labels ("non-responder", "responder")
# 
# Create a boxplot of your biomarker relative to response (responder vs non-responder)

# Load libraries ####
library(tidyverse)

# Setting up working directory ####
setwd("~/GitHub/bioinformatics.ca/intro_to_r_workshop")

# Loading data ####
df <- read.csv("Assignment_data1.csv", header = TRUE, as.is = TRUE)

# Inspecting data ####
str(df)

# Creating responder_group variable ####
df$responder_group <- factor(df$responder,
                             levels = c(0, 1),
                             labels = c("Non-responder", "Responder")
                             )

# Plotting data using ggplot2 #### 
ggplot(df, aes(responder_group, biomarker)) +
  stat_boxplot(geom = "errorbar", width = 0.25) +
  geom_boxplot(aes(fill = responder_group)) +
  theme_classic(base_size = 20) +
  xlab("Responder Group") +
  ylab("Biomarker (unit)") +
  labs(title = "Boxplot using ggplot2") +
  scale_fill_manual("Responder group", values = c("white", "grey"))

# Plotting data using base R ####
boxplot(df$biomarker ~ df$responder_group,
        legend = c("Non-responder", "Responder"),
        xlab = "Responder group",
        ylab = "Biomarker (unit)",
        main = "Boxplot using base R",
        col = c("white", "grey"),
        cex.lab = 1.5)
legend(x = 1.8, y = 94,
       legend = c("Control", "Case"),
       fill = c("white", "grey"),
       title = "Responder group")