
install.packages(c("tidyr", "dplyr", "plotly", "shiny", "ggplot2", "reshape"))

setwd("")

df <- read.csv("Example_data1.csv",header=TRUE,as.is=TRUE)

df$ExposureGroup = factor(df$Exposure,levels = c(0,1),
                          labels = c("Control","Case"))

library(ggplot2)

ggplot(df,aes(x = ExposureGroup, y = Biomarker_value)) + 
  geom_boxplot(aes(fill = ExposureGroup)) + 
  scale_fill_manual(values = c("Control" = "blue", "Case" = "darkorchid"), 
                    name = "Exposure") + 
  theme_classic(base_size = 20) +
  xlab("Exposure Groups") +
  ylab("Biomaker Value")

# Inspecting data ####
dim(df)
length(df$ExposureGroup)
dim(f$Exposure)
str(df)

# Checking names ####
names(df)
head(df)

# Changing variable name #####
names(df)[1] <- "Sample_ID"

# Base R plots ####

hist(df$Biomarker_value,
     xlab = "Biomarker (unit)",
     main = "Biomarker Distribution",
     col = "dodgerblue")
abline(v = 50, col = "black", lwd = 2, lty = 2)

boxplot(df$Biomarker_value ~ df$Exposure,
        legend = c("Control", "Case"),
        xlab = "Exposure",
        ylab = "Biomarker (unit)",
        main = "Boxplot",
        col = c("blue", "darkorchid"),
        cex.lab = 1.5)
legend(x = 1.8, y = 94,
       legend = c("Control", "Case"),
       fill = c("blue", "darkorchid"),
       title = "Exposure")