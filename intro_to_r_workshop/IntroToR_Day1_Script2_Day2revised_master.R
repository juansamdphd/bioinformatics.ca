
## setwd
path <- "~/GitHub/bioinformatics.ca/intro_to_r_workshop"
setwd("~/GitHub/bioinformatics.ca/intro_to_r_workshop")

## read .txt data instead of .csv so use read.table instead of read.csv
df2 = read.table("Example_data2_v2.txt", header = TRUE, as.is =TRUE)

## investigate dataframe
names(df2)
head(df2)
dim(df2)
str(df2)

length(unique(df2$site))
unique(df2$treated)
table(df2$treated)

## create sex, site, and treated factor 
df2$sex_factor = factor(df2$sex, levels = c("M","F"), labels = c("Male","Female"))
df2$site_factor = factor(df2$site, levels = c(1,2), labels = c("site 1","site 2"))
df2$treatment_factor = factor(df2$treated, levels = c(0,1), labels = c("control","treated"))

str(df2)

unique(df2$treatment_factor)
table(df2$treatment_factor, df2$treated)

## quick stats
mean(df2$age)
sd(df2$age)
range(df2$age)

aggregate(df2$age,list(df2$site),mean)
aggregate(df2$age,list(df2$site,df2$sex),mean)
## name grouping variable
aggregate(df2$age,list("site" = df2$site_factor,"sex" = df2$sex_factor),mean)

## explore your factors with table
table(df2$treatment_factor, df2$sex_factor)
table(df2$treatment_factor, df2$site_factor)
table(df2$sex_factor, df2$site_factor)

## run a chi-square test to see if each category is randomly distributed 
## relative to the others
chisq.test(table(df2$treatment_factor, df2$sex_factor))
chisq.test(table(df2$treatment_factor, df2$site_factor))
chisq.test(table(df2$sex_factor, df2$site_factor))

## create barplots of these tables
barplot(table(df2$treatment_factor, df2$sex_factor))
barplot(table(df2$treatment_factor, df2$site_factor))
barplot(table(df2$sex_factor, df2$site_factor))

## dress up one of the barplots 
barplot(table(df2$sex_factor, df2$site_factor),
        beside = TRUE,
        col = c("dodgerblue","darkorchid"),
        ylim = c(0,40),cex.names = 1.5,cex.axis = 1.5)
legend(x = 3, 40,
       legend = levels(df2$sex_factor),
       fill = c("dodgerblue","darkorchid"),cex = 1.2)


## plotting marker data 
pairs(df2[,grep(pattern = "marker",x = names(df2))])

pairs(df2[,c(6,7,8,9,10)])


## plot a boxplot of each maker
boxplot(marker1 ~ treatment_factor + site,data = df2, las=2)


  # plot 1 row x 5 columns of graphs
par(mfrow=c(1,5))

boxplot(marker1 ~ treatment_factor,data = df2, las=2)
boxplot(marker2 ~ treatment_factor,data = df2)
boxplot(marker3 ~ treatment_factor,data = df2)
boxplot(marker4 ~ treatment_factor,data = df2)
boxplot(marker5 ~ treatment_factor,data = df2)

  # 5 different graphs
boxplot(marker1 ~ treatment_factor,data = df2, las=2)
plot(df2$marker1, df2$marker2)
boxplot(marker2 ~ treatment_factor,data = df2)
hist(df2$marker1)
hist(df2$marker2)

  # 2 rows x 2 columns graphs
par(mfrow=c(2,2))
boxplot(marker1 ~ treatment_factor,data = df2, las=2)
plot(df2$marker1, df2$marker2)
boxplot(marker2 ~ treatment_factor,data = df2)
hist(df2$marker1)

  ## run this as a loop
par(mfrow=c(1,5))
for(marker in paste0("marker",1:5)){
  marker_col = df2[,marker]
  treatment_col = df2[,"treatment_factor"]
  boxplot(marker_col ~ treatment_col, 
          main = marker, 
          ylab= marker)
}


  ## make a similar plot with ggplot
library(ggplot2)

  ## first need to reshape our data because we want all the markers on the same plot 
df2_mlt = melt(data = df2,id.vars = c("id","site_factor","treatment_factor","age"),
               measure.vars = c("marker1","marker2","marker3","marker4","marker5"),
               variable_name = "Marker")

df2_mlt$marker_factor = factor(df2_mlt$Marker,
                               levels = c("marker3","marker1","marker2","marker4","marker5"))


  ## boxplot: 
ggplot(df2_mlt,aes(x = treatment_factor, y = value)) +
  geom_boxplot(aes(fill = treatment_factor)) + 
  facet_grid(~Marker) 

  ## updated plot: 
ggplot(df2_mlt,aes(x = treatment_factor, y = value)) +
  geom_boxplot(aes(fill = treatment_factor)) + 
  facet_grid(~marker_factor) + xlab("Treatment") + ylab("Marker (unit)") + 
  scale_fill_manual(values = c("control" = "turquoise","treated" = "orchid"),
                    name= "Treatment")

  ## boxplots in a loop: 
treatment_col = df2[, "treatment_factor"]
marker_name_vec = paste0("marker",1:5)
# "marker1" "marker2" "marker3" "marker4" "marker5"

par(mfrow=c(1,5)) 

  # marker = "marker1" ## <- debugging
for(marker in marker_name_vec){
  marker_col = df2[,marker]
  boxplot(marker_col ~ treatment_col, 
          col = c("blue","yellow"),
          xlab = "Treated", ylab = marker)
}


  ## t-tests on each marker
t.test(marker1 ~ treatment_factor,data = df2)
t.test(marker2 ~ treatment_factor,data = df2)
t.test(marker3 ~ treatment_factor,data = df2)
t.test(marker4 ~ treatment_factor,data = df2)
t.test(marker5 ~ treatment_factor,data = df2)

ttest_out = t.test(marker1 ~ treatment_factor, data = df2)

  ## t-test comparing 2 numeric vectors
t.test(df2$marker1, df2$marker2)

df2$marker1[df2$treatment_factor=='control']

my_ttest_obj = t.test(marker5 ~ treatment_factor,data = df2)
str(my_ttest_obj)
my_ttest_obj$estimate[1]
my_ttest_obj$estimate[2]


for(loop_var in 1:3){ print(loop_var)}


  ## loop computing t-test for each marker
for(marker in paste0("marker",1:5)){
  marker_col = df2[,marker]
  treatment_col = df2[, "treatment_factor"]
  print(marker)
  print(t.test(marker_col ~ treatment_col))
}


## loop computing t-test for each marker
for(marker in paste0("marker",1:5)){
  print(marker)
  print(t.test(df2[,marker] ~ df2[, "treatment_factor"]))
}

## try writing a function to compute
##  the proportion of values in a   
## vector that meet the criteria exp(vector) > 2

marker_prop = function(in_vec){ 
  n_true = sum(exp(in_vec) > 2)
  prop_out = n_true/length(in_vec)
  return(prop_out)
}

## test it:
marker_prop(df2$marker1)


## loop saving output from t-test to combine into a data.frame
marker_name = c()
ctrl_mean = c()
case_mean = c()
p_value = c()
ci_low = c()
ci_high = c()

for(i in 1:5){
  marker = paste0("marker",i)
  marker_col = df2[,marker]
  treatment_col = df2[, "treatment_factor"]
  my_ttest = t.test(marker_col ~ treatment_col)
  
  marker_name[i] = marker
  my_ttest$conf.int
  ctrl_mean[i] = my_ttest$estimate[1]
  case_mean[i] = my_ttest$estimate[2]
  p_value[i] = my_ttest$p.value
  ci_low[i] = my_ttest$conf.int[1]
  ci_high[i] = my_ttest$conf.int[2]
}

results_df = data.frame(marker_name,ctrl_mean,case_mean,p_value, ci_low, ci_high)

(results_sorted = results_df[order(results_df$p_value),])
head(results_sorted)


## three ways to save a .csv file to a specific directory
  ## use the default working dir
# write.csv(results_sorted, 
#           file = "C:/Users/larun/Desktop/CBW/R Workshops 2020/my_results_table.csv", 
#           quote=FALSE, row.names=FALSE)
# 
#   ## set the path in the file name
# write.csv(results_sorted, 
#           file = "C:/Users/larun/Desktop/CBW/R Workshops 2020/my_results_table.csv", 
          # quote=FALSE, row.names=FALSE)

  ## re-set your working directory before saving your file 
# setwd("C:/Users/larun/Desktop/CBW/R Workshops 2020/")
write.csv(results_sorted, 
          file = "my_results_table.csv", 
          quote=FALSE, row.names=FALSE)

###
### FUNCTIONS
###

add_sample = function(arg = "default"){
  return(paste0("site_",arg))
}

marker_prop = function(in_vec){
  n_true = length(in_vec[exp(in_vec) > 2])
  prop_out = n_true/length(in_vec)
  return(prop_out)
}

# my_marker_prop <- function(marker_vec){
#   expo_marker_true = sum(exp(marker_vec) > 2)
#   prop_exp_value = expo_marker_true/length(marker_vec)
#   return(prop_exp_value)
# }

marker_prop(df2$marker1)

for(i in 1:5){
  print(marker_prop(df2[,paste0("marker",i)]))
}


###
### LINEAR REGRESSION 
###

plot(df2$marker2, df2$marker3)


plot(df2$marker2, df2$marker3, 
     xlab = "Marker2", ylab = "Marker3",
     col = "blue", pch = 19, cex.lab = 1.5)


## fit a linear model 
lin_mod = lm(df2$marker3 ~ df2$marker2)

## view a summary of your linear model
summary(lin_mod)

## what is inside your summary object? 
str(summary(lin_mod))

## access the coefficients in your summary object
my_coef_results = data.frame(summary(lin_mod)$coef)
rownames(my_coef_results) = c("Intercept","Marker2")
names(my_coef_results) = c("Est","SE","T","P")
my_coef_results

## plot model diagnostics of my linear model
par(mfrow=c(2,2))
plot(lin_mod)


## re-do your plot with the linear model line of fit
plot(df2$marker2, df2$marker3, 
     xlab = "Marker2", ylab = "Marker3",
     col = "blue", pch = 19, cex.lab = 1.5)
abline(lin_mod, lty = 2)

## write a function that will do a regression and 
  ## print the summary for any two columns
  ## input arguments could be the columns themselves
  ## the input could also be just be the 
  ## characer strings of the columns you'd like to use 

## using full vectors as input
get_reg_summary = function(var1,var2){
  my_lm = lm(var1 ~ var2)
  return(summary(my_lm))
}

## using vector column names as input 
get_reg_summary = function(var_name1,var_name2, in_df = df2){
  my_lm = lm(in_df[,var_name2] ~ in_df[,var_name1])
  return(summary(my_lm))
}


par(mfrow=c(1,1))
par(mar=c(6,3,3,1))
plot(df2$marker2,df2$marker3)

## Include an additional control variable
get_reg_summary = function(var1,var2, sex_var){ 
  my_lm = lm(var1 ~ var2 + sex_var)
  return(summary(my_lm)$coefficients)
}


## Extract only the var2 coefficients
get_reg_summary(var1 = df2$marker2, var2 = df2$marker3, sex_var = df2$sex_factor)

get_reg_summary_var2only = function(var1, var2, sex_var){ 
  my_lm = lm(var1 ~ var2 + sex_var)
  coef_table = summary(my_lm)$coefficients
  coef_line = coef_table["var2", ]
  return(coef_line)
}

get_reg_summary_var2only(var1 = df2$marker2, var2 = df2$marker3, sex_var = df2$sex_factor)



