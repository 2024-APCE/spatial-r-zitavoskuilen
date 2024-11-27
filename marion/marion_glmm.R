renv::restore()

getwd()

# load libraries
library(tidyverse)
library(tidyverse)
library(lme4)
library(ggplot2)


# reading the data file
CS <- read.table("/Users/Zita/Documents/Ecology &  Conservation/APCE2024/GIT/spatial-r-zitavoskuilen/marion/marion_GLMM.csv", header = T, sep = ",") |>
  na.omit() |>
  as_tibble()

CS

#check for outliers, for example:
hist(CS$CS)
#### step 1. linear regression ###
m1<-lm(CS~annual_density, data=CS)
summary(m1)
# Visualizing regression:
ggplot(CS, aes(x=annual_density, y=CS)) +
geom_point(shape=1) + # Use hollow circles
geom_smooth(method=lm, # Add linear regression line
se=FALSE)
# check residuals
par(mfrow=c(2,2))
plot(m1)
hist(residuals(m1))


## step 2 linear regression with density as a factor 

CS$annual_density_cat<-as.factor(CS$annual_density_cat)
m2<-lm(CS~annual_density_cat, data=CS)
summary(m2)

# the intercept is the first category that is fitted, this is what the other categories are compared to
# there are also way more degrees of freedom 
# categorical is more interesting when you are interested in lets say more the levels
# continuous you get one slope, one intercept 


## doing the same but taking the means 
#step 3 linear regression - taking annual means
CS2<- CS |>
dplyr::group_by(annual_density) |>
dplyr::summarize(CS_avg=mean(CS),
  CS_sd=sd(CS),
  n_obs=n(),
  CS_se=CS_sd/sqrt(n_obs))

m3<-lm(CS_avg~annual_density, data=CS2)
summary(m3)


# visualisation
ggplot(CS2, aes(x=annual_density, y=CS_avg)) +
geom_errorbar(aes(ymin=CS_avg-CS_se, ymax=CS_avg+CS_se), width=.1) +
geom_point()+ geom_smooth(method=lm, # Add linear regression line
se=FALSE)

# the standard error has decreased, the p-value is still significant
# but this is because the sample size has decreased because you take the mean 
# we only use 18 data points to fit the model 

library(lme4)
m1<-glmer(CS ~ annual_density + (1|femaleID) + (1|plotID), data= CS, family="gaussian")


summary(m1)
confint(m1)

# the intercept is at 0 density of females, they lay 10.04 eggs 
# random effects, is the variance explained by female id and by plot id, the residuals is what is not explained by the model 
# both female and plot have an effect 


m1 <-glmer(CS ~ annual_density + (1|femaleID), data= CS, family="gaussian")
summary(m1)

# calculate repeatability 
# you take the random effect of female Id and you calculate the ratio of how much of the 
# variance is explained by the female id 
r_m1 <- 1.117/(1.117+1.471)
r_m1

install.packages("squid")
install.packages ("shiny")

library(squid)
library(shiny)

squidApp()

install.packages("markdown")
library(markdown)
