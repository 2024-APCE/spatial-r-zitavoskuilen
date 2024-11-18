#### STRUCTURAL EQUATION MODELLING USING LAVAAN

# analysis of Woody Cover
# Paper:
# browseURL("https://docs.google.com/spreadsheets/d/1Lt7tkF0jlhDziYL5CUI58FvpNM7xnm4Vba9VGXCI5hE/edit?gid=0#gid=0")

# restore libraries

rm(list = ls()) # clear environment

library(tidyverse)
# load the lavaan library
library(lavaan)



# dataset:
browseURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vSOHGtB3_Ok0Zt4Afq4kfXC5j8RpBp_1lvIzzr6C1glSDAGlmYElHQ76s5HDWAPw85nLQ9v1MHMTBPG/pub?gid=2045022995&single=true&output=csv")
# read the data from the google docs link:
SEM_data<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSOHGtB3_Ok0Zt4Afq4kfXC5j8RpBp_1lvIzzr6C1glSDAGlmYElHQ76s5HDWAPw85nLQ9v1MHMTBPG/pub?gid=844195483&single=true&output=csv") 

SEM_data

# standardize all variables to mean 0 and standard deviation 1
# beacuse some variables are measured on the longer scale than others
SEM_data_std <- SEM_data |>
  mutate_all(~(scale(.) %>% as.vector)) |>
  as_tibble()

SEM_data_std

#########
#### OP MAANDAG GEMAAKT TOT HIER!! ####
#########


# note that this does not affect the relations between the variables, only the scales  

# make a pairs panel to inspect linearity of relations and expected normality of residuals
psych::pairs.panels(SEM_data %>% select(dist2river, slope, burnfreq, elevation, woody, dist2cropland, hills, rainfall,       CorProtAr, cec, NDVI, dist2buildings),
                    stars = T, ellipses = F)
psych::pairs.panels(SEM_data_std %>% select(dist2river, slope, burnfreq, elevation, woody, dist2cropland, hills, rainfall,CorProtAr, cec, NDVI,dist2buildings),
                    stars = T, ellipses = F)

# analyse the model (response ~ predictors) with a multiple regression approach 

multreg_std <- lm(woody~slope + rainfall + burnfreq + dist2river, data = SEM_data_std)
summary(multreg_std)

# visualization of the result: 
# browseURL("https://docs.google.com/presentation/d/1Q7uXC5Wiu0G4Xsp5uszCNHKOnf1IMI9doY-13Wbay4A/edit?usp=sharing")

# Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model 

woody_model <- 'woody~slope + rainfall + burnfreq + dist2river + cec
                cec~rainfall + burnfreq
                dist2river~slope'
woody_model

woody_fit <- lavaan::sem(woody_model, data = SEM_data)
# show the model results
summary(woody_fit, standardized = T, fit.measures = T, rsquare = T)


# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR


# also explore the models as shown in fig 5b and 5c of the Anderson2007 paper
# so repeat the model for leaf P content

multreg_std_P <- lm(LF_P~RES_LHU + BIOMASS + FIRE_FRQ + NMS, data = Anderson2007std)
summary(multreg_std_P)

Leaf_P_model <- 'LF_P~BIOMASS + RES_LHU + FIRE_FRQ + NMS
                BIOMASS~FIRE_FRQ + RES_LHU
                NMS~FIRE_FRQ + RES_LHU'
Leaf_P_model

Leaf_P_fit <- lavaan::sem(Leaf_P_model, data = Anderson2007std)
# show the model results
summary(Leaf_P_fit, standardized = T, fit.measures = T, rsquare = T)
