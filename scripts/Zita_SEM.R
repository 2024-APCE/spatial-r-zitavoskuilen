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
names(SEM_data_std)

#########
#### OP MAANDAG GEMAAKT TOT HIER!! ####
#########


# note that this does not affect the relations between the variables, only the scales  

# make a pairs panel to inspect linearity of relations and expected normality of residuals

psych::pairs.panels(SEM_data, dplyr::select(dist2river, slope, burnfreq, elevation, woody, dist2cropland, hills, rainfall, CorProtAr, cec, NDVI, dist2buildings),
                    stars = T, ellipses = F)

psych::pairs.panels(SEM_data_std %>% select(dist2river, slope, burnfreq, elevation, woody, dist2cropland, hills, rainfall,CorProtAr, cec, NDVI,dist2buildings),
                    stars = T, ellipses = F)

# analyse the model (response ~ predictors) with a multiple regression approach 

multreg_std <- lm(woody~slope + rainfall + burnfreq + dist2river, data = SEM_data_std)
summary(multreg_std)

# visualization of the result: 
# browseURL("https://docs.google.com/presentation/d/1Q7uXC5Wiu0G4Xsp5uszCNHKOnf1IMI9doY-13Wbay4A/edit?usp=sharing")

# Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model 

woody_model <- 'woody~ rainfall + burnfreq + cec
                cec~rainfall + burnfreq + dist2cropland
                rainfall~elevation
                burnfreq~dist2cropland + rainfall
                dist2cropland~elevation'
woody_model

woody_fit <- lavaan::sem(woody_model, data = SEM_data_std)
# show the model results
summary(woody_fit, standardized = T, fit.measures = T, rsquare = T)

woody_model2 <- 'woody~cec + burnfreq + rainfall
                cec~rainfall + elevation + burnfreq
                rainfall~elevation
                burnfreq~rainfall'

woody_model2
               

woody_fit2 <- lavaan::sem(woody_model2, data = SEM_data_std)
summary(woody_fit2, standardized = T, fit.measures = T, rsquare = T)


woody_model_3 <- 'woody~rainfall + CorProtAr + burnfreq
                rainfall~ elevation
                burnfreq~CorProtAr + elevation'


woody_fit3 <- lavaan::sem(woody_model_3, data = SEM_data_std)
summary(woody_fit3, standardized = T, fit.measures = T, rsquare = T)

#best so far 

woody_fit4 <- 'woody~ rainfall + CorProtAr
                 rainfall~elevation
                 burnfreq~CorProtAr + rainfall
                 CorProtAr~elevation '
woody_fit4 <- lavaan::sem(woody_fit4, data = SEM_data_std)
summary(woody_fit4, standardized = T, fit.measures = T, rsquare = T)





# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR


