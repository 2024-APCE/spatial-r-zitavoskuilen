renv::restore

#data
CS

library(qdapTools)

#Center Annual Density per individual
ind_avgF<-aggregate(cbind(annual_density)~femaleID,CS,mean) # Calc avg density per fem

## Between individual effect: mean density for each female! This is how individuals differ
CS$Btw_Ind_Dens<-lookup(CS$femaleID,ind_avgF[,c("femaleID","annual_density")])

## Within individual effect: how each value differs from individual mean.
CS$Wthin_Ind_Dens<-CS$annual_density-CS$Btw_Ind_Dens

#Model with annual_density_cen (within individual effect) and avgAnDens (between individual effect)
m6<-glmer(CS~Wthin_Ind_Dens + Btw_Ind_Dens+ (1|femaleID), data= CS, family="gaussian")
summary(m6)
confint(m6)



