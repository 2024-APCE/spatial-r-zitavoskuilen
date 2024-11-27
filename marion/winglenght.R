
### winglength data

library(ggplot2)

# Load the data from excel

getwd()
winglength <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTMx4DFjoaKLJcBrBu8KD6IXmZkJHx9Qrhi6oYXVnWuuqLOzXj_pcivSRi7GW3spA/pub?gid=522699102&single=true&output=csv")

head(winglength)

# plot the winglength per year
ggplot(winglength, aes(x=Year, y=WingLength)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

hist(winglength$WingLength)

m1<-lm(WingLength~Year, data=winglenght)
summary(m1)


# Visualizing regression:
ggplot(winglenght, aes(x=Year, y=WingLength)) +
geom_point() + # Use hollow circles
geom_smooth(method=lm, # Add linear regression line
se=FALSE)
# check residuals
par(mfrow=c(2,2))
plot(m1)
hist(residuals(m1))


model2 <-glmer(WingLength ~  (1|Individual), data= winglength, family="gaussian")
summary(model2)

# repeatability

model2.rep <- 158.44/(87.29+158.44)
model2.rep


# model with year as random
model3 <-glmer(WingLength ~  (1|Individual) + (1|Year), data= winglength, family="gaussian")
summary(model3)

# repeatability

model3.rep <- 142.94/(142.94+46.30+72.97)
model3.rep

### heritability 

data.her <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTMx4DFjoaKLJcBrBu8KD6IXmZkJHx9Qrhi6oYXVnWuuqLOzXj_pcivSRi7GW3spA/pub?gid=1831725737&single=true&output=csv")
data.her

ggplot(data.her, aes(x=dad_winglength, y=child_winglength)) + geom_point() + geom_smooth(method = "lm", se = FALSE)


# predict the heritability of winglength
# omitting the na's and infinity values out of data.her

data.her$child_winglength <- as.numeric(data.her$child_winglength)
data.her$dad_winglength <- as.numeric(data.her$dad_winglength)


model4 <-lm(child_winglength ~ dad_winglength, data= data.her)
summary(model4)

# 2*0.2426 = 0.4852

nrow(data.her)
