####
# Report on temperature affecting laying date for Buzzards, Great Tits and Oystercatchers. 
# Zita Voskuilen 29-11-204
# Advanced population ecology - E&E RUG
####

rm(list = ls())

# restore the libraries of the project 
renv::restore()

getwd()


# load libraries
library(tidyverse)
library(tidyverse)
library(lme4)
library(ggplot2)
library(patchwork)

# load the data for the different species 

# Buzzard 
buzzard_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTSSup9m_QeXpgs8yBMNES4xY8-oUn5f1OF3ikaHqV877E8DJJF2ol_vAxpQKfQ_CzDw2kutURLVVSd/pub?gid=0&single=true&output=csv")

# Oystercatcher
jackdaw_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTSSup9m_QeXpgs8yBMNES4xY8-oUn5f1OF3ikaHqV877E8DJJF2ol_vAxpQKfQ_CzDw2kutURLVVSd/pub?gid=586622482&single=true&output=csv")

# Great tit 
greattit_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTSSup9m_QeXpgs8yBMNES4xY8-oUn5f1OF3ikaHqV877E8DJJF2ol_vAxpQKfQ_CzDw2kutURLVVSd/pub?gid=1633742954&single=true&output=csv")

# temperatures 
buzzardtemp <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTSSup9m_QeXpgs8yBMNES4xY8-oUn5f1OF3ikaHqV877E8DJJF2ol_vAxpQKfQ_CzDw2kutURLVVSd/pub?gid=419522748&single=true&output=csv")

jackdawtemp <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTSSup9m_QeXpgs8yBMNES4xY8-oUn5f1OF3ikaHqV877E8DJJF2ol_vAxpQKfQ_CzDw2kutURLVVSd/pub?gid=1872797805&single=true&output=csv")

greattittemp <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTSSup9m_QeXpgs8yBMNES4xY8-oUn5f1OF3ikaHqV877E8DJJF2ol_vAxpQKfQ_CzDw2kutURLVVSd/pub?gid=1872797805&single=true&output=csv")

# start with visualization of the data for laying date 

# ---- Buzzard ---- 

# removing the rows of data where there is a NA for laying date 
buzzard_data <- buzzard_data |>
  na.omit()

# now leftjoin with the temperature data for the buzzard

buzzard_data <- buzzard_data |>
  left_join(buzzardtemp, by = c("year" = "year"))


# plot the data for the buzzard laying date and temperature
buzzard_plot<- ggplot(buzzard_data, aes(x=avgtemp, y=LayingDate)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Temperature affects laying date Buzzards (1996-2016)", x = "Temperature (period: March 8th - April 13)", y = "Laying date (days after January 1)") + 
  theme_minimal()

# check for outliers
hist(buzzard_data$LayingDate)

bm1 <- lm(LayingDate ~ avgtemp, data = buzzard_data)
summary(bm1)

# check residuals
par(mfrow=c(2,2))
plot(bm1)
hist(residuals(bm1))

## step 2 linear regression with year and adult Id as random factors 

buzzard_data$year <- as.factor(buzzard_data$year)


bm3 <- glmer(LayingDate ~ avgtemp + (1 | adult_ID) + (1| year) , data = buzzard_data)
summary(bm3)

# ---- Buzzard end ---- 

# ---- jackdaw ----

# removing the rows of data where there is a NA for laying date
jackdaw_data <- jackdaw_data |>
  na.omit()

jackdaw_data <- jackdaw_data |>
  left_join(jackdawtemp, by = c("Year" = "year"))

# plot the data for the jackdaw laying date and temperature

jackdaw_plot <- ggplot(jackdaw_data, aes(x=avgtemp, y=LDnumb )) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Temperature affects laying date Jackdaw 2005-2024", x = "Temperature (period: March 6th - April 13th)", y = "Laying date (days after January 1)")+ 
  theme_minimal()

jm1 <- lm(LDnumb  ~ avgtemp, data = jackdaw_data)
summary(jm1)

jackdaw_data$Year <- as.factor(jackdaw_data$Year)


jm2 <- lm(LDnumb  ~ Year, data = jackdaw_data)
summary(jm2)


jm3 <- lmer(LDnumb  ~ avgtemp + (1 | RingID ) + (1| Year) , data = jackdaw_data)
summary(jm3)

# ---- end jackdaw ----

# ---- great tit ----
greattit_data

# remove the rows with NA in LayingDate
greattit_data <- na.omit(greattit_data, cols = "LayingDate")

# becasue the laying dates are measured from 1 april, so i have to add 91 to the laying dates
greattit_data$LayingDate <- greattit_data$LayingDate + 91

# adding the temperature data 
greattit_data <- greattit_data |>
  left_join(greattittemp, by = c("Year" = "year"))

# plotting temp and laying date
greattit_plot <- ggplot(greattit_data, aes(x=avgtemp, y=LayingDate )) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Temperature affects laying date Great Tit 1994-2015", x = "Temperature (period: March 12th - April 30th)", y = "Laying date (days after January 1)") + 
  theme_minimal()
  

# model with the temp and the laying date 

gm1 <- lm(LayingDate ~ avgtemp, data = greattit_data)
summary(gm1)

# add the plot together 

birds_laying_date <- buzzard_plot + jackdaw_plot + greattit_plot+ plot_layout(nrow = 1)
birds_laying_date


### adding in random factors 
# for buzzard 

buzzard_data

bm2x <- lmer(LayingDate ~ (1|adult_ID) + (1|year), data = buzzard_data)
summary(bm2x)

bm2 <- lmer(LayingDate ~ avgtemp + (1|adult_ID) + (1|year), data = buzzard_data)
summary(bm2)



# for jackdaw 

jackdaw_data

jm2 <- lmer(LDnumb ~ avgtemp + (1|FemID) + (1|Year), data = jackdaw_data)
summary(jm2)




# great tit 

greattit_data

gm2 <- lmer(LayingDate  ~ avgtemp + (1|RingID ) + (1|Year), data = greattit_data)
summary(gm2)

# Plot for all individuals 
greattit_plot_2 <- ggplot(greattit_data, aes(x = avgtemp, y = LayingDate, color = factor(RingID))) +
  geom_point(alpha = 0.6) +  # Actual data points
  geom_line(aes(group = RingID), alpha = 0.8) +  # Connect points for the same individual
  labs(
    title = "Great tit",
    subtitle = "Effect of Temperature on Laying Date (Per Individual)",
    x = "Average Temperature (°C)",
    y = "Laying Date (Days after January 1)",
    color = "Individual ID"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

jackdaw_plot_2 <- ggplot(jackdaw_data, aes(x = avgtemp, y = LDnumb, color = factor(FemID))) +
  geom_point(alpha = 0.6) +  # Actual data points
  geom_line(aes(group = FemID), alpha = 0.8) +  # Connect points for the same individual
  labs(
    title = "Jackdaw",
    subtitle = "Effect of Temperature on Laying Date (Per Individual)",
    x = "Average Temperature (°C)",
    y = "Laying Date (Days after January 1)",
    color = "Individual ID"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

buzzard_plot_2 <- ggplot(buzzard_data, aes(x = avgtemp, y = LayingDate, color = factor(adult_ID))) +
  geom_point(alpha = 0.6) +  # Actual data points
  geom_line(aes(group = adult_ID), alpha = 0.8) +  # Connect points for the same individual
  labs(
    title = "Buzzard",
    subtitle = "Effect of Temperature on Laying Date (Per Individual)",
    x = "Average Temperature (°C)",
    y = "Laying Date (Days after January 1)",
    color = "Individual ID"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


birds_laying_date_individual <- greattit_plot_2 + jackdaw_plot_2 + buzzard_plot_2 + plot_layout(nrow = 1)
birds_laying_date_individual


greattit_data <- greattit_data %>%
  group_by(RingID) %>%
  mutate(avgtemp_centered = avgtemp - mean(avgtemp, na.rm = TRUE)) %>%
  ungroup()

greattit_data$binnenInd<-greattit_data$avgtemp-greattit_data$avgtemp_centered

gm3 <- lmer(LayingDate ~ binnenInd+avgtemp_centered + (1 | RingID), data = greattit_data)
summary(gm3)

gm4 <- lmer(LayingDate ~ Year + (1 | RingID)+(1|Year), data = greattit_data)
summary(gm4)


# zero models 
# great tit 
gm0 <- lmer(LayingDate ~  (1 | RingID)+(1|Year), data = greattit_data)
summary(gm0)

# buzzzard
bm0 <- lmer(LayingDate ~  (1 | adult_ID) + (1|year), data = buzzard_data)
summary(bm0)
confint(bm0)

# jackdaw 
jm0 <- lmer(LDnumb ~ (1| FemID) + (1|Year), data= jackdaw_data)
summary(jm0)
confint(jm0)

### adding  year as a fixed factor 

class(buzzard_data$year)
class(jackdaw_data$Year)
jackdaw_data$Year <- as.integer(jackdaw_data$Year)
greattit_data$Year <- as.integer(greattit_data$Year)
buzzard_data$year <- as.integer(buzzard_data$year)

gm.y <- lmer(LayingDate ~ Year + (1 | RingID) + (1|Year), data = greattit_data)
summary(gm.y)
confint(gm.y)

jm.y <- lmer(LDnumb ~ Year+ (1| FemID) + (1|Year) , data= jackdaw_data)
summary(jm.y)
confint(jm.y)

bm.y <- lmer(LayingDate ~ year + (1| adult_ID) + (1|year), data = buzzard_data)
summary(bm.y)
confint(bm.y)

### adding temperature as a fixed factor 

gm.t <- lmer(LayingDate ~ avgtemp + (1| RingID) + (1|Year), data = greattit_data )
summary(gm.y.t)
confint(gm.y.t)

jm.t <- lmer(LDnumb ~ avgtemp + (1| FemID) + (1|Year) , data= jackdaw_data)
summary(jm.t)
confint(jm.t)

bm.t <- lmer(LayingDate ~ avgtemp + (1| adult_ID) + (1|year), data = buzzard_data)
summary(bm.t)
confint(bm.t)

### within and between individual effects
### great tit 

# Step 1: Create "Between-Individual" Temperature Variable
ind_avg_g <- aggregate(avgtemp ~ RingID, data = greattit_data, FUN = mean)
greattit_data <- merge(greattit_data, ind_avg_g, by = "RingID", suffixes = c("", "_between"))

# Step 2: Create "Within-Individual" Temperature Variable
greattit_data$avgtemp_within <- greattit_data$avgtemp - greattit_data$avgtemp_between

# Step 3: Fit the Mixed Model
gm6 <- lmer(LayingDate ~ avgtemp_within + avgtemp_between + (1 | RingID) + (1 | Year), data = greattit_data)
summary(gm6)
confint(gm6)


### jackdaw

head(jackdaw_data)

# Step 1: Create "Between-Individual" Temperature Variable
ind_avg_j <- aggregate(avgtemp ~ FemID , data = jackdaw_data, FUN = mean)
jackdaw_data <- merge(jackdaw_data, ind_avg_j, by = "FemID", suffixes = c("", "_between"))

# Step 2: Create "Within-Individual" Temperature Variable
jackdaw_data$avgtemp_within <- jackdaw_data$avgtemp - jackdaw_data$avgtemp_between

# Step 3: Fit the Mixed Model
gm7 <- lmer(LDnumb  ~ avgtemp_within + avgtemp_between + (1 | FemID) + (1 | Year), data = jackdaw_data)
summary(gm7)
confint(gm7)

### buzzard
ind_avg_b <- aggregate(avgtemp ~ adult_ID, data = buzzard_data, FUN = mean)
buzzard_data <- merge(buzzard_data, ind_avg_b, by = "adult_ID", suffixes = c("", "_between"))

# Step 2: Create "Within-Individual" Temperature Variable
buzzard_data$avgtemp_within <- buzzard_data$avgtemp - buzzard_data$avgtemp_between

# Step 3: Fit the Mixed Model
bm6 <- lmer(LayingDate ~ avgtemp_within + avgtemp_between + (1 | adult_ID) + (1 | year), data = buzzard_data)
summary(bm6)
confint(bm6)

### making a figure with the temperature 
# with temperature on the y axis and year on the x axis for all species in one plot

a <- ggplot(greattit_data, aes(x = Year, y = avgtemp)) + 
  geom_point()+ 
  geom_line(col = "red") 

b <- ggplot(jackdaw_data, aes(x = Year, y = avgtemp)) +
  geom_point() + 
  geom_line(col = "blue")

c <-ggplot(buzzard_data, aes(x = year, y = avgtemp)) +
  geom_point() + 
  geom_line(col = "green")

# put these plots together in one plot 

a + b + c
colnames(buzzard_data)[colnames(buzzard_data) == "year"] <- "Year"

greattit_data$Species <- "Great Tit"
jackdaw_data$Species <- "Jackdaw"
buzzard_data$Species <- "Buzzard"

combined_data <- rbind(
  greattit_data[, c("Year", "avgtemp", "Species")],
  jackdaw_data[, c("Year", "avgtemp", "Species")],
  buzzard_data[, c("Year", "avgtemp", "Species")] # Ensure column names match
)

ggplot(combined_data, aes(x = Year, y = avgtemp, color = Species)) +
  geom_point() +
  geom_line(alpha = 0.8, size = 1.2) +  # Adjust alpha for transparency
  labs(title = "Average Temperature by Year for Different Species",
       x = "Year",
       y = "Average Temperature") +
  theme_minimal()

