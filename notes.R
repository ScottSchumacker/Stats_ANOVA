# Scott Schumacker
# Notes

# Loading libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyr)

# Creating data set
DrugA <- rnorm(100, mean = 80, sd = 1)
DrugB <- rnorm(100, mean = 95, sd = 1)
DrugC <- rnorm(100, mean = 95, sd = 1)

df <- data.frame(DrugA, DrugB, DrugC)

df2 <- df %>% pivot_longer(
  cols = `DrugA`:`DrugC`,
  names_to = "Drug",
  values_to = "Blood_Pressure"
)
head(df2)

# Visualizing data and testing for normality
ggplot(df2, aes(x = Blood_Pressure, fill = Drug)) +
  geom_histogram(color = "black", alpha = 0.6) +
  theme_classic() +
  xlab("Diastolic Blood Pressure")

shapiro.test(DrugA)
shapiro.test(DrugB)
shapiro.test(DrugC)

# Test for equal variance between groups
bartlett.test(Blood_Pressure ~ Drug, data = df2)

# Running the ANOVA
analysisOut <- aov(Blood_Pressure ~ Drug, data = df2)
summary(analysisOut)

TukeyHSD(analysisOut, conf.level = 0.95)