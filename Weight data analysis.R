#To address the research question: does weight differ between genotypes (at x weeks/months)?
#One way ANCOVA

library(readxl)
library(ggplot2)
library(car)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(broom)
library(rstatix)
library(emmeans)

#Converting the variables into the right format
df <- read_xlsx("All weight data.xlsx")
df$genotype <- as.factor(df$genotype)
df$mass_numeric <- as.numeric(df$mass_15w)
df$sex_numeric <- ifelse(df$sex == "male", 1, 0)

  
str(df$sex_numeric)


head(df)
summary(df)

df_clean <- df %>%
  dplyr::filter(is.finite(sex_numeric) & is.finite(mass_numeric))


#Linearity assumptions for sex covariate
ggscatter(
  df_clean, x = "sex_numeric", y = "mass_numeric",
  colour = "genotype", add = "reg.line"
) +
  stat_regline_equation(
    aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~~"),
        colour = genotype)
  )

#Homogenity of regression slopes 
df %>%
  anova_test(mass_numeric ~ genotype*sex_numeric)


#Normality of the residuals
model <- lm(mass_numeric ~ sex_numeric + genotype, data = df)
model.metrics <- augment(model) %>%
  select(-.hat, -.sigma, -.fitted)
head(model.metrics, 3)

shapiro.test((model.metrics$.resid))


#Homogeneity of variances
levene_test(.resid ~ genotype, data = model.metrics)



#Outliers
model.metrics %>%
  filter(abs(.std.resid) >3) %>%
  as.data.frame()

#There are outliers

res.aov <- df %>%
  anova_test(mass_numeric ~ sex_numeric + genotype)
get_anova_table(res.aov)


# Post hoc test - I can't get this bit of code to work!

pwc <- df %>%
  emmeans_test(mass_numeric ~ genotype, covariate = sex_numeric,
               p.adjust.method = "bonferroni")
pwc

get_emmeans(pwc)

#To address the research question: does weight differ between the genotypes and does this change with age?
#Linear Mixed Effects Model




