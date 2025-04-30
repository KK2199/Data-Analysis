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
df <- read_xlsx("All 7m weight data.xlsx")
df$genotype <- as.factor(df$genotype)
df$sex <- as.factor(df$sex)
df$mass_7m <- as.numeric(df$mass_7m)

#Clean the data
clean_data <- df %>%
  filter(if_all(everything(), ~ !is.na(.)))

print(clean_data)
summary(clean_data)

#Linearity assumptions for sex covariate
ggscatter(
  clean_data, x = "sex", y = "mass_7m",
  colour = "genotype", add = "reg.line"
) +
  stat_regline_equation(
    aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~~"),
        colour = genotype)
  )

#Homogenity of regression slopes 
clean_data %>%
  anova_test(mass_7m ~ genotype*sex)


#Normality of the residuals
model <- lm(mass_7m ~ sex + genotype, data =clean_data)
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

#This is to clean outliers
clean_data_new <- model.metrics %>%
  filter(abs(.std.resid) <= 3)


res.aov <- clean_data_new %>%
  anova_test(mass_7m ~ sex + genotype)
get_anova_table(res.aov)

#Graph to display results:
ggplot(clean_data_new, aes(x=genotype, y=mass_7m, color=genotype)) + 
  geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2)) +
  theme_classic()


# Post hoc test 

emm <- emmeans(model, ~ genotype)

pwc <- contrast(emm, method = "pairwise", adjust = "bonferroni")
summary(pwc)



