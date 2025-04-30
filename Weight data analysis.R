#To address the research question: does weight differ between genotypes (at x weeks/months)?
#One way ANCOVA
#Problem I am having with this is I'd like to include all data and just get rid of NAs, 
#but I can't get rid of NAs without getting rid of the whole row for a mouse.

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
df$sex <- as.factor(df$sex)
df$mass_15w <- as.numeric(df$mass_15w)
df$mass_7m <- as.numeric(df$mass_7m)
df$mass_10m <- as.numeric(df$mass_10m)

head(df)
summary(df)


#Converting data to long format
long_data <- pivot_longer(clean_data, 
                          cols = starts_with("mass"), 
                          names_to = "age", 
                          names_prefix = "mass_",
                          values_to = "mass")
print(long_data)

summary(long_data)

long_data_clean <- long_data %>%
  filter(!is.na(mass), !is.na(genotype), !is.na(sex))

print(long_data_clean)

#Linearity assumptions for sex covariate
ggscatter(
  long_data, x = "sex", y = "mass",
  colour = "genotype", add = "reg.line"
) +
  stat_regline_equation(
    aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~~"),
        colour = genotype)
  )

#Homogenity of regression slopes 
long_data %>%
  anova_test(mass ~ genotype*sex, na.action = na.omit)


#Normality of the residuals
model <- lm( ~ sex + genotype, data =long_data)
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
  anova_test( ~ sex + genotype)
get_anova_table(res.aov)


# Post hoc test 

emm <- emmeans(model, ~ genotype)

pwc <- contrast(emm, method = "pairwise", adjust = "bonferroni")
summary(pwc)

emm_df <- as.data.frame(emm)











