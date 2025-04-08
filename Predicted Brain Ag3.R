install.packages("ggpubr")
install.packages("tidyverse")
install.packages("emmeans")
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
df <- read_xlsx("Fake Data for R.xlsx")
df$genotype <- as.factor(df$genotype)
df$age_group <- as.factor(df$age_group)
df$sex_numeric <- as.numeric(df$sex)


head(df)
summary(df)


#Linearity assumptions for sex covariate
ggscatter(
  df, x = "sex_numeric", y = "predicted_brain_age_difference",
  facet.by = c("genotype", "age_group"),
  short.panel.labs = FALSE
) +
  stat_smooth(method = "loess", span = 0.9)

#Linearity assumptions for chron age covariate
ggscatter(
  df, x = "chronological_age", y = "predicted_brain_age_difference",
  facet.by = c("genotype", "age_group"),
  short.panel.labs = FALSE
) +
  stat_smooth(method = "loess", span = 0.9)


#Homogenity of regression slopes 
df %>%
  anova_test(
    predicted_brain_age_difference ~ sex_numeric + chronological_age + genotype + age_group +
      genotype*age_group + genotype*sex_numeric + genotype*chronological_age + sex_numeric*age_group + sex_numeric*chronological_age + 
      chronological_age*age_group + sex_numeric*chronological_age*genotype*age_group
  )


#Normality of the residuals
model <- lm(predicted_brain_age_difference ~ sex_numeric + chronological_age + genotype*age_group, data = df)
model.metrics <- augment(model) %>%
  select(-.hat, -.sigma, -.fitted)
head(model.metrics, 3)

shapiro.test((model.metrics$.resid))
#Not normally distributed

#Homogeneity of variances
levene_test(.resid ~ genotype*age_group, data = model.metrics)

#Levene's test significant so there isnt homogenity of the residual variances for all groups

#Outliers
model.metrics %>%
  filter(abs(.std.resid) >3) %>%
  as.data.frame()

#There are outliers

res.aov <- df %>%
  anova_test(predicted_brain_age_difference ~ sex_numeric + chronological_age + age_group*genotype)
get_anova_table(res.aov)


# Post hoc tests - genotype effect at each age group (one way ANCOVA)

df %>%
  group_by(age_group) %>% 
  anova_test(predicted_brain_age_difference ~ chronological_age + sex_numeric + genotype)

#Genotype is significant at both 10 and 20 months so pairwise comparisons need to be conducted

#Post hoc tests - emmeans

emm <- emmeans(model, ~ genotype*age_group)

pwc <- contrast(emm, method = "pairwise", adjust = "bonferroni")
summary(pwc)

emm_df <- as.data.frame(emm)

#Reporting this graphically
lp <- ggline(
  emm_df, x = "age_group", y = "emmean",
  colour = "genotype", palette = "jco"
  ) + 
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL, colour = genotype),
    width = 0.1
  )

print(lp)



