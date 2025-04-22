#Area under the curve analysis

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rstatix)
library(ggpubr)



#Importing and cleaning up the data
df <- read_excel("All 10 month GTT data.xlsx")

head(df)
df$Genotype <- as.factor(df$Genotype)
df$Sex <- as.factor(df$Sex)
df$Glucose_0 <- as.numeric(df$Glucose_0)
df$Glucose_15 <- as.numeric(df$Glucose_15)
df$Glucose_30 <- as.numeric(df$Glucose_30)
df$Glucose_60 <- as.numeric(df$Glucose_60)
df$Glucose_120 <- as.numeric(df$Glucose_120)


#Remove any columns (and therefore ID) with "na" from data
clean_data <- df %>%
  filter(if_all(everything(), ~ !is.na(.)))

print(clean_data)
summary(clean_data)

#Reshaping data to long format

long_data <- pivot_longer(clean_data, 
                          cols = starts_with("Glucose"), 
                          names_to = "time", 
                          values_to = "glucose")
print(long_data)

summary(long_data)

#Converting the time values into numeric values:
long_data <- long_data %>%
  mutate(
    time_numeric = case_when(
      time == "Glucose_0" ~ 0,
      time == "Glucose_15" ~ 15,
      time == "Glucose_30" ~ 30,
      time == "Glucose_60" ~ 60,
      time == "Glucose_120" ~ 120,
      TRUE ~ NA_real_
    )
  )

#Calculating area under the curve and then combining the datasets

auc_data <- long_data %>%
  arrange(Mouse, time_numeric) %>%
  group_by(Mouse) %>%
  summarize(
    auc = sum(diff(time_numeric) * 
                (head(glucose, -1) + tail(glucose, -1)) / 2, na.rm = TRUE)
  )
print(auc_data)

subject_info <- long_data %>%
  select(Mouse, Sex, Genotype) %>%
  distinct()

auc_summary <- auc_data %>%
  left_join(subject_info, by = "Mouse")

print(auc_summary)


#Stastistical Analysis
#Linearity assumptions for sex covariate
ggscatter(
  auc_summary, x = "Sex", y = "auc",
  color = "Genotype", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Genotype)
  )

#Homogenity of regression slopes 
auc_summary %>% anova_test(auc ~ Genotype*Sex)

#Normality of the residuals
model <- lm(auc ~ Sex + Genotype, data = auc_summary)
model.metrics <- augment(model) %>%
  select(-.hat, -.sigma, -.fitted)
head(model.metrics, 3)

shapiro.test((model.metrics$.resid))


#Homogeneity of variances
levene_test(.resid ~ Genotype, data = model.metrics)

#Ouliers
model.metrics %>%
  filter(abs(.std.resid) >3) %>%
  as.data.frame()
#Outliers kept in dataset

#One-way ANCOVA
res.aov <- auc_summary %>%
  anova_test(auc ~ Sex + Genotype)
get_anova_table(res.aov)

#Genotype not significant. This does not fit with what was previously found.

#Graph to display results:
ggplot(auc_summary, aes(x=Genotype, y=auc, color=Genotype)) + 
  geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2)) +
  theme_classic()










