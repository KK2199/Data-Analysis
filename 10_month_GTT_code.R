install.packages("lmerTest")
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rstatix)
library(ggpubr)
library(lme4)
library(lmerTest)

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

#Remove NA from data
clean_data <- df %>% filter(if_all(everything(), ~ !is.na(.)))
print(clean_data)

#Reshaping data to long format
long_data <- pivot_longer(clean_data, 
                          cols = starts_with("Glucose"), 
                          names_to = "time", 
                          values_to = "glucose")
print(long_data)


#Recoding them 
long_data <- long_data %>%
  mutate(time_numeric = as.numeric(sub("Glucose_", "", time)))
head(long_data)


#Comparing the data by genotype 
long_data %>%
  group_by(Genotype, time_numeric) %>%
  summarize(
    mean_glucose = mean(glucose, na.rm = TRUE),
    sd_glucose = sd(glucose, na.rm = TRUE),
    n = n(),
    se_glucose = sd_glucose / sqrt(n),
    .groups = "drop"
  )

# First, summarize the means
mean_data <- long_data %>%
  group_by(Genotype, time_numeric) %>%
  summarize(mean_glucose = mean(glucose, na.rm = TRUE), .groups = "drop")

mean_data %>%
  group_by(Genotype) %>%
  summarise(mean_glucose = mean(mean_glucose, na.rm = TRUE))

# Plotting the data 
ggplot(mean_data, aes(x = time_numeric, y = mean_glucose, color = Genotype, group = Genotype)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Mean Blood Glucose Over Time by Genotype",
       x = "Time (minutes)",
       y = "Mean Glucose Level (mmol/L)") +
  scale_x_continuous(breaks = c(0, 15, 30, 60, 120), 
                     labels = c("0", "15", "30", "60", "120")) + 
  theme_minimal()


#Determining if there are outliers
ggplot(long_data, aes(x = time_numeric, y = glucose, fill = time_numeric)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 16, alpha = 0.7) +
  facet_wrap(~ Genotype) +
  labs(title = "Glucose Levels Over Time by Genotype",
       x = "Time Point",
       y = "Glucose Level") +
  theme_minimal() +
  theme(legend.position = "none")


#Stastical Analysis using lmm
#Genotype and sex will be fixed effects, 1/ID is random and glucose is dependent

model <- lmer(glucose ~ Genotype + Sex + (1|Mouse), data = long_data)
summary(model)
#No significant t value for 

# Use anova() to check significance for fixed effects


ggplot(long_data, aes(x = Genotype, y = glucose, color = Sex, group = Sex)) +
  stat_summary(fun = mean, geom = "line", size = 1.2, position = position_dodge(0.2)) +
  stat_summary(fun = mean, geom = "point", size = 3, position = position_dodge(0.2)) +
  labs(title = "Mean Glucose by Genotype and Sex",
       x = "Genotype",
       y = "Mean Glucose") +
  theme_minimal()


