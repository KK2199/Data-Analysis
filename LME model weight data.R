#Linear mixed effects model to answer the question: does weight different between the genotypes (and does this change over time)

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lmerTest)


#Converting the variables into the right format
df <- read_xlsx("All weight data.xlsx")
df$genotype <- as.factor(df$genotype)
df$sex <- as.factor(df$sex)
df$mass_15w <- as.numeric(df$mass_15w)
df$mass_7m <- as.numeric(df$mass_7m)
df$mass_10m <- as.numeric(df$mass_10m)

head(df)
summary(df)

#Remove any columns (and therefore ID) with "na" from data
clean_data <- df %>%
  filter(if_all(everything(), ~ !is.na(.)))

print(clean_data)
summary(clean_data)

#Converting this into long data

long_data <- pivot_longer(clean_data, 
                          cols = starts_with("mass"), 
                          names_to = "Age", 
                          values_to = "Mass")
print(long_data)


#Converting the time values into numeric values (all in weeks):
long_data <- long_data %>%
  mutate(
    Age = case_when(
      Age == "mass_15w" ~ 15,
      Age == "mass_7m" ~ 30.4,
      Age == "mass_10m" ~ 43.5,
      TRUE ~ NA_real_
    )
  )

# Visualising the data
ggplot(long_data, aes(x = Age, y = Mass, color = genotype, group = ID)) +
  geom_line() + 
  geom_point() +
  theme_minimal() +
  scale_color_manual(values = c("grb10_+/p" = "lightskyblue", "grb10_m/+" = "mediumpurple1", "wildtype" = "lightgreen")) +
  labs(title = "Weight Changes Over Time by Genotype",
       x = "Age (weeks)", y = "Mass (g)")

#Calculating and visualsing the means
mean_mass <- long_data %>%
  group_by(genotype, Age) %>%
  summarise(mean_mass = mean(Mass, na.rm = TRUE), .groups = "drop")

ggplot(mean_mass, aes(x = Age, y = mean_mass, color = genotype, group = genotype)) +
  geom_line() + 
  geom_point() +
  theme_minimal() +
  scale_color_manual(values = c("grb10_+/p" = "lightskyblue", "grb10_m/+" = "mediumpurple1", "wildtype" = "lightgreen")) +
  labs(title = "Weight Changes Over Time by Genotype",
       x = "Age (weeks)", y = "Mass (g)")


#Linear mixed effects model calculation

#Model formula:
#mass ~ genotype * Age_numeric + sex + (1 | ID)

#change the reference genotype:
long_data$genotype <- relevel(long_data$genotype, ref = "wildtype")

#Fit the model
model <- lmer(Mass ~ genotype * Age + sex + (1 | ID), data = long_data)

# Summarize the model
summary(model)

#Post hoc using emmeans

emm <- emmeans(model, ~ genotype | Age)

pairs(emm, adjust = "bonferroni")




