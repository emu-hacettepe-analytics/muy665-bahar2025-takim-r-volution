# Gerekli kütüphaneler
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# Veri dosyasını oku
file_path <- "C:/R-packages/undesa_pd_2020_ims_stock_by_age_sex_and_destination_2.xlsx"
df <- read_excel(file_path, sheet = "Table 1")

# 2020 yılı, ülke bazlı veriler
df_clean <- df %>%
  filter(Year == 2020, Area == "Country") %>%
  mutate(
    Male = as.numeric(`Total(Males)`),
    Female = as.numeric(`Total(Females)`)
  ) %>%
  drop_na(Male, Female)

# Scatter plot
ggplot(df_clean, aes(x = Male, y = Female)) +
  geom_point(color = "steelblue", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  scale_x_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Migrant Stock by Gender Across Countries (2020)",
    x = "Male Migrant Count (Millions)",
    y = "Female Migrant Count (Millions)"
  ) +
  theme_minimal(base_size = 14)
