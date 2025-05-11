# Gerekli kütüphaneler
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Dosya yolu
file_path <- "C:/R-packages/undesa_pd_2020_ims_stock_by_age_sex_and_destination_2.xlsx"
df <- read_excel(file_path, sheet = "Table 1")

# Yaş kolonları
age_cols <- c("0-4 total", "5-9 total", "10-14 total", "15-19 total", "20-24 total",
              "25-29 total", "30-34 total", "35-39 total", "40-44 total", "45-49 total",
              "50-54 total", "55-59 total", "60-64 total", "65-69 total", "70-74 total", "75+ total")

# Veriyi hazırla
df_age <- df %>%
  filter(Year == 2020, Area == "Country") %>%
  select(Country = `Region, development group, country`, Total = `Total(Both)`, all_of(age_cols)) %>%
  mutate(across(all_of(age_cols), as.numeric),
         Total = as.numeric(Total)) %>%
  drop_na()

# Uzun formata çevir
df_long <- df_age %>%
  pivot_longer(cols = all_of(age_cols), names_to = "Age_Group", values_to = "Count") %>%
  mutate(Share = Count / Total)

# Grafik
ggplot(df_long, aes(x = Total, y = Share)) +
  geom_point(color = "blue", size = 1.5) +
  scale_x_log10(
    breaks = c(1e4, 1e5, 1e6, 1e7, 1e8),
    labels = c("10K", "100K", "1M", "10M", "100M")
  ) +
  scale_y_continuous(labels = label_percent(accuracy = 1), limits = c(0, 0.35)) +
  facet_wrap(~ Age_Group) +
  labs(
    title = "2020 – Migrant Share by Age Group (Log Scale)",
    x = "Total Migrant Stock (log10, Millions)",
    y = "Share of Age Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
