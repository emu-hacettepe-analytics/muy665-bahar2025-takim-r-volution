# Gerekli kütüphaneler
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Dosya yolu
file_path <- "C:/R-packages/undesa_pd_2020_ims_stock_by_age_sex_and_destination_2.xlsx"
df <- read_excel(file_path, sheet = "Table 1")

# Veriyi filtrele
df_filtered <- df %>%
  filter(`Region, development group, country` %in% c("Germany", "Turkey", "United States of America*"),
         Area == "Country") %>%
  mutate(
    Year = as.numeric(Year),
    Total = as.numeric(`Total(Both)`),
    Country = `Region, development group, country`
  ) %>%
  drop_na(Total)

# Grafik
ggplot(df_filtered, aes(x = Year, y = Total, color = Country)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(
    labels = label_number(scale = 1e-6, suffix = "M"),
    limits = c(0, 60000000)  # <-- burası 0–60M arası sınır koyar
  ) +
  labs(
    title = "Total Migrant Stock Over Time (per Country)",
    x = "Year",
    y = "Number of Migrants (in Millions)",
    color = "Country"
  ) +
  theme_minimal(base_size = 13)
