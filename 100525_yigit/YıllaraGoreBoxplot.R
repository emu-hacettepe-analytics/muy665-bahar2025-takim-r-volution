# Gerekli kütüphaneler
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# Dosya yolu
file_path <- "C:/R-packages/undesa_pd_2020_ims_stock_by_age_sex_and_destination_2.xlsx"
df <- read_excel(file_path, sheet = "Table 1")

# Sayısal dönüşüm
df <- df %>%
  mutate(
    Total = as.numeric(`Total(Both)`),
    Year = as.numeric(Year)
  ) %>%
  filter(Area == "Country") %>%
  drop_na(Total, Year)

# 2020 yılına göre en fazla göçmen alan ilk 12 ülkeyi bul
top12_countries <- df %>%
  filter(Year == 2020) %>%
  arrange(desc(Total)) %>%
  slice(1:12) %>%
  pull(`Region, development group, country`)

# Bu 12 ülkenin tüm yıllardaki verisini al
df_top12_years <- df %>%
  filter(`Region, development group, country` %in% top12_countries)

# Grafik: Her ülke için yıllar bazında toplam göçmen stoğu boxplot
ggplot(df_top12_years, aes(x = `Region, development group, country`, y = Total)) +
  geom_boxplot(fill = "lightgreen") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Top 12 Countries – Migrant Stock Distribution Across Years (1990–2020)",
    x = "Country",
    y = "Total Migrant Stock"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
