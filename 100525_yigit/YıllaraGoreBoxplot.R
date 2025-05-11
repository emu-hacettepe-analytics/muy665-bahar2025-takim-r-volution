library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# Dosya yolu
file_path <- "C:/R-packages/undesa_pd_2020_ims_stock_by_age_sex_and_destination_2.xlsx"
df <- read_excel(file_path, sheet = "Table 1")

# Sütun adlarını kontrol et
names(df)

# Gerekli dönüşümler
df <- df %>%
  mutate(
    Total = as.numeric(`Total(Both)`),
    Year = as.numeric(Year)
  ) %>%
  filter(Area == "Country") %>%
  drop_na(Total, Year)

# 2020 yılına göre ilk 12 ülke
top12_countries <- df %>%
  filter(Year == 2020) %>%
  arrange(desc(Total)) %>%
  slice(1:12) %>%
  pull(`Region, development group, country`)

# Tüm yılları al
df_top12_years <- df %>%
  filter(`Region, development group, country` %in% top12_countries)

# Grafik
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

names(df)




library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# Dosya yolu
file_path <- "C:/R-packages/undesa_pd_2020_ims_stock_by_age_sex_and_destination_2.xlsx"
df <- read_excel(file_path, sheet = "Table 1")

# Gerekli dönüşüm ve filtreleme
df <- df %>%
  mutate(
    Year = as.numeric(Year),
    TotalBoth = as.numeric(`Total(Both)`)
  ) %>%
  filter(Area == "Country") %>%
  drop_na(Year, TotalBoth)

# 2020 yılına göre en fazla göçmen alan 12 ülkeyi bul
top12_countries <- df %>%
  filter(Year == 2020) %>%
  arrange(desc(TotalBoth)) %>%
  slice(1:12) %>%
  pull(`Region, development group, country`)

# Bu ülkelerin tüm yıllarını al
df_top12_years <- df %>%
  filter(`Region, development group, country` %in% top12_countries)

# Grafik çiz
ggplot(df_top12_years, aes(x = `Region, development group, country`, y = TotalBoth)) +
  geom_boxplot(fill = "lightgreen") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Top 12 Countries – Migrant Stock Distribution Across Years (1990–2020)",
    x = "Country",
    y = "Total Migrant Stock"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

