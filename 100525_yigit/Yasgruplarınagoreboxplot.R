# Gerekli kütüphaneler
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Excel dosyasını oku
file_path <- "C:/R-packages/undesa_pd_2020_ims_stock_by_age_sex_and_destination_2.xlsx"
df <- read_excel(file_path, sheet = "Table 1")

# 2020 yılı verilerini al, sayısal dönüşüm yap
df_filtered <- df %>%
  filter(Year == 2020, Area == "Country") %>%
  mutate(Total = as.numeric(`Total(Both)`)) %>%
  drop_na(Total)

# En fazla göçmen barındıran ilk 12 ülkeyi bul
top12_countries <- df_filtered %>%
  arrange(desc(Total)) %>%
  slice(1:12) %>%
  pull(`Region, development group, country`)

# Yaş grubu sütunlarını seç (örnek: "0-4", "5-9", ...)
age_columns <- grep("^[0-9]+-[0-9]+", names(df_filtered), value = TRUE)

# İlk 12 ülkeye ait yaş grubu verisini uzun formata çevir
df_age <- df_filtered %>%
  filter(`Region, development group, country` %in% top12_countries) %>%
  select(`Region, development group, country`, all_of(age_columns)) %>%
  pivot_longer(
    cols = -`Region, development group, country`,
    names_to = "Age_Group",
    values_to = "Count"
  ) %>%
  mutate(Count = as.numeric(Count)) %>%
  drop_na()

# Boxplot oluştur
ggplot(df_age, aes(x = `Region, development group, country`, y = Count)) +
  geom_boxplot(fill = "lightblue") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Top 12 Countries – Migrant Distribution by Age Group (2020)",
    x = "Country",
    y = "Migrant Count (per Age Group)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
