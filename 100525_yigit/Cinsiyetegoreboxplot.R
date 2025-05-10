# Gerekli kütüphaneler
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Excel dosyasını oku
file_path <- "C:/R-packages/undesa_pd_2020_ims_stock_by_age_sex_and_destination_2.xlsx"
df <- read_excel(file_path, sheet = "Table 1")

# Sayısal dönüşüm ve 2020 filtrelemesi
df_2020 <- df %>%
  filter(Year == 2020, Area == "Country") %>%
  mutate(
    Total_Female = as.numeric(`Total(Females)`),
    Total_Male = as.numeric(`Total(Males)`),
    Total = Total_Female + Total_Male
  ) %>%
  drop_na(Total_Female, Total_Male)

# En yüksek toplam göçmen stoğuna sahip ilk 12 ülkeyi seç
top12_countries <- df_2020 %>%
  arrange(desc(Total)) %>%
  slice(1:12)

# Uzun formata çevir (kadın–erkek ayırımı için)
df_long <- top12_countries %>%
  select(`Region, development group, country`, Total_Female, Total_Male) %>%
  pivot_longer(cols = c(Total_Female, Total_Male),
               names_to = "Gender",
               values_to = "Count") %>%
  mutate(Gender = ifelse(Gender == "Total_Female", "Female", "Male"))

# Grafik oluştur (boxplot)
ggplot(df_long, aes(x = Gender, y = Count, fill = Gender)) +
  geom_boxplot() +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Top 12 Countries – Migrant Stock Distribution by Gender (2020)",
    x = "Gender",
    y = "Migrant Stock"
  ) +
  theme_minimal(base_size = 12)
