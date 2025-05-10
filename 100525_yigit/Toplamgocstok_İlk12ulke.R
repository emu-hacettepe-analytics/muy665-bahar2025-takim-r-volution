# Gerekli kütüphaneler
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# Dosya yolu
file_path <- "C:/R-packages/undesa_pd_2020_ims_stock_by_age_sex_and_destination_2.xlsx"
df <- read_excel(file_path, sheet = "Table 1")

# Veriyi hazırla
df_clean <- df %>%
  mutate(
    Total = as.numeric(`Total(Both)`),
    Year = as.numeric(Year)
  ) %>%
  filter(Year == 2020, Area == "Country") %>%
  drop_na(Total)

# En yüksek 12 ülkeyi seç
df_top12 <- df_clean %>%
  arrange(desc(Total)) %>%
  slice(1:12)

# Grafik oluştur
ggplot(df_top12, aes(x = reorder(`Region, development group, country`, Total), y = Total)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  geom_text(aes(label = paste0(round(Total / 1e6, 1), "M")), 
            hjust = -0.1, size = 3.5) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M"),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Top 12 Countries by Total Migrant Stock in 2020",
       x = "Country",
       y = "Total Migrant Stock") +
  theme_minimal(base_size = 13)
