# Gerekli kütüphaneler
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# 1. Veriyi oku
file_path <- "C:/R-packages/undesa_pd_2020_ims_stock_by_age_sex_and_destination_2.xlsx"
df <- read_excel(file_path, sheet = "Table 1")
names(df) <- trimws(names(df))  # boşlukları temizle

# 2. Türkiye verisini filtrele ve temizle
df_tr <- df %>%
  filter(`Region, development group, country` == "Turkey") %>%
  mutate(
    Year = as.numeric(Year),
    Total = as.numeric(`Total(Both)`)
  ) %>%
  drop_na(Year, Total)

# 3. 2020 yılını ayrı al
df_2020 <- df_tr %>% filter(Year == 2020)

# 4. Grafik
ggplot(df_tr, aes(x = Year, y = Total)) +
  geom_line(color = "firebrick", linewidth = 1.2) +
  geom_point(color = "firebrick", size = 2) +
  geom_text(
    data = df_2020,
    aes(label = paste0(round(Total / 1e6, 1), "M")),
    vjust = -1, color = "firebrick", size = 4
  ) +
  scale_y_continuous(
    labels = label_number(scale = 1e-6, suffix = "M"),
    limits = c(0, 8000000)  # Y eksenini 0–8M arası sınırla
  ) +
  labs(
    title = "1990–2020 Arası Türkiye'deki Göçmen Stoku",
    x = "Yıl",
    y = "Toplam Göçmen Sayısı (Milyon)"
  ) +
  theme_minimal(base_size = 13)
