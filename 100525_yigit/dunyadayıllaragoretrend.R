library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# Excel verisini oku
file_path <- "C:/R-packages/undesa_pd_2020_ims_stock_by_age_sex_and_destination_2.xlsx"
df <- read_excel(file_path, sheet = "Table 1")
names(df) <- trimws(names(df))

# WORLD verisini hazırla
df_world <- df %>%
  filter(`Region, development group, country` == "WORLD") %>%
  mutate(
    Year = as.numeric(Year),
    Total = as.numeric(`Total(Both)`)
  ) %>%
  drop_na(Year, Total)

# 2020 verisi
df_2020 <- df_world %>% filter(Year == 2020)

# Grafik
ggplot(df_world, aes(x = Year, y = Total)) +
  geom_line(linewidth = 1.2, color = "darkgreen") +
  geom_point(color = "darkgreen", size = 2) +
  geom_text(data = df_2020,
            aes(label = paste0(round(Total / 1e6, 1), "M")),
            vjust = -0.5, color = "darkgreen", size = 4.2) +
  scale_y_continuous(
    labels = label_number(scale = 1e-6, suffix = "M"),
    expand = expansion(mult = c(0, 0.1))  # üstte boşluk bırak
  ) +
  labs(
    title = "1990–2020 Arası Dünya Genelinde Göçmen Stoğu",
    subtitle = "Birlesmis Milletler Uluslararasi Gocmen Verisine Gore",
    x = "Yil",
    y = "Toplam Gocmen Sayisi (Milyon)"
  ) +
  theme_minimal(base_size = 13)
