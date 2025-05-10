# Gerekli kütüphaneler
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# 1. Veriyi oku ve temizle
file_path <- "C:/R-packages/undesa_pd_2020_ims_stock_by_age_sex_and_destination_2.xlsx"
df <- read_excel(file_path, sheet = "Table 1")
names(df) <- trimws(names(df))  # boşlukları temizle

df_world <- df %>%
  filter(`Region, development group, country` == "WORLD") %>%
  mutate(
    Year = as.numeric(Year),
    Total = as.numeric(`Total(Both)`)
  ) %>%
  drop_na(Year, Total)

# 2. Doğrusal regresyon modeli oluştur
model <- lm(Total ~ Year, data = df_world)

# 3. 2024 yılı için tahmin al
new_data <- data.frame(Year = 2024)
pred_2024 <- predict(model, newdata = new_data)

# 4. Tahmini veriyi df'ye ekle
df_world_pred <- bind_rows(
  df_world,
  data.frame(Year = 2024, Total = pred_2024)
)

# 5. Grafik çizimi
ggplot(df_world_pred, aes(x = Year, y = Total)) +
  geom_point(color = "darkgreen", size = 2) +
  geom_line(data = df_world, color = "darkgreen", linewidth = 1) +
  geom_smooth(data = df_world, method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  geom_point(data = filter(df_world_pred, Year == 2024), color = "blue", size = 3) +
  geom_text(
    data = filter(df_world_pred, Year == 2024),
    aes(label = paste0("Tahmin (2024): ", round(Total / 1e6), "M")),
    vjust = -1, hjust = 1, size = 4, color = "blue"
  ) +
  scale_y_continuous(
    labels = label_number(scale = 1e-6, suffix = "M"),
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_x_continuous(
    breaks = seq(1990, 2024, by = 5)  # X ekseninde 2024 dahil edilir
  ) +
  labs(
    title = "1990–2020 Dünya Göçmen Stoku ve 2024 Tahmini",
    subtitle = "Kırmızı çizgi: regresyon eğrisi | Mavi nokta: 2024 tahmini",
    x = "Yıl",
    y = "Toplam Göçmen Sayısı (Milyon)"
  ) +
  theme_minimal(base_size = 13)
