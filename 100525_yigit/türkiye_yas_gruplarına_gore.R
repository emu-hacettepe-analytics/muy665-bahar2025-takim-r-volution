# Gerekli kütüphaneler
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(scales)

# 1. Veri dosyasını oku
file_path <- "C:/R-packages/undesa_pd_2020_ims_stock_by_age_sex_and_destination_2.xlsx"
df <- read_excel(file_path, sheet = "Table 1")
names(df) <- trimws(names(df))  # Başlıklardaki boşlukları temizle

# 2. Yaş grubu sütunlarını bul
age_cols <- names(df)[str_detect(names(df), "^[0-9]{1,2}-[0-9]{1,2} total$|^75\\+ total$")]

# 3. Türkiye'nin 2020 yılı yaş dağılımını al
df_tr_age <- df %>%
  filter(`Region, development group, country` == "Turkey", Year == 2020) %>%
  select(all_of(age_cols)) %>%
  pivot_longer(cols = everything(),
               names_to = "AgeGroup",
               values_to = "Count") %>%
  mutate(
    AgeGroup = str_replace(AgeGroup, " total", ""),
    Count = as.numeric(Count),
    CountM = Count / 1e6  # milyon cinsinden
  ) %>%
  drop_na()

# 4. Grafik çiz
ggplot(df_tr_age, aes(x = AgeGroup, y = Count)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1fM", CountM)),
            vjust = -0.5, size = 3.8, color = "black") +
  scale_y_continuous(
    limits = c(0, 8e5),
    labels = label_number(scale = 1e-6, suffix = "M")
  ) +
  labs(
    title = "2020 Türkiye Göçmen Stoku - Yaş Gruplarına Göre Dağılım",
    x = "Yaş Grubu",
    y = "Göçmen Sayısı (Milyon)"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
