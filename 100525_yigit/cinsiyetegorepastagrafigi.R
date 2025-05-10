# Gerekli kütüphaneler
library(readxl)
library(ggplot2)
library(dplyr)

# 1. Dosya yolu
file_path <- "C:/R-packages/undesa_pd_2020_ims_stock_by_age_sex_and_destination_2.xlsx"

# 2. AN2:BE2 hücre aralığından veriyi oku (sayıların olduğu satır)
df_raw <- read_excel(file_path, sheet = "Table 1", range = "AN2:BE2", col_names = FALSE)

# 3. Erkek ve kadın sayısını al
male_count <- as.numeric(df_raw[[1]])        # AN2 sütunu
female_count <- as.numeric(df_raw[[18]])     # BE2 sütunu

# 4. Veri çerçevesi oluştur
df_gender <- data.frame(
  Gender = c("Erkek", "Kadın"),
  Count = c(male_count, female_count)
)

# 5. Pasta grafiği oluştur
ggplot(df_gender, aes(x = "", y = Count, fill = Gender)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(Gender, ": ", round(Count / sum(Count) * 100, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 5, color = "white") +
  scale_fill_manual(values = c("Kadın" = "#FF6F61", "Erkek" = "#4F9CD3")) +
  labs(
    title = "2020 Yılında Türkiye'deki Göçmen Nüfusunun Cinsiyete Göre Dağılımı",
    x = NULL, y = NULL, fill = "Cinsiyet"
  ) +
  theme_void(base_size = 13)



