library(readxl)
library(dplyr)
library(ggplot2)

# 1. Dosya yolu
file_path <- "C:/R-packages/undesa_pd_2020_ims_stock_by_age_sex_and_destination_2.xlsx"

# 2. Veriyi oku
df <- read_excel(file_path, sheet = "Table 1")

# 3. 2020 yılı ve ülke bazlı veriyi al
df_corr <- df %>%
  filter(Year == 2020, Area == "Country") %>%
  mutate(
    Male = suppressWarnings(as.numeric(`Total(Males)`)),
    Female = suppressWarnings(as.numeric(`Total(Females)`)),
    Country = `Region, development group, country`
  ) %>%
  select(Country, Male, Female) %>%
  drop_na()

# 4. Korelasyon katsayısını hesapla
correlation <- cor(df_corr$Male, df_corr$Female, method = "pearson")
print(paste("Kadın ve erkek göçmen sayıları arasındaki Pearson korelasyon katsayısı:", round(correlation, 3)))

# 5. Scatter plot + regresyon çizgisi
ggplot(df_corr, aes(x = Male, y = Female)) +
  geom_point(color = "darkblue", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick", linewidth = 1) +
  labs(
    title = "Kadın ve Erkek Göçmen Sayıları Arasındaki İlişki (2020)",
    subtitle = paste("Pearson korelasyon katsayısı:", round(correlation, 3)),
    x = "Erkek Göçmen Sayısı",
    y = "Kadın Göçmen Sayısı"
  ) +
  scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  theme_minimal(base_size = 13)
