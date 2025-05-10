library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# Veriyi oku
file_path <- "C:/R-packages/undesa_pd_2020_ims_stock_by_age_sex_and_destination_2.xlsx"
df <- read_excel(file_path, sheet = "Table 1")

# İlk 10 ülkeyi seç
df_top10 <- df %>%
  mutate(
    Total = as.numeric(`Total(Both)`),
    Year = as.numeric(Year)
  ) %>%
  filter(Year == 2020, Area == "Country") %>%
  drop_na(Total) %>%
  arrange(desc(Total)) %>%
  slice(1:10)

# Grafik oluştur
ggplot(df_top10, aes(x = reorder(`Region, development group, country`, Total), y = Total)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Top 10 Countries by Migrant Stock in 2020",
    x = "Country",
    y = "Total Migrants"
  ) +
  theme_minimal(base_size = 13)
