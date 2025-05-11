# Gerekli kütüphaneler
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Dosya yolu (kendi yolunu yazmayı unutma)
file_path <- "C:/R-packages/undesa_pd_2020_ims_stock_by_age_sex_and_destination_2.xlsx"

# Excel'den veriyi oku
df <- read_excel(file_path, sheet = "Table 1")

# Dünya verisi - 2020 yılı için
df_world <- df %>%
  filter(Year == 2020, `Region, development group, country` == "WORLD") %>%
  mutate(
    Female = as.numeric(`Total(Females)`),
    Male = as.numeric(`Total(Males)`)
  ) %>%
  select(Female, Male) %>%
  pivot_longer(cols = everything(), names_to = "Gender", values_to = "Count") %>%
  mutate(
    Percentage = round(Count / sum(Count) * 100, 1),
    Label = paste0(Gender, ": ", Percentage, "%")
  )

# Pasta grafiği
ggplot(df_world, aes(x = "", y = Count, fill = Gender)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5), color = "white", size = 5) +
  labs(title = "Gender Distribution of Migrant Stock Worldwide in 2020") +
  theme_void(base_size = 14) +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

