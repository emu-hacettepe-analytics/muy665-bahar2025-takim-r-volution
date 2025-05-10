# Gerekli kütüphaneler
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Dosya yolu
file_path <- "C:/R-packages/undesa_pd_2020_ims_stock_by_age_sex_and_destination_2.xlsx"
df <- read_excel(file_path, sheet = "Table 1")

# Veriyi hazırla
df <- df %>%
  mutate(
    Total_Female = as.numeric(`Total(Females)`),
    Total_Male = as.numeric(`Total(Males)`),
    Year = as.numeric(Year),
    Total = Total_Female + Total_Male
  ) %>%
  filter(Year == 2020, Area == "Country") %>%
  drop_na(Total)

# En çok göçmen barındıran ilk 12 ülkeyi seç
df_top12 <- df %>%
  arrange(desc(Total)) %>%
  slice(1:12) %>%
  select(`Region, development group, country`, Total_Female, Total_Male)

# Veriyi uzun formata getir
df_long <- df_top12 %>%
  pivot_longer(cols = c(Total_Female, Total_Male),
               names_to = "Gender", 
               values_to = "Count") %>%
  mutate(Gender = ifelse(Gender == "Total_Female", "Female", "Male"))

# Grafik oluştur
ggplot(df_long, aes(x = reorder(`Region, development group, country`, -Count), 
                    y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(Count / 1e6, 1), "M")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 3.5) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M"),
                     limits = c(0, 30e6)) +
  labs(title = "Top 12 Countries by Migrant Stock in 2020 (by Gender)",
       x = "Country",
       y = "Number of Migrants") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")
ggplot(df_long, aes(x = reorder(`Region, development group, country`, -Count), 
                    y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(Count / 1e6, 1), "M")), 
            position = position_dodge(width = 0.9),
            vjust = -0.6, size = 3) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M"),
                     limits = c(0, 30e6), expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Top 12 Countries by Migrant Stock in 2020 (by Gender)",
       x = "Country",
       y = "Number of Migrants") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")
