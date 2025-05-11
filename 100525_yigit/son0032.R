library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Dosya yolu
file_path <- "C:/R-packages/undesa_pd_2020_ims_stock_by_age_sex_and_destination_2.xlsx"

# Başlıklar genelde 10. satırda başlar → skip = 9
df <- read_excel(file_path, sheet = "Table 1", skip = 9)

# Ülke, cinsiyet, yıl, yaş verisini al
df_age <- df %>%
  filter(Year == 2020,
         Area == "Country",
         `Region, development group, country` %in% c("Germany", "Turkey", "United States of America*")) %>%
  select(
    Country = `Region, development group, country`,
    Sex,
    `0-4`:`75+`
  ) %>%
  pivot_longer(
    cols = `0-4`:`75+`,
    names_to = "AgeGroup",
    values_to = "Migrants"
  ) %>%
  mutate(
    Migrants = as.numeric(Migrants)
  ) %>%
  drop_na(Migrants)

# Grafik
ggplot(df_age, aes(x = AgeGroup, y = Migrants, group = Country, color = Country)) +
  geom_line(size = 1.2) +
  geom_point(size = 1.8) +
  scale_y_continuous(labels = scales::label_comma()) +
  facet_wrap(~Sex) +
  labs(
    title = "2020 Migrant Stock by Age Group and Gender",
    x = "Age Group",
    y = "Number of Migrants",
    color = "Country"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
