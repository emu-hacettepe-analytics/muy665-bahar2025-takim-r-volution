library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)

# Yaş grubu kolonlarını tanımla
age_cols <- c("0-4 total", "5-9 total", "10-14 total", "15-19 total", "20-24 total",
              "25-29 total", "30-34 total", "35-39 total", "40-44 total", "45-49 total",
              "50-54 total", "55-59 total", "60-64 total", "65-69 total", "70-74 total", "75+ total")

# Veriyi temizle ve yeniden biçimlendir
df_long <- df %>%
  filter(Year == 2020, Area == "Country") %>%
  mutate(Total = as.numeric(`Total(Both)`)) %>%
  pivot_longer(cols = all_of(age_cols), names_to = "AgeGroup", values_to = "Count") %>%
  mutate(Count = as.numeric(Count),
         Ratio = Count / Total) %>%
  drop_na()

# Scatter plot
ggplot(df_long, aes(x = Total, y = Ratio)) +
  geom_point(color = "darkblue", size = 1.2, alpha = 0.8) +
  scale_x_log10(
    breaks = c(1e4, 1e5, 1e6, 1e7, 1e8),
    labels = label_number(scale = 1e-6, suffix = "M")
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  facet_wrap(~AgeGroup, ncol = 4) +
  labs(
    title = "2020 – Yaş Gruplarına Göre Göçmen Oranları (Log Ölçekli)",
    x = "Toplam Göçmen Sayısı (log10, Milyon)",
    y = "Yaş Grubunun Oranı"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
