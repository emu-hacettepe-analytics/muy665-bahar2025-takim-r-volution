df_scatter <- df %>%
  filter(Year == 2020, Area == "Country") %>%
  mutate(
    Male = as.numeric(`Total(Males)`),
    Female = as.numeric(`Total(Females)`),
    Country = `Region, development group, country`
  )

ggplot(df_scatter, aes(x = Male, y = Female)) +
  geom_point(color = "steelblue", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "2020 – Ülkelerde Erkek ve Kadın Göçmen Sayısı",
       x = "Erkek Göçmen Sayısı", y = "Kadın Göçmen Sayısı") +
  theme_minimal()
library(scales)  # okunabilir eksen etiketi için

ggplot(df_scatter, aes(x = Male, y = Female)) +
  geom_point(color = "steelblue", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  scale_x_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  labs(
    title = "2020 – Ülkelerde Erkek ve Kadın Göçmen Sayısı",
    x = "Erkek Göçmen Sayısı (Milyon)",
    y = "Kadın Göçmen Sayısı (Milyon)"
  ) +
  theme_minimal(base_size = 13)
