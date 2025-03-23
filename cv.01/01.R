library(dslabs)   # Paketi yükle
data(murders)     # murders veri setini çağır

# murder_rate değişkenini murders veri setine ekle
murders$murder_rate <- murders$total / murders$population * 100000

# En düşük cinayet oranına sahip eyaleti bul
threshold <- 0.5
ind <- which.min(murders$murder_rate)

# Koşullu kontrolü düzgün yap
if (min(murders$murder_rate) < threshold) {
  print(murders$state[ind])
} else {
  print("No state has a murder rate that low")
}