str(mtcars)
my_summary_stats <- function(x) {
  if (!is.numeric(x)) {
    stop("Girdi sayısal bir vektör olmalıdır.")
  }
  
  summary_list <- list(
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )
  
  return(summary_list)
}

# Örnek kullanım (mtcars veri seti ile)
data(mtcars)
my_summary_stats(mtcars$mpg)  # mpg sütununun özet istatistiklerini hesaplar
summary_results <- lapply(mtcars, my_summary_stats)
summary_results
# mtcars veri setini yükleme
data(mtcars)

# Tüm sütunlar üzerinde for döngüsü ile my_summary_stats uygulama
for (col_name in names(mtcars)) {
  if (is.numeric(mtcars[[col_name]])) {  # Sayısal sütunları seç
    cat("\nSütun Adı:", col_name, "\n")
    print(my_summary_stats(mtcars[[col_name]]))  # Özet istatistikleri yazdır
  }
}
# apply fonksiyonu ile tüm sayısal sütunlara my_summary_stats uygulama
summary_results <- lapply(mtcars, function(col) {
  if (is.numeric(col)) my_summary_stats(col) else NULL
})

# Sonuçları ekrana yazdır
summary_results

nrow(mtcars)
ncol(mtcars)