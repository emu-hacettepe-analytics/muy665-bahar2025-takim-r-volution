rm(list = ls())

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
file_path <- "data/supplier_scores.xlsx"

df <- read_excel(file_path, .name_repair = "unique") %>%
  select(-matches("^\\.\\.\\."))
names(df)
glimpse(df)
w_raw <- suppressWarnings(as.numeric(df[["Global_Weight_%"]]))

# yüzde (0-100) mi, oran (0-1) mı?
w0 <- if (all(w_raw <= 1, na.rm = TRUE)) w_raw else w_raw/100

# normalize (toplam 1)
w0 <- w0 / sum(w0, na.rm = TRUE)

m <- length(w0)
B_score <- "Supplier2_Score"
D_score <- "Supplier4_Score"

muB <- suppressWarnings(as.numeric(df[[B_score]]))
muD <- suppressWarnings(as.numeric(df[[D_score]]))
crit_label <- if ("Sub_Criterion" %in% names(df)) df$Sub_Criterion else paste0("C", seq_len(nrow(df)))
main_map   <- if ("Main_Criterion" %in% names(df)) df$Main_Criterion else rep("MAIN", nrow(df))
clamp <- function(x, lo=1, hi=5) pmax(lo, pmin(hi, x))

rtriangle <- function(n, a, c, b) {
  u <- runif(n)
  Fc <- (c - a) / (b - a)
  ifelse(u < Fc,
         a + sqrt(u * (b - a) * (c - a)),
         b - sqrt((1 - u) * (b - a) * (b - c)))
}

rdirichlet_mat <- function(n, alpha) {
  K <- length(alpha)
  X <- matrix(rgamma(n*K, shape=alpha, rate=1), nrow=n, ncol=K, byrow=TRUE)
  X / rowSums(X)
}
set.seed(42)

N <- 50000
kappa <- 300
half_range <- 0.5

alpha <- w0 * kappa   # dirichlet parametreleri
W <- rdirichlet_mat(N, alpha)  # N x m ağırlık matrisi
SB <- sapply(seq_len(m), \(j)
             clamp(rtriangle(N, muB[j]-half_range, muB[j], muB[j]+half_range), 1, 5)
)

SD <- sapply(seq_len(m), \(j)
             clamp(rtriangle(N, muD[j]-half_range, muD[j], muD[j]+half_range), 1, 5)
)

delta <- SB - SD              # N x m
gap   <- rowSums(W * delta)   # N uzunluk: (B - D) toplam skor farkı
summary(gap)
mean(gap)
drivers <- W * delta  # her kriterin katkısı (N x m)

corr <- sapply(seq_len(m), \(j) cor(drivers[, j], gap, use="complete.obs"))
tornado_corr <- tibble(
  Criterion = crit_label,
  Correlation = corr,
  AbsCorrelation = abs(corr)
) %>%
  arrange(desc(AbsCorrelation))
top10 <- tornado_corr %>% slice(1:10)

ggplot(top10, aes(x = AbsCorrelation, y = reorder(Criterion, AbsCorrelation))) +
  geom_col() +
  labs(title="Tornado (Sensitivity) – Top 10 Drivers", x="|Correlation with (B–D) gap|", y=NULL)

ggplot(top10, aes(x = Correlation, y = reorder(Criterion, AbsCorrelation))) +
  geom_col() +
  geom_vline(xintercept = 0, linetype="dashed") +
  labs(title="Signed Drivers of (B–D) gap (Top 10)", x="Correlation", y=NULL)
gap_contrib <- W * (SB - SD)  # N x m

main_contrib_df <- lapply(unique(main_map), function(mc) {
  
  idx <- which(main_map == mc)
  contrib_iter <- rowSums(gap_contrib[, idx, drop = FALSE])
  
  data.frame(
    Main_Criterion = mc,
    MeanAbsContribution = mean(abs(contrib_iter))
  )
}) %>%
  bind_rows() %>%
  arrange(desc(MeanAbsContribution))
main_contrib_top6 <- main_contrib_df %>% slice(1:6)

ggplot(main_contrib_top6,
       aes(x = MeanAbsContribution,
           y = reorder(Main_Criterion, MeanAbsContribution))) +
  geom_col() +
  labs(
    title = "Tornado – Main-criterion Sensitivity (Top 6)",
    x = "Mean(|contribution|) to (B–D) gap",
    y = NULL
  )

# --- Supplier skor ortalamaları (mu1..mu4)
mu1 <- suppressWarnings(as.numeric(df[["Supplier1_Score"]]))
mu2 <- suppressWarnings(as.numeric(df[["Supplier2_Score"]]))
mu3 <- suppressWarnings(as.numeric(df[["Supplier3_Score"]]))
mu4 <- suppressWarnings(as.numeric(df[["Supplier4_Score"]]))

# --- Her supplier için simüle skor matrisi (N x m)
S1 <- sapply(seq_len(m), function(j) clamp(rtriangle(N, mu1[j]-half_range, mu1[j], mu1[j]+half_range), 1, 5))
S2 <- sapply(seq_len(m), function(j) clamp(rtriangle(N, mu2[j]-half_range, mu2[j], mu2[j]+half_range), 1, 5))
S3 <- sapply(seq_len(m), function(j) clamp(rtriangle(N, mu3[j]-half_range, mu3[j], mu3[j]+half_range), 1, 5))
S4 <- sapply(seq_len(m), function(j) clamp(rtriangle(N, mu4[j]-half_range, mu4[j], mu4[j]+half_range), 1, 5))

# --- Toplam skorlar (N uzunluk) = satır satır sum(w * score)
totals <- cbind(
  Supplier1 = rowSums(W * S1),
  Supplier2 = rowSums(W * S2),
  Supplier3 = rowSums(W * S3),
  Supplier4 = rowSums(W * S4)
)

dim(totals)   # kontrol: N x 4 olmalı

library(tidyr)
library(dplyr)
library(ggplot2)

ts_df <- as.data.frame(totals) %>%
  pivot_longer(cols = everything(), names_to = "Supplier", values_to = "TotalScore")

ggplot(ts_df, aes(x = TotalScore, y = Supplier)) +
  geom_boxplot(outlier.size = 1.2) +
  labs(title = "Toplam skor dağılımı (Monte Carlo)",
       x = "Total Score", y = "Supplier") +
  theme_minimal()






