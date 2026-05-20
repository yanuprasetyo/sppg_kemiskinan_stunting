# =============================================================================
# 04_tahap4_regresi.R
# Tahap 4: Regresi OLS, Uji Asumsi, Quantile Regression & Spatial Analysis
#
# Prasyarat: jalankan 00_setup.R dan 03_tahap3_tipologi.R terlebih dahulu
#            (agar kolom Cluster tersedia di df).
# Output   : tabel koefisien, diagnostic plots, Moran scatterplot, QR plot
# =============================================================================

# Jika belum dijalankan, source setup & tahap 3 untuk memastikan df lengkap
source("R/00_setup.R")
# Catatan: jika sudah menjalankan 03_tahap3_tipologi.R di sesi yang sama,
# tidak perlu source ulang — cukup pastikan df memiliki kolom Cluster.

cat("=== TAHAP 4: REGRESI & ANALISIS SPASIAL ===\n\n")

# =============================================================================
# 4A. MODEL OLS
# =============================================================================
cat("--- Model OLS ---\n")

# Model 1: Jumlah SPPG ~ Log_Pop + Kemiskinan + Stunting
m1 <- lm(Jumlah_SPPG ~ Log_Pop + Pct_Kemiskinan + Pct_Stunting, data = df)

# Model 2: Rasio SPPG/10k ~ Kemiskinan + Stunting + Log_Pop  (MODEL UTAMA)
m2 <- lm(Rasio_SPPG_10k ~ Pct_Kemiskinan + Pct_Stunting + Log_Pop, data = df)

# Model 3: Rasio SPPG/10k + Interaksi
m3 <- lm(Rasio_SPPG_10k ~ Pct_Kemiskinan * Pct_Stunting + Log_Pop, data = df)

cat("\nModel 1: Jumlah SPPG\n"); print(summary(m1))
cat("\nModel 2: Rasio SPPG/10k\n"); print(summary(m2))
cat("\nModel 3: Rasio + Interaksi\n"); print(summary(m3))

# Perbandingan model
cat("\n--- Perbandingan Model ---\n")
model_compare <- tibble(
  Model  = c("M1: Jumlah SPPG", "M2: Rasio SPPG", "M3: Rasio + Interaksi"),
  R2     = c(summary(m1)$r.squared, summary(m2)$r.squared,
              summary(m3)$r.squared),
  R2_adj = c(summary(m1)$adj.r.squared, summary(m2)$adj.r.squared,
              summary(m3)$adj.r.squared),
  AIC    = c(AIC(m1), AIC(m2), AIC(m3)),
  BIC    = c(BIC(m1), BIC(m2), BIC(m3))
) |>
  mutate(across(c(R2, R2_adj), round, 4),
         across(c(AIC, BIC), round, 2))
print(model_compare)

# Marginal effects M2
b      <- coef(m2)
med_r  <- median(df$Rasio_SPPG_10k)
cat("\n--- Interpretasi Marginal Effect (Model 2) ---\n")
cat(sprintf("  Kemiskinan +1%%  -> Rasio SPPG %+.4f/10k (%+.1f%% dari median)\n",
            b["Pct_Kemiskinan"], b["Pct_Kemiskinan"] / med_r * 100))
cat(sprintf("  Stunting +1%%    -> Rasio SPPG %+.4f/10k (%+.1f%% dari median)\n",
            b["Pct_Stunting"], b["Pct_Stunting"] / med_r * 100))
cat(sprintf("  Populasi x2     -> Rasio SPPG %+.4f/10k\n",
            b["Log_Pop"] * log(2)))

# =============================================================================
# 4B. UJI ASUMSI OLS
# =============================================================================
cat("\n--- Uji Asumsi OLS Model 2 ---\n")

resid_m2 <- residuals(m2)

# 1. Normalitas
sw  <- shapiro.test(resid_m2)
jb  <- tseries::jarque.bera.test(resid_m2)
cat(sprintf("1. Shapiro-Wilk  : W=%.4f, p=%.2e %s\n",
            sw$statistic, sw$p.value,
            ifelse(sw$p.value < 0.05, "-> TIDAK Normal", "-> Normal")))
cat(sprintf("   Jarque-Bera   : JB=%.2f, p=%.2e\n",
            jb$statistic, jb$p.value))
cat(sprintf("   Skewness      : %.4f\n", moments::skewness(resid_m2)))
cat(sprintf("   Kurtosis      : %.4f\n", moments::kurtosis(resid_m2)))

# 2. Homoskedastisitas
bp <- bptest(m2)
cat(sprintf("2. Breusch-Pagan : BP=%.4f, p=%.4f %s\n",
            bp$statistic, bp$p.value,
            ifelse(bp$p.value < 0.05, "-> Heteroskedastis", "-> OK")))

# 3. Multikolinearitas
vif_vals <- vif(m2)
cat("3. VIF:\n")
for (nm in names(vif_vals)) {
  cat(sprintf("   %-22s: %.3f %s\n", nm, vif_vals[nm],
              ifelse(vif_vals[nm] > 10, "-> MASALAH",
                     ifelse(vif_vals[nm] > 5, "-> Perhatian", "-> OK"))))
}

# 4. Durbin-Watson
dw <- dwtest(m2)
cat(sprintf("4. Durbin-Watson : DW=%.4f, p=%.4f\n",
            dw$statistic, dw$p.value))

# 5. Outlier
std_resid <- rstandard(m2)
cook_d    <- cooks.distance(m2)
cat(sprintf("5. |std resid|>2 : %d kab/kota\n", sum(abs(std_resid) > 2)))
cat(sprintf("   Cook's D>4/n  : %d kab/kota\n", sum(cook_d > 4 / nrow(df))))
cat("   Top 5 outlier:\n")
df_diag <- df |>
  mutate(std_resid = std_resid, cook_d = cook_d,
         fitted = fitted(m2), resid = resid_m2)
df_diag |>
  slice_max(abs(std_resid), n = 5) |>
  select(Kabupaten_Kota, Rasio_SPPG_10k, fitted, std_resid, cook_d) |>
  mutate(across(where(is.numeric), round, 4)) |>
  print()

# =============================================================================
# 4C. DIAGNOSTIC PLOTS
# =============================================================================
p_rv_f <- ggplot(df_diag, aes(fitted, resid)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "gray30", linewidth = 0.8) +
  geom_hline(yintercept = c(-2, 2) * sd(resid_m2),
             linetype = "dotted", color = "#A32D2D", alpha = 0.5) +
  geom_point(aes(color = abs(std_resid) > 2,
                 size  = abs(std_resid) > 3),
             alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE,
              color = "#BA7517", linewidth = 0.9, formula = y ~ x) +
  geom_text_repel(
    data  = df_diag |> filter(abs(std_resid) > 2.5),
    aes(label = Kabupaten_Kota), size = 2.8,
    max.overlaps = 10, segment.color = "gray50", segment.size = 0.3
  ) +
  scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "#A32D2D"),
                     labels = c("Normal", "Outlier (|std|>2)")) +
  scale_size_manual(values = c("FALSE" = 1.8, "TRUE" = 3.5), guide = "none") +
  labs(title    = "Residual vs Fitted Values",
       subtitle = "Pola ideal: titik tersebar acak di sekitar garis nol",
       x = "Fitted values", y = "Residuals", color = NULL) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 11))

p_qq <- ggplot(df_diag, aes(sample = std_resid)) +
  stat_qq(aes(color = abs(std_resid) > 2), alpha = 0.65, size = 1.8) +
  stat_qq_line(color = "gray30", linewidth = 0.8) +
  scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "#A32D2D"),
                     guide = "none") +
  labs(title    = "Normal Q-Q Plot Residual",
       subtitle = sprintf("Shapiro-Wilk p = %.2e -> %s",
                          sw$p.value,
                          ifelse(sw$p.value < 0.05, "TIDAK Normal", "Normal")),
       x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme(plot.title    = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 9, color = "gray40"))

p_sl <- ggplot(df_diag, aes(fitted, sqrt(abs(std_resid)))) +
  geom_point(alpha = 0.5, color = "steelblue", size = 1.8) +
  geom_smooth(method = "loess", se = FALSE, color = "#BA7517",
              linewidth = 0.9, formula = y ~ x) +
  labs(title    = "Scale-Location",
       subtitle = sprintf("Breusch-Pagan p=%.4f -> %s",
                          bp$p.value,
                          ifelse(bp$p.value < 0.05, "Heteroskedastis", "OK")),
       x = "Fitted values", y = "\u221A|Standardized Residuals|") +
  theme(plot.title    = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 9, color = "gray40"))

p_cook <- df_diag |>
  mutate(idx = row_number()) |>
  ggplot(aes(idx, cook_d)) +
  geom_col(aes(fill = cook_d > 4 / nrow(df)),
           width = 0.8, show.legend = FALSE) +
  geom_hline(yintercept = 4 / nrow(df), linetype = "dashed",
             color = "#A32D2D", linewidth = 0.8) +
  geom_text_repel(
    data = df_diag |> mutate(idx = row_number()) |>
      filter(cook_d > 4 / nrow(df)),
    aes(label = Kabupaten_Kota), size = 2.5,
    max.overlaps = 8, segment.size = 0.3
  ) +
  scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "#A32D2D")) +
  labs(title    = "Cook's Distance \u2014 Influential Points",
       subtitle = sprintf("Threshold = 4/n=%.4f | %d kab/kota melebihi threshold",
                          4 / nrow(df), sum(cook_d > 4 / nrow(df))),
       x = "Index kab/kota", y = "Cook's Distance") +
  theme(plot.title    = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 9, color = "gray40"))

p_diag <- (p_rv_f + p_qq) / (p_sl + p_cook) +
  plot_annotation(
    title    = "Diagnostic Plots \u2014 OLS Model 2 (Rasio SPPG/10k)",
    subtitle = "Identifikasi pelanggaran asumsi dan outlier berpengaruh",
    theme    = theme(plot.title    = element_text(size = 13, face = "bold"),
                     plot.subtitle = element_text(size = 10, color = "gray40"))
  )

print(p_diag)
ggsave("output/Rplot13_diagnostic_plots.png", p_diag,
       width = 12, height = 9, dpi = 300)
cat("✓ Rplot13_diagnostic_plots.png tersimpan\n")

# =============================================================================
# 4D. QUANTILE REGRESSION
# =============================================================================
cat("\n--- Quantile Regression ---\n")

taus <- c(0.10, 0.25, 0.50, 0.75, 0.90)

qr_list <- lapply(taus, function(tau) {
  qr_fit <- rq(Rasio_SPPG_10k ~ Pct_Kemiskinan + Pct_Stunting + Log_Pop,
               data = df, tau = tau)
  sum_qr  <- summary(qr_fit, se = "boot", R = 500)
  cm      <- sum_qr$coefficients
  data.frame(
    Tau      = tau,
    Variabel = rownames(cm),
    Beta     = cm[, 1], SE = cm[, 2],
    t_val    = cm[, 3], p_val = cm[, 4],
    row.names = NULL, stringsAsFactors = FALSE
  )
})

qr_df <- do.call(rbind, qr_list) |>
  mutate(
    Sig       = ifelse(p_val < 0.001, "***",
                ifelse(p_val < 0.01,  "**",
                ifelse(p_val < 0.05,  "*", "ns"))),
    Tau_label = paste0("Q", Tau * 100)
  ) |>
  filter(Variabel != "(Intercept)")

cat("Koefisien QR:\n")
print(qr_df |>
  select(Tau_label, Variabel, Beta, SE, p_val, Sig) |>
  mutate(across(c(Beta, SE, p_val), round, 4)),
  row.names = FALSE)

# Plot QR
var_labels <- c("Pct_Kemiskinan" = "% Kemiskinan",
                "Pct_Stunting"   = "% Stunting",
                "Log_Pop"        = "Log Populasi")
qr_df$Var_label <- var_labels[qr_df$Variabel]

ols_ref <- data.frame(
  Variabel  = c("Pct_Kemiskinan", "Pct_Stunting", "Log_Pop"),
  Beta_OLS  = as.numeric(coef(m2)[c("Pct_Kemiskinan","Pct_Stunting","Log_Pop")]),
  stringsAsFactors = FALSE
) |> mutate(Var_label = var_labels[Variabel])

p_qr <- ggplot(qr_df, aes(x = Tau, y = Beta,
                            color = Var_label, group = Var_label)) +
  geom_ribbon(aes(ymin = Beta - 1.96 * SE, ymax = Beta + 1.96 * SE,
                  fill = Var_label), alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(aes(shape = Sig != "ns"), size = 3.5) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "gray30", linewidth = 0.8) +
  geom_hline(data = ols_ref,
             aes(yintercept = Beta_OLS, color = Var_label),
             linetype = "dotted", linewidth = 0.9, alpha = 0.6) +
  scale_x_continuous(breaks = taus,
                     labels = paste0("Q", taus * 100)) +
  scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16),
                     labels = c("Tidak signifikan", "Signifikan"),
                     name = NULL) +
  scale_color_manual(values = c("% Kemiskinan" = "#A32D2D",
                                 "% Stunting"   = "#BA7517",
                                 "Log Populasi" = "#185FA5")) +
  scale_fill_manual(values  = c("% Kemiskinan" = "#A32D2D",
                                 "% Stunting"   = "#BA7517",
                                 "Log Populasi" = "#185FA5")) +
  facet_wrap(~ Var_label, scales = "free_y", nrow = 1) +
  labs(
    title    = "Quantile Regression \u2014 Variasi Efek Lintas Distribusi",
    subtitle = "Garis titik-titik = koefisien OLS (referensi) | Pita = CI 95%",
    x = "Kuantil", y = "Koefisien \u03B2",
    color = NULL, fill = NULL
  ) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 10),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9, color = "gray40"))

print(p_qr)
ggsave("output/Rplot14_quantile_regression.png", p_qr,
       width = 12, height = 6, dpi = 300)
cat("✓ Rplot14_quantile_regression.png tersimpan\n")

# =============================================================================
# 4E. SPATIAL AUTOCORRELATION — MORAN'S I
# =============================================================================
cat("\n--- Analisis Spasial: Moran's I ---\n")

# Import data SPPG mentah untuk koordinat
# Sesuaikan path jika perlu
SPPG_RAW_PATH <- "data/Data_SPPG_010526_completed.xlsx"

if (!file.exists(SPPG_RAW_PATH)) {
  warning("File SPPG mentah tidak ditemukan: ", SPPG_RAW_PATH,
          "\nLewati analisis spasial.")
} else {
  df_sppg_raw <- read_excel(SPPG_RAW_PATH)

  # Agregasi centroid per kab/kota
  SPPG_NAME_MAP <- c(
    "MUKO MUKO" = "Mukomuko", "GUNUNGKIDUL" = "Gunung Kidul",
    "ADM. KEP. SERIBU" = "Kepulauan Seribu",
    "BATANGHARI" = "Batang Hari",
    "KOTA BANJARBARU" = "Kota Banjar Baru", "KOTABARU" = "Kota Baru",
    "TULANG BAWANG" = "Tulangbawang",
    "KEPULAUAN TANIMBAR" = "Maluku Tenggara Barat",
    "KAB TIMOR TENGAH SELATAN" = "Timor Tengah Selatan",
    "FAK FAK" = "Fakfak", "PASANGKAYU" = "Mamuju Utara",
    "TOJO UNA UNA" = "Tojo Una-Una", "TOLI TOLI" = "Toli-Toli",
    "KOTA BAU BAU" = "Kota Baubau",
    "KEP. SIAU TAGULANDANG BIARO" = "Siau Tagulandang Biaro",
    "KOTA SAWAHLUNTO" = "Kota Sawah Lunto", "BANYUASIN" = "Banyu Asin",
    "KOTA LUBUK LINGGAU" = "Kota Lubuklinggau",
    "KOTA PADANG SIDEMPUAN" = "Kota Padangsidimpuan",
    "KOTA PEMATANGSIANTAR" = "Kota Pematang Siantar",
    "LABUHANBATU" = "Labuhan Batu",
    "LABUHANBATU SELATAN" = "Labuhan Batu Selatan",
    "LABUHANBATU UTARA" = "Labuhan Batu Utara", "TOBA" = "Toba Samosir"
  )

  centroids <- df_sppg_raw |>
    group_by(kab = `Kab./Kota SPPG`) |>
    summarise(lat = mean(Latitude, na.rm = TRUE),
              lon = mean(Longitude, na.rm = TRUE), .groups = "drop") |>
    mutate(
      kab_clean = str_to_title(kab),
      kab_clean = ifelse(toupper(kab) %in% names(SPPG_NAME_MAP),
                         SPPG_NAME_MAP[toupper(kab)], kab_clean)
    )

  df_geo <- df |>
    left_join(centroids |> select(kab_clean, lat, lon),
              by = c("Kabupaten_Kota" = "kab_clean"))

  cat(sprintf("Berhasil join koordinat: %d / %d kab/kota\n",
              sum(!is.na(df_geo$lat)), nrow(df_geo)))

  df_sp <- df_geo |> filter(!is.na(lat))
  coords <- as.matrix(df_sp |> select(lon, lat))

  # k-NN spatial weights (k=5)
  knn5    <- knearneigh(coords, k = 5)
  nb_knn5 <- knn2nb(knn5)
  W_knn5  <- nb2listw(nb_knn5, style = "W")

  # Moran's I: Rasio SPPG
  moran_rasio <- moran.test(df_sp$Rasio_SPPG_10k, W_knn5,
                             alternative = "greater")
  cat(sprintf("\nMoran's I (Rasio SPPG): I=%.4f, Z=%.4f, p=%.4e %s\n",
              moran_rasio$estimate[1], moran_rasio$statistic,
              moran_rasio$p.value,
              ifelse(moran_rasio$p.value < 0.001, "***", "")))

  # Moran's I: OLS residuals
  resid_sp    <- residuals(m2)[!is.na(df_geo$lat)]
  moran_resid <- moran.test(resid_sp, W_knn5, alternative = "greater")
  cat(sprintf("Moran's I (OLS resid) : I=%.4f, p=%.4e %s\n",
              moran_resid$estimate[1], moran_resid$p.value,
              ifelse(moran_resid$p.value < 0.001, "***", "")))

  # Monte Carlo permutation
  set.seed(42)
  moran_mc <- moran.mc(df_sp$Rasio_SPPG_10k, W_knn5,
                        nsim = 999, alternative = "greater")
  cat(sprintf("Monte Carlo (999 sim) : I=%.4f, p=%.4f\n",
              moran_mc$statistic, moran_mc$p.value))

  # =========================================================================
  # 4F. SPATIAL LAG MODEL (SLM) & SPATIAL ERROR MODEL (SEM)
  # =========================================================================
  cat("\n--- LM Tests untuk pilih SLM vs SEM ---\n")
  ols_sp  <- lm(Rasio_SPPG_10k ~ Pct_Kemiskinan + Pct_Stunting + Log_Pop,
                data = df_sp)
  lm_tests <- lm.RStests(ols_sp, W_knn5,
                           test = c("LMlag","LMerr","RLMlag","RLMerr"))
  print(lm_tests)

  slm <- lagsarlm(
    Rasio_SPPG_10k ~ Pct_Kemiskinan + Pct_Stunting + Log_Pop,
    data = df_sp, listw = W_knn5, method = "eigen"
  )
  sem <- errorsarlm(
    Rasio_SPPG_10k ~ Pct_Kemiskinan + Pct_Stunting + Log_Pop,
    data = df_sp, listw = W_knn5, method = "eigen"
  )

  cat("\n--- Spatial Lag Model ---\n"); print(summary(slm))
  cat("\n--- Spatial Error Model ---\n"); print(summary(sem))

  moran_slm <- moran.test(residuals(slm), W_knn5, alternative = "greater")
  y_sp_mean <- mean(df_sp$Rasio_SPPG_10k)
  r2_slm    <- 1 - slm$SSE / sum((df_sp$Rasio_SPPG_10k - y_sp_mean)^2)
  r2_sem    <- 1 - sem$SSE / sum((df_sp$Rasio_SPPG_10k - y_sp_mean)^2)

  cat(sprintf("\nMoran's I (SLM resid): %.4f %s\n",
              moran_slm$estimate[1],
              ifelse(moran_slm$p.value < 0.05,
                     "-> masih ada autokorelasi",
                     "-> spatial bias teratasi")))

  cat("\n--- Perbandingan Semua Model ---\n")
  tibble(
    Model         = c("OLS", "OLS+Interaksi", "SLM", "SEM"),
    R2            = c(summary(m2)$r.squared, summary(m3)$r.squared,
                       r2_slm, r2_sem),
    AIC           = c(AIC(m2), AIC(m3), AIC(slm), AIC(sem)),
    Moran_Resid   = c(moran_resid$estimate[1], NA,
                       moran_slm$estimate[1], NA),
    Spatial_Param = c(NA, NA, slm$rho, sem$lambda)
  ) |>
    mutate(across(c(R2, Moran_Resid, Spatial_Param), round, 4),
           AIC = round(AIC, 2)) |>
    print()

  # =========================================================================
  # 4G. MORAN SCATTERPLOT (LISA)
  # =========================================================================
  rasio_std <- as.numeric(scale(df_sp$Rasio_SPPG_10k))
  lag_std   <- as.numeric(scale(lag.listw(W_knn5, df_sp$Rasio_SPPG_10k)))

  df_moran <- data.frame(
    Kab      = df_sp$Kabupaten_Kota,
    Provinsi = df_sp$Provinsi,
    x        = rasio_std,
    y        = lag_std,
    stringsAsFactors = FALSE
  ) |>
    mutate(
      Kuadran = ifelse(x >= 0 & y >= 0, "HH hotspot",
                ifelse(x <  0 & y <  0, "LL coldspot",
                ifelse(x >= 0 & y <  0, "HL outlier", "LH outlier"))),
      abs_sum = abs(x) + abs(y)
    )

  label_moran <- df_moran |>
    filter(abs_sum > 3) |>
    slice_max(abs_sum, n = 12)

  warna_moran <- c("HH hotspot"  = "#185FA5", "LL coldspot" = "#A32D2D",
                    "HL outlier"  = "#85B7EB", "LH outlier"  = "#F09595")

  p_moran <- ggplot(df_moran, aes(x = x, y = y, color = Kuadran)) +
    annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf,
             fill = "#185FA5", alpha = 0.05) +
    annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0,
             fill = "#A32D2D", alpha = 0.05) +
    annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = 0,
             fill = "#85B7EB", alpha = 0.05) +
    annotate("rect", xmin = -Inf, xmax = 0, ymin = 0, ymax = Inf,
             fill = "#F09595", alpha = 0.05) +
    geom_vline(xintercept = 0, color = "gray40",
               linewidth = 0.7, linetype = "dashed") +
    geom_hline(yintercept = 0, color = "gray40",
               linewidth = 0.7, linetype = "dashed") +
    geom_point(alpha = 0.65, size = 2) +
    geom_smooth(method = "lm", se = TRUE, color = "gray20",
                linewidth = 0.9, aes(group = 1), formula = y ~ x) +
    geom_text_repel(data = label_moran, aes(label = Kab),
                    size = 2.5, max.overlaps = 15,
                    segment.color = "gray50", segment.size = 0.3) +
    scale_color_manual(values = warna_moran) +
    labs(
      title    = "Moran Scatterplot \u2014 Rasio SPPG per 10.000 Penduduk",
      subtitle = sprintf("Moran's I = %.4f, p < 0.001*** | Slope = nilai Moran's I",
                         moran_rasio$estimate[1]),
      x       = "Rasio SPPG/10k (standardized)",
      y       = "Spatial Lag Rasio SPPG/10k (standardized)",
      color   = "Tipe LISA",
      caption = "HH = klaster tinggi | LL = klaster rendah | HL/LH = outlier spasial"
    ) +
    theme(legend.position = "bottom",
          plot.title    = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(size = 9, color = "gray40"),
          plot.caption  = element_text(size = 8, color = "gray50"))

  print(p_moran)
  ggsave("output/Rplot15_moran_scatterplot.png", p_moran,
         width = 9, height = 8, dpi = 300)
  cat("✓ Rplot15_moran_scatterplot.png tersimpan\n")
}

# =============================================================================
# SIMPAN TABEL KOEFISIEN
# =============================================================================
coef_m2 <- summary(m2)$coefficients
write.csv(
  data.frame(
    Variabel = rownames(coef_m2), Beta = round(coef_m2[, 1], 5),
    SE = round(coef_m2[, 2], 5), t_val = round(coef_m2[, 3], 3),
    p_val = round(coef_m2[, 4], 4),
    Sig = ifelse(coef_m2[, 4] < 0.001, "***",
          ifelse(coef_m2[, 4] < 0.01, "**",
          ifelse(coef_m2[, 4] < 0.05, "*", "ns")))
  ),
  "output/T4_koefisien_OLS.csv", row.names = FALSE
)
write.csv(
  qr_df |> select(Tau_label, Variabel, Beta, SE, p_val, Sig) |>
    mutate(across(c(Beta, SE, p_val), round, 4)),
  "output/T4_koefisien_QR.csv", row.names = FALSE
)

cat("✓ CSV koefisien tersimpan\n")
cat("=== TAHAP 4 SELESAI ===\n\n")
