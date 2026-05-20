# =============================================================================
# 05_tahap5_skenario.R
# Tahap 5: Needs-Based Allocation Model & Scenario Analysis
#   - 5 skenario alokasi dengan total SPPG tetap 27.427
#   - Benefit Incidence Analysis (BIA) per kuintil kemiskinan
#   - Composite Deprivation Index (CDI)
#   - Sensitivity Analysis (25 kombinasi bobot)
#
# Prasyarat: jalankan 00_setup.R (dan 03_tahap3_tipologi.R jika ingin
#            kolom Cluster tersedia di df).
# =============================================================================

source("R/00_setup.R")

cat("=== TAHAP 5: SKENARIO & SIMULASI KEBIJAKAN ===\n\n")

total_sppg <- sum(df$Jumlah_SPPG)  # 27.427
cat(sprintf("Total SPPG yang akan dialokasikan: %d dapur\n\n", total_sppg))

# =============================================================================
# FUNGSI HELPER
# =============================================================================

#' Needs-Based Allocation: distribusikan total_sppg proporsional ke skor kebutuhan
alokasi_ideal <- function(data, w_pop, w_miskin, w_stunting,
                           total = total_sppg) {
  norm_01 <- function(x) {
    rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
    if (rng == 0) return(rep(0, length(x)))
    (x - min(x, na.rm = TRUE)) / rng
  }

  skor <- w_pop      * norm_01(data$Populasi_2026) +
          w_miskin   * norm_01(data$Jml_Penduduk_Miskin) +
          w_stunting * norm_01(data$Pct_Stunting * data$Populasi_2026 / 100)

  sppg_ideal <- round(skor / sum(skor) * total)

  # Koreksi pembulatan agar total pas
  selisih <- total - sum(sppg_ideal)
  if (selisih != 0) {
    idx <- order(skor, decreasing = (selisih > 0))
    for (i in seq_len(abs(selisih))) {
      sppg_ideal[idx[i]] <- sppg_ideal[idx[i]] + sign(selisih)
    }
  }
  sppg_ideal
}

#' Gini Coefficient
gini_coef <- function(x) {
  x <- sort(x[x > 0 & !is.na(x)])
  n <- length(x)
  (2 * sum(seq_len(n) * x) - (n + 1) * sum(x)) / (n * sum(x))
}

#' Concentration Index (CI): negatif = pro-poor, positif = pro-rich
calc_ci <- function(benefit, rank_var) {
  n  <- length(benefit)
  r  <- rank(rank_var, na.last = "keep") / n
  mu <- mean(benefit, na.rm = TRUE)
  2 * cov(benefit, r, use = "complete.obs") / mu
}

#' Hoover Index
hoover_index <- function(x, y) {
  x_share <- x / sum(x, na.rm = TRUE)
  y_share <- y / sum(y, na.rm = TRUE)
  0.5 * sum(abs(x_share - y_share), na.rm = TRUE)
}

# =============================================================================
# 5A. SKENARIO ALOKASI
# =============================================================================
cat("--- Menghitung 5 Skenario Alokasi ---\n")

df <- df |>
  mutate(
    S0_Aktual     = Jumlah_SPPG,
    S1_PopMurni   = alokasi_ideal(cur_data(), 1.0, 0.0, 0.0),
    S2_ProMiskin  = alokasi_ideal(cur_data(), 0.2, 0.6, 0.2),
    S3_ProStunt   = alokasi_ideal(cur_data(), 0.2, 0.2, 0.6),
    S4_Balanced   = alokasi_ideal(cur_data(), 0.2, 0.4, 0.4)
  )

# Verifikasi total
cat("Verifikasi total SPPG per skenario:\n")
for (s in c("S0_Aktual","S1_PopMurni","S2_ProMiskin","S3_ProStunt","S4_Balanced")) {
  cat(sprintf("  %-15s: %d %s\n", s, sum(df[[s]]),
              ifelse(sum(df[[s]]) == total_sppg, "(OK)", "(MISMATCH!)")))
}

# Hitung rasio per 10k dan gap
df <- df |>
  mutate(
    R0 = S0_Aktual    / Populasi_2026 * 10000,
    R1 = S1_PopMurni  / Populasi_2026 * 10000,
    R2 = S2_ProMiskin / Populasi_2026 * 10000,
    R3 = S3_ProStunt  / Populasi_2026 * 10000,
    R4 = S4_Balanced  / Populasi_2026 * 10000,
    Gap_S1 = S1_PopMurni  - S0_Aktual,
    Gap_S2 = S2_ProMiskin - S0_Aktual,
    Gap_S3 = S3_ProStunt  - S0_Aktual,
    Gap_S4 = S4_Balanced  - S0_Aktual
  )

# Ringkasan per skenario
cat("\n--- Ringkasan Statistik per Skenario ---\n")
for (rvar in c("R0","R1","R2","R3","R4")) {
  vals <- df[[rvar]]
  cat(sprintf("  %-3s: mean=%.3f, median=%.3f, sd=%.3f, Gini=%.4f, CI=%+.4f\n",
              rvar, mean(vals), median(vals), sd(vals),
              gini_coef(vals), calc_ci(df[[sub("R","S",rvar)]], df$Pct_Kemiskinan)))
}

cat("\nTop 10 penerima manfaat S4 Balanced:\n")
df |>
  arrange(desc(Gap_S4)) |>
  select(Provinsi, Kabupaten_Kota, S0_Aktual, S4_Balanced,
         Gap_S4, Pct_Kemiskinan, Pct_Stunting) |>
  head(10) |>
  print()

# =============================================================================
# 5B. BENEFIT INCIDENCE ANALYSIS (BIA)
# =============================================================================
cat("\n--- Benefit Incidence Analysis ---\n")

df <- df |>
  mutate(
    Kuintil_Miskin = ntile(Pct_Kemiskinan, 5),
    Kuintil_Label  = paste0("K", Kuintil_Miskin,
      ifelse(Kuintil_Miskin == 1, " (terkaya)", ""),
      ifelse(Kuintil_Miskin == 5, " (termiskin)", ""))
  )

bia <- df |>
  group_by(Kuintil_Miskin, Kuintil_Label) |>
  summarise(
    n                  = n(),
    Pop_total          = sum(Populasi_2026),
    Pddk_Miskin_total  = sum(Jml_Penduduk_Miskin),
    S0_total           = sum(S0_Aktual),
    S2_total           = sum(S2_ProMiskin),
    S4_total           = sum(S4_Balanced),
    Stunting_mean      = round(mean(Pct_Stunting), 1),
    .groups            = "drop"
  ) |>
  mutate(
    Pct_Pop    = round(Pop_total          / sum(Pop_total)          * 100, 1),
    Pct_Miskin = round(Pddk_Miskin_total  / sum(Pddk_Miskin_total)  * 100, 1),
    Pct_S0     = round(S0_total / sum(S0_total) * 100, 1),
    Pct_S2     = round(S2_total / sum(S2_total) * 100, 1),
    Pct_S4     = round(S4_total / sum(S4_total) * 100, 1)
  )

cat("BIA: K1=terkaya (kemiskinan rendah) -> K5=termiskin\n")
print(bia |> select(Kuintil_Label, n, Pct_Pop, Pct_Miskin,
                      Pct_S0, Pct_S2, Pct_S4, Stunting_mean))

# Concentration Index
ci_s0 <- calc_ci(df$S0_Aktual,    df$Pct_Kemiskinan)
ci_s2 <- calc_ci(df$S2_ProMiskin, df$Pct_Kemiskinan)
ci_s4 <- calc_ci(df$S4_Balanced,  df$Pct_Kemiskinan)

cat(sprintf("\nConcentration Index:\n"))
cat(sprintf("  S0 Aktual    : %+.4f %s\n", ci_s0,
            ifelse(ci_s0 < -0.05, "(pro-poor)",
                   ifelse(ci_s0 > 0.05, "(pro-rich)", "(netral)"))))
cat(sprintf("  S2 Pro-Miskin: %+.4f %s\n", ci_s2,
            ifelse(ci_s2 < -0.05, "(pro-poor)",
                   ifelse(ci_s2 > 0.05, "(pro-rich)", "(netral)"))))
cat(sprintf("  S4 Balanced  : %+.4f %s\n", ci_s4,
            ifelse(ci_s4 < -0.05, "(pro-poor)",
                   ifelse(ci_s4 > 0.05, "(pro-rich)", "(netral)"))))

# =============================================================================
# 5C. COMPOSITE DEPRIVATION INDEX (CDI)
# =============================================================================
cat("\n--- Composite Deprivation Index ---\n")

norm_01 <- function(x) {
  rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  if (rng == 0) return(rep(0, length(x)))
  (x - min(x, na.rm = TRUE)) / rng
}

df <- df |>
  mutate(
    CDI_miskin   = norm_01(Pct_Kemiskinan),
    CDI_stunting = norm_01(Pct_Stunting),
    CDI_sppg_gap = norm_01(1 / (Rasio_SPPG_10k + 0.01)),
    CDI          = (CDI_miskin + CDI_stunting + CDI_sppg_gap) / 3,
    Deprivasi    = case_when(
      CDI >= quantile(CDI, 0.80) ~ "Sangat Tinggi (top 20%)",
      CDI >= quantile(CDI, 0.60) ~ "Tinggi (60-80%)",
      CDI >= quantile(CDI, 0.40) ~ "Menengah (40-60%)",
      CDI >= quantile(CDI, 0.20) ~ "Rendah (20-40%)",
      TRUE                        ~ "Sangat Rendah (bottom 20%)"
    )
  )

cat("15 kab/kota paling deprived:\n")
df |>
  slice_max(CDI, n = 15) |>
  select(Provinsi, Kabupaten_Kota, CDI, Pct_Kemiskinan,
         Pct_Stunting, Rasio_SPPG_10k) |>
  mutate(across(where(is.numeric), round, 3)) |>
  print()

# =============================================================================
# 5D. SENSITIVITY ANALYSIS
# =============================================================================
cat("\n--- Sensitivity Analysis (25 kombinasi bobot) ---\n")

bobot_grid <- expand.grid(
  w_pop    = c(0.0, 0.1, 0.2, 0.3, 0.4),
  w_miskin = c(0.2, 0.3, 0.4, 0.5, 0.6)
) |>
  mutate(w_stunting = 1 - w_pop - w_miskin) |>
  filter(w_stunting >= 0)

hasil_sa <- bobot_grid |>
  rowwise() |>
  mutate(
    sppg_ideal    = list(alokasi_ideal(df, w_pop, w_miskin, w_stunting)),
    Rasio_ideal   = list(unlist(sppg_ideal) / df$Populasi_2026 * 10000),
    Gini          = gini_coef(unlist(Rasio_ideal)),
    CI            = calc_ci(unlist(sppg_ideal), df$Pct_Kemiskinan),
    Hoover_miskin = hoover_index(unlist(sppg_ideal), df$Jml_Penduduk_Miskin)
  ) |>
  ungroup() |>
  select(w_pop, w_miskin, w_stunting, Gini, CI, Hoover_miskin) |>
  mutate(across(c(Gini, CI, Hoover_miskin), round, 4)) |>
  arrange(Gini)

cat("Top 10 kombinasi bobot (Gini terendah):\n")
print(head(hasil_sa, 10))

best <- hasil_sa[1, ]
cat(sprintf("\nBobot optimal:\n"))
cat(sprintf("  w_populasi   = %.1f\n", best$w_pop))
cat(sprintf("  w_kemiskinan = %.1f\n", best$w_miskin))
cat(sprintf("  w_stunting   = %.1f\n", best$w_stunting))
cat(sprintf("  Gini optimal = %.4f (vs aktual: %.4f, turun %.1f%%)\n",
            best$Gini, gini_coef(df$R0),
            (gini_coef(df$R0) - best$Gini) / gini_coef(df$R0) * 100))

# =============================================================================
# 5E. VISUALISASI
# =============================================================================

# Plot 1: Gini per skenario
gini_df <- tibble(
  Skenario = c("S0\nAktual", "S1\nPop Murni",
               "S2\nPro-Miskin", "S3\nPro-Stunting", "S4\nBalanced"),
  Gini     = c(gini_coef(df$R0), gini_coef(df$R1),
               gini_coef(df$R2), gini_coef(df$R3), gini_coef(df$R4)),
  Warna    = c("aktual", "s1", "s2", "s3", "s4")
)

p_gini_s <- ggplot(gini_df, aes(x = Skenario, y = Gini, fill = Warna)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = round(Gini, 4)),
            vjust = -0.4, fontface = "bold", size = 4) +
  geom_hline(yintercept = gini_df$Gini[1],
             linetype = "dashed", color = "gray30", linewidth = 0.8) +
  scale_fill_manual(values = c("aktual" = "#888780", "s1" = "#85B7EB",
                                "s2" = "#A32D2D", "s3" = "#BA7517",
                                "s4" = "#1D9E75")) +
  scale_y_continuous(limits = c(0, max(gini_df$Gini) * 1.15)) +
  labs(title    = "Gini Coefficient Rasio SPPG/10k per Skenario",
       subtitle = "Semakin rendah = distribusi semakin merata",
       x = NULL, y = "Gini Coefficient",
       caption  = "Garis putus = kondisi aktual (S0)") +
  theme(plot.title    = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9, color = "gray40"),
        plot.caption  = element_text(size = 8, color = "gray50"))

print(p_gini_s)
ggsave("output/Rplot16_gini_skenario.png", p_gini_s,
       width = 9, height = 6, dpi = 300)
cat("✓ Rplot16_gini_skenario.png tersimpan\n")

# Plot 2: BIA per kuintil
bia_long <- bia |>
  select(Kuintil_Label, Pct_Pop, Pct_Miskin, Pct_S0, Pct_S2, Pct_S4) |>
  pivot_longer(-Kuintil_Label, names_to = "Kategori", values_to = "Persen") |>
  mutate(
    Kategori = recode(Kategori,
      "Pct_Pop"    = "% Populasi",
      "Pct_Miskin" = "% Pddk Miskin",
      "Pct_S0"     = "S0: Aktual",
      "Pct_S2"     = "S2: Pro-Miskin",
      "Pct_S4"     = "S4: Balanced"
    )
  )

p_bia <- ggplot(bia_long, aes(x = Kuintil_Label,
                               y = Persen, group = Kategori)) +
  geom_col(data = bia_long |> filter(Kategori %in%
                                       c("% Populasi","% Pddk Miskin")),
           aes(fill = Kategori), position = "dodge",
           width = 0.4, alpha = 0.4) +
  geom_line(data = bia_long |> filter(!Kategori %in%
                                        c("% Populasi","% Pddk Miskin")),
            aes(color = Kategori), linewidth = 1.2) +
  geom_point(data = bia_long |> filter(!Kategori %in%
                                         c("% Populasi","% Pddk Miskin")),
             aes(color = Kategori), size = 3.5) +
  geom_hline(yintercept = 20, linetype = "dashed",
             color = "gray40", linewidth = 0.7) +
  scale_fill_manual(values = c("% Populasi"    = "#888780",
                                "% Pddk Miskin" = "#A32D2D")) +
  scale_color_manual(values = c("S0: Aktual"     = "#888780",
                                 "S2: Pro-Miskin" = "#A32D2D",
                                 "S4: Balanced"   = "#1D9E75")) +
  labs(
    title    = "Benefit Incidence Analysis \u2014 Siapa yang Menikmati SPPG?",
    subtitle = "K1=terkaya \u2192 K5=termiskin | Garis putus = distribusi proporsional (20%)",
    x = "Kuintil Kemiskinan", y = "Persentase (%)",
    fill = NULL, color = NULL,
    caption = sprintf(
      "Concentration Index: S0=%+.4f | S2=%+.4f | S4=%+.4f",
      ci_s0, ci_s2, ci_s4
    )
  ) +
  theme(legend.position = "bottom",
        plot.title    = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9, color = "gray40"),
        plot.caption  = element_text(size = 8.5, face = "bold",
                                      color = "gray40"))

print(p_bia)
ggsave("output/Rplot17_bia_kuintil.png", p_bia,
       width = 10, height = 7, dpi = 300)
cat("✓ Rplot17_bia_kuintil.png tersimpan\n")

# Plot 3: CDI vs Rasio SPPG
label_cdi <- df |>
  filter(CDI > quantile(CDI, 0.92) | Rasio_SPPG_10k > 2.5)

p_cdi <- ggplot(df, aes(x = Rasio_SPPG_10k, y = CDI)) +
  geom_point(aes(color = Deprivasi, size = Populasi_2026 / 1e6),
             alpha = 0.65) +
  geom_smooth(method = "lm", se = TRUE, color = "gray20",
              linewidth = 0.9, formula = y ~ x) +
  geom_text_repel(data = label_cdi, aes(label = Kabupaten_Kota),
                  size = 2.5, max.overlaps = 12,
                  segment.size = 0.3, segment.color = "gray50") +
  scale_color_manual(values = c(
    "Sangat Tinggi (top 20%)"    = "#A32D2D",
    "Tinggi (60-80%)"            = "#E24B4A",
    "Menengah (40-60%)"          = "#BA7517",
    "Rendah (20-40%)"            = "#85B7EB",
    "Sangat Rendah (bottom 20%)" = "#185FA5"
  )) +
  scale_size_continuous(range = c(1, 7), name = "Populasi (juta)") +
  labs(
    title    = "Composite Deprivation Index vs Rasio SPPG",
    subtitle = "CDI tinggi + Rasio SPPG rendah = daerah paling tertinggal",
    x = "Rasio SPPG per 10.000 penduduk",
    y = "Composite Deprivation Index (CDI)",
    color = "Tingkat Deprivasi",
    caption = "CDI = gabungan kemiskinan + stunting + keterbatasan SPPG"
  ) +
  theme(legend.position = "bottom",
        plot.title    = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9, color = "gray40"),
        plot.caption  = element_text(size = 8, color = "gray50"))

print(p_cdi)
ggsave("output/Rplot18_cdi_scatter.png", p_cdi,
       width = 10, height = 8, dpi = 300)
cat("✓ Rplot18_cdi_scatter.png tersimpan\n")

# Plot 4: Gap per pulau (S4 vs Aktual)
gap_pulau <- df |>
  group_by(Pulau) |>
  summarise(Gap_total = sum(Gap_S4), .groups = "drop") |>
  arrange(Gap_total)

p_gap <- ggplot(gap_pulau,
    aes(x = reorder(Pulau, Gap_total), y = Gap_total,
        fill = Gap_total > 0)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = ifelse(Gap_total > 0,
                               paste0("+", Gap_total), Gap_total),
                hjust = ifelse(Gap_total > 0, -0.1, 1.1)),
            fontface = "bold", size = 3.8) +
  geom_hline(yintercept = 0, color = "gray30", linewidth = 0.5) +
  scale_fill_manual(values = c("FALSE" = "#A32D2D", "TRUE" = "#1D9E75")) +
  scale_y_continuous(
    limits = c(min(gap_pulau$Gap_total) * 1.2,
               max(gap_pulau$Gap_total) * 1.2)
  ) +
  coord_flip() +
  labs(title    = "Redistribusi SPPG: S4 Balanced vs Aktual (per Pulau)",
       subtitle = "Hijau = mendapat tambahan | Merah = berkurang",
       x = NULL, y = "Perubahan jumlah SPPG (dapur)",
       caption  = "S4 Balanced: populasi 20%, kemiskinan 40%, stunting 40%") +
  theme(plot.title    = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9, color = "gray40"),
        plot.caption  = element_text(size = 8, color = "gray50"))

print(p_gap)
ggsave("output/Rplot19_gap_redistribusi_pulau.png", p_gap,
       width = 9, height = 6, dpi = 300)
cat("✓ Rplot19_gap_redistribusi_pulau.png tersimpan\n")

# Plot 5: Sensitivity heatmap
p_sa <- hasil_sa |>
  mutate(
    Pop_label   = paste0("w_pop = ", w_pop * 100, "%"),
    Miskin_lab  = paste0("Kemiskinan\n", w_miskin * 100, "%"),
    Stunt_lab   = paste0("Stunting\n", w_stunting * 100, "%")
  ) |>
  ggplot(aes(x = Miskin_lab, y = Stunt_lab, fill = Gini)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(Gini, 3)), size = 2.8, fontface = "bold") +
  scale_fill_gradient(low = "#1D9E75", high = "#A32D2D", name = "Gini") +
  facet_wrap(~ Pop_label, nrow = 1) +
  labs(
    title    = "Sensitivity Analysis \u2014 Gini Coefficient per Kombinasi Bobot",
    subtitle = "Hijau = Gini rendah (lebih merata) | Merah = Gini tinggi",
    x = "Bobot Kemiskinan", y = "Bobot Stunting",
    caption  = "Setiap panel = bobot populasi berbeda | Total bobot = 100%"
  ) +
  theme(strip.text   = element_text(face = "bold", size = 9),
        axis.text.x  = element_text(angle = 10, hjust = 1),
        plot.title   = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9, color = "gray40"),
        plot.caption = element_text(size = 8, color = "gray50"))

print(p_sa)
ggsave("output/Rplot20_sensitivity_heatmap.png", p_sa,
       width = 14, height = 6, dpi = 300)
cat("✓ Rplot20_sensitivity_heatmap.png tersimpan\n")

# =============================================================================
# SIMPAN DATASET FINAL & RINGKASAN
# =============================================================================
write.csv(
  df |>
    select(No, Provinsi, Kabupaten_Kota, Pulau,
           Populasi_2026, Jumlah_SPPG, Rasio_SPPG_10k, LQ,
           Pct_Kemiskinan, Pct_Stunting,
           S0_Aktual, S1_PopMurni, S2_ProMiskin,
           S3_ProStunt, S4_Balanced,
           Gap_S2, Gap_S3, Gap_S4,
           CDI, Deprivasi) |>
    mutate(across(where(is.numeric), ~ round(.x, 4))),
  "output/Dataset_Final_Lengkap_Analisis.csv",
  row.names = FALSE
)

write.csv(hasil_sa, "output/T5_sensitivity_analysis.csv", row.names = FALSE)
write.csv(bia |> select(-sppg_ideal),
          "output/T5_bia_kuintil.csv", row.names = FALSE)

cat("✓ Dataset final & CSV tersimpan\n")

# Ringkasan akhir
cat("\n")
cat("=======================================================\n")
cat("  RINGKASAN AKHIR SEMUA TAHAP\n")
cat("=======================================================\n")
cat(sprintf("  Total SPPG              : %d dapur\n", total_sppg))
cat(sprintf("  Gini aktual (rasio/10k) : %.4f\n", gini_coef(df$R0)))
cat(sprintf("  Gini S4 Balanced        : %.4f\n", gini_coef(df$R4)))
cat(sprintf("  Penurunan Gini (S4)     : %.1f%%\n",
            (gini_coef(df$R0) - gini_coef(df$R4)) / gini_coef(df$R0) * 100))
cat(sprintf("  Gini optimal (SA)       : %.4f\n", min(hasil_sa$Gini)))
cat(sprintf("  Dapur perlu direlokasi  : ~%d (%.1f%%)\n",
            round(sum(abs(df$Gap_S4)) / 2),
            round(sum(abs(df$Gap_S4)) / 2 / total_sppg * 100, 1)))
cat(sprintf("  CI aktual               : %+.4f (pro-rich)\n", ci_s0))
cat(sprintf("  CI S4 Balanced          : %+.4f\n", ci_s4))
cat(sprintf("  Kab paling deprived     : %s\n",
            df$Kabupaten_Kota[which.max(df$CDI)]))
cat("=======================================================\n")
cat("=== TAHAP 5 SELESAI ===\n\n")
cat("Semua output tersimpan di folder output/\n")

cat("\nFile yang dihasilkan:\n")
files <- list.files("output/", full.names = FALSE)
fi    <- file.info(paste0("output/", files))
for (f in files) {
  cat(sprintf("  %-50s %s KB\n", f,
              format(round(fi[paste0("output/",f),"size"] / 1024, 1),
                     nsmall = 1)))
}
