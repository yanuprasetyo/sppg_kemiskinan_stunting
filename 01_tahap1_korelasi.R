# =============================================================================
# 01_tahap1_korelasi.R
# Tahap 1: Korelasi & ANOVA Antar Pulau
#
# Prasyarat: jalankan 00_setup.R terlebih dahulu.
# Output   : Rplot01 (heatmap korelasi), Rplot_boxplot, Rplot_profil_pulau
# =============================================================================

source("R/00_setup.R")

cat("=== TAHAP 1: KORELASI & ANOVA ===\n\n")

# =============================================================================
# 1A. MATRIKS KORELASI
# =============================================================================
vars_cor <- df |>
  select(Populasi_2026, Jumlah_SPPG, Rasio_SPPG_10k,
         Jml_Penduduk_Miskin, Pct_Kemiskinan, Pct_Stunting)

r_pearson  <- cor(vars_cor, method = "pearson",  use = "complete.obs")
r_spearman <- cor(vars_cor, method = "spearman", use = "complete.obs")

# Label pendek untuk plot
col_labels <- c("Populasi", "Jml SPPG", "Rasio SPPG/10k",
                "Pddk Miskin", "% Kemiskinan", "% Stunting")
colnames(r_pearson)  <- rownames(r_pearson)  <- col_labels
colnames(r_spearman) <- rownames(r_spearman) <- col_labels

cat("--- Pearson Correlation ---\n")
print(round(r_pearson, 3))

cat("\n--- Spearman Correlation ---\n")
print(round(r_spearman, 3))

# Uji signifikansi pasangan kunci
cat("\n--- Uji Signifikansi Korelasi Kunci ---\n")
pairs_key <- list(
  c("Rasio_SPPG_10k", "Pct_Kemiskinan"),
  c("Rasio_SPPG_10k", "Pct_Stunting"),
  c("Jumlah_SPPG",    "Populasi_2026"),
  c("Pct_Kemiskinan", "Pct_Stunting")
)
for (p in pairs_key) {
  ct_p <- cor.test(df[[p[1]]], df[[p[2]]], method = "pearson")
  ct_s <- cor.test(df[[p[1]]], df[[p[2]]], method = "spearman",
                   exact = FALSE)
  sig  <- ifelse(ct_p$p.value < 0.001, "***",
           ifelse(ct_p$p.value < 0.01,  "**",
           ifelse(ct_p$p.value < 0.05,  "*", "ns")))
  cat(sprintf("  %-40s Pearson r=%+.3f %s | Spearman r=%+.3f\n",
              paste0(p[1], " vs ", p[2]),
              ct_p$estimate, sig, ct_s$estimate))
}

# Plot heatmap Pearson
p_heat_p <- ggcorrplot(r_pearson,
    method = "square", type = "full",
    lab = TRUE, lab_size = 3.2,
    colors = c("#A32D2D", "white", "#185FA5"),
    outline.color = "white",
    title = "Pearson Correlation") +
  theme(plot.title = element_text(face = "bold", size = 11))

# Plot heatmap Spearman
p_heat_s <- ggcorrplot(r_spearman,
    method = "square", type = "full",
    lab = TRUE, lab_size = 3.2,
    colors = c("#A32D2D", "white", "#1D9E75"),
    outline.color = "white",
    title = "Spearman Correlation") +
  theme(plot.title = element_text(face = "bold", size = 11))

p_heatmap <- p_heat_p + p_heat_s +
  plot_annotation(
    title    = "Matriks Korelasi — Distribusi SPPG & Indikator Sosial",
    subtitle = "514 Kabupaten/Kota Indonesia, 2026",
    theme    = theme(
      plot.title    = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray40")
    )
  )

print(p_heatmap)
ggsave("output/Rplot01_heatmap_korelasi.png", p_heatmap,
       width = 14, height = 6, dpi = 300)
cat("✓ Rplot01_heatmap_korelasi.png tersimpan\n")

# =============================================================================
# 1B. ANOVA & KRUSKAL-WALLIS ANTAR PULAU
# =============================================================================
cat("\n--- Statistik Deskriptif per Pulau ---\n")
tbl_pulau <- df |>
  group_by(Pulau) |>
  summarise(
    n              = n(),
    Rasio_Mean     = round(mean(Rasio_SPPG_10k), 3),
    Rasio_Median   = round(median(Rasio_SPPG_10k), 3),
    Rasio_SD       = round(sd(Rasio_SPPG_10k), 3),
    Stunting_Mean  = round(mean(Pct_Stunting), 1),
    Miskin_Mean    = round(mean(Pct_Kemiskinan), 1),
    Total_SPPG     = sum(Jumlah_SPPG),
    Rasio_Aktual   = round(sum(Jumlah_SPPG) / sum(Populasi_2026) * 10000, 3),
    .groups        = "drop"
  ) |>
  arrange(desc(Rasio_Aktual))
print(tbl_pulau)

# Uji normalitas per pulau (Shapiro-Wilk)
cat("\n--- Shapiro-Wilk per Pulau ---\n")
df |>
  group_by(Pulau) |>
  summarise(
    n       = n(),
    W       = round(shapiro.test(Rasio_SPPG_10k)$statistic, 4),
    p_value = round(shapiro.test(Rasio_SPPG_10k)$p.value, 4),
    Normal  = ifelse(p_value > 0.05, "Ya", "TIDAK"),
    .groups = "drop"
  ) |>
  print()

# One-way ANOVA
aov_model <- aov(Rasio_SPPG_10k ~ Pulau, data = df)
cat("\n--- One-Way ANOVA ---\n")
print(summary(aov_model))

ss     <- summary(aov_model)[[1]][["Sum Sq"]]
eta_sq <- ss[1] / sum(ss)
cat(sprintf("Eta\u00B2 = %.4f \u2192 %.1f%% varians dijelaskan oleh pulau\n",
            eta_sq, eta_sq * 100))

# Kruskal-Wallis
kw <- kruskal.test(Rasio_SPPG_10k ~ Pulau, data = df)
cat(sprintf("\nKruskal-Wallis: H = %.4f, df = %d, p = %.2e %s\n",
            kw$statistic, kw$parameter, kw$p.value,
            ifelse(kw$p.value < 0.001, "***", "")))

# Post-hoc Dunn (Bonferroni)
cat("\n--- Post-hoc Dunn Test (Bonferroni) ---\n")
dunn.test(df$Rasio_SPPG_10k, df$Pulau,
          method = "bonferroni", kw = FALSE,
          alpha = 0.05, list = FALSE)

# =============================================================================
# 1C. VISUALISASI BOXPLOT & PROFIL
# =============================================================================
pulau_order <- tbl_pulau$Pulau  # urut dari tertinggi

# Boxplot per pulau
p_box <- df |>
  mutate(Pulau = factor(Pulau, levels = rev(pulau_order))) |>
  ggplot(aes(x = Pulau, y = Rasio_SPPG_10k, fill = Pulau)) +
  geom_boxplot(width = 0.6, outlier.size = 1.5,
               outlier.alpha = 0.5, outlier.color = "gray30") +
  geom_hline(yintercept = median(df$Rasio_SPPG_10k),
             linetype = "dashed", color = "gray30", linewidth = 0.8) +
  geom_hline(
    yintercept = sum(df$Jumlah_SPPG) / sum(df$Populasi_2026) * 10000,
    linetype = "dotted", color = "#A32D2D", linewidth = 0.8
  ) +
  scale_fill_manual(values = WARNA_PULAU) +
  scale_y_continuous(limits = c(0, NA)) +
  coord_flip() +
  labs(
    title    = "Distribusi Rasio SPPG per 10.000 Penduduk Antar Pulau",
    subtitle = sprintf(
      "Kruskal-Wallis H=%.2f, p<0,001*** | \u03B7\u00B2=%.3f",
      kw$statistic, eta_sq
    ),
    x = NULL, y = "Rasio SPPG per 10.000 penduduk",
    caption  = "Garis putus = median kab/kota | Garis titik merah = rasio nasional"
  ) +
  theme(legend.position = "none",
        plot.title    = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9, color = "gray40"),
        plot.caption  = element_text(size = 8, color = "gray50"),
        axis.text.y   = element_text(face = "bold"))

print(p_box)
ggsave("output/Rplot02_boxplot_pulau.png", p_box,
       width = 9, height = 6, dpi = 300)
cat("✓ Rplot02_boxplot_pulau.png tersimpan\n")

# Profil per pulau (dual axis)
p_profil <- tbl_pulau |>
  ggplot(aes(x = reorder(Pulau, Rasio_Aktual))) +
  geom_col(aes(y = Rasio_Aktual), fill = "#185FA5",
           alpha = 0.75, width = 0.6) +
  geom_point(aes(y = Stunting_Mean / 25), color = "#BA7517",
             size = 4, shape = 18) +
  geom_line(aes(y = Stunting_Mean / 25, group = 1),
            color = "#BA7517", linewidth = 1, linetype = "dashed") +
  geom_point(aes(y = Miskin_Mean / 25), color = "#A32D2D",
             size = 4, shape = 16) +
  geom_line(aes(y = Miskin_Mean / 25, group = 1),
            color = "#A32D2D", linewidth = 1, linetype = "dotted") +
  scale_y_continuous(
    name     = "Rasio SPPG per 10.000 penduduk",
    sec.axis = sec_axis(~ . * 25, name = "% Kemiskinan / Stunting")
  ) +
  coord_flip() +
  labs(
    title   = "Profil per Pulau: Rasio SPPG vs Kemiskinan & Stunting",
    x       = NULL,
    caption = "Bar biru = Rasio SPPG | \u25C6 Kuning = % Stunting | \u25CF Merah = % Kemiskinan"
  ) +
  theme(plot.title   = element_text(face = "bold", size = 12),
        plot.caption = element_text(size = 8, color = "gray50"))

print(p_profil)
ggsave("output/Rplot03_profil_pulau.png", p_profil,
       width = 9, height = 6, dpi = 300)
cat("✓ Rplot03_profil_pulau.png tersimpan\n")

cat("\n=== TAHAP 1 SELESAI ===\n\n")
