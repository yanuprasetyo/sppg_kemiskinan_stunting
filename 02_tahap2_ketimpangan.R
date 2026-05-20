# =============================================================================
# 02_tahap2_ketimpangan.R
# Tahap 2: Distribusi & Ketimpangan
#   - Gini Coefficient
#   - Lorenz Curve
#   - Theil T Decomposition
#   - Location Quotient (LQ)
#   - Hoover Index
#
# Prasyarat: jalankan 00_setup.R terlebih dahulu.
# =============================================================================

source("R/00_setup.R")

cat("=== TAHAP 2: DISTRIBUSI & KETIMPANGAN ===\n\n")

# =============================================================================
# FUNGSI HELPER
# =============================================================================

#' Hitung Gini Coefficient
gini_coef <- function(x) {
  x <- sort(x[x > 0 & !is.na(x)])
  n <- length(x)
  (2 * sum(seq_len(n) * x) - (n + 1) * sum(x)) / (n * sum(x))
}

#' Hitung Theil T Index (bobot populasi opsional)
theil_t <- function(x, pop = NULL) {
  if (is.null(pop)) pop <- rep(1, length(x))
  valid  <- x > 0 & pop > 0 & !is.na(x) & !is.na(pop)
  x      <- x[valid]; pop <- pop[valid]
  s_pop  <- pop / sum(pop)
  s_x    <- (x * pop) / sum(x * pop)
  sum(s_x * log(s_x / s_pop))
}

#' Hitung Hoover Index
hoover_index <- function(x, y) {
  x_share <- x / sum(x, na.rm = TRUE)
  y_share <- y / sum(y, na.rm = TRUE)
  0.5 * sum(abs(x_share - y_share), na.rm = TRUE)
}

# =============================================================================
# 2A. GINI COEFFICIENT
# =============================================================================
cat("--- Gini Coefficients ---\n")

gini_results <- tibble(
  Variabel = c(
    "Jumlah SPPG (absolut)", "Rasio SPPG per 10k",
    "Populasi 2026", "Penduduk Miskin (absolut)",
    "% Kemiskinan", "% Stunting"
  ),
  Gini = c(
    gini_coef(df$Jumlah_SPPG),
    gini_coef(df$Rasio_SPPG_10k),
    gini_coef(df$Populasi_2026),
    gini_coef(df$Jml_Penduduk_Miskin),
    gini_coef(df$Pct_Kemiskinan),
    gini_coef(df$Pct_Stunting)
  )
) |>
  mutate(
    Gini         = round(Gini, 4),
    Interpretasi = case_when(
      Gini < 0.2 ~ "Merata",
      Gini < 0.3 ~ "Cukup merata",
      Gini < 0.4 ~ "Ketimpangan sedang",
      Gini < 0.5 ~ "Ketimpangan tinggi",
      TRUE        ~ "Ketimpangan sangat tinggi"
    )
  )

print(gini_results)

# Gini per pulau
cat("\nGini Rasio SPPG/10k per Pulau:\n")
df |>
  group_by(Pulau) |>
  summarise(
    n          = n(),
    Gini_Rasio = round(gini_coef(Rasio_SPPG_10k), 4),
    .groups    = "drop"
  ) |>
  arrange(desc(Gini_Rasio)) |>
  print()

# =============================================================================
# 2B. LORENZ CURVE
# =============================================================================
cat("\n--- Lorenz Curve ---\n")

# SPPG vs Populasi (sort populasi ascending)
df_lc1 <- df |>
  arrange(Populasi_2026) |>
  mutate(
    cum_pop  = cumsum(Populasi_2026) / sum(Populasi_2026),
    cum_sppg = cumsum(Jumlah_SPPG)  / sum(Jumlah_SPPG)
  )
lc1 <- bind_rows(tibble(cum_pop = 0, cum_sppg = 0),
                  select(df_lc1, cum_pop, cum_sppg))

# SPPG vs Kemiskinan (paling miskin pertama)
df_lc2 <- df |>
  arrange(desc(Pct_Kemiskinan)) |>
  mutate(
    cum_mis  = cumsum(Jml_Penduduk_Miskin) / sum(Jml_Penduduk_Miskin),
    cum_sppg = cumsum(Jumlah_SPPG)         / sum(Jumlah_SPPG)
  )
lc2 <- bind_rows(tibble(cum_mis = 0, cum_sppg = 0),
                  select(df_lc2, cum_mis, cum_sppg))

# Hitung Gini dari Lorenz
gini_lorenz <- function(x, y) {
  1 - 2 * sum(diff(x) * (head(y, -1) + tail(y, -1))) / 2
}
g1 <- gini_lorenz(lc1$cum_pop, lc1$cum_sppg)
g2 <- gini_lorenz(lc2$cum_mis, lc2$cum_sppg)
cat(sprintf("  Gini SPPG vs Populasi  : %.4f\n", g1))
cat(sprintf("  Gini SPPG vs Kemiskinan: %.4f\n", g2))

# Plot Lorenz
p_lc1 <- ggplot(lc1, aes(cum_pop, cum_sppg)) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_ribbon(aes(ymin = cum_sppg, ymax = cum_pop),
              fill = "#185FA5", alpha = 0.15) +
  geom_line(color = "#185FA5", linewidth = 1.3) +
  annotate("text", x = 0.25, y = 0.55,
           label = sprintf("Gini = %.3f", g1),
           color = "#185FA5", fontface = "bold", size = 4) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  labs(title    = "Lorenz Curve: SPPG vs Populasi",
       subtitle = "Area biru = ketimpangan distribusi",
       x = "Kumulatif kab/kota (% populasi)",
       y = "Kumulatif SPPG (%)") +
  theme(plot.title    = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 9, color = "gray40"))

p_lc2 <- ggplot(lc2, aes(cum_mis, cum_sppg)) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_ribbon(aes(ymin = cum_sppg, ymax = cum_mis),
              fill = "#A32D2D", alpha = 0.15) +
  geom_line(color = "#A32D2D", linewidth = 1.3) +
  annotate("text", x = 0.2, y = 0.65,
           label = sprintf("Gini = %.3f\n(lebih timpang)", g2),
           color = "#A32D2D", fontface = "bold", size = 3.8) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  labs(title    = "Lorenz Curve: SPPG vs Penduduk Miskin",
       subtitle = "Diurutkan dari kab/kota paling miskin",
       x = "Kumulatif penduduk miskin (%)",
       y = "Kumulatif SPPG (%)") +
  theme(plot.title    = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 9, color = "gray40"))

p_lorenz <- p_lc1 + p_lc2 +
  plot_annotation(
    title    = "Lorenz Curve \u2014 Distribusi SPPG",
    subtitle = "Kurva mendekati diagonal = merata | Menjauh = timpang",
    theme    = theme(plot.title    = element_text(size = 13, face = "bold"),
                     plot.subtitle = element_text(size = 10, color = "gray40"))
  )

print(p_lorenz)
ggsave("output/Rplot04_lorenz_curve.png", p_lorenz,
       width = 12, height = 6, dpi = 300)
cat("✓ Rplot04_lorenz_curve.png tersimpan\n")

# =============================================================================
# 2C. THEIL T DECOMPOSITION
# =============================================================================
cat("\n--- Theil T Decomposition ---\n")

T_total   <- theil_t(df$Rasio_SPPG_10k, df$Populasi_2026)

# Between-pulau
prov_agg <- df |>
  group_by(Pulau) |>
  summarise(sppg = sum(Jumlah_SPPG), pop = sum(Populasi_2026),
            rasio = sppg / pop * 10000, .groups = "drop")
T_between <- theil_t(prov_agg$rasio, prov_agg$pop)

# Within-pulau
within_df <- df |>
  group_by(Pulau) |>
  summarise(
    T_within  = theil_t(Rasio_SPPG_10k, Populasi_2026),
    pop_share = sum(Populasi_2026) / sum(df$Populasi_2026),
    .groups   = "drop"
  ) |>
  mutate(
    Kontribusi = T_within * pop_share,
    Pct_Total  = round(Kontribusi / T_total * 100, 1)
  )
T_within <- sum(within_df$Kontribusi)

cat(sprintf("  Total Theil T  : %.4f\n", T_total))
cat(sprintf("  Between-pulau  : %.4f (%.1f%%)\n",
            T_between, T_between / T_total * 100))
cat(sprintf("  Within-pulau   : %.4f (%.1f%%)\n",
            T_within, T_within / T_total * 100))
cat("\nKontribusi per pulau:\n")
print(within_df |> arrange(desc(Kontribusi)) |>
        mutate(across(c(T_within, pop_share, Kontribusi), round, 4)))

# Plot Theil decomposition
p_theil <- ggplot(within_df |> mutate(Pulau = reorder(Pulau, Kontribusi)),
    aes(x = Pulau, y = Kontribusi, fill = Pulau)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", Pct_Total)),
            hjust = -0.15, size = 3.5) +
  scale_fill_manual(values = WARNA_PULAU) +
  scale_y_continuous(limits = c(0, max(within_df$Kontribusi) * 1.25)) +
  coord_flip() +
  labs(
    title    = "Dekomposisi Theil T \u2014 Kontribusi Ketimpangan per Pulau",
    subtitle = sprintf("Total Theil = %.4f | Antar-pulau = %.1f%% | Dalam-pulau = %.1f%%",
                       T_total, T_between / T_total * 100,
                       T_within / T_total * 100),
    x = NULL, y = "Kontribusi terhadap Total Theil T",
    caption  = "Semakin panjang bar = semakin besar kontribusi ketimpangan dalam-pulau"
  ) +
  theme(plot.title    = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9, color = "gray40"),
        plot.caption  = element_text(size = 8, color = "gray50"))

print(p_theil)
ggsave("output/Rplot05_theil_decomposition.png", p_theil,
       width = 9, height = 6, dpi = 300)
cat("✓ Rplot05_theil_decomposition.png tersimpan\n")

# =============================================================================
# 2D. LOCATION QUOTIENT (LQ)
# =============================================================================
cat("\n--- Location Quotient ---\n")

cat(sprintf("  LQ > 1,5 (over-served)  : %d kab/kota\n", sum(df$LQ > 1.5)))
cat(sprintf("  LQ 1,0-1,5              : %d kab/kota\n",
            sum(df$LQ >= 1 & df$LQ <= 1.5)))
cat(sprintf("  LQ 0,5-1,0              : %d kab/kota\n",
            sum(df$LQ >= 0.5 & df$LQ < 1.0)))
cat(sprintf("  LQ < 0,5 (under-served) : %d kab/kota\n", sum(df$LQ < 0.5)))
cat(sprintf("  LQ = 0 (tanpa SPPG)     : %d kab/kota\n",
            sum(df$Jumlah_SPPG == 0)))

cat("\nTop 10 OVER-served:\n")
df |> filter(Jumlah_SPPG > 0) |>
  slice_max(LQ, n = 10) |>
  select(Provinsi, Kabupaten_Kota, LQ, Rasio_SPPG_10k,
         Pct_Kemiskinan, Pct_Stunting) |>
  mutate(across(c(LQ, Rasio_SPPG_10k), round, 3)) |>
  print()

cat("\nTop 10 UNDER-served (ada SPPG):\n")
df |> filter(Jumlah_SPPG > 0) |>
  slice_min(LQ, n = 10) |>
  select(Provinsi, Kabupaten_Kota, LQ, Rasio_SPPG_10k,
         Pct_Kemiskinan, Pct_Stunting) |>
  mutate(across(c(LQ, Rasio_SPPG_10k), round, 3)) |>
  print()

# Plot histogram LQ
p_lq <- ggplot(df, aes(x = LQ)) +
  geom_histogram(
    breaks = c(0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 2.0, 5.0),
    aes(fill = after_stat(x) < 1),
    color = "white", linewidth = 0.4
  ) +
  geom_vline(xintercept = 1, linetype = "dashed",
             color = "gray20", linewidth = 1) +
  scale_fill_manual(
    values = c("TRUE" = "#E24B4A", "FALSE" = "#1D9E75"),
    labels = c("Over-served (LQ \u2265 1)", "Under-served (LQ < 1)")
  ) +
  annotate("text", x = 1.05, y = Inf,
           label = "LQ = 1\n(proporsional)",
           vjust = 1.5, size = 3, color = "gray20", hjust = 0) +
  scale_x_continuous(breaks = c(0, 0.5, 1, 1.5, 2, 3, 4, 5)) +
  labs(
    title    = "Distribusi Location Quotient (LQ) \u2014 514 Kab/Kota",
    subtitle = sprintf("%d under-served (LQ<1) | %d over-served (LQ>1) | %d tanpa SPPG",
                       sum(df$LQ < 1 & df$Jumlah_SPPG > 0),
                       sum(df$LQ > 1), sum(df$Jumlah_SPPG == 0)),
    x = "Location Quotient (LQ)", y = "Jumlah kab/kota", fill = NULL
  ) +
  theme(legend.position = "bottom",
        plot.title    = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9, color = "gray40"))

print(p_lq)
ggsave("output/Rplot06_lq_histogram.png", p_lq,
       width = 9, height = 6, dpi = 300)
cat("✓ Rplot06_lq_histogram.png tersimpan\n")

# =============================================================================
# 2E. HOOVER INDEX & RINGKASAN
# =============================================================================
cat("\n--- Hoover Index ---\n")

h_pop     <- hoover_index(df$Jumlah_SPPG, df$Populasi_2026)
h_miskin  <- hoover_index(df$Jumlah_SPPG, df$Jml_Penduduk_Miskin)
h_stunting <- hoover_index(df$Jumlah_SPPG,
                            df$Pct_Stunting * df$Populasi_2026 / 100)
tot <- sum(df$Jumlah_SPPG)

cat(sprintf("  vs Populasi   : %.4f \u2192 %.1f%% SPPG perlu direlokasi (~%d dapur)\n",
            h_pop,     h_pop * 100,     round(h_pop * tot)))
cat(sprintf("  vs Kemiskinan : %.4f \u2192 %.1f%% SPPG perlu direlokasi (~%d dapur)\n",
            h_miskin,  h_miskin * 100,  round(h_miskin * tot)))
cat(sprintf("  vs Stunting   : %.4f \u2192 %.1f%% SPPG perlu direlokasi (~%d dapur)\n",
            h_stunting, h_stunting * 100, round(h_stunting * tot)))

# Ringkasan semua indeks ke CSV
ringkasan_indeks <- tibble(
  Metrik       = c("Gini SPPG absolut", "Gini Rasio SPPG/10k",
                   "Gini % Kemiskinan", "Gini % Stunting",
                   "Theil T total", "Theil antar-pulau (%)",
                   "Theil dalam-pulau (%)",
                   "Hoover vs Populasi (%)", "Hoover vs Kemiskinan (%)",
                   "Hoover vs Stunting (%)",
                   "LQ < 0.5 (under-served)", "LQ > 1.5 (over-served)"),
  Nilai        = c(
    round(gini_coef(df$Jumlah_SPPG), 4),
    round(gini_coef(df$Rasio_SPPG_10k), 4),
    round(gini_coef(df$Pct_Kemiskinan), 4),
    round(gini_coef(df$Pct_Stunting), 4),
    round(T_total, 4),
    round(T_between / T_total * 100, 1),
    round(T_within  / T_total * 100, 1),
    round(h_pop * 100, 1), round(h_miskin * 100, 1),
    round(h_stunting * 100, 1),
    sum(df$LQ < 0.5), sum(df$LQ > 1.5)
  )
)

write.csv(ringkasan_indeks, "output/T2_indeks_ketimpangan.csv",
          row.names = FALSE)
write.csv(df |> select(Provinsi, Kabupaten_Kota, Pulau, Jumlah_SPPG,
                        Rasio_SPPG_10k, LQ) |>
            mutate(across(c(Rasio_SPPG_10k, LQ), round, 4)),
          "output/T2_location_quotient_514.csv", row.names = FALSE)

cat("\n✓ CSV tersimpan di folder output/\n")
cat("=== TAHAP 2 SELESAI ===\n\n")
