# =============================================================================
# 03_tahap3_tipologi.R
# Tahap 3: Tipologi & Klasterisasi
#   - Analisis Kuadran 2x2 (SPPG vs Stunting / Kemiskinan)
#   - K-Means Clustering (k=4) + PCA Biplot
#   - Profil klaster
#
# Prasyarat: jalankan 00_setup.R terlebih dahulu.
# Output   : plot kuadran, PCA biplot, profil klaster, CSV tipologi
# =============================================================================

source("R/00_setup.R")

cat("=== TAHAP 3: TIPOLOGI & KLASTERISASI ===\n\n")

# =============================================================================
# 3A. ANALISIS KUADRAN
# =============================================================================
# Garis batas = median nasional
med_rasio  <- median(df$Rasio_SPPG_10k)
med_stunt  <- median(df$Pct_Stunting)
med_miskin <- median(df$Pct_Kemiskinan)

cat(sprintf("Garis batas kuadran:\n"))
cat(sprintf("  Median Rasio SPPG/10k : %.3f\n", med_rasio))
cat(sprintf("  Median %% Stunting     : %.3f\n", med_stunt))
cat(sprintf("  Median %% Kemiskinan   : %.3f\n", med_miskin))

# Klasifikasi kuadran SPPG vs Stunting & vs Kemiskinan
df <- df |>
  mutate(
    Kuadran_Stunting = case_when(
      Rasio_SPPG_10k >= med_rasio & Pct_Stunting <  med_stunt ~ "Q1: Ideal",
      Rasio_SPPG_10k >= med_rasio & Pct_Stunting >= med_stunt ~ "Q2: Mismatch",
      Rasio_SPPG_10k <  med_rasio & Pct_Stunting <  med_stunt ~ "Q3: Monitor",
      Rasio_SPPG_10k <  med_rasio & Pct_Stunting >= med_stunt ~ "Q4: PRIORITAS"
    ),
    Kuadran_Miskin = case_when(
      Rasio_SPPG_10k >= med_rasio & Pct_Kemiskinan <  med_miskin ~ "Q1: Ideal",
      Rasio_SPPG_10k >= med_rasio & Pct_Kemiskinan >= med_miskin ~ "Q2: Mismatch",
      Rasio_SPPG_10k <  med_rasio & Pct_Kemiskinan <  med_miskin ~ "Q3: Monitor",
      Rasio_SPPG_10k <  med_rasio & Pct_Kemiskinan >= med_miskin ~ "Q4: PRIORITAS"
    )
  )

cat("\n--- Distribusi Kuadran SPPG vs Stunting ---\n")
q_stunt_sum <- df |>
  group_by(Kuadran_Stunting) |>
  summarise(
    n               = n(),
    Pop_juta        = round(sum(Populasi_2026) / 1e6, 1),
    Rasio_mean      = round(mean(Rasio_SPPG_10k), 3),
    Stunting_mean   = round(mean(Pct_Stunting), 1),
    Miskin_mean     = round(mean(Pct_Kemiskinan), 1),
    .groups         = "drop"
  )
print(q_stunt_sum)

cat("\n--- 15 Prioritas Utama (Q4: SPPG rendah + stunting tinggi) ---\n")
df |>
  filter(Kuadran_Stunting == "Q4: PRIORITAS") |>
  arrange(Rasio_SPPG_10k, desc(Pct_Stunting)) |>
  select(Provinsi, Kabupaten_Kota, Rasio_SPPG_10k,
         Pct_Stunting, Pct_Kemiskinan, Populasi_2026) |>
  head(15) |>
  print()

# Helper: plot kuadran
plot_kuadran <- function(df, y_var, y_lab, med_y, y_max, n_quad,
                          quad_col, judul, sub) {
  label_pts <- df |>
    mutate(.y = .data[[y_var]]) |>
    filter(
      (get(quad_col) == "Q4: PRIORITAS" &
         (Rasio_SPPG_10k < 0.2 | .y > quantile(.y, 0.92))) |
        (get(quad_col) == "Q1: Ideal" & Rasio_SPPG_10k > 2.5) |
        (get(quad_col) == "Q2: Mismatch" & Rasio_SPPG_10k > 2.0 &
           .y > quantile(.y, 0.85))
    ) |>
    slice_max(Rasio_SPPG_10k + .y, n = 15)

  df |>
    mutate(.y = .data[[y_var]], .quad = .data[[quad_col]]) |>
    ggplot(aes(x = Rasio_SPPG_10k, y = .y)) +
    # Shading
    annotate("rect", xmin = med_rasio, xmax = Inf,
             ymin = -Inf, ymax = med_y,
             fill = "#185FA5", alpha = 0.04) +
    annotate("rect", xmin = med_rasio, xmax = Inf,
             ymin = med_y, ymax = Inf,
             fill = "#BA7517", alpha = 0.04) +
    annotate("rect", xmin = -Inf, xmax = med_rasio,
             ymin = -Inf, ymax = med_y,
             fill = "#888780", alpha = 0.04) +
    annotate("rect", xmin = -Inf, xmax = med_rasio,
             ymin = med_y, ymax = Inf,
             fill = "#A32D2D", alpha = 0.06) +
    geom_point(aes(color = .quad, size = Populasi_2026 / 1e6),
               alpha = 0.65) +
    geom_text_repel(
      data  = label_pts |> mutate(.y = .data[[y_var]], .quad = .data[[quad_col]]),
      aes(label = Kabupaten_Kota), size = 2.5,
      max.overlaps = 15, segment.color = "gray50", segment.size = 0.3
    ) +
    geom_vline(xintercept = med_rasio, linetype = "dashed",
               color = "gray30", linewidth = 0.8) +
    geom_hline(yintercept = med_y, linetype = "dashed",
               color = "gray30", linewidth = 0.8) +
    scale_color_manual(values = WARNA_KUADRAN) +
    scale_size_continuous(range = c(1, 8), name = "Populasi (juta)") +
    labs(title    = judul,
         subtitle = sub,
         x = "Rasio SPPG per 10.000 penduduk",
         y = y_lab, color = NULL,
         caption = "Ukuran titik = besarnya populasi kab/kota") +
    theme(legend.position = "bottom",
          plot.title    = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(size = 9, color = "gray40"),
          plot.caption  = element_text(size = 8, color = "gray50"))
}

# Plot kuadran SPPG vs Stunting
p_quad_s <- plot_kuadran(
  df, "Pct_Stunting", "Prevalensi Stunting Balita (%)",
  med_stunt, max(df$Pct_Stunting) + 2,
  q_stunt_sum, "Kuadran_Stunting",
  "Analisis Kuadran: Rasio SPPG vs Prevalensi Stunting",
  sprintf("Garis batas: median rasio = %.3f | median stunting = %.1f%%",
          med_rasio, med_stunt)
)
print(p_quad_s)
ggsave("output/Rplot07_kuadran_stunting.png", p_quad_s,
       width = 10, height = 8, dpi = 300)
cat("✓ Rplot07_kuadran_stunting.png tersimpan\n")

# Plot kuadran SPPG vs Kemiskinan
p_quad_m <- plot_kuadran(
  df, "Pct_Kemiskinan", "Persentase Penduduk Miskin (%)",
  med_miskin, max(df$Pct_Kemiskinan) + 2,
  q_stunt_sum, "Kuadran_Miskin",
  "Analisis Kuadran: Rasio SPPG vs Tingkat Kemiskinan",
  sprintf("Garis batas: median rasio = %.3f | median kemiskinan = %.1f%%",
          med_rasio, med_miskin)
)
print(p_quad_m)
ggsave("output/Rplot08_kuadran_kemiskinan.png", p_quad_m,
       width = 10, height = 8, dpi = 300)
cat("✓ Rplot08_kuadran_kemiskinan.png tersimpan\n")

# =============================================================================
# 3B. K-MEANS CLUSTERING
# =============================================================================
set.seed(42)

# Matrix fitur (dinormalisasi)
X_clust <- df |>
  transmute(
    Rasio_SPPG_10k,
    Pct_Kemiskinan,
    Pct_Stunting,
    Log_Pop = log(Populasi_2026)
  ) |>
  scale()

cat("\n--- Mencari k optimal ---\n")

# Elbow method
p_elbow <- fviz_nbclust(
  X_clust, kmeans, method = "wss", k.max = 8,
  nstart = 25, linecolor = "#185FA5"
) +
  geom_vline(xintercept = 4, linetype = "dashed",
             color = "#A32D2D", linewidth = 0.8) +
  labs(title = "Elbow Method — k Optimal",
       subtitle = "Garis merah = k=4") +
  theme(plot.title = element_text(face = "bold", size = 12))

# Silhouette method
p_sil <- fviz_nbclust(
  X_clust, kmeans, method = "silhouette", k.max = 8,
  nstart = 25, linecolor = "#1D9E75"
) +
  labs(title    = "Silhouette Method",
       subtitle = "Nilai lebih tinggi = klaster lebih kompak") +
  theme(plot.title = element_text(face = "bold", size = 12))

p_kopt <- p_elbow + p_sil +
  plot_annotation(title = "Penentuan Jumlah Klaster Optimal (k)",
                  theme = theme(plot.title = element_text(size = 13,
                                                           face = "bold")))
print(p_kopt)
ggsave("output/Rplot09_k_optimal.png", p_kopt,
       width = 12, height = 5, dpi = 300)

# Silhouette scores numerik
cat("\nSilhouette score per k:\n")
sil_scores <- sapply(2:8, function(k) {
  set.seed(42)
  km  <- kmeans(X_clust, centers = k, nstart = 25, iter.max = 500)
  sil <- silhouette(km$cluster, dist(X_clust))
  mean(sil[, 3])
})
print(tibble(k = 2:8, Silhouette = round(sil_scores, 4)) |>
        mutate(Terbaik = ifelse(Silhouette == max(Silhouette), "<- optimal", "")))

# --- Final clustering k=4 ---
set.seed(42)
km4 <- kmeans(X_clust, centers = 4, nstart = 50, iter.max = 1000)
df$Cluster_raw <- km4$cluster

# Profil tiap klaster untuk menentukan nama
profil_raw <- df |>
  group_by(Cluster_raw) |>
  summarise(
    n             = n(),
    Rasio_mean    = mean(Rasio_SPPG_10k),
    Stunting_mean = mean(Pct_Stunting),
    Miskin_mean   = mean(Pct_Kemiskinan),
    Pop_mean_ribu = mean(Populasi_2026) / 1000,
    Total_SPPG    = sum(Jumlah_SPPG),
    .groups       = "drop"
  ) |>
  arrange(desc(Rasio_mean))

cat("\nProfil klaster raw (untuk menentukan nama):\n")
print(profil_raw |> mutate(across(where(is.numeric), round, 2)))

# Beri nama klaster berdasarkan profil:
#   Rasio tertinggi + stunting menengah -> A: Aksesi Aktif
#   Miskin & stunting tertinggi         -> C: Krisis Ganda
#   Populasi besar + rasio bagus        -> D: Dominan Terlayani
#   Sisanya                             -> B: Bawah Rata-Rata
nama_klaster <- profil_raw |>
  mutate(
    Tipe = case_when(
      Rasio_mean == max(Rasio_mean)    ~ "A: Aksesi Aktif",
      Miskin_mean == max(Miskin_mean)  ~ "C: Krisis Ganda",
      Pop_mean_ribu == max(Pop_mean_ribu) ~ "D: Dominan Terlayani",
      TRUE                             ~ "B: Bawah Rata-Rata"
    )
  ) |>
  select(Cluster_raw, Tipe)

df <- df |>
  left_join(nama_klaster, by = "Cluster_raw") |>
  rename(Cluster = Tipe)

cat("\nDistribusi klaster:\n")
print(table(df$Cluster))

# Silhouette score final
sil_final <- silhouette(km4$cluster, dist(X_clust))
cat(sprintf("\nSilhouette score (k=4): %.4f\n", mean(sil_final[, 3])))

# --- Profil final ---
cat("\n--- Profil Klaster Final ---\n")
profil_final <- df |>
  group_by(Cluster) |>
  summarise(
    n              = n(),
    Rasio_mean     = round(mean(Rasio_SPPG_10k), 3),
    Rasio_median   = round(median(Rasio_SPPG_10k), 3),
    Stunting_mean  = round(mean(Pct_Stunting), 1),
    Miskin_mean    = round(mean(Pct_Kemiskinan), 1),
    Pop_mean_ribu  = round(mean(Populasi_2026) / 1000, 0),
    Total_SPPG     = sum(Jumlah_SPPG),
    Pct_SPPG       = round(sum(Jumlah_SPPG) / sum(df$Jumlah_SPPG) * 100, 1),
    Pulau_dominan  = names(sort(table(Pulau), decreasing = TRUE))[1],
    .groups        = "drop"
  ) |>
  arrange(Rasio_mean)
print(profil_final)

# =============================================================================
# 3C. VISUALISASI KLASTER
# =============================================================================

# PCA untuk scatter 2D
pca_res <- prcomp(X_clust, scale. = FALSE)
pve     <- summary(pca_res)$importance[2, ]
cat(sprintf("\nPCA variance explained: PC1=%.1f%%, PC2=%.1f%%\n",
            pve[1] * 100, pve[2] * 100))

df_pca <- data.frame(
  PC1     = pca_res$x[, 1],
  PC2     = pca_res$x[, 2],
  Cluster = df$Cluster,
  Kab     = df$Kabupaten_Kota,
  Rasio   = df$Rasio_SPPG_10k,
  Stunting = df$Pct_Stunting
)

label_pca <- df_pca |>
  filter(abs(PC1) + abs(PC2) > 4) |>
  slice_max(abs(PC1) + abs(PC2), n = 8)

p_pca <- ggplot(df_pca, aes(PC1, PC2, color = Cluster)) +
  stat_ellipse(aes(fill = Cluster), geom = "polygon",
               alpha = 0.08, level = 0.85, linewidth = 0.5) +
  geom_point(alpha = 0.65, size = 2) +
  geom_text_repel(data = label_pca, aes(label = Kab),
                  size = 2.5, max.overlaps = 10,
                  segment.color = "gray50", segment.size = 0.3) +
  scale_color_manual(values = WARNA_KLASTER) +
  scale_fill_manual(values  = WARNA_KLASTER) +
  labs(
    title    = "K-Means Clustering (k=4) \u2014 Proyeksi PCA",
    subtitle = sprintf("PC1=%.1f%% | PC2=%.1f%% | Silhouette=%.3f",
                       pve[1] * 100, pve[2] * 100, mean(sil_final[, 3])),
    x = sprintf("PC1 (%.1f%% variance)", pve[1] * 100),
    y = sprintf("PC2 (%.1f%% variance)", pve[2] * 100),
    color = "Tipe Klaster", fill = "Tipe Klaster"
  ) +
  theme(legend.position = "bottom",
        plot.title    = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9, color = "gray40"))

print(p_pca)
ggsave("output/Rplot10_kmeans_pca.png", p_pca,
       width = 10, height = 7, dpi = 300)
cat("✓ Rplot10_kmeans_pca.png tersimpan\n")

# Klaster pada ruang kuadran SPPG vs Stunting
p_quad_cluster <- ggplot(df,
    aes(x = Rasio_SPPG_10k, y = Pct_Stunting,
        color = Cluster, size = Populasi_2026 / 1e6)) +
  geom_point(alpha = 0.65) +
  geom_vline(xintercept = med_rasio, linetype = "dashed",
             color = "gray30", linewidth = 0.7) +
  geom_hline(yintercept = med_stunt, linetype = "dashed",
             color = "gray30", linewidth = 0.7) +
  scale_color_manual(values = WARNA_KLASTER) +
  scale_size_continuous(range = c(1, 8), guide = "none") +
  annotate("label", x = max(df$Rasio_SPPG_10k) * 0.85,
           y = 10, label = "Q1: Ideal",
           fill = "#E6F1FB", color = "#185FA5",
           size = 3, fontface = "bold") +
  annotate("label", x = 0.1, y = max(df$Pct_Stunting) - 2,
           label = "Q4: PRIORITAS",
           fill = "#FCEBEB", color = "#A32D2D",
           size = 3, fontface = "bold") +
  labs(
    title    = "Klaster K-Means pada Ruang Kuadran SPPG vs Stunting",
    subtitle = "Warna = tipe klaster | Ukuran = populasi",
    x = "Rasio SPPG per 10.000 penduduk",
    y = "Prevalensi Stunting Balita (%)",
    color = "Tipe Klaster"
  ) +
  theme(legend.position = "bottom",
        plot.title    = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9, color = "gray40"))

print(p_quad_cluster)
ggsave("output/Rplot11_kuadran_klaster.png", p_quad_cluster,
       width = 10, height = 8, dpi = 300)
cat("✓ Rplot11_kuadran_klaster.png tersimpan\n")

# Profil chart bar + line
profil_long <- profil_final |>
  select(Cluster, `Rasio SPPG/10k` = Rasio_mean,
         `% Stunting` = Stunting_mean, `% Kemiskinan` = Miskin_mean) |>
  pivot_longer(-Cluster, names_to = "Indikator", values_to = "Nilai")

p_profil <- ggplot(profil_long,
    aes(x = Cluster, y = Nilai, fill = Cluster)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = round(Nilai, 1)),
            vjust = -0.4, size = 3.5, fontface = "bold") +
  facet_wrap(~ Indikator, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = WARNA_KLASTER) +
  labs(
    title    = "Profil 4 Klaster \u2014 Perbandingan Indikator Kunci",
    subtitle = "Tipe C (Krisis Ganda) punya stunting & kemiskinan tertinggi tapi SPPG terendah",
    x = NULL, y = NULL
  ) +
  theme(strip.text  = element_text(face = "bold", size = 10),
        axis.text.x = element_text(angle = 15, hjust = 1, size = 8),
        plot.title    = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9, color = "gray40"))

print(p_profil)
ggsave("output/Rplot12_profil_klaster.png", p_profil,
       width = 12, height = 6, dpi = 300)
cat("✓ Rplot12_profil_klaster.png tersimpan\n")

# =============================================================================
# SIMPAN OUTPUT
# =============================================================================
write.csv(
  df |>
    select(No, Provinsi, Kabupaten_Kota, Pulau,
           Populasi_2026, Jumlah_SPPG, Rasio_SPPG_10k, LQ,
           Pct_Kemiskinan, Pct_Stunting,
           Kuadran_Stunting, Kuadran_Miskin, Cluster) |>
    mutate(across(c(Rasio_SPPG_10k, LQ), round, 4)),
  "output/T3_tipologi_514_KabKota.csv",
  row.names = FALSE
)
cat("✓ T3_tipologi_514_KabKota.csv tersimpan\n")

cat("=== TAHAP 3 SELESAI ===\n\n")
