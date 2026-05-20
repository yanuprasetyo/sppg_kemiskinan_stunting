# =============================================================================
# 00_setup.R
# Setup: Instalasi Package, Import Data, Pembersihan & Variabel Turunan
#
# Jalankan script ini PERTAMA sebelum script lainnya.
# Semua object yang dihasilkan di sini akan digunakan oleh script 01-05.
# =============================================================================

# -----------------------------------------------------------------------------
# 1. INSTALASI PACKAGE
# -----------------------------------------------------------------------------
packages_needed <- c(
  # Data wrangling & visualisasi inti
  "tidyverse", "readxl", "ggplot2", "ggcorrplot", "patchwork",
  "ggrepel", "scales", "RColorBrewer", "viridis", "corrplot",
  # Statistik inferensial & regresi
  "car", "lmtest", "sandwich", "moments", "tseries",
  # Analisis spasial
  "spdep", "spatialreg",
  # Klasterisasi
  "cluster", "factoextra",
  # Analisis ketimpangan & non-parametrik
  "quantreg", "ineq", "dunn.test",
  # Pelaporan
  "knitr", "kableExtra"
)

# Install hanya yang belum ada
new_pkgs <- packages_needed[!(packages_needed %in%
                                 installed.packages()[, "Package"])]
if (length(new_pkgs) > 0) {
  message("Menginstall ", length(new_pkgs), " package baru...")
  install.packages(new_pkgs)
}

# Load semua package
suppressPackageStartupMessages(
  invisible(lapply(packages_needed, library, character.only = TRUE))
)

cat("\n✓ Semua package berhasil di-load!\n\n")

# -----------------------------------------------------------------------------
# 2. KONFIGURASI GLOBAL
# -----------------------------------------------------------------------------
# Tema ggplot default
theme_set(theme_minimal(base_size = 11, base_family = "Arial"))

# Warna konsisten per pulau (dipakai di semua script)
WARNA_PULAU <- c(
  "Jawa"           = "#185FA5",
  "Sumatera"       = "#D85A30",
  "Kalimantan"     = "#BA7517",
  "Sulawesi"       = "#534AB7",
  "Bali & NTT"     = "#1D9E75",
  "Maluku & Papua" = "#A32D2D"
)

# Warna konsisten per klaster
WARNA_KLASTER <- c(
  "A: Aksesi Aktif"      = "#185FA5",
  "B: Bawah Rata-Rata"   = "#888780",
  "C: Krisis Ganda"      = "#A32D2D",
  "D: Dominan Terlayani" = "#1D9E75"
)

# Warna konsisten per kuadran
WARNA_KUADRAN <- c(
  "Q1: Ideal"     = "#185FA5",
  "Q2: Mismatch"  = "#BA7517",
  "Q3: Monitor"   = "#888780",
  "Q4: PRIORITAS" = "#A32D2D"
)

# Buat folder output jika belum ada
if (!dir.exists("output")) dir.create("output")

# -----------------------------------------------------------------------------
# 3. IMPORT DATA
# -----------------------------------------------------------------------------
cat("Mengimport data...\n")

# Sesuaikan path file jika perlu
DATA_PATH <- "data/Dataset_Final_SPPG_514_KabKota.xlsx"

if (!file.exists(DATA_PATH)) {
  stop("File data tidak ditemukan: ", DATA_PATH,
       "\nPastikan file ada di folder data/")
}

df_raw <- read_excel(DATA_PATH, sheet = "Data_Lengkap_514", skip = 1)

cat("✓ Data terimport:", nrow(df_raw), "baris,", ncol(df_raw), "kolom\n")

# -----------------------------------------------------------------------------
# 4. RENAME KOLOM
# -----------------------------------------------------------------------------
df <- df_raw
colnames(df) <- c(
  "No", "Provinsi", "Kabupaten_Kota",
  "Populasi_2026", "Jumlah_SPPG",
  "Jml_Penduduk_Miskin", "Pct_Kemiskinan", "Pct_Stunting",
  "Jml_SD", "Jml_SMP", "Jml_SMA", "Jml_SMK",
  "Jml_Sekolah_Total"
)

# -----------------------------------------------------------------------------
# 5. PEMBERSIHAN DATA
# -----------------------------------------------------------------------------
# Buang baris catatan/footer (NA di kolom kunci)
df <- df |>
  filter(!is.na(Provinsi), !is.na(Kabupaten_Kota), !is.na(Populasi_2026)) |>
  mutate(across(c(Populasi_2026, Jumlah_SPPG, Jml_Penduduk_Miskin,
                  Pct_Kemiskinan, Pct_Stunting,
                  Jml_SD, Jml_SMP, Jml_SMA, Jml_SMK, Jml_Sekolah_Total),
                as.numeric)) |>
  filter(!is.na(Populasi_2026), Populasi_2026 > 0)

cat("✓ Setelah pembersihan:", nrow(df), "baris\n")

# -----------------------------------------------------------------------------
# 6. VARIABEL TURUNAN
# -----------------------------------------------------------------------------
df <- df |>
  mutate(
    # Rasio utama
    Rasio_SPPG_10k = round((Jumlah_SPPG / Populasi_2026) * 10000, 4),
    Log_Pop        = log(Populasi_2026),

    # Location Quotient
    LQ = (Jumlah_SPPG / sum(Jumlah_SPPG, na.rm = TRUE)) /
         (Populasi_2026 / sum(Populasi_2026, na.rm = TRUE)),

    # Flag coverage
    Memiliki_SPPG = Jumlah_SPPG > 0
  )

# -----------------------------------------------------------------------------
# 7. MAPPING PULAU
# -----------------------------------------------------------------------------
pulau_map <- c(
  "Aceh" = "Sumatera", "Sumatera Utara" = "Sumatera",
  "Sumatera Barat" = "Sumatera", "Riau" = "Sumatera",
  "Jambi" = "Sumatera", "Sumatera Selatan" = "Sumatera",
  "Bengkulu" = "Sumatera", "Lampung" = "Sumatera",
  "Kepulauan Bangka Belitung" = "Sumatera",
  "Kepulauan Riau" = "Sumatera",
  "DKI Jakarta" = "Jawa", "Jawa Barat" = "Jawa",
  "Jawa Tengah" = "Jawa", "DI Yogyakarta" = "Jawa",
  "Jawa Timur" = "Jawa", "Banten" = "Jawa",
  "Bali" = "Bali & NTT", "Nusa Tenggara Barat" = "Bali & NTT",
  "Nusa Tenggara Timur" = "Bali & NTT",
  "Kalimantan Barat" = "Kalimantan", "Kalimantan Tengah" = "Kalimantan",
  "Kalimantan Selatan" = "Kalimantan", "Kalimantan Timur" = "Kalimantan",
  "Kalimantan Utara" = "Kalimantan",
  "Sulawesi Utara" = "Sulawesi", "Sulawesi Tengah" = "Sulawesi",
  "Sulawesi Selatan" = "Sulawesi", "Sulawesi Tenggara" = "Sulawesi",
  "Gorontalo" = "Sulawesi", "Sulawesi Barat" = "Sulawesi",
  "Maluku" = "Maluku & Papua", "Maluku Utara" = "Maluku & Papua",
  "Papua" = "Maluku & Papua", "Papua Barat" = "Maluku & Papua",
  "Papua Barat Daya" = "Maluku & Papua",
  "Papua Pegunungan" = "Maluku & Papua",
  "Papua Selatan" = "Maluku & Papua", "Papua Tengah" = "Maluku & Papua"
)

df <- df |>
  mutate(Pulau = pulau_map[Provinsi])

# -----------------------------------------------------------------------------
# 8. VALIDASI AKHIR
# -----------------------------------------------------------------------------
cat("\n=== VALIDASI DATASET ===\n")
cat(sprintf("  Baris          : %d (harus 514)\n", nrow(df)))
cat(sprintf("  Kolom          : %d\n", ncol(df)))
cat(sprintf("  Total SPPG     : %s (harus 27.427)\n",
            format(sum(df$Jumlah_SPPG), big.mark = ".")))
cat(sprintf("  Kab tanpa SPPG : %d (harus 16)\n", sum(!df$Memiliki_SPPG)))
cat(sprintf("  Provinsi unik  : %d (harus 38)\n", n_distinct(df$Provinsi)))
cat(sprintf("  Pulau NA       : %d (harus 0)\n", sum(is.na(df$Pulau))))
cat(sprintf("  Nilai kosong   : %d (harus 0)\n",
            sum(is.na(df[, c("Populasi_2026","Jumlah_SPPG",
                             "Pct_Kemiskinan","Pct_Stunting")]))))

cat("\n✓ Setup selesai! Lanjutkan dengan script 01–05.\n\n")
