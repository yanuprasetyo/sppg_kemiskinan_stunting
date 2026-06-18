# 🍽️ Distribusi & Ketimpangan Dapur SPPG Program MBG
## Analisis di 514 Kabupaten/Kota Indonesia — Mei 2026

[![R Version](https://img.shields.io/badge/R-4.6.0+-blue?logo=r)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Status](https://img.shields.io/badge/Status-Complete-brightgreen)]()
[![Data](https://img.shields.io/badge/Data-BGN%20%7C%20BPS%20%7C%20Kemenkes-orange)]()

---

## 📋 Tentang Proyek Ini

Repositori ini berisi seluruh kode analisis statistik untuk kajian:

> **"Distribusi dan Ketimpangan Dapur SPPG Program Makan Bergizi Gratis (MBG) terhadap Populasi, Sekolah, Kemiskinan, dan Stunting: Analisis Komprehensif 514 Kabupaten/Kota Indonesia"**

Program Makan Bergizi Gratis (MBG) adalah program prioritas nasional yang menyediakan makanan bergizi bagi anak sekolah, balita, ibu hamil, dan ibu menyusui melalui jaringan **Satuan Pelayanan Pemenuhan Gizi (SPPG)**. Per 30 April 2026, telah beroperasi **27.427 dapur SPPG** di seluruh Indonesia.

Kajian ini menjawab satu pertanyaan mendasar: **apakah 27.427 dapur SPPG tersebut terdistribusi secara adil kepada yang paling membutuhkan?**

---

## 🔑 Temuan Utama

| # | Temuan | Angka Kunci |
|---|--------|-------------|
| 1 | Distribusi SPPG mengikuti **populasi, bukan kebutuhan** | Korelasi SPPG–Populasi r = **0,931** vs SPPG–Kemiskinan r = **−0,321** |
| 2 | **80,4% ketimpangan terjadi di dalam pulau**, bukan antar pulau | Theil T decomposition: within = 80,4% |
| 3 | **61 kab/kota Klaster Krisis Ganda** hanya dapat 1,3% total SPPG | Stunting 34,9%, miskin 25,6%, SPPG = 354 dapur |
| 4 | Distribusi SPPG bersifat **pro-rich**, bukan pro-poor | Concentration Index = **+0,120** |
| 5 | **Formula optimal**: kemiskinan 40%, stunting 40%, populasi 20% | Menurunkan Gini **8,5%** tanpa tambah dapur |

---

## 📁 Struktur Repositori

```
sppg_analysis/
│
├── 📄 README.md
│
├── 📂 R/                              # Script analisis
│   ├── 00_setup.R                     # Setup: package, import, cleaning
│   ├── 01_tahap1_korelasi.R           # Korelasi & ANOVA antar pulau
│   ├── 02_tahap2_ketimpangan.R        # Gini, Lorenz, Theil, LQ, Hoover
│   ├── 03_tahap3_tipologi.R           # Analisis kuadran & K-Means
│   ├── 04_tahap4_regresi.R            # OLS, QR, Moran's I, Spatial Models
│   └── 05_tahap5_skenario.R           # Skenario alokasi & simulasi kebijakan
│
├── 📂 data/                           # Letakkan file data di sini
│   ├── Dataset_Final_SPPG_514_KabKota.xlsx   # Data utama (514 kab/kota)
│   └── Data_SPPG_010526_completed.xlsx       # Data mentah SPPG (koordinat)
│
└── 📂 output/                         # Plot & CSV (auto-generated)
    ├── Rplot01_heatmap_korelasi.png
    ├── ...                            # 20 plot total
    └── Dataset_Final_Lengkap_Analisis.csv
```

---

## 🗄️ Sumber Data

| Data | Sumber | Periode | Granularitas |
|------|--------|---------|--------------|
| Lokasi & jumlah SPPG | Badan Gizi Nasional (BGN) | 30 April 2026 | Kab/Kota |
| Proyeksi populasi | BPS | 2026 | Kab/Kota |
| Data kemiskinan | BPS | 2023–2024 | Kab/Kota |
| Prevalensi stunting | Kemenkes – SSGI | 2022 | Kab/Kota |
| Satuan pendidikan | Kemdikdasmen | 18 Mei 2026 | Provinsi |

Dataset final mencakup **514 kabupaten/kota**, **13 variabel**, **nol missing values**.

---

## 📊 Metode Analisis

### Tahap 1 — Korelasi & ANOVA

| Metode | Tujuan | Output |
|--------|--------|--------|
| Pearson & Spearman Correlation | Hubungan antar variabel | Heatmap korelasi |
| One-Way ANOVA + Kruskal-Wallis | Perbedaan distribusi antar pulau | Boxplot per pulau |
| Dunn Post-hoc (Bonferroni) | Identifikasi pasangan yang berbeda | Tabel signifikansi |

### Tahap 2 — Distribusi & Ketimpangan

| Metode | Tujuan | Output |
|--------|--------|--------|
| Gini Coefficient | Ketimpangan agregat | Tabel indeks |
| Lorenz Curve | Visualisasi distribusi kumulatif | Kurva Lorenz |
| Theil T Decomposition | Pisah ketimpangan antar vs dalam pulau | Bar chart dekomposisi |
| Location Quotient (LQ) | Over/under-served per kab/kota | Histogram LQ |
| Hoover Index | Berapa % SPPG perlu direlokasi | Tabel angka konkret |

### Tahap 3 — Tipologi & Klasterisasi

| Metode | Tujuan | Output |
|--------|--------|--------|
| Analisis Kuadran 2×2 | Klasifikasi SPPG vs Stunting/Kemiskinan | Scatter plot kuadran |
| K-Means Clustering (k=4) | Tipologi kab/kota | PCA biplot + profil |
| Elbow & Silhouette Method | Penentuan k optimal | Plot diagnostik |

### Tahap 4 — Regresi & Analisis Spasial

| Metode | Tujuan | Output |
|--------|--------|--------|
| OLS Regression (3 model) | Determinan distribusi SPPG | Tabel koefisien |
| Uji Asumsi Lengkap | Validasi model OLS | Diagnostic plots |
| Quantile Regression (Q10–Q90) | Efek di semua segmen distribusi | Plot QR |
| Moran's I (Global + Monte Carlo) | Deteksi autokorelasi spasial | Moran scatterplot |
| LM Tests (Lagrange Multiplier) | Pilih SLM vs SEM | Tabel LM |
| Spatial Lag Model (SLM) | Koreksi spatial bias | Koefisien spasial |
| Spatial Error Model (SEM) | Alternatif spasial | Perbandingan model |

### Tahap 5 — Simulasi Kebijakan

| Metode | Tujuan | Output |
|--------|--------|--------|
| Needs-Based Allocation (5 skenario) | Simulasi redistribusi | Perbandingan Gini |
| Benefit Incidence Analysis (BIA) | Siapa yang diuntungkan? | BIA per kuintil |
| Concentration Index | Pro-poor atau pro-rich? | Angka CI |
| Composite Deprivation Index (CDI) | Daerah paling tertinggal | Scatter CDI |
| Sensitivity Analysis (25 bobot) | Bobot alokasi optimal | Heatmap Gini |

---

## ⚙️ Cara Menjalankan

### Prasyarat

- **R** versi 4.0 atau lebih baru — [Download R](https://cran.r-project.org/)
- **RStudio** (direkomendasikan) — [Download RStudio](https://posit.co/download/rstudio-desktop/)
- **Rtools** (khusus Windows) — [Download Rtools](https://cran.rstudio.com/bin/windows/Rtools/)

### Langkah 1 — Clone repositori

```bash
git clone https://github.com/[username]/sppg_analysis.git
cd sppg_analysis
```

### Langkah 2 — Siapkan data

Letakkan file data di folder `data/`:

```
data/
├── Dataset_Final_SPPG_514_KabKota.xlsx
└── Data_SPPG_010526_completed.xlsx     # opsional, untuk analisis spasial
```

> **Catatan:** File data tidak disertakan di repositori karena ukuran file. Hubungi peneliti untuk akses data, atau gunakan data publik dari sumber yang tercantum di atas.

### Langkah 3 — Jalankan script

Buka RStudio, lalu jalankan **secara berurutan**:

```r
# Wajib dijalankan pertama — install package & siapkan data
source("R/00_setup.R")

# Tahap 1 hingga 5
source("R/01_tahap1_korelasi.R")
source("R/02_tahap2_ketimpangan.R")
source("R/03_tahap3_tipologi.R")
source("R/04_tahap4_regresi.R")
source("R/05_tahap5_skenario.R")
```

Atau jalankan semua sekaligus:

```r
for (f in list.files("R/", full.names = TRUE)) source(f)
```

Semua output (20 plot PNG + 8 file CSV) tersimpan otomatis di folder `output/`.

---

## 📦 Package yang Digunakan

Package diinstall otomatis oleh `00_setup.R`.

| Kategori | Package |
|----------|---------|
| Data wrangling | `tidyverse`, `readxl` |
| Visualisasi | `ggplot2`, `ggcorrplot`, `patchwork`, `ggrepel`, `viridis`, `scales`, `RColorBrewer`, `corrplot` |
| Statistik inferensial | `car`, `lmtest`, `sandwich`, `moments`, `tseries` |
| Analisis spasial | `spdep`, `spatialreg` |
| Klasterisasi | `cluster`, `factoextra` |
| Ketimpangan & QR | `quantreg`, `ineq`, `dunn.test` |
| Pelaporan | `knitr`, `kableExtra` |

---

## 📈 Output yang Dihasilkan

### Plot PNG (300 DPI)

| File | Deskripsi |
|------|-----------|
| `Rplot01_heatmap_korelasi.png` | Matriks korelasi Pearson & Spearman |
| `Rplot02_boxplot_pulau.png` | Distribusi rasio SPPG per pulau |
| `Rplot03_profil_pulau.png` | Profil SPPG vs kemiskinan & stunting per pulau |
| `Rplot04_lorenz_curve.png` | Lorenz Curve SPPG vs Populasi & Kemiskinan |
| `Rplot05_theil_decomposition.png` | Dekomposisi Theil T per pulau |
| `Rplot06_lq_histogram.png` | Distribusi Location Quotient 514 kab/kota |
| `Rplot07_kuadran_stunting.png` | Analisis kuadran SPPG vs Stunting |
| `Rplot08_kuadran_kemiskinan.png` | Analisis kuadran SPPG vs Kemiskinan |
| `Rplot09_k_optimal.png` | Elbow & Silhouette method |
| `Rplot10_kmeans_pca.png` | PCA biplot K-Means 4 klaster |
| `Rplot11_kuadran_klaster.png` | Klaster pada ruang kuadran |
| `Rplot12_profil_klaster.png` | Profil 4 klaster perbandingan indikator |
| `Rplot13_diagnostic_plots.png` | Diagnostic plots OLS (4 panel) |
| `Rplot14_quantile_regression.png` | Koefisien QR lintas kuantil |
| `Rplot15_moran_scatterplot.png` | Moran scatterplot & klasifikasi LISA |
| `Rplot16_gini_skenario.png` | Gini per skenario alokasi |
| `Rplot17_bia_kuintil.png` | Benefit Incidence Analysis |
| `Rplot18_cdi_scatter.png` | Composite Deprivation Index |
| `Rplot19_gap_redistribusi_pulau.png` | Gap redistribusi S4 vs aktual |
| `Rplot20_sensitivity_heatmap.png` | Sensitivity analysis 25 bobot |

### File CSV

| File | Deskripsi |
|------|-----------|
| `T2_indeks_ketimpangan.csv` | Semua indeks: Gini, Theil, Hoover, LQ |
| `T2_location_quotient_514.csv` | LQ per kab/kota |
| `T3_tipologi_514_KabKota.csv` | Kuadran + klaster per kab/kota |
| `T4_koefisien_OLS.csv` | Koefisien OLS Model 2 |
| `T4_koefisien_QR.csv` | Koefisien quantile regression |
| `T5_bia_kuintil.csv` | BIA per kuintil kemiskinan |
| `T5_sensitivity_analysis.csv` | 25 skenario sensitivity analysis |
| `Dataset_Final_Lengkap_Analisis.csv` | Dataset 514 kab/kota lengkap |

---

## 🗺️ Hasil Klasterisasi

K-Means (k=4, Silhouette = 0,266) mengidentifikasi **4 tipe kab/kota**:

```
┌─────────────────────────────────────────────────────────────────┐
│  TIPE A: Aksesi Aktif — 108 kab/kota                            │
│  Rasio SPPG: 1,309 | Stunting: 22,8% | Miskin: 8,8%            │
│  Mayoritas Sumatera. Akses SPPG cukup baik.                     │
├─────────────────────────────────────────────────────────────────┤
│  TIPE B: Bawah Rata-Rata — 196 kab/kota (TERBESAR)             │
│  Rasio SPPG: 0,601 | Stunting: 23,0% | Miskin: 8,6%            │
│  Tersebar di Sumatera, Kalimantan, Sulawesi.                    │
├─────────────────────────────────────────────────────────────────┤
│  TIPE C: Krisis Ganda — 61 kab/kota  ⚠️  PRIORITAS ABSOLUT    │
│  Rasio SPPG: 0,349 | Stunting: 34,9% | Miskin: 25,6%           │
│  Mayoritas Papua. Hanya dapat 1,3% dari total SPPG nasional!    │
├─────────────────────────────────────────────────────────────────┤
│  TIPE D: Dominan Terlayani — 149 kab/kota                       │
│  Rasio SPPG: 1,025 | Stunting: 17,3% | Miskin: 8,4%            │
│  Mayoritas Jawa. Menguasai 74,1% total SPPG nasional.           │
└─────────────────────────────────────────────────────────────────┘
```

---

## 🔬 Ringkasan Hasil Regresi

**OLS Model 2 — Rasio SPPG/10k sebagai variabel dependen:**

| Variabel | β | p-value | Interpretasi |
|----------|---|---------|--------------|
| % Kemiskinan | −0,0128 | < 0,001 *** | Lebih miskin → SPPG lebih sedikit |
| % Stunting | −0,0075 | 0,005 ** | Stunting lebih tinggi → SPPG lebih sedikit |
| Log Populasi | +0,054 | 0,005 ** | Populasi lebih besar → SPPG lebih banyak |
| R² | 0,136 | — | Model menjelaskan 13,6% variasi |

**Moran's I = 0,360 (p<0,001)** → OLS bias spasial, Spatial Lag Model lebih tepat.

**Spatial Lag Model:**

| Parameter | Nilai | Interpretasi |
|-----------|-------|-------------|
| ρ (spatial lag) | 0,629 *** | Efek tetangga sangat dominan |
| R² | 0,291 | Naik 2,1× dari OLS |
| Moran's I residual | tidak signifikan | Spatial bias teratasi |

---

## 📐 Perbandingan Skenario Alokasi

| Skenario | Pop | Miskin | Stunting | Gini | ΔGini | CI |
|----------|-----|--------|----------|------|-------|----|
| S0: Aktual | — | — | — | 0,257 | — | +0,120 |
| S1: Proporsional Pop | 100% | 0% | 0% | 0,249 | −3,1% | +0,090 |
| S2: Pro-Kemiskinan | 20% | 60% | 20% | 0,242 | −5,8% | −0,030 |
| S3: Pro-Stunting | 20% | 20% | 60% | 0,240 | −6,6% | −0,010 |
| **S4: Balanced ⭐** | **20%** | **40%** | **40%** | **0,235** | **−8,5%** | **−0,020** |

> CI = Concentration Index: negatif = pro-poor, positif = pro-rich

---

## 💡 Rekomendasi Kebijakan

**1. Adopsi formula alokasi berbasis kebutuhan**
Formula: `Skor = 0,2 × Populasi + 0,4 × Jml_Penduduk_Miskin + 0,4 × Proxy_Stunting`
Potensi: turunkan Gini 8,5% tanpa menambah satu pun dapur baru.

**2. Akselerasi 16 kab/kota tanpa SPPG dalam 6–12 bulan**
Prioritas: Dogiyai (stunting 55,8%), Intan Jaya (48,4%), Tolikara (46,4%), Puncak (42,5%), Nduga (39,3%).

**3. Intervensi berlapis: antar dan dalam wilayah**
80,4% ketimpangan terjadi di dalam pulau — redistribusi Jawa→luar Jawa tidak cukup. Setiap provinsi perlu analisis intra-provinsi sendiri.

**4. Monitoring dengan Composite Deprivation Index**
Publikasikan CDI (gabungan kemiskinan + stunting + keterbatasan SPPG) semesteran sebagai dashboard prioritas kebijakan.

**5. Manfaatkan efek klaster spasial**
ρ=0,629 berarti efek penularan spasial kuat — tempatkan SPPG di "gateway district" yang strategis, biarkan efek menyebar ke kabupaten sekitar yang lebih terpencil.

---

## 📖 Sitasi

@techreport{prasetyo2026sppg,
  author      = {Prasetyo, Yanu Endar and, Yulinda Nurul and
                 Bahagijo, Sugeng and
                 Rossinda, Shabilla},
  title       = {Ketimpangan Distribusi {SPPG} di {Indonesia}:
                 Mengapa Wilayah dengan Kemiskinan dan Stunting
                 Tinggi Justru Tertinggal?},
  type        = {Policy Brief},
  institution = {Pusat Riset Kependudukan,
                 Badan Riset dan Inovasi Nasional ({BRIN})},
  address     = {Jakarta},
  year        = {2026}
}
---

## 📚 Referensi

- Anselin, L. (1995). Local Indicators of Spatial Association—LISA. *Geographical Analysis*, 27(2), 93–115.
- Koenker, R., & Bassett, G. (1978). Regression Quantiles. *Econometrica*, 46(1), 33–50.
- LeSage, J., & Pace, R. K. (2009). *Introduction to Spatial Econometrics*. CRC Press.
- Moran, P. A. P. (1950). Notes on Continuous Stochastic Phenomena. *Biometrika*, 37(1–2), 17–23.
- Theil, H. (1967). *Economics and Information Theory*. North-Holland.
- Wagstaff, A. (2000). Socioeconomic Inequalities in Child Mortality. *Bulletin WHO*, 78(1), 19–29.

---

## 🤝 Kontribusi

Kontribusi sangat disambut:

1. Fork repositori ini
2. Buat branch baru: `git checkout -b feature/analisis-baru`
3. Commit: `git commit -m 'Tambah analisis X'`
4. Push: `git push origin feature/analisis-baru`
5. Buat Pull Request

Gunakan tab [Issues](../../issues) untuk melaporkan bug atau mengusulkan fitur baru.

---

## 📄 Lisensi

**MIT License** — bebas digunakan, dimodifikasi, dan didistribusikan dengan mencantumkan atribusi. Lihat [LICENSE](LICENSE).

---

<div align="center">

**Dibuat untuk mendukung kebijakan berbasis bukti Program MBG Indonesia 🇮🇩**

*Distribusikan SPPG berdasarkan kebutuhan, bukan sekadar jumlah penduduk.*

</div>
