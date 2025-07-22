# 🎯 DAST - Dashboard Analisis Statistik Terpadu

## 📊 Enhanced R Shiny Dashboard for Social Vulnerability Analysis

**DAST** adalah platform komprehensif berbasis R Shiny untuk analisis statistik data kerentanan sosial Indonesia dengan fitur-fitur canggih dan visualisasi interaktif.

---

## 🎉 **MAJOR UPDATE - Semua Fitur Diperbaiki!**

### ✅ **PERBAIKAN UTAMA YANG SUDAH DILAKUKAN:**

#### 1. **🗺️ Peta Geografis - ALL 511 Data Points**
- ✅ **Menampilkan SEMUA 511 kabupaten/kota** (bukan sample)
- ✅ **Rectangle markers** menggantikan circle untuk visibility yang lebih baik
- ✅ **Enhanced popup** dengan styling HTML yang menarik
- ✅ **CartoDB.Positron tiles** untuk tampilan peta yang lebih clean
- ✅ **Real-time counter** menampilkan total data di pojok kanan atas

#### 2. **📸 Download JPG untuk Semua Grafik**
- ✅ **Manajemen Data**: Download semua plot kategorisasi dalam 1 JPG
- ✅ **Uji Asumsi**: Q-Q plot + histogram + box plot + ringkasan uji
- ✅ **Statistik Inferensia**: Plot uji + diagnostic plots
- ✅ **Format JPG berkualitas tinggi** (150 DPI, quality 95%)
- ✅ **Layout grid yang smart** (2x2, 3x2) untuk presentasi optimal

#### 3. **📄 Download Report yang Diperbaiki**
- ✅ **Format Word (.docx) profesional** menggantikan PDF bermasalah
- ✅ **Semua PNG diubah ke JPG** untuk kompatibilitas lebih baik
- ✅ **Beautiful Word formatting** dengan tabel berwarna dan styling profesional
- ✅ **Real-time data integration** dalam report dengan flextable

---

## 🚀 **FITUR UTAMA**

### **1. Manajemen Data**
- Transformasi data kontinyu ke kategorik
- Multiple metode kategorisasi (quantile, equal-width, custom)
- Interpretasi otomatis hasil kategorisasi
- **NEW**: Download semua grafik kategorisasi dalam 1 JPG

### **2. Eksplorasi Data**
- Statistik deskriptif lengkap
- Visualisasi interaktif (histogram, box plot, Q-Q plot)
- **ENHANCED**: Peta geografis dengan ALL 511 kabupaten/kota
- Tabel data dengan search dan filtering

### **3. Uji Asumsi Data**
- Uji normalitas (Shapiro-Wilk, Kolmogorov-Smirnov, Anderson-Darling)
- Uji homogenitas (Levene, Bartlett)
- **NEW**: Download semua plot asumsi + ringkasan hasil uji

### **4. Statistik Inferensia**
- Uji t (1 & 2 sampel)
- Uji proporsi (1 & 2 sampel)
- Uji varians (1 & 2 sampel)
- ANOVA (1 & 2 arah)
- **NEW**: Download plot uji + diagnostic plots

### **5. Regresi Linear Berganda**
- Model building dengan multiple predictors
- Uji asumsi lengkap (normalitas, homoskedastisitas, multikolinearitas)
- Diagnostic plots comprehensive
- **ENHANCED**: Download 6 plot diagnostik dalam 1 JPG

---

## 📁 **Dataset**

### **Social Vulnerability Index (SoVI) Data**
- **515 kabupaten/kota** di Indonesia
- **16 variabel utama**: CHILDREN, ELDERLY, POVERTY, EDUCATION, dll
- **Data geografis lengkap**: 511 koordinat latitude/longitude
- **Sumber**: Badan Pusat Statistik dan instansi terkait

---

## 🎯 **CARA PENGGUNAAN**

### **1. Menjalankan Dashboard**
```r
# Install required packages
install.packages(c("shiny", "DT", "ggplot2", "dplyr", "leaflet", 
                   "plotly", "car", "nortest", "lmtest", "moments", 
                   "gridExtra", "kableExtra", "Cairo", "officer", "flextable"))

# Run the dashboard
shiny::runApp("app.R")
```

### **2. Fitur Peta Geografis (ALL 511 Data)**
1. Masuk ke **"Eksplorasi Data"**
2. Pilih tab **"Peta Geografis"**
3. Pilih variabel yang ingin divisualisasikan
4. Peta akan menampilkan **SEMUA 511 kabupaten/kota** dengan:
   - Rectangle markers berwarna sesuai nilai variabel
   - Popup detail saat diklik
   - Label saat hover
   - Counter total data di pojok kanan atas

### **3. Download JPG Komprehensif**
1. **Manajemen Data**: 
   - Lakukan kategorisasi variabel
   - Klik tombol kuning **"Download Semua Grafik (JPG)"**
   - Akan menghasilkan 1 file JPG dengan 4 plot: kategorisasi + perbandingan

2. **Uji Asumsi Data**:
   - Pilih variabel dan uji yang diinginkan
   - Klik tombol kuning **"Download Semua Grafik (JPG)"**
   - Akan menghasilkan Q-Q plot + histogram + box plot + ringkasan uji

3. **Statistik Inferensia**:
   - Pilih jenis uji statistik
   - Klik tombol kuning **"Download Semua Grafik (JPG)"**
   - Akan menghasilkan plot uji + diagnostic plots + interpretasi

### **4. Download Report Word**
- Semua tombol hijau **"Download Laporan (Word)"** sekarang menghasilkan file **Word (.docx)**
- Format profesional dengan tabel berwarna, header styling, dan layout yang menarik
- Bisa dibuka di Microsoft Word, LibreOffice, Google Docs, dan aplikasi office lainnya

---

## 📋 **FILE OUTPUT**

### **Format JPG (Baru - Berkualitas Tinggi)**
- `DAST_Manajemen_Data_Grafik_[tanggal].jpg`
- `DAST_Uji_Asumsi_Grafik_[variabel]_[tanggal].jpg`
- `DAST_Statistik_Inferensia_[jenis_uji]_[tanggal].jpg`
- `Plot_[variabel]_[tanggal].jpg`
- `DAST_Plot_Asumsi_Regresi_[tanggal].jpg`

### **Format Word (Report Profesional)**
- `DAST_Laporan_Lengkap_[tanggal].docx`
- `DAST_Manajemen_Data_[tanggal].docx`
- `DAST_Uji_Asumsi_[variabel]_[tanggal].docx`
- `DAST_Statistik_Inferensia_[jenis_uji]_[tanggal].docx`
- `DAST_Laporan_Regresi_[tanggal].docx`

### **Format CSV (Data Export)**
- `Kategorisasi_[variabel]_[tanggal].csv`
- `DAST_Data_[tanggal].csv`

---

## 🛠️ **Technical Requirements**

### **R Packages Required:**
```r
library(shiny)          # Web application framework
library(DT)             # Interactive tables
library(ggplot2)        # Advanced plotting
library(dplyr)          # Data manipulation
library(leaflet)        # Interactive maps
library(plotly)         # Interactive plots
library(car)            # Statistical tests
library(nortest)        # Normality tests
library(lmtest)         # Linear model tests
library(moments)        # Statistical moments
library(gridExtra)      # Plot arrangements
library(kableExtra)     # Enhanced tables
library(Cairo)          # High-quality graphics
library(officer)        # Word document creation
library(flextable)      # Beautiful tables in Word
```

### **Data Files Required:**
- `sovi_data.csv` - Main dataset (515 observations, 16 variables)
- `sovi_data_longitudelatitude.csv` - Geographic coordinates (511 locations)

---

## 🎨 **Enhanced Features**

### **Map Visualization**
- **Interactive rectangle markers** instead of circles
- **Gradient color mapping** based on variable values
- **HTML-styled popups** with detailed information
- **Hover labels** for quick reference
- **Zoom and pan** functionality
- **Professional CartoDB tiles**

### **Download System**
- **One-click multi-plot downloads** in JPG format
- **High-resolution outputs** (150 DPI, 95% quality)
- **Smart grid layouts** for optimal presentation
- **Professional Word reports** with colored tables and beautiful formatting
- **Timestamped filenames** for organization

### **Statistical Analysis**
- **Complete assumption testing** with interpretations
- **Multiple test options** for robustness
- **Visual diagnostic tools** for data quality assessment
- **Automated interpretations** in Indonesian language
- **Professional statistical reporting**

---

## 📧 **Support & Contact**

Untuk pertanyaan, bug reports, atau feature requests:
- **GitHub Issues**: [Moses745/Dashboard](https://github.com/Moses745/Dashboard/issues)
- **Repository**: [https://github.com/Moses745/Dashboard](https://github.com/Moses745/Dashboard)

---

## 📜 **License**

This project is open source and available under the MIT License.

---

## 🏆 **Update Log**

### **v2.1.0 - Professional Word Reports (Latest)**
- ✅ Converted all downloads to professional Word (.docx) format
- ✅ Added beautiful flextable formatting with colors
- ✅ Enhanced visual appeal with professional styling
- ✅ Better compatibility across all office applications

### **v2.0.0 - Major Enhancement**
- ✅ Fixed map to show ALL 511 geographic data points
- ✅ Added comprehensive JPG download for all plot sections  
- ✅ Enhanced UI with color-coded download buttons
- ✅ Improved plot quality and formatting
- ✅ Added real-time data integration

### **v1.0.0 - Initial Release**
- Basic R Shiny dashboard functionality
- Statistical analysis tools
- Basic visualization features

---

**🎯 Dashboard siap digunakan dengan semua fitur yang telah diperbaiki dan disempurnakan!**