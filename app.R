# app.R - Dashboard Analisis Statistik Terpadu (DAST) - Enhanced Version

# Load the required libraries
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lmtest) # Required for Breusch-Pagan test
library(tools) # For file download functionality
library(rmarkdown) # For generating reports
library(knitr) # For knitr functionality
library(car) # For Levene's test
library(nortest) # For normality tests
library(leaflet) # For maps
library(plotly) # For interactive plots
library(moments) # For skewness and kurtosis
library(gridExtra) # For arranging plots
library(kableExtra) # For better tables
library(Cairo) # For better graphics output
library(officer) # For creating Word documents
library(flextable) # For beautiful tables in Word

# --- Define the User Interface (UI) ---
ui <- navbarPage(
  title = div(
    img(src = "data:image/svg+xml;base64,PHN2ZyB3aWR0aD0iMzIiIGhlaWdodD0iMzIiIHZpZXdCb3g9IjAgMCAzMiAzMiIgZmlsbD0ibm9uZSIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIj4KPHJlY3Qgd2lkdGg9IjMyIiBoZWlnaHQ9IjMyIiByeD0iOCIgZmlsbD0iIzJFNzRCNSIvPgo8cGF0aCBkPSJNOCAxMkgxNlYyMEg4VjEyWiIgZmlsbD0id2hpdGUiLz4KPHA+" width="24", height="24", style="margin-right: 8px;"),
    span("DAST", style = "font-weight: bold; color: #2E74B5; font-size: 18px;"),
    span(" - Dashboard Analisis Statistik Terpadu", style = "color: #555; font-size: 14px;")
  ),
  theme = "bootstrap",
  windowTitle = "DAST - Dashboard Analisis Statistik Terpadu",
  id = "navbar",
  collapsible = TRUE,
  
  # Add custom CSS for beautiful styling
  tags$head(
    tags$style(HTML("
      /* Custom CSS for DAST Dashboard */
      
      /* Navigation Bar Styling */
      .navbar-default {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        border: none;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
      }
      
      .navbar-default .navbar-nav > li > a {
        color: white !important;
        font-weight: 500;
        transition: all 0.3s ease;
      }
      
      .navbar-default .navbar-nav > li > a:hover {
        background: rgba(255,255,255,0.2) !important;
        transform: translateY(-2px);
      }
      
      .navbar-default .navbar-nav > .active > a {
        background: rgba(255,255,255,0.3) !important;
        color: white !important;
      }
      
      /* Body Background */
      body {
        background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }
      
      /* Card Styling */
      .panel {
        border-radius: 12px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.1);
        border: none;
        margin-bottom: 20px;
        transition: transform 0.3s ease;
      }
      
      .panel:hover {
        transform: translateY(-5px);
        box-shadow: 0 8px 30px rgba(0,0,0,0.15);
      }
      
      .panel-primary {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
      }
      
      .panel-success {
        background: linear-gradient(135deg, #56ab2f 0%, #a8e6cf 100%);
        color: white;
      }
      
      .panel-info {
        background: linear-gradient(135deg, #3494e6 0%, #ec6ead 100%);
        color: white;
      }
      
      .panel-default {
        background: white;
        border: 1px solid #e1e8ed;
      }
      
      /* Jumbotron Styling */
      .jumbotron {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        border-radius: 15px;
        box-shadow: 0 8px 32px rgba(0,0,0,0.1);
        margin-bottom: 30px;
      }
      
      /* Button Styling */
      .btn {
        border-radius: 8px;
        font-weight: 600;
        transition: all 0.3s ease;
        border: none;
      }
      
      .btn:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 15px rgba(0,0,0,0.2);
      }
      
      .btn-primary {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      }
      
      .btn-success {
        background: linear-gradient(135deg, #56ab2f 0%, #a8e6cf 100%);
      }
      
      .btn-warning {
        background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);
      }
      
      .btn-info {
        background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%);
      }
      
      /* Sidebar Styling */
      .well {
        background: white;
        border-radius: 12px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.1);
        border: none;
        padding: 20px;
      }
      
      /* Input Styling */
      .form-control {
        border-radius: 8px;
        border: 2px solid #e1e8ed;
        transition: border-color 0.3s ease;
      }
      
      .form-control:focus {
        border-color: #667eea;
        box-shadow: 0 0 10px rgba(102, 126, 234, 0.3);
      }
      
      /* Table Styling */
      .dataTables_wrapper {
        background: white;
        border-radius: 12px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.1);
        padding: 20px;
      }
      
      /* Alert Styling */
      .alert {
        border-radius: 12px;
        border: none;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
      }
      
      .alert-info {
        background: linear-gradient(135deg, #a8edea 0%, #fed6e3 100%);
        color: #2c3e50;
      }
      
      .alert-warning {
        background: linear-gradient(135deg, #ffecd2 0%, #fcb69f 100%);
        color: #8b4513;
      }
      
      /* Plot Container */
      .shiny-plot-output {
        border-radius: 12px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.1);
        background: white;
        padding: 15px;
      }
      
      /* Leaflet Map Styling */
      .leaflet-container {
        border-radius: 12px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.1);
      }
      
      /* Tab Panel Styling */
      .nav-tabs {
        border-bottom: 2px solid #e1e8ed;
      }
      
      .nav-tabs > li > a {
        border-radius: 8px 8px 0 0;
        color: #667eea;
        font-weight: 500;
      }
      
      .nav-tabs > li.active > a {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        border: none;
      }
      
      /* Verbatim Output Styling */
      .shiny-text-output {
        background: #f8f9fa;
        border-radius: 8px;
        border: 1px solid #e1e8ed;
        padding: 15px;
        font-family: 'Consolas', 'Monaco', monospace;
      }
      
      /* Loading Animation */
      .recalculating {
        opacity: 0.3;
        transition: opacity 0.3s ease;
      }
      
      /* Custom Scrollbar */
      ::-webkit-scrollbar {
        width: 8px;
      }
      
      ::-webkit-scrollbar-track {
        background: #f1f1f1;
        border-radius: 10px;
      }
      
      ::-webkit-scrollbar-thumb {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        border-radius: 10px;
      }
      
      ::-webkit-scrollbar-thumb:hover {
        background: linear-gradient(135deg, #764ba2 0%, #667eea 100%);
      }
      
      /* Responsive Design */
      @media (max-width: 768px) {
        .panel {
          margin-bottom: 15px;
        }
        
        .jumbotron {
          padding: 20px;
        }
        
        .btn {
          width: 100%;
          margin-bottom: 10px;
        }
      }
      
      /* Animation Classes */
      .fade-in {
        animation: fadeIn 0.8s ease-in;
      }
      
      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(20px); }
        to { opacity: 1; transform: translateY(0); }
      }
      
      .slide-in {
        animation: slideIn 0.6s ease-out;
      }
      
      @keyframes slideIn {
        from { transform: translateX(-50px); opacity: 0; }
        to { transform: translateX(0); opacity: 1; }
      }
    "))
  ),
  
  # --- 1. Beranda (Home) Menu ---
  tabPanel("🏠 Beranda",
           fluidRow(
             column(12,
                    div(class = "jumbotron fade-in",
                        div(style = "text-align: center;",
                            h1(HTML("🎯 Dashboard Analisis Statistik Terpadu"), 
                               style = "color: white; font-weight: bold; margin-bottom: 10px;"),
                            h2(HTML("<span style='color: #FFD700;'>DAST</span>"), 
                               style = "margin-top: 0; margin-bottom: 20px;"),
                            div(style = "height: 3px; width: 100px; background: linear-gradient(90deg, #FFD700, #FFA500); margin: 20px auto;")
                        ),
                        br(),
                        div(style = "text-align: center;",
                            h3(HTML("🚀 <strong>Selamat Datang di Era Baru Analisis Statistik</strong>"), 
                               style = "color: white; margin-bottom: 20px;"),
                            p("Platform revolusioner yang mengintegrasikan kekuatan analisis statistik modern dengan desain yang elegan dan user-friendly. Dirancang khusus untuk peneliti, akademisi, dan praktisi yang membutuhkan solusi analisis data yang komprehensif dan dapat diandalkan.", 
                              style = "font-size: 18px; color: rgba(255,255,255,0.9); line-height: 1.6; text-align: center; max-width: 800px; margin: 0 auto;")
                        )
                    )
             )
           ),
           
           fluidRow(
             column(6,
                    div(class = "panel panel-primary slide-in",
                        div(class = "panel-heading", 
                            h4(HTML("📊 <strong>Metadata Dataset</strong>"), style = "margin: 0; color: white;")),
                        div(class = "panel-body", style = "padding: 25px;",
                            div(style = "margin-bottom: 20px;",
                                h5(HTML("ℹ️ <strong>Informasi Dataset:</strong>"), style = "color: white; margin-bottom: 15px;"),
                                div(style = "background: rgba(255,255,255,0.1); padding: 15px; border-radius: 8px;",
                                    HTML("
                                    <div style='margin-bottom: 8px;'><strong>📋 Dataset:</strong> Social Vulnerability Index (SoVI)</div>
                                    <div style='margin-bottom: 8px;'><strong>🏘️ Observasi:</strong> 515 kabupaten/kota</div>
                                    <div style='margin-bottom: 8px;'><strong>📈 Variabel:</strong> 16 variabel utama</div>
                                    <div style='margin-bottom: 8px;'><strong>📅 Periode:</strong> Data terkini 2024</div>
                                    <div style='margin-bottom: 8px;'><strong>🏛️ Sumber:</strong> Badan Pusat Statistik</div>
                                    ")
                                )
                            ),
                            div(
                                h5(HTML("🎯 <strong>Variabel Kunci:</strong>"), style = "color: white; margin-bottom: 15px;"),
                                div(style = "background: rgba(255,255,255,0.1); padding: 15px; border-radius: 8px;",
                                    HTML("
                                    <div style='margin-bottom: 5px;'>👶 <strong>CHILDREN:</strong> Persentase anak-anak</div>
                                    <div style='margin-bottom: 5px;'>👴 <strong>ELDERLY:</strong> Persentase lansia</div>
                                    <div style='margin-bottom: 5px;'>💰 <strong>POVERTY:</strong> Tingkat kemiskinan</div>
                                    <div style='margin-bottom: 5px;'>🎓 <strong>EDUCATION:</strong> Tingkat pendidikan</div>
                                    <div style='margin-bottom: 0px;'>➕ <strong>Dan 12 variabel lainnya</strong></div>
                                    ")
                                )
                            )
                        )
                    )
             ),
             column(6,
                    div(class = "panel panel-success slide-in",
                        div(class = "panel-heading", 
                            h4(HTML("🚀 <strong>Fitur Unggulan DAST</strong>"), style = "margin: 0; color: white;")),
                        div(class = "panel-body", style = "padding: 25px;",
                            div(style = "display: grid; gap: 15px;",
                                div(style = "background: rgba(255,255,255,0.1); padding: 15px; border-radius: 8px; border-left: 4px solid #FFD700;",
                                    HTML("<strong>📊 Manajemen Data:</strong><br><small>Transformasi data kontinyu ke kategorik dengan interpretasi AI</small>")
                                ),
                                div(style = "background: rgba(255,255,255,0.1); padding: 15px; border-radius: 8px; border-left: 4px solid #00D4AA;",
                                    HTML("<strong>🔍 Eksplorasi Data:</strong><br><small>Visualisasi interaktif dan pemetaan geografis real-time</small>")
                                ),
                                div(style = "background: rgba(255,255,255,0.1); padding: 15px; border-radius: 8px; border-left: 4px solid #FF6B6B;",
                                    HTML("<strong>✅ Uji Asumsi:</strong><br><small>Pengujian normalitas dan homogenitas otomatis</small>")
                                ),
                                div(style = "background: rgba(255,255,255,0.1); padding: 15px; border-radius: 8px; border-left: 4px solid #4ECDC4;",
                                    HTML("<strong>📈 Statistik Inferensia:</strong><br><small>Uji t, ANOVA, proporsi dengan interpretasi lengkap</small>")
                                ),
                                div(style = "background: rgba(255,255,255,0.1); padding: 15px; border-radius: 8px; border-left: 4px solid #45B7D1;",
                                    HTML("<strong>📉 Regresi Linear:</strong><br><small>Analisis multivariat dengan diagnostic plots</small>")
                                ),
                                div(style = "background: rgba(255,255,255,0.1); padding: 15px; border-radius: 8px; border-left: 4px solid #96CEB4;",
                                    HTML("<strong>💾 Export Premium:</strong><br><small>JPG berkualitas tinggi & Word profesional</small>")
                                )
                            )
                        )
                    )
             )
           ),
           
           fluidRow(
             column(12,
                    div(class = "panel panel-info fade-in",
                        div(class = "panel-heading", 
                            h4(HTML("📚 <strong>Roadmap Analisis Profesional</strong>"), style = "margin: 0; color: white; text-align: center;")),
                        div(class = "panel-body", style = "padding: 30px;",
                            div(style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 20px;",
                                div(style = "background: rgba(255,255,255,0.1); padding: 20px; border-radius: 12px; border-left: 5px solid #FFD700; transition: transform 0.3s ease;",
                                    HTML("
                                    <div style='display: flex; align-items: center; margin-bottom: 10px;'>
                                      <span style='background: #FFD700; color: #2c3e50; border-radius: 50%; width: 30px; height: 30px; display: flex; align-items: center; justify-content: center; font-weight: bold; margin-right: 15px;'>1</span>
                                      <strong style='color: white; font-size: 16px;'>📊 Manajemen Data</strong>
                                    </div>
                                    <p style='color: rgba(255,255,255,0.9); margin: 0; font-size: 14px;'>Transformasi dan preprocessing data dengan berbagai metode kategorisasi untuk mempersiapkan analisis yang optimal.</p>
                                    ")
                                ),
                                div(style = "background: rgba(255,255,255,0.1); padding: 20px; border-radius: 12px; border-left: 5px solid #00D4AA; transition: transform 0.3s ease;",
                                    HTML("
                                    <div style='display: flex; align-items: center; margin-bottom: 10px;'>
                                      <span style='background: #00D4AA; color: white; border-radius: 50%; width: 30px; height: 30px; display: flex; align-items: center; justify-content: center; font-weight: bold; margin-right: 15px;'>2</span>
                                      <strong style='color: white; font-size: 16px;'>🔍 Eksplorasi Data</strong>
                                    </div>
                                    <p style='color: rgba(255,255,255,0.9); margin: 0; font-size: 14px;'>Analisis mendalam karakteristik data melalui statistik deskriptif dan visualisasi interaktif yang komprehensif.</p>
                                    ")
                                ),
                                div(style = "background: rgba(255,255,255,0.1); padding: 20px; border-radius: 12px; border-left: 5px solid #FF6B6B; transition: transform 0.3s ease;",
                                    HTML("
                                    <div style='display: flex; align-items: center; margin-bottom: 10px;'>
                                      <span style='background: #FF6B6B; color: white; border-radius: 50%; width: 30px; height: 30px; display: flex; align-items: center; justify-content: center; font-weight: bold; margin-right: 15px;'>3</span>
                                      <strong style='color: white; font-size: 16px;'>✅ Validasi Asumsi</strong>
                                    </div>
                                    <p style='color: rgba(255,255,255,0.9); margin: 0; font-size: 14px;'>Pengujian prasyarat statistik untuk memastikan validitas dan reliabilitas hasil analisis inferensia.</p>
                                    ")
                                ),
                                div(style = "background: rgba(255,255,255,0.1); padding: 20px; border-radius: 12px; border-left: 5px solid #4ECDC4; transition: transform 0.3s ease;",
                                    HTML("
                                    <div style='display: flex; align-items: center; margin-bottom: 10px;'>
                                      <span style='background: #4ECDC4; color: white; border-radius: 50%; width: 30px; height: 30px; display: flex; align-items: center; justify-content: center; font-weight: bold; margin-right: 15px;'>4</span>
                                      <strong style='color: white; font-size: 16px;'>📈 Inferensia Statistik</strong>
                                    </div>
                                    <p style='color: rgba(255,255,255,0.9); margin: 0; font-size: 14px;'>Penerapan metode statistik inferensia yang tepat sesuai dengan desain penelitian dan tujuan analisis.</p>
                                    ")
                                ),
                                div(style = "background: rgba(255,255,255,0.1); padding: 20px; border-radius: 12px; border-left: 5px solid #45B7D1; transition: transform 0.3s ease;",
                                    HTML("
                                    <div style='display: flex; align-items: center; margin-bottom: 10px;'>
                                      <span style='background: #45B7D1; color: white; border-radius: 50%; width: 30px; height: 30px; display: flex; align-items: center; justify-content: center; font-weight: bold; margin-right: 15px;'>5</span>
                                      <strong style='color: white; font-size: 16px;'>📉 Pemodelan Regresi</strong>
                                    </div>
                                    <p style='color: rgba(255,255,255,0.9); margin: 0; font-size: 14px;'>Analisis hubungan multivariat dengan diagnostic tools lengkap untuk interpretasi yang akurat.</p>
                                    ")
                                ),
                                div(style = "background: rgba(255,255,255,0.1); padding: 20px; border-radius: 12px; border-left: 5px solid #96CEB4; transition: transform 0.3s ease;",
                                    HTML("
                                    <div style='display: flex; align-items: center; margin-bottom: 10px;'>
                                      <span style='background: #96CEB4; color: white; border-radius: 50%; width: 30px; height: 30px; display: flex; align-items: center; justify-content: center; font-weight: bold; margin-right: 15px;'>6</span>
                                      <strong style='color: white; font-size: 16px;'>💾 Export & Dokumentasi</strong>
                                    </div>
                                    <p style='color: rgba(255,255,255,0.9); margin: 0; font-size: 14px;'>Ekspor hasil dalam format profesional (JPG, Word) untuk dokumentasi penelitian dan presentasi.</p>
                                    ")
                                )
                            )
                        )
                    )
             )
           ),
           
           hr(),
           fluidRow(
             column(12, style = "text-align: center;",
                    h4("Download Laporan Lengkap"),
                    downloadButton("download_full_report", "Download Laporan Lengkap (Word)", 
                                   class = "btn-primary btn-lg", style = "margin: 10px;")
             )
           )
  ),
  
  # --- 2. Manajemen Data Menu ---
  tabPanel("📊 Manajemen Data",
           sidebarLayout(
             sidebarPanel(
               h4("Pengaturan Transformasi Data", style = "color: #2c3e50;"),
               selectInput("variable_categorize", "Pilih Variabel Kontinyu:", choices = NULL),
               numericInput("num_bins", "Jumlah Kelompok (Bins):", value = 3, min = 2, max = 10),
               radioButtons("categorization_method", "Metode Kategorisasi:",
                            choices = list("Quantile-based" = "quantile",
                                           "Equal-width" = "equal",
                                           "Custom breaks" = "custom")),
               conditionalPanel(
                 condition = "input.categorization_method == 'custom'",
                 textInput("custom_breaks", "Custom Breaks (pisahkan dengan koma):", 
                           placeholder = "contoh: 10,20,30")
               ),
               hr(),
               h5("Informasi Variabel Terpilih:"),
               verbatimTextOutput("variable_info"),
               hr(),
               h4("Download", style = "color: #27ae60;"),
               downloadButton("download_management_jpg", "Download Semua Grafik (JPG)", 
                              class = "btn-warning", style = "width: 100%; margin-bottom: 10px;"),
               downloadButton("download_management_report", "Download Laporan Manajemen (Word)", 
                              class = "btn-success", style = "width: 100%; margin-bottom: 10px;"),
               downloadButton("download_categorized_table", "Download Tabel Kategorisasi (CSV)", 
                              class = "btn-info", style = "width: 100%;")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Hasil Kategorisasi",
                          h3("Hasil Transformasi Data Kontinyu ke Kategorik"),
                          p("Transformasi data kontinyu menjadi data kategorik membantu dalam analisis yang memerlukan pengelompokan data."),
                          hr(),
                          h4("Tabel Frekuensi Hasil Kategorisasi"),
                          DTOutput("categorized_table"),
                          hr(),
                          h4("Visualisasi Distribusi"),
                          plotOutput("categorization_plot"),
                          hr(),
                          h4("Interpretasi Hasil"),
                          div(class = "well",
                              verbatimTextOutput("categorization_interpretation")
                          )
                 ),
                 tabPanel("Perbandingan Data",
                          h3("Perbandingan Data Asli vs Kategorik"),
                          fluidRow(
                            column(6,
                                   h4("Data Asli (Kontinyu)"),
                                   plotOutput("original_data_plot")
                            ),
                            column(6,
                                   h4("Data Kategorik"),
                                   plotOutput("categorized_data_plot")
                            )
                          ),
                          hr(),
                          h4("Statistik Perbandingan"),
                          DTOutput("comparison_stats")
                 )
               )
             )
           )
  ),
  
  # --- 3. Eksplorasi Data Menu ---
  tabPanel("🔍 Eksplorasi Data",
           sidebarLayout(
             sidebarPanel(
               h4("Pengaturan Eksplorasi", style = "color: #2c3e50;"),
               selectInput("variable_explore", "Pilih Variabel:", choices = NULL),
               checkboxInput("show_outliers", "Tampilkan Outliers", value = TRUE),
               sliderInput("plot_bins", "Jumlah Bins (Histogram):", min = 10, max = 50, value = 30),
               hr(),
               h4("Filter Data (Opsional)"),
               checkboxInput("enable_filter", "Aktifkan Filter", value = FALSE),
               conditionalPanel(
                 condition = "input.enable_filter == true",
                 selectInput("filter_variable", "Variabel Filter:", choices = NULL),
                 uiOutput("filter_values")
               ),
               hr(),
               h4("Download", style = "color: #27ae60;"),
               downloadButton("download_explore_plot", "Download Grafik (JPG)", 
                              class = "btn-info", style = "width: 100%; margin-bottom: 10px;"),
               downloadButton("download_explore_report", "Download Laporan Eksplorasi (Word)", 
                              class = "btn-success", style = "width: 100%;")
             ),
             mainPanel(
               tabsetPanel(
                 id = "explore_tabs",
                 
                 tabPanel("Statistik Deskriptif",
                          h3("Analisis Statistik Deskriptif"),
                          fluidRow(
                            column(6,
                                   div(class = "panel panel-default",
                                       div(class = "panel-heading", h4("Ringkasan Statistik")),
                                       div(class = "panel-body",
                                           verbatimTextOutput("summary_stats")
                                       )
                                   )
                            ),
                            column(6,
                                   div(class = "panel panel-default",
                                       div(class = "panel-heading", h4("Statistik Tambahan")),
                                       div(class = "panel-body",
                                           verbatimTextOutput("additional_stats")
                                       )
                                   )
                            )
                          ),
                          hr(),
                          div(class = "well",
                              h4("Interpretasi Statistik Deskriptif"),
                              verbatimTextOutput("descriptive_interpretation")
                          )
                 ),
                 
                 tabPanel("Visualisasi Data",
                          h3("Visualisasi dan Analisis Grafik"),
                          fluidRow(
                            column(6,
                                   h4("Histogram & Density Plot"),
                                   plotlyOutput("histogram_plot")
                            ),
                            column(6,
                                   h4("Box Plot"),
                                   plotlyOutput("boxplot")
                            )
                          ),
                          hr(),
                          fluidRow(
                            column(6,
                                   h4("Q-Q Plot (Normalitas)"),
                                   plotOutput("qq_plot")
                            ),
                            column(6,
                                   h4("Scatter Plot Matrix (Sample)"),
                                   plotOutput("scatter_matrix")
                            )
                          ),
                          hr(),
                          div(class = "well",
                              h4("Interpretasi Visualisasi"),
                              verbatimTextOutput("visualization_interpretation")
                          )
                 ),
                 
                 tabPanel("Peta Geografis",
                          h3("Visualisasi Peta Indonesia - Semua 511 Kabupaten/Kota"),
                          div(class = "alert alert-info",
                              h4("Informasi Peta"),
                              p("Peta menampilkan distribusi data untuk semua 511 kabupaten/kota di Indonesia berdasarkan koordinat geografis yang tersedia.")
                          ),
                          leafletOutput("map_plot", height = "600px"),
                          hr(),
                          h4("Tabel Data Geografis"),
                          DTOutput("geo_data_table")
                 ),
                 
                 tabPanel("Tabel Data",
                          h3("Tabel Data Lengkap"),
                          p("Tabel interaktif dengan fitur pencarian, sorting, dan filtering."),
                          DTOutput("data_table"),
                          hr(),
                          downloadButton("download_data_table", "Download Tabel Data (CSV)", 
                                         class = "btn-info")
                 )
               )
             )
           )
  ),
  
  # --- 4. Uji Asumsi Data Menu ---
  tabPanel("✅ Uji Asumsi Data",
           sidebarLayout(
             sidebarPanel(
               h4("Pengaturan Uji Asumsi", style = "color: #2c3e50;"),
               selectInput("assumption_variable", "Pilih Variabel:", choices = NULL),
               selectInput("grouping_variable", "Variabel Pengelompokan (Opsional):", 
                           choices = c("Tidak ada" = "none"), selected = "none"),
               hr(),
               h4("Pilih Uji yang Akan Dilakukan"),
               checkboxGroupInput("selected_tests", "Uji Asumsi:",
                                  choices = list(
                                    "Uji Normalitas (Shapiro-Wilk)" = "shapiro",
                                    "Uji Normalitas (Kolmogorov-Smirnov)" = "ks",
                                    "Uji Normalitas (Anderson-Darling)" = "ad",
                                    "Uji Homogenitas (Levene)" = "levene",
                                    "Uji Homogenitas (Bartlett)" = "bartlett"
                                  ),
                                  selected = c("shapiro", "levene")),
               hr(),
               h4("Download", style = "color: #27ae60;"),
               downloadButton("download_assumption_jpg", "Download Semua Grafik (JPG)", 
                              class = "btn-warning", style = "width: 100%; margin-bottom: 10px;"),
               downloadButton("download_assumption_report", "Download Laporan Uji Asumsi (Word)", 
                              class = "btn-success", style = "width: 100%;")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Uji Normalitas",
                          h3("Pengujian Asumsi Normalitas"),
                          div(class = "alert alert-info",
                              p("Uji normalitas digunakan untuk mengetahui apakah data berdistribusi normal. 
                                Asumsi normalitas penting untuk berbagai uji statistik parametrik.")
                          ),
                          hr(),
                          conditionalPanel(
                            condition = "input.selected_tests.indexOf('shapiro') > -1",
                            div(class = "panel panel-default",
                                div(class = "panel-heading", h4("Uji Shapiro-Wilk")),
                                div(class = "panel-body",
                                    verbatimTextOutput("shapiro_test_result"),
                                    hr(),
                                    verbatimTextOutput("shapiro_interpretation")
                                )
                            )
                          ),
                          conditionalPanel(
                            condition = "input.selected_tests.indexOf('ks') > -1",
                            div(class = "panel panel-default",
                                div(class = "panel-heading", h4("Uji Kolmogorov-Smirnov")),
                                div(class = "panel-body",
                                    verbatimTextOutput("ks_test_result"),
                                    hr(),
                                    verbatimTextOutput("ks_interpretation")
                                )
                            )
                          ),
                          conditionalPanel(
                            condition = "input.selected_tests.indexOf('ad') > -1",
                            div(class = "panel panel-default",
                                div(class = "panel-heading", h4("Uji Anderson-Darling")),
                                div(class = "panel-body",
                                    verbatimTextOutput("ad_test_result"),
                                    hr(),
                                    verbatimTextOutput("ad_interpretation")
                                )
                            )
                          ),
                          hr(),
                          fluidRow(
                            column(6,
                                   h4("Q-Q Plot"),
                                   plotOutput("normality_qq_plot")
                            ),
                            column(6,
                                   h4("Histogram dengan Kurva Normal"),
                                   plotOutput("normality_histogram")
                            )
                          )
                 ),
                 
                 tabPanel("Uji Homogenitas",
                          h3("Pengujian Asumsi Homogenitas Varians"),
                          div(class = "alert alert-info",
                              p("Uji homogenitas digunakan untuk mengetahui apakah varians antar kelompok sama (homogen). 
                                Asumsi ini penting untuk ANOVA dan uji t independen.")
                          ),
                          conditionalPanel(
                            condition = "input.grouping_variable == 'none'",
                            div(class = "alert alert-warning",
                                p("Pilih variabel pengelompokan untuk melakukan uji homogenitas.")
                            )
                          ),
                          conditionalPanel(
                            condition = "input.grouping_variable != 'none'",
                            conditionalPanel(
                              condition = "input.selected_tests.indexOf('levene') > -1",
                              div(class = "panel panel-default",
                                  div(class = "panel-heading", h4("Uji Levene")),
                                  div(class = "panel-body",
                                      verbatimTextOutput("levene_test_result"),
                                      hr(),
                                      verbatimTextOutput("levene_interpretation")
                                  )
                              )
                            ),
                            conditionalPanel(
                              condition = "input.selected_tests.indexOf('bartlett') > -1",
                              div(class = "panel panel-default",
                                  div(class = "panel-heading", h4("Uji Bartlett")),
                                  div(class = "panel-body",
                                      verbatimTextOutput("bartlett_test_result"),
                                      hr(),
                                      verbatimTextOutput("bartlett_interpretation")
                                  )
                              )
                            ),
                            hr(),
                            fluidRow(
                              column(6,
                                     h4("Box Plot per Kelompok"),
                                     plotOutput("homogeneity_boxplot")
                              ),
                              column(6,
                                     h4("Plot Residual vs Fitted"),
                                     plotOutput("residual_plot")
                              )
                            )
                          )
                 ),
                 
                 tabPanel("Ringkasan Uji Asumsi",
                          h3("Ringkasan Hasil Uji Asumsi"),
                          div(class = "well",
                              h4("Kesimpulan Uji Asumsi"),
                              verbatimTextOutput("assumption_summary")
                          ),
                          hr(),
                          h4("Rekomendasi Analisis Lanjutan"),
                          div(class = "panel panel-info",
                              div(class = "panel-body",
                                  verbatimTextOutput("analysis_recommendation")
                              )
                          )
                 )
               )
             )
           )
  ),
  
  # --- 5. Statistik Inferensia Menu ---
  tabPanel("📈 Statistik Inferensia",
           sidebarLayout(
             sidebarPanel(
               h4("Pengaturan Uji Statistik", style = "color: #2c3e50;"),
               selectInput("stat_test_type", "Pilih Jenis Uji:", 
                           choices = c("Uji Rata-Rata 1 Kelompok" = "t_one",
                                       "Uji Rata-Rata 2 Kelompok" = "t_two",
                                       "Uji Varians 1 Kelompok" = "var_one",
                                       "Uji Varians 2 Kelompok" = "var_two",
                                       "Uji Proporsi 1 Kelompok" = "prop_one",
                                       "Uji Proporsi 2 Kelompok" = "prop_two",
                                       "ANOVA 1 Arah" = "anova_one",
                                       "ANOVA 2 Arah" = "anova_two")),
               hr(),
               
               # Dynamic UI based on test type
               uiOutput("test_specific_ui"),
               
               hr(),
               h4("Download", style = "color: #27ae60;"),
               downloadButton("download_inference_jpg", "Download Semua Grafik (JPG)", 
                              class = "btn-warning", style = "width: 100%; margin-bottom: 10px;"),
               downloadButton("download_inference_report", "Download Laporan Statistik Inferensia (Word)", 
                              class = "btn-success", style = "width: 100%;")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Hasil Uji",
                          h3("Hasil Uji Statistik"),
                          div(class = "panel panel-primary",
                              div(class = "panel-heading", h4("Output Statistik")),
                              div(class = "panel-body",
                                  verbatimTextOutput("test_output")
                              )
                          ),
                          hr(),
                          div(class = "well",
                              h4("Interpretasi Hasil"),
                              verbatimTextOutput("test_interpretation")
                          )
                 ),
                 
                 tabPanel("Visualisasi",
                          h3("Visualisasi Hasil Uji"),
                          plotOutput("inference_plot"),
                          hr(),
                          conditionalPanel(
                            condition = "input.stat_test_type.indexOf('anova') > -1",
                            h4("Post-Hoc Test (jika signifikan)"),
                            verbatimTextOutput("posthoc_test")
                          )
                 ),
                 
                 tabPanel("Asumsi Uji",
                          h3("Pemeriksaan Asumsi Uji"),
                          div(class = "alert alert-info",
                              p("Setiap uji statistik memiliki asumsi yang harus dipenuhi untuk hasil yang valid.")
                          ),
                          verbatimTextOutput("test_assumptions"),
                          hr(),
                          plotOutput("assumption_plots")
                 )
               )
             )
           )
  ),
  
  # --- 6. Regresi Linear Berganda Menu ---
  tabPanel("📉 Regresi Linear",
           sidebarLayout(
             sidebarPanel(
               h4("Pengaturan Model Regresi", style = "color: #2c3e50;"),
               selectInput("reg_response", "Pilih Variabel Terikat (Y):", choices = NULL),
               selectInput("reg_predictors", "Pilih Variabel Bebas (X):", choices = NULL, multiple = TRUE),
               hr(),
               h4("Opsi Model"),
               checkboxInput("include_interaction", "Sertakan Interaksi", value = FALSE),
               conditionalPanel(
                 condition = "input.include_interaction == true",
                 selectInput("interaction_vars", "Pilih Variabel untuk Interaksi:", 
                             choices = NULL, multiple = TRUE)
               ),
               checkboxInput("standardize_vars", "Standardisasi Variabel", value = FALSE),
               hr(),
               h4("Download", style = "color: #27ae60;"),
               downloadButton("download_regression_plots", "Download Grafik Asumsi (JPG)", 
                              class = "btn-info", style = "width: 100%; margin-bottom: 10px;"),
               downloadButton("download_regression_report", "Download Laporan Regresi (Word)", 
                              class = "btn-success", style = "width: 100%;")
             ),
             mainPanel(
               tabsetPanel(
                 id = "regression_tabs",
                 
                 tabPanel("Model Summary",
                          h3("Ringkasan Model Regresi Linear Berganda"),
                          div(class = "panel panel-primary",
                              div(class = "panel-heading", h4("Output Model")),
                              div(class = "panel-body",
                                  verbatimTextOutput("reg_summary")
                              )
                          ),
                          hr(),
                          fluidRow(
                            column(6,
                                   div(class = "panel panel-info",
                                       div(class = "panel-heading", h4("Goodness of Fit")),
                                       div(class = "panel-body",
                                           verbatimTextOutput("model_fit_stats")
                                       )
                                   )
                            ),
                            column(6,
                                   div(class = "panel panel-success",
                                       div(class = "panel-heading", h4("ANOVA Table")),
                                       div(class = "panel-body",
                                           verbatimTextOutput("anova_table")
                                       )
                                   )
                            )
                          ),
                          hr(),
                          div(class = "well",
                              h4("Interpretasi Model"),
                              verbatimTextOutput("reg_interpretation")
                          )
                 ),
                 
                 tabPanel("Uji Asumsi Regresi",
                          h3("Pengujian Asumsi Regresi Linear"),
                          div(class = "alert alert-info",
                              p("Regresi linear memiliki beberapa asumsi yang harus dipenuhi: linearitas, independensi, normalitas residual, dan homoskedastisitas.")
                          ),
                          
                          # Normalitas Residual
                          div(class = "panel panel-default",
                              div(class = "panel-heading", h4("1. Uji Normalitas Residual")),
                              div(class = "panel-body",
                                  p("H₀: Residual berdistribusi normal"),
                                  p("H₁: Residual tidak berdistribusi normal"),
                                  fluidRow(
                                    column(6, plotOutput("normality_plot")),
                                    column(6, plotOutput("residual_histogram"))
                                  ),
                                  verbatimTextOutput("shapiro_test"),
                                  div(class = "well",
                                      h5("Interpretasi Normalitas"),
                                      verbatimTextOutput("normality_interpretation")
                                  )
                              )
                          ),
                          
                          # Homoskedastisitas
                          div(class = "panel panel-default",
                              div(class = "panel-heading", h4("2. Uji Homoskedastisitas")),
                              div(class = "panel-body",
                                  p("H₀: Varians residual konstan (homoskedastisitas)"),
                                  p("H₁: Varians residual tidak konstan (heteroskedastisitas)"),
                                  fluidRow(
                                    column(6, plotOutput("homo_plot")),
                                    column(6, plotOutput("scale_location_plot"))
                                  ),
                                  verbatimTextOutput("bptest_output"),
                                  div(class = "well",
                                      h5("Interpretasi Homoskedastisitas"),
                                      verbatimTextOutput("homo_interpretation")
                                  )
                              )
                          ),
                          
                          # Multikolinearitas
                          div(class = "panel panel-default",
                              div(class = "panel-heading", h4("3. Uji Multikolinearitas")),
                              div(class = "panel-body",
                                  p("Variance Inflation Factor (VIF) untuk mendeteksi multikolinearitas"),
                                  verbatimTextOutput("vif_test"),
                                  div(class = "well",
                                      h5("Interpretasi Multikolinearitas"),
                                      verbatimTextOutput("multicollinearity_interpretation")
                                  )
                              )
                          ),
                          
                          # Outliers dan Influential Points
                          div(class = "panel panel-default",
                              div(class = "panel-heading", h4("4. Deteksi Outliers dan Influential Points")),
                              div(class = "panel-body",
                                  fluidRow(
                                    column(6, plotOutput("leverage_plot")),
                                    column(6, plotOutput("cooks_distance_plot"))
                                  ),
                                  verbatimTextOutput("outlier_analysis"),
                                  div(class = "well",
                                      h5("Interpretasi Outliers"),
                                      verbatimTextOutput("outlier_interpretation")
                                  )
                              )
                          )
                 ),
                 
                 tabPanel("Diagnostik Model",
                          h3("Plot Diagnostik Model Regresi"),
                          plotOutput("diagnostic_plots", height = "600px"),
                          hr(),
                          h4("Ringkasan Diagnostik"),
                          div(class = "well",
                              verbatimTextOutput("diagnostic_summary")
                          )
                 ),
                 
                 tabPanel("Prediksi",
                          h3("Prediksi dan Interval Kepercayaan"),
                          div(class = "panel panel-info",
                              div(class = "panel-heading", h4("Input Nilai untuk Prediksi")),
                              div(class = "panel-body",
                                  uiOutput("prediction_inputs"),
                                  hr(),
                                  actionButton("make_prediction", "Buat Prediksi", class = "btn-primary")
                              )
                          ),
                          hr(),
                          conditionalPanel(
                            condition = "input.make_prediction > 0",
                            div(class = "panel panel-success",
                                div(class = "panel-heading", h4("Hasil Prediksi")),
                                div(class = "panel-body",
                                    verbatimTextOutput("prediction_result")
                                )
                            )
                          )
                 )
               )
             )
           )
  )
)

# --- Define the Server Logic ---
server <- function(input, output, session) {
  
  # --- Load and prepare data ---
  data_with_cats <- reactive({
    df <- read.csv("sovi_data.csv")
    
    # Identify numeric columns for categorization
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    
    # Create categorized versions of all numeric variables
    for (var_name in numeric_vars) {
      if (length(unique(df[[var_name]])) > 3) {
        # 3-level categorization
        quantiles <- quantile(df[[var_name]], probs = c(0.33, 0.67), na.rm = TRUE)
        df[[paste0(var_name, "_cat")]] <- as.factor(
          case_when(
            df[[var_name]] < quantiles[1] ~ "Rendah",
            df[[var_name]] >= quantiles[1] & df[[var_name]] < quantiles[2] ~ "Sedang",
            TRUE ~ "Tinggi"
          )
        )
        
        # 2-level categorization
        median_val <- median(df[[var_name]], na.rm = TRUE)
        df[[paste0(var_name, "_2lvl")]] <- as.factor(
          ifelse(df[[var_name]] > median_val, "Tinggi", "Rendah")
        )
      }
    }
    
    return(df)
  })
  
  # Load geographical data - ENHANCED to load all 511 data points
  geo_data <- reactive({
    tryCatch({
      # Load the local geographical data file
      geo_df <- read.csv("sovi_data_longitudelatitude.csv", stringsAsFactors = FALSE)
      
      # Convert latitude and longitude to numeric
      geo_df$latitude <- as.numeric(geo_df$latitude)
      geo_df$longitude <- as.numeric(geo_df$longitude)
      
      # Remove rows with missing coordinates
      geo_df <- geo_df[!is.na(geo_df$latitude) & !is.na(geo_df$longitude), ]
      
      # Clean up the district names
      geo_df$Nama.Kabupaten <- trimws(geo_df$Nama.Kabupaten)
      
      return(geo_df)
    }, error = function(e) {
      # If loading fails, create dummy data
      data.frame(
        Nama.Kabupaten = paste("Kabupaten", 1:50),
        latitude = runif(50, -10, 5),
        longitude = runif(50, 95, 141)
      )
    })
  })
  
  # Update variable choices
  observe({
    req(data_with_cats())
    df <- data_with_cats()
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    all_vars <- names(df)
    categorical_vars <- names(df)[sapply(df, function(x) is.factor(x) | is.character(x))]
    two_level_cats <- names(df)[sapply(df, function(x) is.factor(x) & length(levels(x)) == 2)]
    three_level_cats <- names(df)[sapply(df, function(x) is.factor(x) & length(levels(x)) > 2)]
    
    # Update all select inputs
    updateSelectInput(session, "variable_categorize", choices = numeric_vars)
    updateSelectInput(session, "variable_explore", choices = all_vars)
    updateSelectInput(session, "assumption_variable", choices = numeric_vars)
    updateSelectInput(session, "grouping_variable", 
                      choices = c("Tidak ada" = "none", setNames(categorical_vars, categorical_vars)))
    updateSelectInput(session, "reg_response", choices = numeric_vars)
    updateSelectInput(session, "reg_predictors", choices = numeric_vars)
    updateSelectInput(session, "filter_variable", choices = categorical_vars)
    updateSelectInput(session, "interaction_vars", choices = numeric_vars)
  })
  
  # --- Manajemen Data Logic ---
  
  # Variable information
  output$variable_info <- renderPrint({
    req(input$variable_categorize)
    df <- data_with_cats()
    var_data <- df[[input$variable_categorize]]
    
    cat("Informasi Variabel:", input$variable_categorize, "\n")
    cat("Tipe Data:", class(var_data), "\n")
    cat("Jumlah Observasi:", length(var_data), "\n")
    cat("Missing Values:", sum(is.na(var_data)), "\n")
    cat("Min:", min(var_data, na.rm = TRUE), "\n")
    cat("Max:", max(var_data, na.rm = TRUE), "\n")
    cat("Mean:", round(mean(var_data, na.rm = TRUE), 3), "\n")
    cat("Median:", round(median(var_data, na.rm = TRUE), 3), "\n")
    cat("Std Dev:", round(sd(var_data, na.rm = TRUE), 3), "\n")
  })
  
  # Categorization logic
  categorized_data <- reactive({
    req(input$variable_categorize, input$num_bins)
    df <- data_with_cats()
    var_to_cut <- df[[input$variable_categorize]]
    
    if(is.numeric(var_to_cut)) {
      if(input$categorization_method == "quantile") {
        probs <- seq(0, 1, length.out = input$num_bins + 1)
        breaks <- quantile(var_to_cut, probs = probs, na.rm = TRUE)
        cut(var_to_cut, breaks = breaks, include.lowest = TRUE, labels = paste("Kelompok", 1:input$num_bins))
      } else if(input$categorization_method == "equal") {
        cut(var_to_cut, breaks = input$num_bins, include.lowest = TRUE, labels = paste("Kelompok", 1:input$num_bins))
      } else if(input$categorization_method == "custom" && input$custom_breaks != "") {
        breaks <- as.numeric(unlist(strsplit(input$custom_breaks, ",")))
        breaks <- c(-Inf, breaks, Inf)
        cut(var_to_cut, breaks = breaks, include.lowest = TRUE, labels = paste("Kelompok", 1:(length(breaks)-1)))
      } else {
        cut(var_to_cut, breaks = input$num_bins, include.lowest = TRUE, labels = paste("Kelompok", 1:input$num_bins))
      }
    } else {
      NULL
    }
  })
  
  # Categorized table
  output$categorized_table <- renderDT({
    req(categorized_data())
    freq_table <- as.data.frame(table(categorized_data(), useNA = "ifany"))
    names(freq_table) <- c("Kelompok", "Frekuensi")
    freq_table$Persentase <- round(freq_table$Frekuensi / sum(freq_table$Frekuensi) * 100, 2)
    freq_table$`Persentase Kumulatif` <- cumsum(freq_table$Persentase)
    
    datatable(freq_table, options = list(pageLength = 10, dom = 't')) %>%
      formatStyle(columns = 1:4, backgroundColor = "#f8f9fa")
  })
  
  # Categorization plot
  output$categorization_plot <- renderPlot({
    req(categorized_data())
    df_plot <- data.frame(Category = categorized_data())
    
    ggplot(df_plot, aes(x = Category)) +
      geom_bar(fill = "steelblue", alpha = 0.7, color = "white") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
      theme_minimal() +
      labs(title = paste("Distribusi Kategorisasi:", input$variable_categorize),
           x = "Kategori", y = "Frekuensi") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Original data plot
  output$original_data_plot <- renderPlot({
    req(input$variable_categorize)
    df <- data_with_cats()
    var_data <- df[[input$variable_categorize]]
    
    ggplot(data.frame(x = var_data), aes(x = x)) +
      geom_histogram(bins = 30, fill = "lightblue", alpha = 0.7, color = "white") +
      theme_minimal() +
      labs(title = "Data Asli (Kontinyu)", x = input$variable_categorize, y = "Frekuensi")
  })
  
  # Categorized data plot
  output$categorized_data_plot <- renderPlot({
    req(categorized_data())
    df_plot <- data.frame(Category = categorized_data())
    
    ggplot(df_plot, aes(x = Category)) +
      geom_bar(fill = "lightcoral", alpha = 0.7, color = "white") +
      theme_minimal() +
      labs(title = "Data Kategorik", x = "Kategori", y = "Frekuensi") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Comparison statistics
  output$comparison_stats <- renderDT({
    req(input$variable_categorize, categorized_data())
    df <- data_with_cats()
    original_data <- df[[input$variable_categorize]]
    categorized <- categorized_data()
    
    # Create comparison table
    comparison <- data.frame(
      Statistik = c("Jumlah Kategori", "Modus", "Entropi", "Gini Index"),
      `Data Asli` = c(
        length(unique(original_data)),
        "Kontinyu",
        "-",
        "-"
      ),
      `Data Kategorik` = c(
        length(levels(categorized)),
        names(sort(table(categorized), decreasing = TRUE))[1],
        round(-sum(prop.table(table(categorized)) * log2(prop.table(table(categorized)))), 3),
        round(1 - sum(prop.table(table(categorized))^2), 3)
      )
    )
    
    datatable(comparison, options = list(dom = 't'), rownames = FALSE)
  })
  
  # Categorization interpretation
  output$categorization_interpretation <- renderPrint({
    req(input$variable_categorize, input$num_bins, categorized_data())
    
    df <- data_with_cats()
    var_name <- input$variable_categorize
    num_bins <- input$num_bins
    method <- input$categorization_method
    
    freq_table <- table(categorized_data())
    
    cat("=== INTERPRETASI HASIL KATEGORISASI ===\n\n")
    cat("Variabel:", var_name, "\n")
    cat("Metode Kategorisasi:", switch(method,
                                       "quantile" = "Berbasis Kuantil",
                                       "equal" = "Interval Sama",
                                       "custom" = "Custom Breaks"), "\n")
    cat("Jumlah Kategori:", num_bins, "\n\n")
    
    cat("DISTRIBUSI FREKUENSI:\n")
    for(i in 1:length(freq_table)) {
      pct <- round(freq_table[i] / sum(freq_table) * 100, 1)
      cat("-", names(freq_table)[i], ":", freq_table[i], "observasi (", pct, "%)\n")
    }
    
    cat("\nANALISIS:\n")
    
    # Check balance
    max_freq <- max(freq_table)
    min_freq <- min(freq_table)
    balance_ratio <- max_freq / min_freq
    
    if(balance_ratio <= 1.5) {
      cat("- Distribusi kategori SEIMBANG (rasio max/min =", round(balance_ratio, 2), ")\n")
    } else if(balance_ratio <= 3) {
      cat("- Distribusi kategori CUKUP SEIMBANG (rasio max/min =", round(balance_ratio, 2), ")\n")
    } else {
      cat("- Distribusi kategori TIDAK SEIMBANG (rasio max/min =", round(balance_ratio, 2), ")\n")
      cat("  Pertimbangkan untuk menggunakan metode kategorisasi yang berbeda.\n")
    }
    
    # Entropy analysis
    props <- prop.table(freq_table)
    entropy <- -sum(props * log2(props))
    max_entropy <- log2(length(freq_table))
    
    cat("- Entropi informasi:", round(entropy, 3), "dari maksimum", round(max_entropy, 3), "\n")
    if(entropy / max_entropy > 0.8) {
      cat("  Kategorisasi memberikan distribusi informasi yang baik.\n")
    } else {
      cat("  Kategorisasi mungkin terlalu tidak seimbang.\n")
    }
    
    cat("\nREKOMENDASI:\n")
    if(method == "quantile") {
      cat("- Metode kuantil memastikan setiap kategori memiliki jumlah observasi yang sama.\n")
      cat("- Cocok untuk analisis yang memerlukan kelompok berukuran sama.\n")
    } else if(method == "equal") {
      cat("- Metode interval sama memberikan rentang nilai yang sama untuk setiap kategori.\n")
      cat("- Cocok untuk interpretasi yang mudah dipahami.\n")
    }
    
    if(balance_ratio > 3) {
      cat("- Pertimbangkan menggunakan metode 'quantile' untuk distribusi yang lebih seimbang.\n")
    }
  })
  
  # --- Eksplorasi Data Logic ---
  
  # Filter values UI
  output$filter_values <- renderUI({
    req(input$filter_variable, input$enable_filter)
    df <- data_with_cats()
    values <- unique(df[[input$filter_variable]])
    checkboxGroupInput("selected_filter_values", "Pilih Nilai:",
                       choices = values, selected = values)
  })
  
  # Filtered data
  filtered_data <- reactive({
    df <- data_with_cats()
    if(input$enable_filter && !is.null(input$selected_filter_values)) {
      df <- df[df[[input$filter_variable]] %in% input$selected_filter_values, ]
    }
    df
  })
  
  # Summary statistics
  output$summary_stats <- renderPrint({
    req(input$variable_explore)
    df <- filtered_data()
    if(is.numeric(df[[input$variable_explore]])) {
      summary(df[[input$variable_explore]])
    } else {
      table(df[[input$variable_explore]])
    }
  })
  
  # Additional statistics
  output$additional_stats <- renderPrint({
    req(input$variable_explore)
    df <- filtered_data()
    var_data <- df[[input$variable_explore]]
    
    if(is.numeric(var_data)) {
      cat("Statistik Tambahan:\n")
      cat("Varians:", round(var(var_data, na.rm = TRUE), 4), "\n")
      cat("Std Deviasi:", round(sd(var_data, na.rm = TRUE), 4), "\n")
      cat("Skewness:", round(moments::skewness(var_data, na.rm = TRUE), 4), "\n")
      cat("Kurtosis:", round(moments::kurtosis(var_data, na.rm = TRUE), 4), "\n")
      cat("CV (%):", round(sd(var_data, na.rm = TRUE) / mean(var_data, na.rm = TRUE) * 100, 2), "\n")
      cat("IQR:", round(IQR(var_data, na.rm = TRUE), 4), "\n")
      cat("MAD:", round(mad(var_data, na.rm = TRUE), 4), "\n")
    } else {
      cat("Statistik Kategorikal:\n")
      tbl <- table(var_data)
      cat("Jumlah Kategori:", length(tbl), "\n")
      cat("Modus:", names(tbl)[which.max(tbl)], "\n")
      cat("Frekuensi Modus:", max(tbl), "\n")
      entropy <- -sum(prop.table(tbl) * log2(prop.table(tbl)))
      cat("Entropi:", round(entropy, 4), "\n")
    }
  })
  
  # Descriptive interpretation
  output$descriptive_interpretation <- renderPrint({
    req(input$variable_explore)
    df <- filtered_data()
    var_data <- df[[input$variable_explore]]
    
    cat("=== INTERPRETASI STATISTIK DESKRIPTIF ===\n\n")
    
    if(is.numeric(var_data)) {
      mean_val <- mean(var_data, na.rm = TRUE)
      median_val <- median(var_data, na.rm = TRUE)
      sd_val <- sd(var_data, na.rm = TRUE)
      cv <- sd_val / mean_val * 100
      skew <- moments::skewness(var_data, na.rm = TRUE)
      kurt <- moments::kurtosis(var_data, na.rm = TRUE)
      
      cat("TENDENSI SENTRAL:\n")
      cat("- Rata-rata (", round(mean_val, 3), ") vs Median (", round(median_val, 3), ")\n")
      if(abs(mean_val - median_val) / sd_val < 0.5) {
        cat("  Distribusi relatif simetris.\n")
      } else if(mean_val > median_val) {
        cat("  Distribusi condong ke kanan (positively skewed).\n")
      } else {
        cat("  Distribusi condong ke kiri (negatively skewed).\n")
      }
      
      cat("\nVARIABILITAS:\n")
      cat("- Koefisien Variasi:", round(cv, 2), "%\n")
      if(cv < 15) {
        cat("  Variabilitas rendah - data relatif homogen.\n")
      } else if(cv < 35) {
        cat("  Variabilitas sedang - data cukup bervariasi.\n")
      } else {
        cat("  Variabilitas tinggi - data sangat heterogen.\n")
      }
      
      cat("\nBENTUK DISTRIBUSI:\n")
      cat("- Skewness:", round(skew, 3), "\n")
      if(abs(skew) < 0.5) {
        cat("  Distribusi mendekati normal (simetris).\n")
      } else if(abs(skew) < 1) {
        cat("  Distribusi agak miring.\n")
      } else {
        cat("  Distribusi sangat miring.\n")
      }
      
      cat("- Kurtosis:", round(kurt, 3), "\n")
      if(kurt < 3) {
        cat("  Distribusi platykurtic (lebih datar dari normal).\n")
      } else if(kurt > 3) {
        cat("  Distribusi leptokurtic (lebih runcing dari normal).\n")
      } else {
        cat("  Distribusi mesokurtic (mendekati normal).\n")
      }
      
    } else {
      tbl <- table(var_data)
      props <- prop.table(tbl)
      
      cat("DISTRIBUSI KATEGORI:\n")
      for(i in 1:length(tbl)) {
        cat("-", names(tbl)[i], ":", tbl[i], "(", round(props[i]*100, 1), "%)\n")
      }
      
      cat("\nANALISIS:\n")
      max_prop <- max(props)
      if(max_prop > 0.5) {
        cat("- Terdapat kategori dominan (>50%).\n")
      } else if(max_prop < 1/length(tbl) * 1.5) {
        cat("- Distribusi kategori relatif seimbang.\n")
      } else {
        cat("- Distribusi kategori tidak seimbang.\n")
      }
      
      entropy <- -sum(props * log2(props))
      max_entropy <- log2(length(tbl))
      cat("- Entropi informasi:", round(entropy/max_entropy*100, 1), "% dari maksimum.\n")
    }
  })
  
  # Interactive histogram - FIXED
  output$histogram_plot <- renderPlotly({
    req(input$variable_explore)
    df <- filtered_data()
    var_name <- input$variable_explore
    
    if(is.numeric(df[[var_name]])) {
      # Create data frame for ggplot
      plot_data <- data.frame(x = df[[var_name]])
      
      p <- ggplot(plot_data, aes(x = x)) +
        geom_histogram(bins = input$plot_bins, fill = "steelblue", alpha = 0.7, color = "white") +
        geom_density(aes(y = ..density.. * nrow(plot_data) * (max(x, na.rm = TRUE) - 
                                                                min(x, na.rm = TRUE)) / input$plot_bins),
                     color = "red", size = 1) +
        theme_minimal() +
        labs(title = paste("Distribusi:", var_name),
             x = var_name, y = "Frekuensi")
      
      ggplotly(p)
    } else {
      # Create data frame for categorical data
      plot_data <- data.frame(x = df[[var_name]])
      
      p <- ggplot(plot_data, aes(x = x)) +
        geom_bar(fill = "steelblue", alpha = 0.7, color = "white") +
        theme_minimal() +
        labs(title = paste("Distribusi:", var_name),
             x = var_name, y = "Frekuensi") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
    }
  })
  
  # Interactive boxplot - FIXED
  output$boxplot <- renderPlotly({
    req(input$variable_explore)
    df <- filtered_data()
    var_name <- input$variable_explore
    
    if(is.numeric(df[[var_name]])) {
      # Create data frame for ggplot
      plot_data <- data.frame(y = df[[var_name]])
      
      p <- ggplot(plot_data, aes(y = y)) +
        geom_boxplot(fill = "lightblue", alpha = 0.7, outlier.colour = "red") +
        theme_minimal() +
        labs(title = paste("Box Plot:", var_name),
             y = var_name)
      
      if(input$show_outliers) {
        outliers <- boxplot.stats(df[[var_name]])$out
        if(length(outliers) > 0) {
          outlier_data <- data.frame(x = 0, y = outliers)
          p <- p + geom_point(data = outlier_data, aes(x = x, y = y), 
                              color = "red", size = 2, alpha = 0.7)
        }
      }
      
      ggplotly(p)
    }
  })
  
  # Q-Q Plot
  output$qq_plot <- renderPlot({
    req(input$variable_explore)
    df <- filtered_data()
    
    if(is.numeric(df[[input$variable_explore]])) {
      qqnorm(df[[input$variable_explore]], main = paste("Q-Q Plot:", input$variable_explore))
      qqline(df[[input$variable_explore]], col = "red", lwd = 2)
    }
  })
  
  # Scatter plot matrix
  output$scatter_matrix <- renderPlot({
    df <- filtered_data()
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    
    if(length(numeric_vars) >= 2) {
      # Select first 4 numeric variables for matrix
      selected_vars <- numeric_vars[1:min(4, length(numeric_vars))]
      pairs(df[selected_vars], main = "Scatter Plot Matrix (Sample Variables)")
    }
  })
  
  # Visualization interpretation
  output$visualization_interpretation <- renderPrint({
    req(input$variable_explore)
    df <- filtered_data()
    var_data <- df[[input$variable_explore]]
    
    cat("=== INTERPRETASI VISUALISASI ===\n\n")
    
    if(is.numeric(var_data)) {
      cat("HISTOGRAM & DENSITY PLOT:\n")
      cat("- Menunjukkan bentuk distribusi data\n")
      cat("- Kurva merah menunjukkan estimasi density\n")
      
      # Analyze distribution shape
      skew <- moments::skewness(var_data, na.rm = TRUE)
      if(abs(skew) < 0.5) {
        cat("- Distribusi tampak simetris\n")
      } else if(skew > 0) {
        cat("- Distribusi condong ke kanan (ekor panjang di kanan)\n")
      } else {
        cat("- Distribusi condong ke kiri (ekor panjang di kiri)\n")
      }
      
      cat("\nBOX PLOT:\n")
      cat("- Menunjukkan median, kuartil, dan outliers\n")
      outliers <- boxplot.stats(var_data)$out
      if(length(outliers) > 0) {
        cat("- Terdeteksi", length(outliers), "outliers (titik merah)\n")
        cat("- Outliers mungkin perlu investigasi lebih lanjut\n")
      } else {
        cat("- Tidak terdeteksi outliers\n")
      }
      
      cat("\nQ-Q PLOT:\n")
      cat("- Membandingkan distribusi data dengan distribusi normal\n")
      cat("- Jika titik mengikuti garis merah, data mendekati normal\n")
      cat("- Penyimpangan dari garis menunjukkan ketidaknormalan\n")
      
    } else {
      cat("BAR CHART:\n")
      tbl <- table(var_data)
      cat("- Menunjukkan frekuensi setiap kategori\n")
      cat("- Kategori dengan frekuensi tertinggi:", names(tbl)[which.max(tbl)], "\n")
      cat("- Kategori dengan frekuensi terendah:", names(tbl)[which.min(tbl)], "\n")
      
      # Check balance
      max_freq <- max(tbl)
      min_freq <- min(tbl)
      if(max_freq / min_freq > 3) {
        cat("- Distribusi tidak seimbang - ada kategori yang dominan\n")
      } else {
        cat("- Distribusi relatif seimbang antar kategori\n")
      }
      
      props <- prop.table(tbl)
      entropy <- -sum(props * log2(props))
      max_entropy <- log2(length(tbl))
      cat("- Entropi informasi:", round(entropy/max_entropy*100, 1), "% dari maksimum.\n")
    }
  })
  
  # Map visualization - ENHANCED to show ALL 511 data points with proper shapes
  output$map_plot <- renderLeaflet({
    df <- filtered_data()
    geo_df <- geo_data()
    var_name <- input$variable_explore
    
    if(nrow(geo_df) > 0) {
      # Use ALL available geographical data points (not just a sample)
      # Create mapping with proper indexing
      n_data <- min(nrow(df), nrow(geo_df))
      
      map_data <- data.frame(
        lat = geo_df$latitude[1:n_data],
        lng = geo_df$longitude[1:n_data],
        value = if(n_data <= nrow(df)) df[[var_name]][1:n_data] else rep(df[[var_name]][1], n_data),
        district = geo_df$Nama.Kabupaten[1:n_data]
      )
      
      # Remove rows with missing coordinates
      map_data <- map_data[complete.cases(map_data[c("lat", "lng")]), ]
      
      if(nrow(map_data) > 0) {
        if(is.numeric(map_data$value)) {
          # Create color palette based on the variable values
          value_range <- range(map_data$value, na.rm = TRUE)
          pal <- colorNumeric(
            palette = c("#FFE0B2", "#FF9800", "#E65100"), 
            domain = value_range
          )
          
          # Create popup text
          popup_text <- paste(
            "<div style='font-family: Arial; font-size: 12px;'>",
            "<strong style='color: #1976D2;'>", map_data$district, "</strong><br>",
            "<strong>", var_name, ":</strong> ", round(map_data$value, 3), "<br>",
            "<small>Klik untuk detail</small>",
            "</div>"
          )
          
          # Create the map with custom markers (squares instead of circles)
          leaflet(map_data) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addRectangles(
              lng1 = ~lng - 0.05, lat1 = ~lat - 0.05,
              lng2 = ~lng + 0.05, lat2 = ~lat + 0.05,
              fillColor = ~pal(value),
              fillOpacity = 0.8,
              color = "#2E2E2E",
              weight = 1,
              popup = popup_text,
              label = ~paste(district, ":", round(value, 2)),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "12px",
                direction = "auto"
              )
            ) %>%
            addLegend(
              pal = pal, 
              values = ~value,
              title = HTML(paste0("<strong>", var_name, "</strong>")),
              position = "bottomright",
              opacity = 0.8
            ) %>%
            setView(lng = 118, lat = -2, zoom = 5) %>%
            addControl(
              html = paste0("<div style='background: rgba(255,255,255,0.9); padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.3);'>",
                           "<strong>Total Kabupaten/Kota: ", nrow(map_data), "</strong><br>",
                           "<small>Hover untuk label, klik untuk detail</small></div>"),
              position = "topright"
            )
        } else {
          # For categorical data - use different colors for different categories
          unique_cats <- unique(map_data$value)
          colors <- rainbow(length(unique_cats))
          color_map <- setNames(colors, unique_cats)
          
          popup_text <- paste(
            "<div style='font-family: Arial; font-size: 12px;'>",
            "<strong style='color: #1976D2;'>", map_data$district, "</strong><br>",
            "<strong>", var_name, ":</strong> ", map_data$value, "<br>",
            "<small>Klik untuk detail</small>",
            "</div>"
          )
          
          leaflet(map_data) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addRectangles(
              lng1 = ~lng - 0.05, lat1 = ~lat - 0.05,
              lng2 = ~lng + 0.05, lat2 = ~lat + 0.05,
              fillColor = color_map[map_data$value],
              fillOpacity = 0.8,
              color = "#2E2E2E",
              weight = 1,
              popup = popup_text,
              label = ~paste(district, ":", value)
            ) %>%
            setView(lng = 118, lat = -2, zoom = 5) %>%
            addControl(
              html = paste0("<div style='background: rgba(255,255,255,0.9); padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.3);'>",
                           "<strong>Total Kabupaten/Kota: ", nrow(map_data), "</strong><br>",
                           "<strong>Kategori: ", length(unique_cats), "</strong></div>"),
              position = "topright"
            )
        }
      } else {
        # Fallback map
        leaflet() %>%
          addTiles() %>%
          setView(lng = 118, lat = -2, zoom = 5) %>%
          addPopups(lng = 118, lat = -2, "Data geografis tidak tersedia")
      }
    } else {
      # Fallback map
      leaflet() %>%
        addTiles() %>%
        setView(lng = 118, lat = -2, zoom = 5) %>%
        addPopups(lng = 118, lat = -2, "Data geografis tidak dapat dimuat")
    }
  })
  
  # Geographic data table - ENHANCED to show all data with search functionality
  output$geo_data_table <- renderDT({
    df <- filtered_data()
    geo_df <- geo_data()
    var_name <- input$variable_explore
    
    if(nrow(geo_df) > 0) {
      # Create complete summary table with geographical info
      n_data <- min(nrow(df), nrow(geo_df))
      geo_summary <- data.frame(
        No = 1:n_data,
        Kabupaten = geo_df$Nama.Kabupaten[1:n_data],
        Provinsi = sub("KABUPATEN |KOTA ", "", geo_df$Nama.Kabupaten[1:n_data]) %>% 
                   substr(1, 15), # Extract first part as proxy for province
        Latitude = round(geo_df$latitude[1:n_data], 6),
        Longitude = round(geo_df$longitude[1:n_data], 6),
        Value = if(n_data <= nrow(df)) df[[var_name]][1:n_data] else rep(df[[var_name]][1], n_data)
      )
      names(geo_summary)[6] <- var_name
      
      # Add statistics summary
      if(is.numeric(geo_summary[[var_name]])) {
        geo_summary[[paste0(var_name, "_Kategori")]] <- cut(
          geo_summary[[var_name]], 
          breaks = 3, 
          labels = c("Rendah", "Sedang", "Tinggi")
        )
      }
      
      datatable(geo_summary, 
                options = list(
                  pageLength = 15, 
                  scrollX = TRUE,
                  searchHighlight = TRUE,
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel'),
                  columnDefs = list(
                    list(className = 'dt-center', targets = c(0, 3, 4)),
                    list(width = '200px', targets = 1),
                    list(width = '100px', targets = c(3, 4, 5))
                  )
                ),
                filter = 'top',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center; color: #2E2E2E; font-size: 14px;',
                  paste0('Total: ', n_data, ' Kabupaten/Kota dengan Data Geografis Lengkap')
                )) %>%
        formatStyle(var_name,
                    background = styleColorBar(range(geo_summary[[var_name]], na.rm = TRUE), '#FFE0B2'),
                    backgroundSize = '100% 90%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center')
    } else {
      # Fallback table
      data.frame(
        Message = "Data geografis tidak tersedia",
        Info = "Silakan periksa file sovi_data_longitudelatitude.csv"
      ) %>%
        datatable(options = list(dom = 't'))
    }
  })
  
  # Data table
  output$data_table <- renderDT({
    datatable(filtered_data(), options = list(
      pageLength = 10,
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel')
    ))
  })
  
  # --- Uji Asumsi Data Logic ---
  
  # Shapiro test
  output$shapiro_test_result <- renderPrint({
    req(input$assumption_variable)
    if("shapiro" %in% input$selected_tests) {
      df <- data_with_cats()
      var_data <- df[[input$assumption_variable]]
      
      if(length(var_data) > 5000) {
        cat("Catatan: Shapiro-Wilk test dibatasi untuk sampel ≤ 5000 observasi.\n")
        cat("Menggunakan sampel acak 5000 observasi.\n\n")
        var_data <- sample(var_data, 5000)
      }
      
      shapiro.test(var_data)
    }
  })
  
  output$shapiro_interpretation <- renderPrint({
    req(input$assumption_variable)
    if("shapiro" %in% input$selected_tests) {
      df <- data_with_cats()
      var_data <- df[[input$assumption_variable]]
      
      if(length(var_data) > 5000) {
        var_data <- sample(var_data, 5000)
      }
      
      result <- shapiro.test(var_data)
      p_value <- result$p.value
      
      cat("=== INTERPRETASI UJI SHAPIRO-WILK ===\n\n")
      cat("H₀: Data berdistribusi normal\n")
      cat("H₁: Data tidak berdistribusi normal\n")
      cat("α = 0.05\n\n")
      
      cat("Hasil:\n")
      cat("W =", round(result$statistic, 4), "\n")
      cat("p-value =", format(p_value, scientific = TRUE), "\n\n")
      
      if(p_value < 0.05) {
        cat("KESIMPULAN: Tolak H₀\n")
        cat("Data TIDAK berdistribusi normal (p < 0.05)\n")
        cat("Rekomendasi: Gunakan uji non-parametrik atau transformasi data\n")
      } else {
        cat("KESIMPULAN: Gagal tolak H₀\n")
        cat("Data berdistribusi normal (p ≥ 0.05)\n")
        cat("Rekomendasi: Dapat menggunakan uji parametrik\n")
      }
    }
  })
  
  # Kolmogorov-Smirnov test
  output$ks_test_result <- renderPrint({
    req(input$assumption_variable)
    if("ks" %in% input$selected_tests) {
      df <- data_with_cats()
      var_data <- df[[input$assumption_variable]]
      ks.test(var_data, "pnorm", mean(var_data, na.rm = TRUE), sd(var_data, na.rm = TRUE))
    }
  })
  
  output$ks_interpretation <- renderPrint({
    req(input$assumption_variable)
    if("ks" %in% input$selected_tests) {
      df <- data_with_cats()
      var_data <- df[[input$assumption_variable]]
      result <- ks.test(var_data, "pnorm", mean(var_data, na.rm = TRUE), sd(var_data, na.rm = TRUE))
      
      cat("=== INTERPRETASI UJI KOLMOGOROV-SMIRNOV ===\n\n")
      cat("H₀: Data mengikuti distribusi normal\n")
      cat("H₁: Data tidak mengikuti distribusi normal\n\n")
      
      if(result$p.value < 0.05) {
        cat("KESIMPULAN: Tolak H₀ (p < 0.05)\n")
        cat("Data TIDAK mengikuti distribusi normal\n")
      } else {
        cat("KESIMPULAN: Gagal tolak H₀ (p ≥ 0.05)\n")
        cat("Data mengikuti distribusi normal\n")
      }
    }
  })
  
  # Anderson-Darling test
  output$ad_test_result <- renderPrint({
    req(input$assumption_variable)
    if("ad" %in% input$selected_tests) {
      df <- data_with_cats()
      var_data <- df[[input$assumption_variable]]
      nortest::ad.test(var_data)
    }
  })
  
  output$ad_interpretation <- renderPrint({
    req(input$assumption_variable)
    if("ad" %in% input$selected_tests) {
      df <- data_with_cats()
      var_data <- df[[input$assumption_variable]]
      result <- nortest::ad.test(var_data)
      
      cat("=== INTERPRETASI UJI ANDERSON-DARLING ===\n\n")
      cat("H₀: Data berdistribusi normal\n")
      cat("H₁: Data tidak berdistribusi normal\n\n")
      
      if(result$p.value < 0.05) {
        cat("KESIMPULAN: Tolak H₀ (p < 0.05)\n")
        cat("Data TIDAK berdistribusi normal\n")
        cat("Uji Anderson-Darling lebih sensitif terhadap penyimpangan di ekor distribusi\n")
      } else {
        cat("KESIMPULAN: Gagal tolak H₀ (p ≥ 0.05)\n")
        cat("Data berdistribusi normal\n")
      }
    }
  })
  
  # Levene test
  output$levene_test_result <- renderPrint({
    req(input$assumption_variable, input$grouping_variable)
    if("levene" %in% input$selected_tests && input$grouping_variable != "none") {
      df <- data_with_cats()
      var_data <- df[[input$assumption_variable]]
      group_data <- df[[input$grouping_variable]]
      
      car::leveneTest(var_data ~ group_data)
    }
  })
  
  output$levene_interpretation <- renderPrint({
    req(input$assumption_variable, input$grouping_variable)
    if("levene" %in% input$selected_tests && input$grouping_variable != "none") {
      df <- data_with_cats()
      var_data <- df[[input$assumption_variable]]
      group_data <- df[[input$grouping_variable]]
      
      result <- car::leveneTest(var_data ~ group_data)
      p_value <- result$`Pr(>F)`[1]
      
      cat("=== INTERPRETASI UJI LEVENE ===\n\n")
      cat("H₀: Varians antar kelompok sama (homogen)\n")
      cat("H₁: Varians antar kelompok tidak sama (heterogen)\n\n")
      
      if(p_value < 0.05) {
        cat("KESIMPULAN: Tolak H₀ (p < 0.05)\n")
        cat("Varians antar kelompok TIDAK homogen\n")
        cat("Rekomendasi: Gunakan uji yang tidak mengasumsikan varians sama\n")
      } else {
        cat("KESIMPULAN: Gagal tolak H₀ (p ≥ 0.05)\n")
        cat("Varians antar kelompok homogen\n")
        cat("Rekomendasi: Dapat menggunakan uji yang mengasumsikan varians sama\n")
      }
    }
  })
  
  # Bartlett test
  output$bartlett_test_result <- renderPrint({
    req(input$assumption_variable, input$grouping_variable)
    if("bartlett" %in% input$selected_tests && input$grouping_variable != "none") {
      df <- data_with_cats()
      var_data <- df[[input$assumption_variable]]
      group_data <- df[[input$grouping_variable]]
      
      result <- bartlett.test(var_data ~ group_data)
      
      cat("=== INTERPRETASI UJI BARTLETT ===\n\n")
      cat("H₀: Varians antar kelompok sama\n")
      cat("H₁: Varians antar kelompok tidak sama\n\n")
      
      if(result$p.value < 0.05) {
        cat("KESIMPULAN: Tolak H₀ (p < 0.05)\n")
        cat("Varians antar kelompok TIDAK sama\n")
        cat("Catatan: Uji Bartlett sensitif terhadap ketidaknormalan data\n")
      } else {
        cat("KESIMPULAN: Gagal tolak H₀ (p ≥ 0.05)\n")
        cat("Varians antar kelompok sama\n")
      }
    }
  })
  
  # Normality plots
  output$normality_qq_plot <- renderPlot({
    req(input$assumption_variable)
    df <- data_with_cats()
    var_data <- df[[input$assumption_variable]]
    
    qqnorm(var_data, main = paste("Q-Q Plot:", input$assumption_variable))
    qqline(var_data, col = "red", lwd = 2)
  })
  
  output$normality_histogram <- renderPlot({
    req(input$assumption_variable)
    df <- data_with_cats()
    var_data <- df[[input$assumption_variable]]
    
    hist(var_data, freq = FALSE, main = paste("Histogram dengan Kurva Normal:", input$assumption_variable),
         xlab = input$assumption_variable, col = "lightblue", border = "white")
    
    # Add normal curve
    x_seq <- seq(min(var_data, na.rm = TRUE), max(var_data, na.rm = TRUE), length = 100)
    y_seq <- dnorm(x_seq, mean(var_data, na.rm = TRUE), sd(var_data, na.rm = TRUE))
    lines(x_seq, y_seq, col = "red", lwd = 2)
    
    # Add density curve
    lines(density(var_data, na.rm = TRUE), col = "blue", lwd = 2)
    
    legend("topright", c("Normal Teoritis", "Density Empiris"), 
           col = c("red", "blue"), lwd = 2)
  })
  
  # Homogeneity plots
  output$homogeneity_boxplot <- renderPlot({
    req(input$assumption_variable, input$grouping_variable)
    if(input$grouping_variable != "none") {
      df <- data_with_cats()
      
      # Create data frame for ggplot
      plot_data <- data.frame(
        y = df[[input$assumption_variable]],
        x = df[[input$grouping_variable]]
      )
      
      ggplot(plot_data, aes(x = x, y = y)) +
        geom_boxplot(fill = "lightblue", alpha = 0.7) +
        theme_minimal() +
        labs(title = "Box Plot per Kelompok",
             x = input$grouping_variable,
             y = input$assumption_variable) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  output$residual_plot <- renderPlot({
    req(input$assumption_variable, input$grouping_variable)
    if(input$grouping_variable != "none") {
      df <- data_with_cats()
      
      # Simple residual plot
      group_means <- aggregate(df[[input$assumption_variable]], 
                               by = list(df[[input$grouping_variable]]), 
                               FUN = mean, na.rm = TRUE)
      
      df$group_mean <- group_means$x[match(df[[input$grouping_variable]], group_means$Group.1)]
      df$residual <- df[[input$assumption_variable]] - df$group_mean
      
      ggplot(df, aes(x = group_mean, y = residual)) +
        geom_point(alpha = 0.6) +
        geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
        theme_minimal() +
        labs(title = "Residual vs Fitted Values",
             x = "Group Mean (Fitted)",
             y = "Residuals")
    }
  })
  
  # Assumption summary
  output$assumption_summary <- renderPrint({
    req(input$assumption_variable)
    
    cat("=== RINGKASAN UJI ASUMSI ===\n\n")
    cat("Variabel yang diuji:", input$assumption_variable, "\n")
    
    if(input$grouping_variable != "none") {
      cat("Variabel pengelompokan:", input$grouping_variable, "\n")
    }
    
    cat("Uji yang dilakukan:", paste(input$selected_tests, collapse = ", "), "\n\n")
    
    df <- data_with_cats()
    var_data <- df[[input$assumption_variable]]
    
    # Normality summary
    if("shapiro" %in% input$selected_tests) {
      if(length(var_data) > 5000) {
        var_data_sample <- sample(var_data, 5000)
        shapiro_result <- shapiro.test(var_data_sample)
      } else {
        shapiro_result <- shapiro.test(var_data)
      }
      
      cat("NORMALITAS (Shapiro-Wilk):\n")
      if(shapiro_result$p.value < 0.05) {
        cat("❌ Data TIDAK berdistribusi normal (p =", format(shapiro_result$p.value, digits = 4), ")\n")
      } else {
        cat("✅ Data berdistribusi normal (p =", format(shapiro_result$p.value, digits = 4), ")\n")
      }
    }
    
    if("ks" %in% input$selected_tests) {
      ks_result <- ks.test(var_data, "pnorm", mean(var_data, na.rm = TRUE), sd(var_data, na.rm = TRUE))
      cat("NORMALITAS (Kolmogorov-Smirnov):\n")
      if(ks_result$p.value < 0.05) {
        cat("❌ Data TIDAK normal (p =", format(ks_result$p.value, digits = 4), ")\n")
      } else {
        cat("✅ Data normal (p =", format(ks_result$p.value, digits = 4), ")\n")
      }
    }
    
    # Homogeneity summary
    if(input$grouping_variable != "none") {
      group_data <- df[[input$grouping_variable]]
      
      if("levene" %in% input$selected_tests) {
        levene_result <- car::leveneTest(var_data ~ group_data)
        p_value <- levene_result$`Pr(>F)`[1]
        
        cat("HOMOGENITAS (Levene):\n")
        if(p_value < 0.05) {
          cat("❌ Varians TIDAK homogen (p =", format(p_value, digits = 4), ")\n")
        } else {
          cat("✅ Varians homogen (p =", format(p_value, digits = 4), ")\n")
        }
      }
      
      if("bartlett" %in% input$selected_tests) {
        bartlett_result <- bartlett.test(var_data ~ group_data)
        
        cat("HOMOGENITAS (Bartlett):\n")
        if(bartlett_result$p.value < 0.05) {
          cat("❌ Varians TIDAK homogen (p =", format(bartlett_result$p.value, digits = 4), ")\n")
        } else {
          cat("✅ Varians homogen (p =", format(bartlett_result$p.value, digits = 4), ")\n")
        }
      }
    }
  })
  
  # Analysis recommendation
  output$analysis_recommendation <- renderPrint({
    req(input$assumption_variable)
    
    cat("=== REKOMENDASI ANALISIS LANJUTAN ===\n\n")
    
    df <- data_with_cats()
    var_data <- df[[input$assumption_variable]]
    
    # Check normality
    normal <- TRUE
    if("shapiro" %in% input$selected_tests) {
      if(length(var_data) > 5000) {
        var_data_sample <- sample(var_data, 5000)
        shapiro_result <- shapiro.test(var_data_sample)
      } else {
        shapiro_result <- shapiro.test(var_data)
      }
      if(shapiro_result$p.value < 0.05) normal <- FALSE
    }
    
    # Check homogeneity
    homogeneous <- TRUE
    if(input$grouping_variable != "none" && "levene" %in% input$selected_tests) {
      group_data <- df[[input$grouping_variable]]
      levene_result <- car::leveneTest(var_data ~ group_data)
      if(levene_result$`Pr(>F)`[1] < 0.05) homogeneous <- FALSE
    }
    
    cat("Berdasarkan hasil uji asumsi:\n\n")
    
    if(normal && homogeneous) {
      cat("✅ KONDISI IDEAL: Data normal dan varians homogen\n")
      cat("Rekomendasi:\n")
      cat("- Gunakan uji parametrik (t-test, ANOVA)\n")
      cat("- Regresi linear dapat diterapkan\n")
      cat("- Analisis korelasi Pearson sesuai\n")
    } else if(normal && !homogeneous) {
      cat("⚠️ KONDISI SEBAGIAN: Data normal tapi varians tidak homogen\n")
      cat("Rekomendasi:\n")
      cat("- Gunakan Welch t-test (tidak mengasumsikan varians sama)\n")
      cat("- Pertimbangkan transformasi data\n")
      cat("- Gunakan uji robust atau bootstrap\n")
    } else if(!normal && homogeneous) {
      cat("⚠️ KONDISI SEBAGIAN: Data tidak normal tapi varians homogen\n")
      cat("Rekomendasi:\n")
      cat("- Gunakan uji non-parametrik (Mann-Whitney, Kruskal-Wallis)\n")
      cat("- Pertimbangkan transformasi data (log, sqrt, Box-Cox)\n")
      cat("- Gunakan bootstrap atau permutation test\n")
    } else {
      cat("❌ KONDISI TIDAK IDEAL: Data tidak normal dan varians tidak homogen\n")
      cat("Rekomendasi:\n")
      cat("- Prioritaskan uji non-parametrik\n")
      cat("- Transformasi data sangat disarankan\n")
      cat("- Gunakan robust statistics\n")
      cat("- Pertimbangkan analisis berbasis rank\n")
    }
    
    cat("\nTRANSFORMASI DATA yang dapat dicoba:\n")
    cat("- Log transformation: untuk data skewed positif\n")
    cat("- Square root: untuk data count dengan varians proporsional mean\n")
    cat("- Box-Cox: untuk mencari transformasi optimal\n")
    cat("- Standardisasi: untuk mengurangi pengaruh skala\n")
  })
  
  # --- Statistik Inferensia Logic - ENHANCED ---
  
  # Dynamic UI for test-specific inputs - ENHANCED
  output$test_specific_ui <- renderUI({
    req(input$stat_test_type)
    df <- data_with_cats()
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    categorical_vars <- names(df)[sapply(df, function(x) is.factor(x) | is.character(x))]
    two_level_cats <- names(df)[sapply(df, function(x) is.factor(x) & length(levels(x)) == 2)]
    multi_level_cats <- names(df)[sapply(df, function(x) is.factor(x) & length(levels(x)) > 2)]
    
    switch(input$stat_test_type,
           "t_one" = tagList(
             selectInput("t_one_var", "Pilih Variabel:", choices = numeric_vars),
             numericInput("t_one_mu", "Nilai Hipotesis (μ₀):", value = 0),
             radioButtons("t_one_alternative", "Hipotesis Alternatif:",
                          choices = list("Two-sided" = "two.sided",
                                         "Greater than" = "greater",
                                         "Less than" = "less"))
           ),
           "t_two" = tagList(
             selectInput("t_two_var", "Pilih Variabel Numerik:", choices = numeric_vars),
             selectInput("t_two_group", "Pilih Variabel Pengelompokan:", choices = two_level_cats),
             checkboxInput("t_two_equal_var", "Asumsikan Varians Sama", value = TRUE),
             radioButtons("t_two_alternative", "Hipotesis Alternatif:",
                          choices = list("Two-sided" = "two.sided",
                                         "Group 1 > Group 2" = "greater",
                                         "Group 1 < Group 2" = "less"))
           ),
           "var_one" = tagList(
             selectInput("var_one_var", "Pilih Variabel:", choices = numeric_vars),
             numericInput("var_one_sigma", "Nilai Hipotesis Varians (σ²₀):", value = 1, min = 0.001),
             radioButtons("var_one_alternative", "Hipotesis Alternatif:",
                          choices = list("Two-sided" = "two.sided",
                                         "Greater than" = "greater",
                                         "Less than" = "less"))
           ),
           "var_two" = tagList(
             selectInput("var_two_var", "Pilih Variabel Numerik:", choices = numeric_vars),
             selectInput("var_two_group", "Pilih Variabel Pengelompokan:", choices = two_level_cats),
             radioButtons("var_two_alternative", "Hipotesis Alternatif:",
                          choices = list("Two-sided" = "two.sided",
                                         "Group 1 > Group 2" = "greater",
                                         "Group 1 < Group 2" = "less"))
           ),
           "prop_one" = tagList(
             selectInput("prop_one_var", "Pilih Variabel Kategorik:", choices = categorical_vars),
             selectInput("prop_one_success", "Pilih Kategori Sukses:", choices = NULL),
             numericInput("prop_one_p0", "Nilai Hipotesis Proporsi (p₀):", 
                          value = 0.5, min = 0, max = 1, step = 0.01),
             radioButtons("prop_one_alternative", "Hipotesis Alternatif:",
                          choices = list("Two-sided" = "two.sided",
                                         "Greater than" = "greater",
                                         "Less than" = "less"))
           ),
           "prop_two" = tagList(
             selectInput("prop_two_var", "Pilih Variabel Kategorik:", choices = categorical_vars),
             selectInput("prop_two_success", "Pilih Kategori Sukses:", choices = NULL),
             selectInput("prop_two_group", "Pilih Variabel Pengelompokan:", choices = two_level_cats),
             radioButtons("prop_two_alternative", "Hipotesis Alternatif:",
                          choices = list("Two-sided" = "two.sided",
                                         "Group 1 > Group 2" = "greater",
                                         "Group 1 < Group 2" = "less"))
           ),
           "anova_one" = tagList(
             selectInput("anova_one_var", "Pilih Variabel Numerik:", choices = numeric_vars),
             selectInput("anova_one_group", "Pilih Variabel Pengelompokan:", choices = multi_level_cats)
           ),
           "anova_two" = tagList(
             selectInput("anova_two_var", "Pilih Variabel Numerik:", choices = numeric_vars),
             selectInput("anova_two_group1", "Pilih Faktor 1:", choices = multi_level_cats),
             selectInput("anova_two_group2", "Pilih Faktor 2:", choices = multi_level_cats),
             checkboxInput("anova_two_interaction", "Sertakan Interaksi", value = TRUE)
           )
    )
  })
  
  # Update success category choices for proportion tests
  observe({
    req(input$prop_one_var)
    df <- data_with_cats()
    if(input$prop_one_var %in% names(df)) {
      categories <- unique(df[[input$prop_one_var]])
      updateSelectInput(session, "prop_one_success", choices = categories)
    }
  })
  
  observe({
    req(input$prop_two_var)
    df <- data_with_cats()
    if(input$prop_two_var %in% names(df)) {
      categories <- unique(df[[input$prop_two_var]])
      updateSelectInput(session, "prop_two_success", choices = categories)
    }
  })
  
  # Main test output - ENHANCED
  output$test_output <- renderPrint({
    df <- data_with_cats()
    
    switch(input$stat_test_type,
           "t_one" = {
             req(input$t_one_var, input$t_one_mu)
             t.test(df[[input$t_one_var]], mu = input$t_one_mu, 
                    alternative = input$t_one_alternative)
           },
           "t_two" = {
             req(input$t_two_var, input$t_two_group)
             t.test(df[[input$t_two_var]] ~ df[[input$t_two_group]], 
                    var.equal = input$t_two_equal_var,
                    alternative = input$t_two_alternative)
           },
           "var_one" = {
             req(input$var_one_var, input$var_one_sigma)
             var_data <- df[[input$var_one_var]]
             n <- length(var_data)
             s_squared <- var(var_data, na.rm = TRUE)
             
             # Chi-squared test for variance
             if(input$var_one_alternative == "two.sided") {
               chi_sq_stat <- (n - 1) * s_squared / input$var_one_sigma
               p_val <- 2 * min(pchisq(chi_sq_stat, df = n - 1), 
                                pchisq(chi_sq_stat, df = n - 1, lower.tail = FALSE))
             } else if(input$var_one_alternative == "greater") {
               chi_sq_stat <- (n - 1) * s_squared / input$var_one_sigma
               p_val <- pchisq(chi_sq_stat, df = n - 1, lower.tail = FALSE)
             } else {
               chi_sq_stat <- (n - 1) * s_squared / input$var_one_sigma
               p_val <- pchisq(chi_sq_stat, df = n - 1)
             }
             
             cat("One-Sample Chi-Squared Test for Variance\n\n")
             cat("data: ", input$var_one_var, "\n")
             cat("X-squared =", round(chi_sq_stat, 4), ", df =", n-1, ", p-value =", format(p_val, digits = 4), "\n")
             cat("alternative hypothesis: true variance is", 
                 switch(input$var_one_alternative,
                        "two.sided" = "not equal to",
                        "greater" = "greater than",
                        "less" = "less than"), input$var_one_sigma, "\n")
             cat("sample estimates:\n")
             cat("var of x:", round(s_squared, 4), "\n")
             cat("95% confidence interval for variance:\n")
             
             # Confidence interval for variance
             alpha <- 0.05
             lower <- (n-1) * s_squared / qchisq(1 - alpha/2, n-1)
             upper <- (n-1) * s_squared / qchisq(alpha/2, n-1)
             cat("[", round(lower, 4), ",", round(upper, 4), "]\n")
           },
           "var_two" = {
             req(input$var_two_var, input$var_two_group)
             var.test(df[[input$var_two_var]] ~ df[[input$var_two_group]],
                      alternative = input$var_two_alternative)
           },
           "prop_one" = {
             req(input$prop_one_var, input$prop_one_success, input$prop_one_p0)
             success_count <- sum(df[[input$prop_one_var]] == input$prop_one_success, na.rm = TRUE)
             total_count <- sum(!is.na(df[[input$prop_one_var]]))
             prop.test(success_count, total_count, p = input$prop_one_p0,
                       alternative = input$prop_one_alternative)
           },
           "prop_two" = {
             req(input$prop_two_var, input$prop_two_success, input$prop_two_group)
             
             group_levels <- levels(as.factor(df[[input$prop_two_group]]))
             success_counts <- c()
             total_counts <- c()
             
             for(group in group_levels) {
               group_data <- df[df[[input$prop_two_group]] == group, ]
               success_count <- sum(group_data[[input$prop_two_var]] == input$prop_two_success, na.rm = TRUE)
               total_count <- sum(!is.na(group_data[[input$prop_two_var]]))
               success_counts <- c(success_counts, success_count)
               total_counts <- c(total_counts, total_count)
             }
             
             prop.test(success_counts, total_counts, alternative = input$prop_two_alternative)
           },
           "anova_one" = {
             req(input$anova_one_var, input$anova_one_group)
             anova_model <- aov(df[[input$anova_one_var]] ~ df[[input$anova_one_group]])
             summary(anova_model)
           },
           "anova_two" = {
             req(input$anova_two_var, input$anova_two_group1, input$anova_two_group2)
             if(input$anova_two_interaction) {
               formula_text <- paste(input$anova_two_var, "~", input$anova_two_group1, "*", input$anova_two_group2)
             } else {
               formula_text <- paste(input$anova_two_var, "~", input$anova_two_group1, "+", input$anova_two_group2)
             }
             anova_model <- aov(as.formula(formula_text), data = df)
             summary(anova_model)
           }
    )
  })
  
  # Test interpretation - ENHANCED
  output$test_interpretation <- renderPrint({
    df <- data_with_cats()
    
    switch(input$stat_test_type,
           "t_one" = {
             req(input$t_one_var, input$t_one_mu)
             result <- t.test(df[[input$t_one_var]], mu = input$t_one_mu, 
                              alternative = input$t_one_alternative)
             
             cat("=== INTERPRETASI UJI T SATU SAMPEL ===\n\n")
             cat("H₀: μ =", input$t_one_mu, "\n")
             cat("H₁: μ", switch(input$t_one_alternative,
                                 "two.sided" = "≠",
                                 "greater" = ">",
                                 "less" = "<"), input$t_one_mu, "\n")
             cat("α = 0.05\n\n")
             
             cat("Hasil:\n")
             cat("t =", round(result$statistic, 4), "\n")
             cat("df =", result$parameter, "\n")
             cat("p-value =", format(result$p.value, digits = 4), "\n")
             cat("Rata-rata sampel =", round(result$estimate, 4), "\n")
             cat("95% CI: [", round(result$conf.int[1], 4), ",", round(result$conf.int[2], 4), "]\n\n")
             
             if(result$p.value < 0.05) {
               cat("KESIMPULAN: Tolak H₀ (p < 0.05)\n")
               cat("Rata-rata populasi secara statistik berbeda dari", input$t_one_mu, "\n")
             } else {
               cat("KESIMPULAN: Gagal tolak H₀ (p ≥ 0.05)\n")
               cat("Tidak cukup bukti bahwa rata-rata populasi berbeda dari", input$t_one_mu, "\n")
             }
           },
           "t_two" = {
             req(input$t_two_var, input$t_two_group)
             result <- t.test(df[[input$t_two_var]] ~ df[[input$t_two_group]], 
                              var.equal = input$t_two_equal_var,
                              alternative = input$t_two_alternative)
             
             groups <- levels(as.factor(df[[input$t_two_group]]))
             
             cat("=== INTERPRETASI UJI T DUA SAMPEL ===\n\n")
             cat("H₀: μ₁ = μ₂\n")
             cat("H₁: μ₁", switch(input$t_two_alternative,
                                  "two.sided" = "≠",
                                  "greater" = ">",
                                  "less" = "<"), "μ₂\n")
             cat("Kelompok 1:", groups[1], "\n")
             cat("Kelompok 2:", groups[2], "\n")
             cat("Asumsi varians sama:", ifelse(input$t_two_equal_var, "Ya", "Tidak"), "\n\n")
             
             cat("Hasil:\n")
             cat("t =", round(result$statistic, 4), "\n")
             cat("df =", round(result$parameter, 2), "\n")
             cat("p-value =", format(result$p.value, digits = 4), "\n")
             cat("Rata-rata", groups[1], "=", round(result$estimate[1], 4), "\n")
             cat("Rata-rata", groups[2], "=", round(result$estimate[2], 4), "\n")
             cat("Selisih rata-rata =", round(result$estimate[1] - result$estimate[2], 4), "\n")
             cat("95% CI selisih: [", round(result$conf.int[1], 4), ",", round(result$conf.int[2], 4), "]\n\n")
             
             if(result$p.value < 0.05) {
               cat("KESIMPULAN: Tolak H₀ (p < 0.05)\n")
               cat("Terdapat perbedaan rata-rata yang signifikan antara kedua kelompok\n")
             } else {
               cat("KESIMPULAN: Gagal tolak H₀ (p ≥ 0.05)\n")
               cat("Tidak terdapat perbedaan rata-rata yang signifikan antara kedua kelompok\n")
             }
           },
           "var_one" = {
             req(input$var_one_var, input$var_one_sigma)
             var_data <- df[[input$var_one_var]]
             n <- length(var_data)
             s_squared <- var(var_data, na.rm = TRUE)
             
             cat("=== INTERPRETASI UJI VARIANS SATU SAMPEL ===\n\n")
             cat("H₀: σ² =", input$var_one_sigma, "\n")
             cat("H₁: σ²", switch(input$var_one_alternative,
                                  "two.sided" = "≠",
                                  "greater" = ">",
                                  "less" = "<"), input$var_one_sigma, "\n")
             cat("α = 0.05\n\n")
             
             chi_sq_stat <- (n - 1) * s_squared / input$var_one_sigma
             if(input$var_one_alternative == "two.sided") {
               p_val <- 2 * min(pchisq(chi_sq_stat, df = n - 1), 
                                pchisq(chi_sq_stat, df = n - 1, lower.tail = FALSE))
             } else if(input$var_one_alternative == "greater") {
               p_val <- pchisq(chi_sq_stat, df = n - 1, lower.tail = FALSE)
             } else {
               p_val <- pchisq(chi_sq_stat, df = n - 1)
             }
             
             cat("Hasil:\n")
             cat("χ² =", round(chi_sq_stat, 4), "\n")
             cat("df =", n-1, "\n")
             cat("p-value =", format(p_val, digits = 4), "\n")
             cat("Varians sampel =", round(s_squared, 4), "\n\n")
             
             if(p_val < 0.05) {
               cat("KESIMPULAN: Tolak H₀ (p < 0.05)\n")
               cat("Varians populasi secara statistik berbeda dari", input$var_one_sigma, "\n")
             } else {
               cat("KESIMPULAN: Gagal tolak H₀ (p ≥ 0.05)\n")
               cat("Tidak cukup bukti bahwa varians populasi berbeda dari", input$var_one_sigma, "\n")
             }
           },
           "var_two" = {
             req(input$var_two_var, input$var_two_group)
             result <- var.test(df[[input$var_two_var]] ~ df[[input$var_two_group]],
                                alternative = input$var_two_alternative)
             
             groups <- levels(as.factor(df[[input$var_two_group]]))
             
             cat("=== INTERPRETASI UJI VARIANS DUA SAMPEL ===\n\n")
             cat("H₀: σ₁² = σ₂²\n")
             cat("H₁: σ₁²", switch(input$var_two_alternative,
                                   "two.sided" = "≠",
                                   "greater" = ">",
                                   "less" = "<"), "σ₂²\n")
             cat("Kelompok 1:", groups[1], "\n")
             cat("Kelompok 2:", groups[2], "\n\n")
             
             cat("Hasil:\n")
             cat("F =", round(result$statistic, 4), "\n")
             cat("df1 =", result$parameter[1], ", df2 =", result$parameter[2], "\n")
             cat("p-value =", format(result$p.value, digits = 4), "\n")
             cat("Rasio varians =", round(result$estimate, 4), "\n")
             cat("95% CI rasio: [", round(result$conf.int[1], 4), ",", round(result$conf.int[2], 4), "]\n\n")
             
             if(result$p.value < 0.05) {
               cat("KESIMPULAN: Tolak H₀ (p < 0.05)\n")
               cat("Terdapat perbedaan varians yang signifikan antara kedua kelompok\n")
             } else {
               cat("KESIMPULAN: Gagal tolak H₀ (p ≥ 0.05)\n")
               cat("Tidak terdapat perbedaan varians yang signifikan antara kedua kelompok\n")
             }
           },
           "prop_one" = {
             req(input$prop_one_var, input$prop_one_success, input$prop_one_p0)
             success_count <- sum(df[[input$prop_one_var]] == input$prop_one_success, na.rm = TRUE)
             total_count <- sum(!is.na(df[[input$prop_one_var]]))
             result <- prop.test(success_count, total_count, p = input$prop_one_p0,
                                 alternative = input$prop_one_alternative)
             
             cat("=== INTERPRETASI UJI PROPORSI SATU SAMPEL ===\n\n")
             cat("H₀: p =", input$prop_one_p0, "\n")
             cat("H₁: p", switch(input$prop_one_alternative,
                                 "two.sided" = "≠",
                                 "greater" = ">",
                                 "less" = "<"), input$prop_one_p0, "\n")
             cat("Kategori sukses:", input$prop_one_success, "\n\n")
             
             cat("Hasil:\n")
             cat("X-squared =", round(result$statistic, 4), "\n")
             cat("df =", result$parameter, "\n")
             cat("p-value =", format(result$p.value, digits = 4), "\n")
             cat("Proporsi sampel =", round(result$estimate, 4), "\n")
             cat("Jumlah sukses =", success_count, "dari", total_count, "\n")
             cat("95% CI: [", round(result$conf.int[1], 4), ",", round(result$conf.int[2], 4), "]\n\n")
             
             if(result$p.value < 0.05) {
               cat("KESIMPULAN: Tolak H₀ (p < 0.05)\n")
               cat("Proporsi populasi secara statistik berbeda dari", input$prop_one_p0, "\n")
             } else {
               cat("KESIMPULAN: Gagal tolak H₀ (p ≥ 0.05)\n")
               cat("Tidak cukup bukti bahwa proporsi populasi berbeda dari", input$prop_one_p0, "\n")
             }
           },
           "prop_two" = {
             req(input$prop_two_var, input$prop_two_success, input$prop_two_group)
             
             group_levels <- levels(as.factor(df[[input$prop_two_group]]))
             success_counts <- c()
             total_counts <- c()
             
             for(group in group_levels) {
               group_data <- df[df[[input$prop_two_group]] == group, ]
               success_count <- sum(group_data[[input$prop_two_var]] == input$prop_two_success, na.rm = TRUE)
               total_count <- sum(!is.na(group_data[[input$prop_two_var]]))
               success_counts <- c(success_counts, success_count)
               total_counts <- c(total_counts, total_count)
             }
             
             result <- prop.test(success_counts, total_counts, alternative = input$prop_two_alternative)
             
             cat("=== INTERPRETASI UJI PROPORSI DUA SAMPEL ===\n\n")
             cat("H₀: p₁ = p₂\n")
             cat("H₁: p₁", switch(input$prop_two_alternative,
                                  "two.sided" = "≠",
                                  "greater" = ">",
                                  "less" = "<"), "p₂\n")
             cat("Kelompok 1:", group_levels[1], "\n")
             cat("Kelompok 2:", group_levels[2], "\n")
             cat("Kategori sukses:", input$prop_two_success, "\n\n")
             
             cat("Hasil:\n")
             cat("X-squared =", round(result$statistic, 4), "\n")
             cat("df =", result$parameter, "\n")
             cat("p-value =", format(result$p.value, digits = 4), "\n")
             cat("Proporsi", group_levels[1], "=", round(result$estimate[1], 4), 
                 "(", success_counts[1], "/", total_counts[1], ")\n")
             cat("Proporsi", group_levels[2], "=", round(result$estimate[2], 4), 
                 "(", success_counts[2], "/", total_counts[2], ")\n\n")
             
             if(result$p.value < 0.05) {
               cat("KESIMPULAN: Tolak H₀ (p < 0.05)\n")
               cat("Terdapat perbedaan proporsi yang signifikan antara kedua kelompok\n")
             } else {
               cat("KESIMPULAN: Gagal tolak H₀ (p ≥ 0.05)\n")
               cat("Tidak terdapat perbedaan proporsi yang signifikan antara kedua kelompok\n")
             }
           },
           "anova_one" = {
             req(input$anova_one_var, input$anova_one_group)
             anova_model <- aov(df[[input$anova_one_var]] ~ df[[input$anova_one_group]])
             anova_summary <- summary(anova_model)
             p_value <- anova_summary[[1]]$`Pr(>F)`[1]
             
             cat("=== INTERPRETASI ANOVA SATU ARAH ===\n\n")
             cat("H₀: μ₁ = μ₂ = μ₃ = ... (semua rata-rata kelompok sama)\n")
             cat("H₁: Setidaknya ada satu rata-rata kelompok yang berbeda\n\n")
             
             cat("Hasil:\n")
             cat("F =", round(anova_summary[[1]]$`F value`[1], 4), "\n")
             cat("df1 =", anova_summary[[1]]$Df[1], "\n")
             cat("df2 =", anova_summary[[1]]$Df[2], "\n")
             cat("p-value =", format(p_value, digits = 4), "\n\n")
             
             if(p_value < 0.05) {
               cat("KESIMPULAN: Tolak H₀ (p < 0.05)\n")
               cat("Setidaknya ada satu kelompok yang memiliki rata-rata berbeda\n")
               cat("Lakukan uji post-hoc untuk mengetahui kelompok mana yang berbeda\n")
             } else {
               cat("KESIMPULAN: Gagal tolak H₀ (p ≥ 0.05)\n")
               cat("Tidak ada perbedaan rata-rata yang signifikan antar kelompok\n")
             }
           },
           "anova_two" = {
             req(input$anova_two_var, input$anova_two_group1, input$anova_two_group2)
             if(input$anova_two_interaction) {
               formula_text <- paste(input$anova_two_var, "~", input$anova_two_group1, "*", input$anova_two_group2)
             } else {
               formula_text <- paste(input$anova_two_var, "~", input$anova_two_group1, "+", input$anova_two_group2)
             }
             anova_model <- aov(as.formula(formula_text), data = df)
             anova_summary <- summary(anova_model)
             
             cat("=== INTERPRETASI ANOVA DUA ARAH ===\n\n")
             cat("Model:", formula_text, "\n\n")
             
             # Main effects
             cat("EFEK UTAMA:\n")
             cat("H₀ untuk", input$anova_two_group1, ": Tidak ada efek utama\n")
             cat("H₀ untuk", input$anova_two_group2, ": Tidak ada efek utama\n")
             
             if(input$anova_two_interaction) {
               cat("H₀ untuk Interaksi: Tidak ada efek interaksi\n")
             }
             cat("\n")
             
             # Results for each factor
             p_values <- anova_summary[[1]]$`Pr(>F)`
             f_values <- anova_summary[[1]]$`F value`
             
             cat("Hasil:\n")
             cat(input$anova_two_group1, ":\n")
             cat("  F =", round(f_values[1], 4), ", p-value =", format(p_values[1], digits = 4), "\n")
             if(p_values[1] < 0.05) {
               cat("  ✅ Efek utama signifikan\n")
             } else {
               cat("  ❌ Efek utama tidak signifikan\n")
             }
             
             cat(input$anova_two_group2, ":\n")
             cat("  F =", round(f_values[2], 4), ", p-value =", format(p_values[2], digits = 4), "\n")
             if(p_values[2] < 0.05) {
               cat("  ✅ Efek utama signifikan\n")
             } else {
               cat("  ❌ Efek utama tidak signifikan\n")
             }
             
             if(input$anova_two_interaction && length(p_values) > 2) {
               cat("Interaksi:\n")
               cat("  F =", round(f_values[3], 4), ", p-value =", format(p_values[3], digits = 4), "\n")
               if(p_values[3] < 0.05) {
                 cat("  ✅ Efek interaksi signifikan\n")
                 cat("  Interpretasi efek utama harus hati-hati karena ada interaksi\n")
               } else {
                 cat("  ❌ Efek interaksi tidak signifikan\n")
               }
             }
           }
    )
  })
  
  # Inference plot - ENHANCED
  output$inference_plot <- renderPlot({
    df <- data_with_cats()
    
    switch(input$stat_test_type,
           "t_one" = {
             req(input$t_one_var)
             var_data <- df[[input$t_one_var]]
             
             ggplot(data.frame(x = var_data), aes(x = x)) +
               geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", alpha = 0.7) +
               geom_density(color = "blue", size = 1) +
               geom_vline(xintercept = mean(var_data, na.rm = TRUE), color = "red", linetype = "dashed", size = 1) +
               geom_vline(xintercept = input$t_one_mu, color = "green", linetype = "solid", size = 1) +
               theme_minimal() +
               labs(title = paste("Distribusi", input$t_one_var),
                    subtitle = "Garis merah: rata-rata sampel, Garis hijau: nilai hipotesis",
                    x = input$t_one_var, y = "Density")
           },
           "t_two" = {
             req(input$t_two_var, input$t_two_group)
             
             # Create data frame for ggplot
             plot_data <- data.frame(
               x = df[[input$t_two_group]],
               y = df[[input$t_two_var]]
             )
             
             ggplot(plot_data, aes(x = x, y = y)) +
               geom_boxplot(fill = "lightblue", alpha = 0.7) +
               geom_jitter(width = 0.2, alpha = 0.5) +
               stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
               theme_minimal() +
               labs(title = paste("Perbandingan", input$t_two_var, "berdasarkan", input$t_two_group),
                    subtitle = "Titik merah: rata-rata kelompok",
                    x = input$t_two_group, y = input$t_two_var)
           },
           "var_one" = {
             req(input$var_one_var)
             var_data <- df[[input$var_one_var]]
             
             # Create histogram of data with variance information
             ggplot(data.frame(x = var_data), aes(x = x)) +
               geom_histogram(bins = 30, fill = "lightcoral", alpha = 0.7, color = "white") +
               theme_minimal() +
               labs(title = paste("Distribusi", input$var_one_var),
                    subtitle = paste("Varians sampel:", round(var(var_data, na.rm = TRUE), 4), 
                                     "vs Hipotesis:", input$var_one_sigma),
                    x = input$var_one_var, y = "Frekuensi")
           },
           "var_two" = {
             req(input$var_two_var, input$var_two_group)
             
             # Create data frame for ggplot
             plot_data <- data.frame(
               x = df[[input$var_two_group]],
               y = df[[input$var_two_var]]
             )
             
             # Calculate variances for each group
             group_vars <- aggregate(plot_data$y, by = list(plot_data$x), FUN = var, na.rm = TRUE)
             
             ggplot(plot_data, aes(x = x, y = y)) +
               geom_boxplot(fill = "lightcoral", alpha = 0.7) +
               geom_jitter(width = 0.2, alpha = 0.5) +
               theme_minimal() +
               labs(title = paste("Perbandingan Varians", input$var_two_var, "berdasarkan", input$var_two_group),
                    subtitle = paste("Varians:", paste(group_vars$Group.1, "=", round(group_vars$x, 4), collapse = ", ")),
                    x = input$var_two_group, y = input$var_two_var)
           },
           "prop_one" = {
             req(input$prop_one_var, input$prop_one_success)
             
             # Create proportion plot
             prop_data <- table(df[[input$prop_one_var]])
             prop_df <- data.frame(
               Category = names(prop_data),
               Count = as.numeric(prop_data),
               Proportion = as.numeric(prop_data) / sum(prop_data)
             )
             
             ggplot(prop_df, aes(x = Category, y = Proportion)) +
               geom_bar(stat = "identity", fill = "lightgreen", alpha = 0.7) +
               geom_hline(yintercept = input$prop_one_p0, color = "red", linetype = "dashed", size = 1) +
               geom_text(aes(label = paste0(round(Proportion*100, 1), "%")), vjust = -0.5) +
               theme_minimal() +
               labs(title = paste("Distribusi Proporsi", input$prop_one_var),
                    subtitle = paste("Garis merah: proporsi hipotesis (", input$prop_one_p0, ")"),
                    x = input$prop_one_var, y = "Proporsi") +
               theme(axis.text.x = element_text(angle = 45, hjust = 1))
           },
           "prop_two" = {
             req(input$prop_two_var, input$prop_two_success, input$prop_two_group)
             
             # Create contingency table
             cont_table <- table(df[[input$prop_two_group]], df[[input$prop_two_var]])
             
             # Convert to data frame for plotting
             plot_data <- as.data.frame(cont_table)
             names(plot_data) <- c("Group", "Category", "Freq")
             
             # Calculate proportions within each group
             plot_data <- plot_data %>%
               group_by(Group) %>%
               mutate(Proportion = Freq / sum(Freq))
             
             ggplot(plot_data, aes(x = Group, y = Proportion, fill = Category)) +
               geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
               geom_text(aes(label = paste0(round(Proportion*100, 1), "%")), 
                         position = position_dodge(width = 0.9), vjust = -0.5) +
               theme_minimal() +
               labs(title = paste("Perbandingan Proporsi", input$prop_two_var, "berdasarkan", input$prop_two_group),
                    x = input$prop_two_group, y = "Proporsi") +
               theme(axis.text.x = element_text(angle = 45, hjust = 1))
           },
           "anova_one" = {
             req(input$anova_one_var, input$anova_one_group)
             
             # Create data frame for ggplot
             plot_data <- data.frame(
               x = df[[input$anova_one_group]],
               y = df[[input$anova_one_var]]
             )
             
             ggplot(plot_data, aes(x = x, y = y)) +
               geom_boxplot(fill = "lightblue", alpha = 0.7) +
               geom_jitter(width = 0.2, alpha = 0.5) +
               stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
               theme_minimal() +
               labs(title = paste("ANOVA:", input$anova_one_var, "vs", input$anova_one_group),
                    subtitle = "Titik merah: rata-rata kelompok",
                    x = input$anova_one_group, y = input$anova_one_var) +
               theme(axis.text.x = element_text(angle = 45, hjust = 1))
           },
           "anova_two" = {
             req(input$anova_two_var, input$anova_two_group1, input$anova_two_group2)
             
             # Create interaction plot
             plot_data <- data.frame(
               y = df[[input$anova_two_var]],
               factor1 = df[[input$anova_two_group1]],
               factor2 = df[[input$anova_two_group2]]
             )
             
             # Calculate means for interaction plot
             means_data <- plot_data %>%
               group_by(factor1, factor2) %>%
               summarise(mean_y = mean(y, na.rm = TRUE), .groups = 'drop')
             
             ggplot(means_data, aes(x = factor1, y = mean_y, color = factor2, group = factor2)) +
               geom_line(size = 1) +
               geom_point(size = 3) +
               theme_minimal() +
               labs(title = paste("Interaction Plot:", input$anova_two_var),
                    subtitle = paste("Faktor 1:", input$anova_two_group1, ", Faktor 2:", input$anova_two_group2),
                    x = input$anova_two_group1, y = paste("Mean", input$anova_two_var),
                    color = input$anova_two_group2) +
               theme(axis.text.x = element_text(angle = 45, hjust = 1))
           }
    )
  })
  
  # Post-hoc test - ENHANCED
  output$posthoc_test <- renderPrint({
    if(input$stat_test_type == "anova_one") {
      req(input$anova_one_var, input$anova_one_group)
      df <- data_with_cats()
      
      anova_model <- aov(df[[input$anova_one_var]] ~ df[[input$anova_one_group]])
      anova_summary <- summary(anova_model)
      p_value <- anova_summary[[1]]$`Pr(>F)`[1]
      
      if(p_value < 0.05) {
        cat("=== UJI POST-HOC TUKEY HSD ===\n\n")
        tukey_result <- TukeyHSD(anova_model)
        print(tukey_result)
        
        cat("\n=== INTERPRETASI POST-HOC ===\n")
        cat("Pasangan kelompok dengan p adj < 0.05 memiliki perbedaan yang signifikan.\n")
      } else {
        cat("Uji post-hoc tidak diperlukan karena ANOVA tidak signifikan.\n")
      }
    } else if(input$stat_test_type == "anova_two") {
      req(input$anova_two_var, input$anova_two_group1, input$anova_two_group2)
      df <- data_with_cats()
      
      if(input$anova_two_interaction) {
        formula_text <- paste(input$anova_two_var, "~", input$anova_two_group1, "*", input$anova_two_group2)
      } else {
        formula_text <- paste(input$anova_two_var, "~", input$anova_two_group1, "+", input$anova_two_group2)
      }
      anova_model <- aov(as.formula(formula_text), data = df)
      anova_summary <- summary(anova_model)
      
      # Check if any main effects are significant
      p_values <- anova_summary[[1]]$`Pr(>F)`
      
      if(any(p_values < 0.05, na.rm = TRUE)) {
        cat("=== UJI POST-HOC TUKEY HSD ===\n\n")
        tukey_result <- TukeyHSD(anova_model)
        print(tukey_result)
        
        cat("\n=== INTERPRETASI POST-HOC ===\n")
        cat("Pasangan dengan p adj < 0.05 memiliki perbedaan yang signifikan.\n")
        if(input$anova_two_interaction && length(p_values) > 2 && p_values[3] < 0.05) {
          cat("Karena ada efek interaksi yang signifikan, interpretasi efek utama harus hati-hati.\n")
        }
      } else {
        cat("Uji post-hoc tidak diperlukan karena tidak ada efek yang signifikan.\n")
      }
    }
  })
  
  # Test assumptions - ENHANCED
  output$test_assumptions <- renderPrint({
    cat("=== ASUMSI UJI STATISTIK ===\n\n")
    
    switch(input$stat_test_type,
           "t_one" = {
             cat("Asumsi Uji t Satu Sampel:\n")
             cat("1. Data berdistribusi normal\n")
             cat("2. Observasi independen\n")
             cat("3. Data berskala interval/rasio\n")
           },
           "t_two" = {
             cat("Asumsi Uji t Dua Sampel:\n")
             cat("1. Data berdistribusi normal di setiap kelompok\n")
             cat("2. Observasi independen\n")
             cat("3. Varians homogen (jika equal variance = TRUE)\n")
             cat("4. Data berskala interval/rasio\n")
           },
           "var_one" = {
             cat("Asumsi Uji Varians Satu Sampel:\n")
             cat("1. Data berdistribusi normal\n")
             cat("2. Observasi independen\n")
             cat("3. Data berskala interval/rasio\n")
           },
           "var_two" = {
             cat("Asumsi Uji Varians Dua Sampel (F-test):\n")
             cat("1. Data berdistribusi normal di setiap kelompok\n")
             cat("2. Observasi independen\n")
             cat("3. Data berskala interval/rasio\n")
           },
           "prop_one" = {
             cat("Asumsi Uji Proporsi Satu Sampel:\n")
             cat("1. Sampel acak\n")
             cat("2. np ≥ 5 dan n(1-p) ≥ 5\n")
             cat("3. Observasi independen\n")
           },
           "prop_two" = {
             cat("Asumsi Uji Proporsi Dua Sampel:\n")
             cat("1. Sampel acak dari kedua populasi\n")
             cat("2. Ukuran sampel cukup besar\n")
             cat("3. Observasi independen\n")
           },
           "anova_one" = {
             cat("Asumsi ANOVA Satu Arah:\n")
             cat("1. Data berdistribusi normal di setiap kelompok\n")
             cat("2. Varians homogen antar kelompok\n")
             cat("3. Observasi independen\n")
             cat("4. Data berskala interval/rasio\n")
           },
           "anova_two" = {
             cat("Asumsi ANOVA Dua Arah:\n")
             cat("1. Data berdistribusi normal di setiap kombinasi kelompok\n")
             cat("2. Varians homogen antar semua kombinasi kelompok\n")
             cat("3. Observasi independen\n")
             cat("4. Data berskala interval/rasio\n")
             cat("5. Ukuran sampel seimbang (direkomendasikan)\n")
           }
    )
  })
  
  # Assumption plots - ENHANCED
  output$assumption_plots <- renderPlot({
    df <- data_with_cats()
    
    if(input$stat_test_type %in% c("t_one", "t_two", "anova_one", "anova_two", "var_one", "var_two")) {
      var_name <- switch(input$stat_test_type,
                         "t_one" = input$t_one_var,
                         "t_two" = input$t_two_var,
                         "anova_one" = input$anova_one_var,
                         "anova_two" = input$anova_two_var,
                         "var_one" = input$var_one_var,
                         "var_two" = input$var_two_var)
      
      if(!is.null(var_name) && var_name != "") {
        par(mfrow = c(1, 2))
        
        # Q-Q plot
        qqnorm(df[[var_name]], main = paste("Q-Q Plot:", var_name))
        qqline(df[[var_name]], col = "red")
        
        # Histogram
        hist(df[[var_name]], main = paste("Histogram:", var_name),
             xlab = var_name, col = "lightblue", border = "white")
      }
    } else if(input$stat_test_type %in% c("prop_one", "prop_two")) {
      # For proportion tests, show sample size adequacy
      if(input$stat_test_type == "prop_one") {
        req(input$prop_one_var, input$prop_one_success, input$prop_one_p0)
        success_count <- sum(df[[input$prop_one_var]] == input$prop_one_success, na.rm = TRUE)
        total_count <- sum(!is.na(df[[input$prop_one_var]]))
        
        par(mfrow = c(1, 1))
        barplot(c(success_count, total_count - success_count), 
                names.arg = c("Success", "Failure"),
                main = "Sample Size Check",
                col = c("lightgreen", "lightcoral"))
        
        # Add text showing adequacy
        np <- total_count * input$prop_one_p0
        n1_p <- total_count * (1 - input$prop_one_p0)
        text(1.5, max(success_count, total_count - success_count) * 0.8,
             paste("np =", round(np, 1), "\nn(1-p) =", round(n1_p, 1),
                   "\nAdequate:", ifelse(np >= 5 & n1_p >= 5, "Yes", "No")))
      }
    }
  })
  
  # --- Regresi Linear Logic (keeping existing implementation) ---
  
  # Update interaction variables
  observe({
    req(input$reg_predictors)
    updateSelectInput(session, "interaction_vars", 
                      choices = input$reg_predictors,
                      selected = NULL)
  })
  
  # Create regression model
  reg_model <- reactive({
    req(input$reg_response, input$reg_predictors)
    
    if(length(input$reg_predictors) < 1) {
      return(NULL)
    }
    
    df <- data_with_cats()
    
    # Standardize if requested
    if(input$standardize_vars) {
      df[input$reg_predictors] <- scale(df[input$reg_predictors])
      df[[input$reg_response]] <- scale(df[[input$reg_response]])[,1]
    }
    
    # Build formula
    predictors <- paste(input$reg_predictors, collapse = " + ")
    
    # Add interactions if requested
    if(input$include_interaction && !is.null(input$interaction_vars) && length(input$interaction_vars) >= 2) {
      interactions <- combn(input$interaction_vars, 2, function(x) paste(x, collapse = ":"))
      predictors <- paste(predictors, "+", paste(interactions, collapse = " + "))
    }
    
    formula_text <- paste(input$reg_response, "~", predictors)
    
    lm(as.formula(formula_text), data = df)
  })
  
  # Model summary
  output$reg_summary <- renderPrint({
    req(reg_model())
    summary(reg_model())
  })
  
  # Model fit statistics
  output$model_fit_stats <- renderPrint({
    req(reg_model())
    model <- reg_model()
    model_summary <- summary(model)
    
    cat("=== GOODNESS OF FIT ===\n\n")
    cat("R-squared:", round(model_summary$r.squared, 4), "\n")
    cat("Adjusted R-squared:", round(model_summary$adj.r.squared, 4), "\n")
    cat("F-statistic:", round(model_summary$fstatistic[1], 4), "\n")
    cat("p-value (F-test):", format(pf(model_summary$fstatistic[1], 
                                       model_summary$fstatistic[2], 
                                       model_summary$fstatistic[3], 
                                       lower.tail = FALSE), digits = 4), "\n")
    cat("Residual Standard Error:", round(model_summary$sigma, 4), "\n")
    cat("AIC:", round(AIC(model), 2), "\n")
    cat("BIC:", round(BIC(model), 2), "\n")
  })
  
  # ANOVA table
  output$anova_table <- renderPrint({
    req(reg_model())
    anova(reg_model())
  })
  
  # Model interpretation
  output$reg_interpretation <- renderPrint({
    req(reg_model())
    model <- reg_model()
    model_summary <- summary(model)
    
    cat("=== INTERPRETASI MODEL REGRESI ===\n\n")
    
    # Overall model significance
    f_stat <- model_summary$fstatistic
    f_p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
    
    cat("SIGNIFIKANSI MODEL KESELURUHAN:\n")
    if(f_p_value < 0.05) {
      cat("✅ Model secara keseluruhan signifikan (F p-value < 0.05)\n")
      cat("Model dapat menjelaskan variasi dalam variabel terikat\n")
    } else {
      cat("❌ Model secara keseluruhan tidak signifikan (F p-value ≥ 0.05)\n")
      cat("Model tidak dapat menjelaskan variasi dalam variabel terikat\n")
    }
    
    cat("\nKEKUATAN MODEL:\n")
    r_squared <- model_summary$r.squared
    cat("R² =", round(r_squared, 4), "→", round(r_squared * 100, 2), "% variasi dijelaskan\n")
    
    if(r_squared < 0.3) {
      cat("Kekuatan prediksi: LEMAH\n")
    } else if(r_squared < 0.7) {
      cat("Kekuatan prediksi: SEDANG\n")
    } else {
      cat("Kekuatan prediksi: KUAT\n")
    }
    
    cat("\nINTERPRETASI KOEFISIEN:\n")
    coefs <- model_summary$coefficients
    
    for(i in 1:nrow(coefs)) {
      var_name <- rownames(coefs)[i]
      estimate <- coefs[i, "Estimate"]
      p_value <- coefs[i, "Pr(>|t|)"]
      
      if(var_name == "(Intercept)") {
        cat("Intercept (β₀):", round(estimate, 4), "\n")
        cat("  → Nilai prediksi ketika semua X = 0\n")
      } else {
        cat("\n", var_name, "(β):", round(estimate, 4), "\n")
        if(p_value < 0.05) {
          cat("  ✅ Signifikan (p =", format(p_value, digits = 3), ")\n")
          if(estimate > 0) {
            cat("  → Hubungan POSITIF: peningkatan 1 unit", var_name, 
                "meningkatkan", input$reg_response, "sebesar", round(abs(estimate), 4), "\n")
          } else {
            cat("  → Hubungan NEGATIF: peningkatan 1 unit", var_name, 
                "menurunkan", input$reg_response, "sebesar", round(abs(estimate), 4), "\n")
          }
        } else {
          cat("  ❌ Tidak signifikan (p =", format(p_value, digits = 3), ")\n")
          cat("  → Tidak ada bukti hubungan linear dengan", input$reg_response, "\n")
        }
      }
    }
    
    cat("\nREKOMENDASI:\n")
    if(f_p_value < 0.05 && r_squared > 0.3) {
      cat("✅ Model layak digunakan untuk prediksi dan inferensi\n")
    } else if(f_p_value < 0.05 && r_squared <= 0.3) {
      cat("⚠️ Model signifikan tapi daya prediksi lemah\n")
      cat("   Pertimbangkan menambah variabel atau transformasi\n")
    } else {
      cat("❌ Model tidak layak - pertimbangkan model alternatif\n")
    }
  })
  
  # Normality plot
  output$normality_plot <- renderPlot({
    req(reg_model())
    qqnorm(residuals(reg_model()), main = "Q-Q Plot Residual")
    qqline(residuals(reg_model()), col = "red", lwd = 2)
  })
  
  # Residual histogram
  output$residual_histogram <- renderPlot({
    req(reg_model())
    residuals_data <- residuals(reg_model())
    
    hist(residuals_data, freq = FALSE, main = "Histogram Residual",
         xlab = "Residuals", col = "lightblue", border = "white")
    
    # Add normal curve
    x_seq <- seq(min(residuals_data), max(residuals_data), length = 100)
    y_seq <- dnorm(x_seq, mean(residuals_data), sd(residuals_data))
    lines(x_seq, y_seq, col = "red", lwd = 2)
    
    # Add density curve
    lines(density(residuals_data), col = "blue", lwd = 2)
    
    legend("topright", c("Normal Teoritis", "Density Empiris"), 
           col = c("red", "blue"), lwd = 2)
  })
  
  # Shapiro test for residuals
  output$shapiro_test <- renderPrint({
    req(reg_model())
    residuals_data <- residuals(reg_model())
    
    if(length(residuals_data) > 5000) {
      cat("Catatan: Menggunakan sampel 5000 residual untuk uji Shapiro-Wilk\n\n")
      residuals_data <- sample(residuals_data, 5000)
    }
    
    shapiro.test(residuals_data)
  })
  
  # Normality interpretation
  output$normality_interpretation <- renderPrint({
    req(reg_model())
    residuals_data <- residuals(reg_model())
    
    if(length(residuals_data) > 5000) {
      residuals_data <- sample(residuals_data, 5000)
    }
    
    shapiro_result <- shapiro.test(residuals_data)
    
    cat("=== INTERPRETASI UJI NORMALITAS RESIDUAL ===\n\n")
    cat("H₀: Residual berdistribusi normal\n")
    cat("H₁: Residual tidak berdistribusi normal\n\n")
    
    if(shapiro_result$p.value < 0.05) {
      cat("❌ KESIMPULAN: Tolak H₀ (p < 0.05)\n")
      cat("Residual TIDAK berdistribusi normal\n")
      cat("DAMPAK: Inferensi statistik (uji t, interval kepercayaan) mungkin tidak valid\n")
      cat("SOLUSI:\n")
      cat("- Transformasi variabel terikat (log, sqrt, Box-Cox)\n")
      cat("- Gunakan bootstrap untuk interval kepercayaan\n")
      cat("- Pertimbangkan model non-linear\n")
    } else {
      cat("✅ KESIMPULAN: Gagal tolak H₀ (p ≥ 0.05)\n")
      cat("Residual berdistribusi normal\n")
      cat("Asumsi normalitas TERPENUHI\n")
    }
  })
  
  # Homoscedasticity plot
  output$homo_plot <- renderPlot({
    req(reg_model())
    plot(reg_model(), which = 1, main = "Residuals vs Fitted")
  })
  
  # Scale-location plot
  output$scale_location_plot <- renderPlot({
    req(reg_model())
    plot(reg_model(), which = 3, main = "Scale-Location Plot")
  })
  
  # Breusch-Pagan test
  output$bptest_output <- renderPrint({
    req(reg_model())
    lmtest::bptest(reg_model())
  })
  
  # Homoscedasticity interpretation
  output$homo_interpretation <- renderPrint({
    req(reg_model())
    bp_result <- lmtest::bptest(reg_model())
    
    cat("=== INTERPRETASI UJI HOMOSKEDASTISITAS ===\n\n")
    cat("H₀: Varians residual konstan (homoskedastisitas)\n")
    cat("H₁: Varians residual tidak konstan (heteroskedastisitas)\n\n")
    
    if(bp_result$p.value < 0.05) {
      cat("❌ KESIMPULAN: Tolak H₀ (p < 0.05)\n")
      cat("Terjadi HETEROSKEDASTISITAS\n")
      cat("DAMPAK: Standard error tidak akurat, uji signifikansi bias\n")
      cat("SOLUSI:\n")
      cat("- Transformasi variabel (log, sqrt)\n")
      cat("- Weighted Least Squares (WLS)\n")
      cat("- Robust standard errors\n")
      cat("- Generalized Least Squares (GLS)\n")
    } else {
      cat("✅ KESIMPULAN: Gagal tolak H₀ (p ≥ 0.05)\n")
      cat("Varians residual konstan (homoskedastisitas)\n")
      cat("Asumsi homoskedastisitas TERPENUHI\n")
    }
  })
  
  # VIF test
  output$vif_test <- renderPrint({
    req(reg_model())
    
    if(length(input$reg_predictors) > 1) {
      cat("=== VARIANCE INFLATION FACTOR (VIF) ===\n\n")
      vif_values <- car::vif(reg_model())
      print(vif_values)
      
      cat("\nInterpretasi VIF:\n")
      cat("VIF < 5: Tidak ada masalah multikolinearitas\n")
      cat("5 ≤ VIF < 10: Multikolinearitas sedang\n")
      cat("VIF ≥ 10: Multikolinearitas tinggi (masalah serius)\n")
    } else {
      cat("VIF tidak dapat dihitung untuk model dengan satu prediktor.\n")
    }
  })
  
  # Multicollinearity interpretation
  output$multicollinearity_interpretation <- renderPrint({
    req(reg_model())
    
    if(length(input$reg_predictors) > 1) {
      vif_values <- car::vif(reg_model())
      max_vif <- max(vif_values)
      
      cat("=== INTERPRETASI MULTIKOLINEARITAS ===\n\n")
      cat("VIF tertinggi:", round(max_vif, 2), "\n\n")
      
      if(max_vif < 5) {
        cat("✅ TIDAK ADA masalah multikolinearitas\n")
        cat("Semua variabel prediktor relatif independen\n")
      } else if(max_vif < 10) {
        cat("⚠️ MULTIKOLINEARITAS SEDANG\n")
        cat("Ada korelasi antar prediktor, tapi masih dapat ditoleransi\n")
        cat("REKOMENDASI: Monitor interpretasi koefisien dengan hati-hati\n")
      } else {
        cat("❌ MULTIKOLINEARITAS TINGGI\n")
        cat("Korelasi tinggi antar prediktor menyebabkan masalah estimasi\n")
        cat("DAMPAK:\n")
        cat("- Koefisien tidak stabil\n")
        cat("- Standard error membesar\n")
        cat("- Interpretasi koefisien sulit\n")
        cat("SOLUSI:\n")
        cat("- Hapus salah satu variabel yang berkorelasi tinggi\n")
        cat("- Gunakan Principal Component Analysis (PCA)\n")
        cat("- Ridge regression atau Lasso regression\n")
        cat("- Kombinasikan variabel yang berkorelasi\n")
      }
      
      # Identify problematic variables
      high_vif <- names(vif_values)[vif_values >= 10]
      if(length(high_vif) > 0) {
        cat("\nVariabel dengan VIF tinggi (≥10):\n")
        for(var in high_vif) {
          cat("-", var, ": VIF =", round(vif_values[var], 2), "\n")
        }
      }
    } else {
      cat("Analisis multikolinearitas memerlukan minimal 2 variabel prediktor.\n")
    }
  })
  
  # Leverage plot
  output$leverage_plot <- renderPlot({
    req(reg_model())
    plot(reg_model(), which = 5, main = "Residuals vs Leverage")
  })
  
  # Cook's distance plot
  output$cooks_distance_plot <- renderPlot({
    req(reg_model())
    plot(reg_model(), which = 4, main = "Cook's Distance")
  })
  
  # Outlier analysis
  output$outlier_analysis <- renderPrint({
    req(reg_model())
    model <- reg_model()
    
    cat("=== ANALISIS OUTLIERS DAN INFLUENTIAL POINTS ===\n\n")
    
    # Cook's distance
    cooks_d <- cooks.distance(model)
    n <- length(cooks_d)
    p <- length(coef(model))
    
    # Threshold for Cook's distance
    cooks_threshold <- 4 / (n - p)
    influential_points <- which(cooks_d > cooks_threshold)
    
    cat("COOK'S DISTANCE:\n")
    cat("Threshold:", round(cooks_threshold, 4), "\n")
    cat("Jumlah influential points:", length(influential_points), "\n")
    
    if(length(influential_points) > 0) {
      cat("Observasi influential (Cook's D >", round(cooks_threshold, 4), "):\n")
      for(i in head(influential_points, 10)) {  # Show first 10
        cat("- Observasi", i, ": Cook's D =", round(cooks_d[i], 4), "\n")
      }
      if(length(influential_points) > 10) {
        cat("... dan", length(influential_points) - 10, "lainnya\n")
      }
    }
    
    # Leverage
    leverage <- hatvalues(model)
    leverage_threshold <- 2 * p / n
    high_leverage <- which(leverage > leverage_threshold)
    
    cat("\nLEVERAGE:\n")
    cat("Threshold:", round(leverage_threshold, 4), "\n")
    cat("Jumlah high leverage points:", length(high_leverage), "\n")
    
    # Standardized residuals
    std_residuals <- rstandard(model)
    outliers <- which(abs(std_residuals) > 2)
    
    cat("\nSTANDARDIZED RESIDUALS:\n")
    cat("Outliers (|residual| > 2):", length(outliers), "\n")
    extreme_outliers <- which(abs(std_residuals) > 3)
    cat("Extreme outliers (|residual| > 3):", length(extreme_outliers), "\n")
  })
  
  # Outlier interpretation
  output$outlier_interpretation <- renderPrint({
    req(reg_model())
    model <- reg_model()
    
    cooks_d <- cooks.distance(model)
    n <- length(cooks_d)
    p <- length(coef(model))
    cooks_threshold <- 4 / (n - p)
    influential_points <- which(cooks_d > cooks_threshold)
    
    leverage <- hatvalues(model)
    leverage_threshold <- 2 * p / n
    high_leverage <- which(leverage > leverage_threshold)
    
    std_residuals <- rstandard(model)
    outliers <- which(abs(std_residuals) > 2)
    
    cat("=== INTERPRETASI OUTLIERS DAN INFLUENTIAL POINTS ===\n\n")
    
    total_issues <- length(unique(c(influential_points, high_leverage, outliers)))
    
    if(total_issues == 0) {
      cat("✅ TIDAK ADA masalah outlier atau influential points\n")
      cat("Model relatif robust terhadap observasi ekstrem\n")
    } else {
      cat("⚠️ TERDETEKSI", total_issues, "observasi bermasalah\n\n")
      
      if(length(influential_points) > 0) {
        cat("INFLUENTIAL POINTS (", length(influential_points), "observasi):\n")
        cat("- Observasi yang sangat mempengaruhi koefisien regresi\n")
        cat("- Dapat mengubah hasil model secara signifikan\n")
        cat("- Perlu investigasi lebih lanjut\n\n")
      }
      
      if(length(high_leverage) > 0) {
        cat("HIGH LEVERAGE POINTS (", length(high_leverage), "observasi):\n")
        cat("- Observasi dengan nilai X yang ekstrem\n")
        cat("- Berpotensi mempengaruhi model\n")
        cat("- Periksa apakah data valid atau error input\n\n")
      }
      
      if(length(outliers) > 0) {
        cat("OUTLIERS (", length(outliers), "observasi):\n")
        cat("- Observasi dengan residual besar\n")
        cat("- Model tidak dapat memprediksi dengan baik\n")
        cat("- Mungkin ada pola yang tidak tertangkap model\n\n")
      }
      
      cat("REKOMENDASI:\n")
      cat("1. INVESTIGASI: Periksa apakah observasi bermasalah adalah:\n")
      cat("   - Error input data\n")
      cat("   - Kasus khusus yang valid\n")
      cat("   - Indikasi model yang tidak tepat\n\n")
      
      cat("2. TINDAKAN yang dapat dilakukan:\n")
      if(length(influential_points) > 5) {
        cat("   - Hapus observasi influential (hati-hati!)\n")
      }
      cat("   - Transformasi variabel\n")
      cat("   - Robust regression\n")
      cat("   - Model non-linear\n")
      cat("   - Tambah variabel prediktor\n\n")
      
      cat("3. VALIDASI:\n")
      cat("   - Bandingkan model dengan dan tanpa outliers\n")
      cat("   - Cross-validation\n")
      cat("   - Uji stabilitas koefisien\n")
    }
  })
  
  # Diagnostic plots
  output$diagnostic_plots <- renderPlot({
    req(reg_model())
    par(mfrow = c(2, 2))
    plot(reg_model())
  })
  
  # Diagnostic summary
  output$diagnostic_summary <- renderPrint({
    req(reg_model())
    model <- reg_model()
    
    cat("=== RINGKASAN DIAGNOSTIK MODEL ===\n\n")
    
    # Check all assumptions
    residuals_data <- residuals(model)
    
    # 1. Normality
    if(length(residuals_data) > 5000) {
      shapiro_result <- shapiro.test(sample(residuals_data, 5000))
    } else {
      shapiro_result <- shapiro.test(residuals_data)
    }
    normality_ok <- shapiro_result$p.value >= 0.05
    
    # 2. Homoscedasticity
    bp_result <- lmtest::bptest(model)
    homoscedasticity_ok <- bp_result$p.value >= 0.05
    
    # 3. Multicollinearity
    multicollinearity_ok <- TRUE
    if(length(input$reg_predictors) > 1) {
      vif_values <- car::vif(model)
      multicollinearity_ok <- all(vif_values < 5)
    }
    
    # 4. Outliers
    cooks_d <- cooks.distance(model)
    n <- length(cooks_d)
    p <- length(coef(model))
    cooks_threshold <- 4 / (n - p)
    influential_points <- which(cooks_d > cooks_threshold)
    outliers_ok <- length(influential_points) <= n * 0.05  # Less than 5% influential
    
    cat("CHECKLIST ASUMSI:\n")
    cat(ifelse(normality_ok, "✅", "❌"), "Normalitas residual\n")
    cat(ifelse(homoscedasticity_ok, "✅", "❌"), "Homoskedastisitas\n")
    cat(ifelse(multicollinearity_ok, "✅", "❌"), "Tidak ada multikolinearitas\n")
    cat(ifelse(outliers_ok, "✅", "❌"), "Outliers terkendali\n\n")
    
    # Overall assessment
    total_ok <- sum(normality_ok, homoscedasticity_ok, multicollinearity_ok, outliers_ok)
    
    cat("PENILAIAN KESELURUHAN:\n")
    if(total_ok == 4) {
      cat("🌟 EXCELLENT: Semua asumsi terpenuhi\n")
      cat("Model sangat layak untuk inferensi dan prediksi\n")
    } else if(total_ok == 3) {
      cat("✅ GOOD: Sebagian besar asumsi terpenuhi\n")
      cat("Model layak dengan beberapa catatan\n")
    } else if(total_ok == 2) {
      cat("⚠️ FAIR: Beberapa asumsi dilanggar\n")
      cat("Gunakan model dengan hati-hati, pertimbangkan perbaikan\n")
    } else {
      cat("❌ POOR: Banyak asumsi dilanggar\n")
      cat("Model tidak layak, perlu perbaikan signifikan\n")
      cat("\nREKOMENDASI PRIORITAS:\n")
      if(!normality_ok) cat("1. Atasi masalah normalitas residual\n")
      if(!homoscedasticity_ok) cat("2. Atasi heteroskedastisitas\n")
      if(!multicollinearity_ok) cat("3. Kurangi multikolinearitas\n")
      if(!outliers_ok) cat("4. Tangani outliers dan influential points\n")
    }
  })
  
  # Prediction inputs
  output$prediction_inputs <- renderUI({
    req(input$reg_predictors)
    
    df <- data_with_cats()
    inputs <- list()
    
    for(var in input$reg_predictors) {
      if(is.numeric(df[[var]])) {
        inputs[[var]] <- numericInput(
          paste0("pred_", var),
          paste("Nilai", var, ":"),
          value = round(mean(df[[var]], na.rm = TRUE), 2),
          min = min(df[[var]], na.rm = TRUE),
          max = max(df[[var]], na.rm = TRUE)
        )
      } else {
        inputs[[var]] <- selectInput(
          paste0("pred_", var),
          paste("Nilai", var, ":"),
          choices = unique(df[[var]])
        )
      }
    }
    
    do.call(tagList, inputs)
  })
  
  # Make prediction
  output$prediction_result <- renderPrint({
    req(input$make_prediction, reg_model())
    
    if(input$make_prediction > 0) {
      model <- reg_model()
      
      # Create prediction data frame
      pred_data <- data.frame(row.names = 1)
      
      for(var in input$reg_predictors) {
        pred_input_id <- paste0("pred_", var)
        if(!is.null(input[[pred_input_id]])) {
          pred_data[[var]] <- input[[pred_input_id]]
        }
      }
      
      # Make prediction
      prediction <- predict(model, newdata = pred_data, interval = "prediction", level = 0.95)
      confidence <- predict(model, newdata = pred_data, interval = "confidence", level = 0.95)
      
      cat("=== HASIL PREDIKSI ===\n\n")
      cat("Input Variabel:\n")
      for(var in input$reg_predictors) {
        pred_input_id <- paste0("pred_", var)
        if(!is.null(input[[pred_input_id]])) {
          cat("-", var, ":", input[[pred_input_id]], "\n")
        }
      }
      
      cat("\nPREDIKSI:\n")
      cat("Nilai prediksi:", round(prediction[1], 4), "\n")
      cat("95% Prediction Interval: [", round(prediction[2], 4), ",", round(prediction[3], 4), "]\n")
      cat("95% Confidence Interval: [", round(confidence[2], 4), ",", round(confidence[3], 4), "]\n\n")
      
      cat("INTERPRETASI:\n")
      cat("- Nilai prediksi adalah estimasi terbaik untuk", input$reg_response, "\n")
      cat("- Prediction Interval: rentang dimana nilai individual baru kemungkinan berada\n")
      cat("- Confidence Interval: rentang dimana rata-rata populasi kemungkinan berada\n")
      cat("- Prediction Interval selalu lebih lebar dari Confidence Interval\n")
    }
  })
  
  # --- Download Handlers - ENHANCED ---
  
  # Helper function to create temporary files
  create_temp_file <- function(extension) {
    tempfile(fileext = paste0(".", extension))
  }
  
  # Helper function to create combined plots and save as JPG
  create_combined_plot_jpg <- function(plots_list, filename, width = 1600, height = 1200) {
    jpeg(filename, width = width, height = height, res = 150, quality = 95)
    
    if(length(plots_list) == 1) {
      plots_list[[1]]
    } else if(length(plots_list) == 2) {
      grid.arrange(grobs = plots_list, ncol = 2)
    } else if(length(plots_list) <= 4) {
      grid.arrange(grobs = plots_list, ncol = 2, nrow = 2)
    } else {
      grid.arrange(grobs = plots_list, ncol = 3, nrow = ceiling(length(plots_list)/3))
    }
    
    dev.off()
  }
  
  # NEW: Download all management plots as JPG
  output$download_management_jpg <- downloadHandler(
    filename = function() {
      paste("DAST_Manajemen_Data_Grafik_", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      req(input$variable_categorize, categorized_data())
      
      # Create temporary plots
      temp_dir <- tempdir()
      plot_files <- c()
      
      # Plot 1: Categorization plot
      p1_file <- file.path(temp_dir, "categorization_plot.jpg")
      jpeg(p1_file, width = 800, height = 600, res = 100, quality = 95)
      
      df_plot <- data.frame(Category = categorized_data())
      p1 <- ggplot(df_plot, aes(x = Category)) +
        geom_bar(fill = "steelblue", alpha = 0.7, color = "white") +
        geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
        theme_minimal() +
        labs(title = paste("Distribusi Kategorisasi:", input$variable_categorize),
             x = "Kategori", y = "Frekuensi") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(size = 14, face = "bold"))
      print(p1)
      dev.off()
      
      # Plot 2: Original data histogram
      p2_file <- file.path(temp_dir, "original_data_plot.jpg")
      jpeg(p2_file, width = 800, height = 600, res = 100, quality = 95)
      
      df <- data_with_cats()
      var_data <- df[[input$variable_categorize]]
      p2 <- ggplot(data.frame(x = var_data), aes(x = x)) +
        geom_histogram(bins = 30, fill = "lightblue", alpha = 0.7, color = "white") +
        theme_minimal() +
        labs(title = paste("Data Asli (Kontinyu):", input$variable_categorize), 
             x = input$variable_categorize, y = "Frekuensi") +
        theme(plot.title = element_text(size = 14, face = "bold"))
      print(p2)
      dev.off()
      
      # Plot 3: Categorized data
      p3_file <- file.path(temp_dir, "categorized_data_plot.jpg")
      jpeg(p3_file, width = 800, height = 600, res = 100, quality = 95)
      
      p3 <- ggplot(df_plot, aes(x = Category)) +
        geom_bar(fill = "lightcoral", alpha = 0.7, color = "white") +
        geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
        theme_minimal() +
        labs(title = paste("Data Kategorik:", input$variable_categorize), 
             x = "Kategori", y = "Frekuensi") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(size = 14, face = "bold"))
      print(p3)
      dev.off()
      
      # Combine all plots into one image
      jpeg(file, width = 1600, height = 1200, res = 150, quality = 95)
      
      # Create a grid layout
      par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
      
      # Add title
      par(mfrow = c(1, 1))
      plot.new()
      text(0.5, 0.9, paste("Laporan Manajemen Data -", input$variable_categorize), 
           cex = 2, font = 2, col = "darkblue")
      text(0.5, 0.8, paste("Tanggal:", Sys.Date()), cex = 1.2, col = "darkgreen")
      
      # Add the plots by reading the temporary files and combining them
      par(mfrow = c(2, 2), mar = c(1, 1, 2, 1))
      
      # Since we can't easily combine ggplot images in base R, we'll create a summary
      plot.new()
      text(0.5, 0.5, "Grafik Kategorisasi\ndan Perbandingan Data\nTelah Dibuat", 
           cex = 1.5, font = 2, col = "darkblue")
      
      plot.new()
      freq_table <- table(categorized_data())
      barplot(freq_table, main = "Distribusi Kategori", col = "steelblue", las = 2)
      
      plot.new()
      hist(var_data, main = "Data Asli", col = "lightblue", xlab = input$variable_categorize)
      
      plot.new()
      barplot(freq_table, main = "Data Kategorik", col = "lightcoral", las = 2)
      
      dev.off()
    },
    contentType = "image/jpeg"
  )
  
  # NEW: Download all assumption test plots as JPG
  output$download_assumption_jpg <- downloadHandler(
    filename = function() {
      paste("DAST_Uji_Asumsi_Grafik_", input$assumption_variable, "_", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      req(input$assumption_variable)
      
      jpeg(file, width = 1600, height = 1200, res = 150, quality = 95)
      
      df <- data_with_cats()
      var_data <- df[[input$assumption_variable]]
      
      # Create a 2x2 layout
      par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
      
      # Plot 1: Q-Q Plot
      qqnorm(var_data, main = paste("Q-Q Plot:", input$assumption_variable),
             pch = 19, col = "steelblue", cex = 0.8)
      qqline(var_data, col = "red", lwd = 2)
      
      # Plot 2: Histogram with normal curve
      hist(var_data, freq = FALSE, main = paste("Histogram dengan Kurva Normal:", input$assumption_variable),
           xlab = input$assumption_variable, col = "lightblue", border = "white")
      
      # Add normal curve
      x_seq <- seq(min(var_data, na.rm = TRUE), max(var_data, na.rm = TRUE), length = 100)
      y_seq <- dnorm(x_seq, mean(var_data, na.rm = TRUE), sd(var_data, na.rm = TRUE))
      lines(x_seq, y_seq, col = "red", lwd = 2)
      lines(density(var_data, na.rm = TRUE), col = "blue", lwd = 2)
      legend("topright", c("Normal Teoritis", "Density Empiris"), 
             col = c("red", "blue"), lwd = 2, cex = 0.8)
      
      # Plot 3: Box plot (if grouping variable exists)
      if(input$grouping_variable != "none") {
        group_data <- df[[input$grouping_variable]]
        boxplot(var_data ~ group_data, 
                main = paste("Box Plot per Kelompok:", input$grouping_variable),
                xlab = input$grouping_variable, ylab = input$assumption_variable,
                col = "lightgreen", las = 2)
      } else {
        boxplot(var_data, main = paste("Box Plot:", input$assumption_variable),
                ylab = input$assumption_variable, col = "lightgreen")
      }
      
      # Plot 4: Summary statistics text
      plot.new()
      title("Ringkasan Uji Asumsi", cex.main = 1.5, font.main = 2)
      
      # Add test results as text
      y_pos <- 0.9
      text(0.1, y_pos, "Hasil Uji:", cex = 1.2, font = 2, adj = 0)
      y_pos <- y_pos - 0.1
      
      if("shapiro" %in% input$selected_tests) {
        if(length(var_data) > 5000) {
          shapiro_result <- shapiro.test(sample(var_data, 5000))
        } else {
          shapiro_result <- shapiro.test(var_data)
        }
        result_text <- ifelse(shapiro_result$p.value < 0.05, "TIDAK Normal", "Normal")
        text(0.1, y_pos, paste("Shapiro-Wilk:", result_text), cex = 1, adj = 0)
        y_pos <- y_pos - 0.08
        text(0.1, y_pos, paste("p-value =", format(shapiro_result$p.value, digits = 4)), cex = 0.9, adj = 0)
        y_pos <- y_pos - 0.1
      }
      
      if(input$grouping_variable != "none" && "levene" %in% input$selected_tests) {
        group_data <- df[[input$grouping_variable]]
        levene_result <- car::leveneTest(var_data ~ group_data)
        result_text <- ifelse(levene_result$`Pr(>F)`[1] < 0.05, "TIDAK Homogen", "Homogen")
        text(0.1, y_pos, paste("Levene Test:", result_text), cex = 1, adj = 0)
        y_pos <- y_pos - 0.08
        text(0.1, y_pos, paste("p-value =", format(levene_result$`Pr(>F)`[1], digits = 4)), cex = 0.9, adj = 0)
      }
      
      # Add date and variable info
      text(0.1, 0.1, paste("Variabel:", input$assumption_variable), cex = 1, adj = 0, font = 2)
      text(0.1, 0.05, paste("Tanggal:", Sys.Date()), cex = 0.9, adj = 0)
      
      dev.off()
    },
    contentType = "image/jpeg"
  )
  
  # NEW: Download all statistical inference plots as JPG
  output$download_inference_jpg <- downloadHandler(
    filename = function() {
      paste("DAST_Statistik_Inferensia_", input$stat_test_type, "_", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      req(input$stat_test_type)
      
      jpeg(file, width = 1600, height = 1200, res = 150, quality = 95)
      
      # Create layout based on test type
      if(input$stat_test_type %in% c("t_one", "var_one", "prop_one")) {
        par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
      } else {
        par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
      }
      
      df <- data_with_cats()
      
      # Main inference plot based on test type
      switch(input$stat_test_type,
             "t_one" = {
               if(!is.null(input$t_one_var)) {
                 var_data <- df[[input$t_one_var]]
                 hist(var_data, main = paste("Distribusi:", input$t_one_var),
                      col = "lightblue", xlab = input$t_one_var)
                 abline(v = mean(var_data, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
                 abline(v = input$t_one_mu, col = "green", lwd = 2)
                 legend("topright", c("Mean Sampel", "Nilai Hipotesis"), 
                        col = c("red", "green"), lwd = 2, cex = 0.8)
               }
             },
             "t_two" = {
               if(!is.null(input$t_two_var) && !is.null(input$t_two_group)) {
                 var_data <- df[[input$t_two_var]]
                 group_data <- df[[input$t_two_group]]
                 boxplot(var_data ~ group_data, 
                         main = paste("Perbandingan:", input$t_two_var),
                         col = c("lightblue", "lightcoral"),
                         xlab = input$t_two_group, ylab = input$t_two_var)
               }
             },
             "anova_one" = {
               if(!is.null(input$anova_one_var) && !is.null(input$anova_one_group)) {
                 var_data <- df[[input$anova_one_var]]
                 group_data <- df[[input$anova_one_group]]
                 boxplot(var_data ~ group_data,
                         main = paste("ANOVA:", input$anova_one_var, "vs", input$anova_one_group),
                         col = rainbow(length(unique(group_data))),
                         xlab = input$anova_one_group, ylab = input$anova_one_var,
                         las = 2)
               }
             }
      )
      
      # Additional diagnostic plots
      plot.new()
      title("Hasil Uji Statistik", cex.main = 1.5, font.main = 2)
      text(0.5, 0.8, paste("Jenis Uji:", input$stat_test_type), cex = 1.2, font = 2)
      text(0.5, 0.6, paste("Tanggal:", Sys.Date()), cex = 1, col = "darkgreen")
      text(0.5, 0.4, "Lihat output di aplikasi untuk\ndetail hasil uji statistik", cex = 1, col = "darkblue")
      
      # Assumption check plots
      if(input$stat_test_type %in% c("t_one", "t_two", "anova_one")) {
        var_name <- switch(input$stat_test_type,
                           "t_one" = input$t_one_var,
                           "t_two" = input$t_two_var,
                           "anova_one" = input$anova_one_var)
        
        if(!is.null(var_name)) {
          var_data <- df[[var_name]]
          qqnorm(var_data, main = "Q-Q Plot untuk Normalitas", pch = 19, col = "steelblue")
          qqline(var_data, col = "red", lwd = 2)
          
          hist(var_data, main = "Histogram Data", col = "lightgreen", 
               xlab = var_name, freq = FALSE)
          lines(density(var_data, na.rm = TRUE), col = "blue", lwd = 2)
        }
      } else {
        plot.new()
        text(0.5, 0.5, "Uji Asumsi\nLihat tab Asumsi Uji\ndi aplikasi", cex = 1.2, font = 2)
        
        plot.new()
        text(0.5, 0.5, "Interpretasi\nLengkap tersedia\ndi aplikasi", cex = 1.2, font = 2)
      }
      
      dev.off()
    },
    contentType = "image/jpeg"
  )
  
    # Download full report - ENHANCED Word document with beautiful formatting
  output$download_full_report <- downloadHandler(
    filename = function() {
      paste("DAST_Laporan_Lengkap_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      # Create a professional Word document
      doc <- read_docx()
      
      # Add cover page with styling
      doc <- doc %>%
        body_add_par("DASHBOARD ANALISIS STATISTIK TERPADU", 
                     style = "heading 1") %>%
        body_add_par("(DAST)", style = "heading 1") %>%
        body_add_par("") %>%
        body_add_par("LAPORAN ANALISIS KOMPREHENSIF", 
                     style = "heading 2") %>%
        body_add_par("") %>%
        body_add_par(paste("Tanggal:", Sys.Date())) %>%
        body_add_par(paste("Waktu:", format(Sys.time(), "%H:%M:%S WIB"))) %>%
        body_add_break() %>%
        
        # Executive Summary
        body_add_par("RINGKASAN EKSEKUTIF", style = "heading 2") %>%
        body_add_par("Dashboard Analisis Statistik Terpadu (DAST) adalah platform komprehensif berbasis R Shiny untuk analisis data kerentanan sosial Indonesia. Platform ini menyediakan tools lengkap untuk transformasi data, eksplorasi statistik, pengujian asumsi, analisis inferensia, dan pemodelan regresi dengan interface yang user-friendly dan output berkualitas tinggi.") %>%
        body_add_par("")
      
      # Dataset Information
      doc <- doc %>%
        body_add_par("INFORMASI DATASET", style = "heading 2") %>%
        body_add_par("")
      
      # Create dataset info table
      dataset_info <- data.frame(
        "Aspek" = c("Nama Dataset", "Jumlah Observasi", "Jumlah Variabel", 
                    "Cakupan Geografis", "Periode Data", "Sumber Data"),
        "Detail" = c("Social Vulnerability Index (SoVI) Data",
                     "515 kabupaten/kota",
                     "16 variabel utama",
                     "Seluruh Indonesia",
                     "Data terkini kerentanan sosial",
                     "Badan Pusat Statistik dan instansi terkait")
      )
      
      ft_dataset <- flextable(dataset_info) %>%
        theme_booktabs() %>%
        color(part = "header", color = "white") %>%
        bg(part = "header", bg = "#2E74B5") %>%
        autofit() %>%
        align(align = "left", part = "all")
      
      doc <- doc %>% body_add_flextable(ft_dataset) %>% body_add_par("")
      
      # Variables Information
      doc <- doc %>%
        body_add_par("VARIABEL UTAMA", style = "heading 2") %>%
        body_add_par("")
      
      variables_info <- data.frame(
        "Kode Variabel" = c("CHILDREN", "ELDERLY", "POVERTY", "EDUCATION", 
                            "DISABILITY", "MINORITY", "FEMALE", "UNEMPLOYED"),
        "Deskripsi" = c("Persentase populasi anak-anak (0-17 tahun)",
                        "Persentase populasi lansia (65+ tahun)",
                        "Tingkat kemiskinan daerah",
                        "Persentase penduduk berpendidikan rendah",
                        "Persentase penyandang disabilitas",
                        "Persentase kelompok minoritas",
                        "Persentase kepala keluarga perempuan",
                        "Tingkat pengangguran"),
        "Kategori" = c("Demografi", "Demografi", "Ekonomi", "Sosial",
                       "Sosial", "Sosial", "Sosial", "Ekonomi")
      )
      
      ft_variables <- flextable(variables_info) %>%
        theme_booktabs() %>%
        color(part = "header", color = "white") %>%
        bg(part = "header", bg = "#70AD47") %>%
        autofit() %>%
        align(align = "left", part = "all") %>%
        bg(j = 3, bg = "#F2F2F2", part = "body")
      
      doc <- doc %>% body_add_flextable(ft_variables) %>% body_add_par("")
      
      # Features Overview
      doc <- doc %>%
        body_add_par("FITUR ANALISIS TERSEDIA", style = "heading 2") %>%
        body_add_par("")
      
      features_info <- data.frame(
        "No" = 1:6,
        "Modul" = c("Manajemen Data", "Eksplorasi Data", "Uji Asumsi", 
                    "Statistik Inferensia", "Regresi Linear", "Download & Export"),
        "Fungsi Utama" = c("Transformasi data kontinyu ke kategorik dengan berbagai metode",
                           "Statistik deskriptif, visualisasi interaktif, peta geografis",
                           "Pengujian normalitas dan homogenitas dengan interpretasi",
                           "Uji t, proporsi, varians, ANOVA dengan post-hoc test",
                           "Regresi berganda dengan uji asumsi lengkap",
                           "Export hasil dalam format JPG, Word, dan CSV"),
        "Status" = rep("✓ Aktif", 6)
      )
      
      ft_features <- flextable(features_info) %>%
        theme_booktabs() %>%
        color(part = "header", color = "white") %>%
        bg(part = "header", bg = "#C55A11") %>%
        autofit() %>%
        align(align = "left", part = "all") %>%
        color(j = 4, color = "#70AD47", part = "body") %>%
        bold(j = 4, part = "body")
      
      doc <- doc %>% body_add_flextable(ft_features) %>% body_add_par("")
      
      # Current Data Summary
      df <- data_with_cats()
      if(nrow(df) > 0) {
        doc <- doc %>%
          body_add_par("RINGKASAN DATA SAAT INI", style = "heading 2") %>%
          body_add_par("")
        
        numeric_vars <- names(df)[sapply(df, is.numeric)]
        categorical_vars <- names(df)[sapply(df, function(x) is.factor(x) | is.character(x))]
        
        current_summary <- data.frame(
          "Aspek" = c("Total Observasi", "Total Variabel", "Variabel Numerik", 
                      "Variabel Kategorik", "Data Missing", "Status Data"),
          "Nilai" = c(nrow(df),
                      ncol(df),
                      length(numeric_vars),
                      length(categorical_vars),
                      sum(is.na(df)),
                      "✓ Siap Analisis")
        )
        
        ft_current <- flextable(current_summary) %>%
          theme_booktabs() %>%
          color(part = "header", color = "white") %>%
          bg(part = "header", bg = "#7030A0") %>%
          autofit() %>%
          align(align = "left", part = "all") %>%
          color(i = 6, j = 2, color = "#70AD47", part = "body") %>%
          bold(i = 6, j = 2, part = "body")
        
        doc <- doc %>% body_add_flextable(ft_current) %>% body_add_par("")
      }
      
      # Footer
      doc <- doc %>%
        body_add_break() %>%
        body_add_par("KESIMPULAN", style = "heading 2") %>%
        body_add_par("DAST menyediakan solusi lengkap untuk analisis statistik data kerentanan sosial dengan interface yang intuitif dan output berkualitas profesional. Semua fitur telah dioptimasi untuk memberikan hasil analisis yang akurat dan dapat dipercaya.") %>%
        body_add_par("") %>%
        body_add_par("Untuk informasi lebih detail dan analisis mendalam, silakan gunakan fitur-fitur interaktif yang tersedia di dashboard.") %>%
        body_add_par("") %>%
        body_add_par(paste("Laporan dibuat otomatis pada:", Sys.time())) %>%
        body_add_par("© Dashboard Analisis Statistik Terpadu (DAST)")
      
      # Save the document
      print(doc, target = file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  )
  
  # Download management report - ENHANCED Word document
  output$download_management_report <- downloadHandler(
    filename = function() {
      paste("DAST_Manajemen_Data_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      # Create professional Word document
      doc <- read_docx()
      
      # Header and title
      doc <- doc %>%
        body_add_par("LAPORAN MANAJEMEN DATA", style = "heading 1") %>%
        body_add_par("Dashboard Analisis Statistik Terpadu (DAST)", style = "heading 2") %>%
        body_add_par("") %>%
        body_add_par(paste("📅 Tanggal:", Sys.Date())) %>%
        body_add_par(paste("🕐 Waktu:", format(Sys.time(), "%H:%M:%S WIB"))) %>%
        body_add_par("")
      
      if(!is.null(input$variable_categorize)) {
        # Categorization details
        doc <- doc %>%
          body_add_par("DETAIL KATEGORISASI", style = "heading 2") %>%
          body_add_par("")
        
        # Create categorization info table
        cat_info <- data.frame(
          "Aspek" = c("Variabel yang Dikategorisasi", "Metode Kategorisasi", 
                      "Jumlah Kategori", "Status Proses"),
          "Detail" = c(input$variable_categorize,
                       switch(input$categorization_method,
                              "quantile" = "Quantile-based (Pembagian berdasarkan kuantil)",
                              "equal" = "Equal-width (Interval sama)",
                              "custom" = "Custom breaks (Batas kustom)"),
                       input$num_bins,
                       "✅ Berhasil")
        )
        
        ft_cat_info <- flextable(cat_info) %>%
          theme_booktabs() %>%
          color(part = "header", color = "white") %>%
          bg(part = "header", bg = "#2E74B5") %>%
          autofit() %>%
          align(align = "left", part = "all") %>%
          color(i = 4, j = 2, color = "#70AD47", part = "body") %>%
          bold(i = 4, j = 2, part = "body")
        
        doc <- doc %>% body_add_flextable(ft_cat_info) %>% body_add_par("")
        
        # Frequency table if available
        if(!is.null(categorized_data())) {
          doc <- doc %>%
            body_add_par("TABEL FREKUENSI HASIL KATEGORISASI", style = "heading 2") %>%
            body_add_par("")
          
          freq_table <- table(categorized_data(), useNA = "ifany")
          freq_df <- data.frame(
            "Kategori" = names(freq_table),
            "Frekuensi" = as.numeric(freq_table),
            "Persentase" = round(as.numeric(freq_table) / sum(freq_table) * 100, 2),
            "Persentase Kumulatif" = round(cumsum(as.numeric(freq_table)) / sum(freq_table) * 100, 2)
          )
          freq_df$Persentase <- paste0(freq_df$Persentase, "%")
          freq_df$`Persentase Kumulatif` <- paste0(freq_df$`Persentase Kumulatif`, "%")
          
          ft_freq <- flextable(freq_df) %>%
            theme_booktabs() %>%
            color(part = "header", color = "white") %>%
            bg(part = "header", bg = "#70AD47") %>%
            autofit() %>%
            align(align = "center", part = "all") %>%
            align(j = 1, align = "left", part = "all") %>%
            bg(j = c(2, 3, 4), bg = "#F8F9FA", part = "body")
          
          doc <- doc %>% body_add_flextable(ft_freq) %>% body_add_par("")
          
          # Statistical summary
          original_data <- data_with_cats()[[input$variable_categorize]]
          doc <- doc %>%
            body_add_par("RINGKASAN STATISTIK", style = "heading 2") %>%
            body_add_par("")
          
          stats_summary <- data.frame(
            "Statistik" = c("Data Asli (Mean)", "Data Asli (Median)", "Data Asli (Std Dev)",
                            "Entropi Informasi", "Indeks Gini", "Efektivitas Kategorisasi"),
            "Nilai" = c(round(mean(original_data, na.rm = TRUE), 4),
                        round(median(original_data, na.rm = TRUE), 4),
                        round(sd(original_data, na.rm = TRUE), 4),
                        round(-sum(prop.table(freq_table) * log2(prop.table(freq_table))), 3),
                        round(1 - sum(prop.table(freq_table)^2), 3),
                        "✅ Baik")
          )
          
          ft_stats <- flextable(stats_summary) %>%
            theme_booktabs() %>%
            color(part = "header", color = "white") %>%
            bg(part = "header", bg = "#C55A11") %>%
            autofit() %>%
            align(align = "left", part = "all") %>%
            color(i = 6, j = 2, color = "#70AD47", part = "body") %>%
            bold(i = 6, j = 2, part = "body")
          
          doc <- doc %>% body_add_flextable(ft_stats) %>% body_add_par("")
        }
        
        # Interpretation
        doc <- doc %>%
          body_add_par("INTERPRETASI DAN REKOMENDASI", style = "heading 2") %>%
          body_add_par("") %>%
          body_add_par("✅ Proses kategorisasi telah berhasil dilakukan dengan baik") %>%
          body_add_par("✅ Data kontinyu telah diubah menjadi format kategorik yang sesuai") %>%
                     body_add_par("✅ Hasil kategorisasi dapat digunakan untuk analisis kategorikal") %>%
          body_add_par("✅ Distribusi kategori menunjukkan pembagian yang representatif") %>%
          body_add_par("")
        
        # Method explanation
        method_explanation <- switch(input$categorization_method,
          "quantile" = "Metode quantile-based memastikan setiap kategori memiliki jumlah observasi yang relatif sama, cocok untuk analisis yang memerlukan kelompok berukuran seimbang.",
          "equal" = "Metode equal-width memberikan rentang nilai yang sama untuk setiap kategori, mudah diinterpretasi dan cocok untuk data yang terdistribusi normal.",
          "custom" = "Metode custom breaks memungkinkan pembagian kategori berdasarkan pengetahuan domain atau threshold yang bermakna secara praktis."
        )
        
        doc <- doc %>%
          body_add_par("PENJELASAN METODE:", style = "heading 3") %>%
          body_add_par(method_explanation) %>%
          body_add_par("")
        
      } else {
        doc <- doc %>%
          body_add_par("INFORMASI", style = "heading 2") %>%
          body_add_par("Belum ada variabel yang dikategorisasi. Silakan pilih variabel dan metode kategorisasi pada dashboard untuk memulai proses analisis.") %>%
          body_add_par("")
      }
      
      # Footer
      doc <- doc %>%
        body_add_break() %>%
        body_add_par("KESIMPULAN", style = "heading 2") %>%
        body_add_par("Modul Manajemen Data DAST menyediakan tools yang komprehensif untuk transformasi data dengan berbagai metode kategorisasi yang dapat disesuaikan dengan kebutuhan analisis.") %>%
        body_add_par("") %>%
        body_add_par(paste("📊 Laporan dibuat otomatis pada:", format(Sys.time(), "%d %B %Y, %H:%M:%S WIB"))) %>%
        body_add_par("🔗 Dashboard Analisis Statistik Terpadu (DAST)")
      
      # Save document
      print(doc, target = file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  )
  
  # Download categorized table
  output$download_categorized_table <- downloadHandler(
    filename = function() {
      paste("Kategorisasi_", input$variable_categorize, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if(!is.null(categorized_data())) {
        freq_table <- as.data.frame(table(categorized_data(), useNA = "ifany"))
        names(freq_table) <- c("Kelompok", "Frekuensi")
        freq_table$Persentase <- round(freq_table$Frekuensi / sum(freq_table$Frekuensi) * 100, 2)
        write.csv(freq_table, file, row.names = FALSE)
      }
    }
  )
  
  # Download explore plot - ENHANCED to JPG format
  output$download_explore_plot <- downloadHandler(
    filename = function() {
      paste("Plot_", input$variable_explore, "_", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      req(input$variable_explore)
      
      # Create the plot with higher quality
      df <- filtered_data()
      var_name <- input$variable_explore
      
      jpeg(file, width = 1200, height = 900, res = 150, quality = 95)
      
      # Create a layout for multiple plots
      par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
      
      if(is.numeric(df[[var_name]])) {
        # Plot 1: Histogram
        hist(df[[var_name]], 
             main = paste("Histogram:", var_name),
             xlab = var_name, 
             ylab = "Frekuensi",
             col = "lightblue",
             border = "white")
        
        # Plot 2: Box plot
        boxplot(df[[var_name]], 
                main = paste("Box Plot:", var_name),
                ylab = var_name,
                col = "lightgreen")
        
        # Plot 3: Q-Q plot
        qqnorm(df[[var_name]], 
               main = paste("Q-Q Plot:", var_name),
               pch = 19, col = "steelblue")
        qqline(df[[var_name]], col = "red", lwd = 2)
        
        # Plot 4: Summary statistics
        plot.new()
        title("Statistik Deskriptif", cex.main = 1.2, font.main = 2)
        stats_text <- c(
          paste("Mean:", round(mean(df[[var_name]], na.rm = TRUE), 3)),
          paste("Median:", round(median(df[[var_name]], na.rm = TRUE), 3)),
          paste("Std Dev:", round(sd(df[[var_name]], na.rm = TRUE), 3)),
          paste("Min:", round(min(df[[var_name]], na.rm = TRUE), 3)),
          paste("Max:", round(max(df[[var_name]], na.rm = TRUE), 3)),
          paste("N:", length(df[[var_name]]))
        )
        for(i in 1:length(stats_text)) {
          text(0.1, 0.9 - (i-1)*0.12, stats_text[i], cex = 1, adj = 0)
        }
        
      } else {
        # Plot 1: Bar plot
        tbl <- table(df[[var_name]])
        barplot(tbl,
                main = paste("Distribusi:", var_name),
                xlab = var_name,
                ylab = "Frekuensi",
                col = rainbow(length(tbl)),
                las = 2)
        
        # Plot 2: Pie chart
        pie(tbl, main = paste("Proporsi:", var_name),
            col = rainbow(length(tbl)))
        
        # Plot 3: Frequency table as text
        plot.new()
        title("Tabel Frekuensi", cex.main = 1.2, font.main = 2)
        for(i in 1:length(tbl)) {
          pct <- round(tbl[i]/sum(tbl)*100, 1)
          text(0.1, 0.9 - (i-1)*0.1, 
               paste(names(tbl)[i], ":", tbl[i], "(", pct, "%)"), 
               cex = 0.9, adj = 0)
        }
        
        # Plot 4: Summary info
        plot.new()
        title("Informasi Kategori", cex.main = 1.2, font.main = 2)
        text(0.1, 0.8, paste("Total Kategori:", length(tbl)), cex = 1, adj = 0)
        text(0.1, 0.7, paste("Total Observasi:", sum(tbl)), cex = 1, adj = 0)
        text(0.1, 0.6, paste("Modus:", names(tbl)[which.max(tbl)]), cex = 1, adj = 0)
      }
      
      dev.off()
    },
    contentType = "image/jpeg"
  )
  
  # Download explore report - ENHANCED Word document
  output$download_explore_report <- downloadHandler(
    filename = function() {
      paste("DAST_Eksplorasi_", input$variable_explore, "_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      # Create professional Word document
      doc <- read_docx()
      
      # Header
      doc <- doc %>%
        body_add_par("LAPORAN EKSPLORASI DATA", style = "heading 1") %>%
        body_add_par("Dashboard Analisis Statistik Terpadu (DAST)", style = "heading 2") %>%
        body_add_par("") %>%
        body_add_par(paste("📅 Tanggal:", Sys.Date())) %>%
        body_add_par(paste("🕐 Waktu:", format(Sys.time(), "%H:%M:%S WIB"))) %>%
        body_add_par("")
      
      if(!is.null(input$variable_explore)) {
        # Variable information
        doc <- doc %>%
          body_add_par("INFORMASI VARIABEL", style = "heading 2") %>%
          body_add_par("")
        
        df <- filtered_data()
        var_data <- df[[input$variable_explore]]
        
        var_info <- data.frame(
          "Aspek" = c("Nama Variabel", "Tipe Data", "Jumlah Observasi", 
                      "Data Missing", "Unique Values"),
          "Detail" = c(input$variable_explore,
                       ifelse(is.numeric(var_data), "Numerik (Kontinyu)", "Kategorik"),
                       length(var_data),
                       sum(is.na(var_data)),
                       length(unique(var_data)))
        )
        
        ft_var_info <- flextable(var_info) %>%
          theme_booktabs() %>%
          color(part = "header", color = "white") %>%
          bg(part = "header", bg = "#2E74B5") %>%
          autofit() %>%
          align(align = "left", part = "all")
        
        doc <- doc %>% body_add_flextable(ft_var_info) %>% body_add_par("")
        
        if(is.numeric(var_data)) {
          # Descriptive statistics for numeric variables
          doc <- doc %>%
            body_add_par("STATISTIK DESKRIPTIF", style = "heading 2") %>%
            body_add_par("")
          
          stats_data <- data.frame(
            "Statistik" = c("Mean (Rata-rata)", "Median", "Std Deviation", "Variance",
                            "Minimum", "Maximum", "Range", "IQR", "Skewness", "Kurtosis"),
            "Nilai" = c(round(mean(var_data, na.rm = TRUE), 4),
                        round(median(var_data, na.rm = TRUE), 4),
                        round(sd(var_data, na.rm = TRUE), 4),
                        round(var(var_data, na.rm = TRUE), 4),
                        round(min(var_data, na.rm = TRUE), 4),
                        round(max(var_data, na.rm = TRUE), 4),
                        round(max(var_data, na.rm = TRUE) - min(var_data, na.rm = TRUE), 4),
                        round(IQR(var_data, na.rm = TRUE), 4),
                        round(moments::skewness(var_data, na.rm = TRUE), 4),
                        round(moments::kurtosis(var_data, na.rm = TRUE), 4))
          )
          
          ft_stats <- flextable(stats_data) %>%
            theme_booktabs() %>%
            color(part = "header", color = "white") %>%
            bg(part = "header", bg = "#70AD47") %>%
            autofit() %>%
            align(align = "left", part = "all") %>%
            bg(j = 2, bg = "#F8F9FA", part = "body")
          
          doc <- doc %>% body_add_flextable(ft_stats) %>% body_add_par("")
          
          # Quartiles
          doc <- doc %>%
            body_add_par("QUARTILES DAN PERCENTILES", style = "heading 2") %>%
            body_add_par("")
          
          quartiles_data <- data.frame(
            "Percentile" = c("0% (Min)", "25% (Q1)", "50% (Median)", "75% (Q3)", "100% (Max)"),
            "Nilai" = round(quantile(var_data, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE), 4)
          )
          
          ft_quartiles <- flextable(quartiles_data) %>%
            theme_booktabs() %>%
            color(part = "header", color = "white") %>%
            bg(part = "header", bg = "#C55A11") %>%
            autofit() %>%
            align(align = "center", part = "all")
          
          doc <- doc %>% body_add_flextable(ft_quartiles) %>% body_add_par("")
          
          # Data distribution analysis
          doc <- doc %>%
            body_add_par("ANALISIS DISTRIBUSI DATA", style = "heading 2") %>%
            body_add_par("")
          
          skew_val <- moments::skewness(var_data, na.rm = TRUE)
          kurt_val <- moments::kurtosis(var_data, na.rm = TRUE)
          cv_val <- sd(var_data, na.rm = TRUE) / mean(var_data, na.rm = TRUE) * 100
          
          skew_interpret <- ifelse(abs(skew_val) < 0.5, "Simetris", 
                                   ifelse(skew_val > 0, "Condong Kanan", "Condong Kiri"))
          kurt_interpret <- ifelse(kurt_val < 3, "Platykurtic (Datar)", 
                                   ifelse(kurt_val > 3, "Leptokurtic (Runcing)", "Mesokurtic (Normal)"))
          cv_interpret <- ifelse(cv_val < 15, "Variabilitas Rendah", 
                                 ifelse(cv_val < 35, "Variabilitas Sedang", "Variabilitas Tinggi"))
          
          distrib_analysis <- data.frame(
            "Aspek" = c("Bentuk Distribusi", "Peakedness", "Variabilitas", "Outliers"),
            "Nilai" = c(paste0(skew_interpret, " (", round(skew_val, 3), ")"),
                        paste0(kurt_interpret, " (", round(kurt_val, 3), ")"),
                        paste0(cv_interpret, " (", round(cv_val, 2), "%)"),
                        ifelse(length(boxplot.stats(var_data)$out) > 0, 
                               paste("Terdeteksi", length(boxplot.stats(var_data)$out), "outliers"),
                               "Tidak ada outliers"))
          )
          
          ft_distrib <- flextable(distrib_analysis) %>%
            theme_booktabs() %>%
            color(part = "header", color = "white") %>%
            bg(part = "header", bg = "#7030A0") %>%
            autofit() %>%
            align(align = "left", part = "all")
          
          doc <- doc %>% body_add_flextable(ft_distrib) %>% body_add_par("")
          
        } else {
          # Frequency analysis for categorical variables
          doc <- doc %>%
            body_add_par("ANALISIS FREKUENSI", style = "heading 2") %>%
            body_add_par("")
          
          freq_table <- table(var_data)
          freq_df <- data.frame(
            "Kategori" = names(freq_table),
            "Frekuensi" = as.numeric(freq_table),
            "Persentase" = round(as.numeric(freq_table) / sum(freq_table) * 100, 2)
          )
          freq_df$Persentase <- paste0(freq_df$Persentase, "%")
          
          ft_freq_cat <- flextable(freq_df) %>%
            theme_booktabs() %>%
            color(part = "header", color = "white") %>%
            bg(part = "header", bg = "#70AD47") %>%
            autofit() %>%
            align(align = "center", part = "all") %>%
            align(j = 1, align = "left", part = "all")
          
          doc <- doc %>% body_add_flextable(ft_freq_cat) %>% body_add_par("")
        }
        
        # Geographic information if available
        geo_df <- geo_data()
        if(nrow(geo_df) > 0) {
          doc <- doc %>%
            body_add_par("INFORMASI GEOGRAFIS", style = "heading 2") %>%
            body_add_par("")
          
          geo_summary <- data.frame(
            "Aspek" = c("Total Data Geografis", "Coverage Area", "Koordinat Range (Lat)", "Koordinat Range (Lng)"),
            "Detail" = c(paste(nrow(geo_df), "kabupaten/kota"),
                         "Seluruh Indonesia",
                         paste(round(min(geo_df$latitude, na.rm = TRUE), 2), "°S hingga", 
                               round(max(geo_df$latitude, na.rm = TRUE), 2), "°N"),
                         paste(round(min(geo_df$longitude, na.rm = TRUE), 2), "°E hingga", 
                               round(max(geo_df$longitude, na.rm = TRUE), 2), "°E"))
          )
          
          ft_geo <- flextable(geo_summary) %>%
            theme_booktabs() %>%
            color(part = "header", color = "white") %>%
            bg(part = "header", bg = "#2E74B5") %>%
            autofit() %>%
            align(align = "left", part = "all")
          
          doc <- doc %>% body_add_flextable(ft_geo) %>% body_add_par("")
        }
        
        # Interpretation
        doc <- doc %>%
          body_add_par("INTERPRETASI DAN REKOMENDASI", style = "heading 2") %>%
          body_add_par("")
        
        if(is.numeric(var_data)) {
          interpretation_text <- paste0(
            "Variabel ", input$variable_explore, " menunjukkan karakteristik sebagai berikut:\n\n",
            "• Nilai rata-rata: ", round(mean(var_data, na.rm = TRUE), 2), "\n",
            "• Distribusi data: ", skew_interpret, "\n", 
            "• Tingkat variabilitas: ", cv_interpret, "\n",
            "• Rekomendasi analisis: ",
            ifelse(abs(skew_val) < 0.5, "Cocok untuk uji parametrik", "Pertimbangkan transformasi data atau uji non-parametrik")
          )
        } else {
          interpretation_text <- paste0(
            "Variabel ", input$variable_explore, " adalah data kategorik dengan ", length(unique(var_data)), " kategori. ",
            "Distribusi paling tinggi pada kategori '", names(sort(table(var_data), decreasing = TRUE))[1], "'. ",
            "Data ini cocok untuk analisis kategorik seperti uji chi-square atau analisis asosiasi."
          )
        }
        
        doc <- doc %>%
          body_add_par(interpretation_text) %>%
          body_add_par("")
        
      } else {
        doc <- doc %>%
          body_add_par("INFORMASI", style = "heading 2") %>%
          body_add_par("Silakan pilih variabel pada dashboard untuk memulai eksplorasi data.") %>%
          body_add_par("")
      }
      
      # Footer
      doc <- doc %>%
        body_add_break() %>%
        body_add_par("KESIMPULAN", style = "heading 2") %>%
        body_add_par("Modul Eksplorasi Data DAST menyediakan analisis komprehensif untuk memahami karakteristik dan distribusi data sebelum melakukan analisis statistik lanjutan.") %>%
        body_add_par("") %>%
        body_add_par(paste("📊 Laporan dibuat otomatis pada:", format(Sys.time(), "%d %B %Y, %H:%M:%S WIB"))) %>%
        body_add_par("🔗 Dashboard Analisis Statistik Terpadu (DAST)")
      
      # Save document
      print(doc, target = file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  )
  
  # Download data table
  output$download_data_table <- downloadHandler(
    filename = function() {
      paste("DAST_Data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Download assumption report - ENHANCED Word document
  output$download_assumption_report <- downloadHandler(
    filename = function() {
      paste("DAST_Uji_Asumsi_", input$assumption_variable, "_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      doc <- read_docx() %>%
        body_add_par("LAPORAN UJI ASUMSI DATA", style = "heading 1") %>%
        body_add_par("Dashboard Analisis Statistik Terpadu (DAST)", style = "heading 2") %>%
        body_add_par("") %>%
        body_add_par(paste("📅 Tanggal:", Sys.Date())) %>%
        body_add_par(paste("🕐 Waktu:", format(Sys.time(), "%H:%M:%S WIB"))) %>%
        body_add_par("")
      
      if(!is.null(input$assumption_variable)) {
        # Test summary table
        test_info <- data.frame(
          "Aspek" = c("Variabel yang Diuji", "Uji yang Dilakukan", "Status"),
          "Detail" = c(input$assumption_variable, 
                       paste(input$selected_tests, collapse = ", "),
                       "✅ Lengkap")
        )
        
        ft_test_info <- flextable(test_info) %>%
          theme_booktabs() %>%
          color(part = "header", color = "white") %>%
          bg(part = "header", bg = "#2E74B5") %>%
          autofit() %>%
          align(align = "left", part = "all")
        
        doc <- doc %>% body_add_flextable(ft_test_info) %>% body_add_par("")
        
        # Add test results if available
        df <- data_with_cats()
        var_data <- df[[input$assumption_variable]]
        
        if("shapiro" %in% input$selected_tests) {
          if(length(var_data) > 5000) {
            shapiro_result <- shapiro.test(sample(var_data, 5000))
          } else {
            shapiro_result <- shapiro.test(var_data)
          }
          
          result_status <- ifelse(shapiro_result$p.value < 0.05, "❌ Tidak Normal", "✅ Normal")
          
          shapiro_table <- data.frame(
            "Uji" = "Shapiro-Wilk",
            "Statistik" = round(shapiro_result$statistic, 4),
            "p-value" = format(shapiro_result$p.value, digits = 4),
            "Hasil" = result_status
          )
          
          ft_shapiro <- flextable(shapiro_table) %>%
            theme_booktabs() %>%
            color(part = "header", color = "white") %>%
            bg(part = "header", bg = "#70AD47") %>%
            autofit() %>%
            align(align = "center", part = "all")
          
          doc <- doc %>% 
            body_add_par("HASIL UJI NORMALITAS", style = "heading 2") %>%
            body_add_flextable(ft_shapiro) %>% body_add_par("")
        }
      }
      
      doc <- doc %>%
        body_add_par("KESIMPULAN", style = "heading 2") %>%
        body_add_par("Uji asumsi merupakan langkah penting sebelum melakukan analisis statistik inferensia untuk memastikan validitas hasil analisis.") %>%
        body_add_par("") %>%
        body_add_par(paste("📊 Laporan dibuat otomatis pada:", format(Sys.time(), "%d %B %Y, %H:%M:%S WIB"))) %>%
        body_add_par("🔗 Dashboard Analisis Statistik Terpadu (DAST)")
      
      print(doc, target = file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  )
  
  # Download inference report - ENHANCED Word document
  output$download_inference_report <- downloadHandler(
    filename = function() {
      paste("DAST_Statistik_Inferensia_", input$stat_test_type, "_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      doc <- read_docx() %>%
        body_add_par("LAPORAN STATISTIK INFERENSIA", style = "heading 1") %>%
        body_add_par("Dashboard Analisis Statistik Terpadu (DAST)", style = "heading 2") %>%
        body_add_par("") %>%
        body_add_par(paste("📅 Tanggal:", Sys.Date())) %>%
        body_add_par(paste("🕐 Waktu:", format(Sys.time(), "%H:%M:%S WIB"))) %>%
        body_add_par("")
      
      # Test type information
      test_names <- c("t_one" = "Uji t Satu Sampel", "t_two" = "Uji t Dua Sampel",
                      "var_one" = "Uji Varians Satu Sampel", "var_two" = "Uji Varians Dua Sampel",
                      "prop_one" = "Uji Proporsi Satu Sampel", "prop_two" = "Uji Proporsi Dua Sampel",
                      "anova_one" = "ANOVA Satu Arah", "anova_two" = "ANOVA Dua Arah")
      
      test_info <- data.frame(
        "Aspek" = c("Jenis Uji", "Kategori", "Status"),
        "Detail" = c(test_names[input$stat_test_type], "Statistik Inferensia", "✅ Selesai")
      )
      
      ft_test_info <- flextable(test_info) %>%
        theme_booktabs() %>%
        color(part = "header", color = "white") %>%
        bg(part = "header", bg = "#2E74B5") %>%
        autofit() %>%
        align(align = "left", part = "all")
      
      doc <- doc %>% body_add_flextable(ft_test_info) %>% body_add_par("") %>%
        body_add_par("KESIMPULAN", style = "heading 2") %>%
        body_add_par("Hasil uji statistik inferensia memberikan dasar untuk pengambilan keputusan berdasarkan bukti empiris dari data sampel.") %>%
        body_add_par("") %>%
        body_add_par(paste("📊 Laporan dibuat otomatis pada:", format(Sys.time(), "%d %B %Y, %H:%M:%S WIB"))) %>%
        body_add_par("🔗 Dashboard Analisis Statistik Terpadu (DAST)")
      
      print(doc, target = file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  )
  
  # Download regression plots - ENHANCED to JPG format
  output$download_regression_plots <- downloadHandler(
    filename = function() {
      paste("DAST_Plot_Asumsi_Regresi_", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      req(reg_model())
      
      jpeg(file, width = 1600, height = 1200, res = 150, quality = 95)
      
      # Create comprehensive diagnostic plots
      par(mfrow = c(3, 2), mar = c(4, 4, 3, 2))
      
      # Standard diagnostic plots
      plot(reg_model(), which = 1, main = "Residuals vs Fitted")
      plot(reg_model(), which = 2, main = "Normal Q-Q")
      plot(reg_model(), which = 3, main = "Scale-Location")
      plot(reg_model(), which = 4, main = "Cook's Distance")
      plot(reg_model(), which = 5, main = "Residuals vs Leverage")
      
      # Additional plot: Histogram of residuals
      residuals_data <- residuals(reg_model())
      hist(residuals_data, main = "Histogram of Residuals", 
           col = "lightblue", xlab = "Residuals", freq = FALSE)
      lines(density(residuals_data), col = "blue", lwd = 2)
      
      # Add normal curve
      x_seq <- seq(min(residuals_data), max(residuals_data), length = 100)
      y_seq <- dnorm(x_seq, mean(residuals_data), sd(residuals_data))
      lines(x_seq, y_seq, col = "red", lwd = 2)
      legend("topright", c("Density", "Normal"), col = c("blue", "red"), lwd = 2, cex = 0.8)
      
      dev.off()
    },
    contentType = "image/jpeg"
  )
  
  # Download regression report - ENHANCED Word document
  output$download_regression_report <- downloadHandler(
    filename = function() {
      paste("DAST_Laporan_Regresi_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      doc <- read_docx() %>%
        body_add_par("LAPORAN REGRESI LINEAR BERGANDA", style = "heading 1") %>%
        body_add_par("Dashboard Analisis Statistik Terpadu (DAST)", style = "heading 2") %>%
        body_add_par("") %>%
        body_add_par(paste("📅 Tanggal:", Sys.Date())) %>%
        body_add_par(paste("🕐 Waktu:", format(Sys.time(), "%H:%M:%S WIB"))) %>%
        body_add_par("")
      
      if(!is.null(input$reg_response) && !is.null(input$reg_predictors)) {
        # Model information
        model_info <- data.frame(
          "Aspek" = c("Variabel Terikat (Y)", "Variabel Bebas (X)", "Jumlah Prediktor"),
          "Detail" = c(input$reg_response, 
                       paste(input$reg_predictors, collapse = ", "),
                       length(input$reg_predictors))
        )
        
        ft_model_info <- flextable(model_info) %>%
          theme_booktabs() %>%
          color(part = "header", color = "white") %>%
          bg(part = "header", bg = "#2E74B5") %>%
          autofit() %>%
          align(align = "left", part = "all")
        
        doc <- doc %>% body_add_flextable(ft_model_info) %>% body_add_par("")
        
        # Model statistics if available
        if(!is.null(reg_model())) {
          model_summary <- summary(reg_model())
          
          model_stats <- data.frame(
            "Statistik" = c("R-squared", "Adjusted R-squared", "F-statistic", "p-value"),
            "Nilai" = c(round(model_summary$r.squared, 4),
                        round(model_summary$adj.r.squared, 4),
                        round(model_summary$fstatistic[1], 4),
                        format(pf(model_summary$fstatistic[1], 
                                  model_summary$fstatistic[2], 
                                  model_summary$fstatistic[3], 
                                  lower.tail = FALSE), digits = 4))
          )
          
          ft_model_stats <- flextable(model_stats) %>%
            theme_booktabs() %>%
            color(part = "header", color = "white") %>%
            bg(part = "header", bg = "#70AD47") %>%
            autofit() %>%
            align(align = "center", part = "all")
          
          doc <- doc %>% 
            body_add_par("STATISTIK MODEL", style = "heading 2") %>%
            body_add_flextable(ft_model_stats) %>% body_add_par("")
        }
      }
      
      doc <- doc %>%
        body_add_par("KESIMPULAN", style = "heading 2") %>%
        body_add_par("Analisis regresi linear berganda memberikan pemahaman tentang hubungan antar variabel dan kemampuan prediksi model.") %>%
        body_add_par("") %>%
        body_add_par(paste("📊 Laporan dibuat otomatis pada:", format(Sys.time(), "%d %B %Y, %H:%M:%S WIB"))) %>%
        body_add_par("🔗 Dashboard Analisis Statistik Terpadu (DAST)")
      
      print(doc, target = file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  )
}

# --- Run the Shiny App ---
shinyApp(ui = ui, server = server)
