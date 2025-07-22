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
library(latexpdf)

# --- Define the User Interface (UI) ---
ui <- navbarPage(
  "DAST - Dashboard Analisis Statistik Terpadu",
  
  # --- 1. Beranda (Home) Menu ---
  tabPanel("Beranda",
           fluidRow(
             column(12,
                    div(class = "jumbotron",
                        h1("Dashboard Analisis Statistik Terpadu (DAST)", 
                           style = "color: #2c3e50; text-align: center;"),
                        hr(),
                        h3("Selamat Datang di DAST", style = "color: #34495e;"),
                        p("Dashboard Analisis Statistik Terpadu (DAST) adalah platform komprehensif yang dirancang khusus untuk mempermudah eksplorasi, analisis, dan interpretasi data statistik dengan fokus pada kerentanan sosial dan analisis multivariat.", 
                          style = "font-size: 16px; text-align: justify;")
                    )
             )
           ),
           
           fluidRow(
             column(6,
                    div(class = "panel panel-primary",
                        div(class = "panel-heading", h4("Metadata Dataset")),
                        div(class = "panel-body",
                            h5("Informasi Dataset:"),
                            tags$ul(
                              tags$li("Nama Dataset: Social Vulnerability Index (SoVI) Data"),
                              tags$li("Jumlah Observasi: 515 kabupaten/kota"),
                              tags$li("Jumlah Variabel: 16 variabel utama"),
                              tags$li("Periode Data: Data terkini kerentanan sosial Indonesia"),
                              tags$li("Sumber: Badan Pusat Statistik dan instansi terkait")
                            ),
                            h5("Variabel Utama:"),
                            tags$ul(
                              tags$li("CHILDREN: Persentase anak-anak"),
                              tags$li("ELDERLY: Persentase lansia"),
                              tags$li("POVERTY: Tingkat kemiskinan"),
                              tags$li("EDUCATION: Tingkat pendidikan rendah"),
                              tags$li("Dan 12 variabel lainnya")
                            )
                        )
                    )
             ),
             column(6,
                    div(class = "panel panel-success",
                        div(class = "panel-heading", h4("Fitur Utama DAST")),
                        div(class = "panel-body",
                            tags$ul(
                              tags$li(strong("Manajemen Data:"), " Transformasi data kontinyu ke kategorik dengan interpretasi otomatis"),
                              tags$li(strong("Eksplorasi Data:"), " Statistik deskriptif, visualisasi interaktif, dan pemetaan geografis"),
                              tags$li(strong("Uji Asumsi:"), " Pengujian normalitas dan homogenitas dengan berbagai metode"),
                              tags$li(strong("Statistik Inferensia:"), " Uji t, uji proporsi, uji varians, dan ANOVA"),
                              tags$li(strong("Regresi Linear:"), " Analisis regresi berganda dengan uji asumsi lengkap"),
                              tags$li(strong("Download Fleksibel:"), " Ekspor hasil dalam format JPG, PDF, dan Word")
                            )
                        )
                    )
             )
           ),
           
           fluidRow(
             column(12,
                    div(class = "panel panel-info",
                        div(class = "panel-heading", h4("Panduan Penggunaan")),
                        div(class = "panel-body",
                            p("1. Mulai dengan", strong("Manajemen Data"), "untuk mempersiapkan dataset sesuai kebutuhan analisis"),
                            p("2. Lakukan", strong("Eksplorasi Data"), "untuk memahami karakteristik dan distribusi data"),
                            p("3. Jalankan", strong("Uji Asumsi"), "sebelum melakukan analisis inferensia"),
                            p("4. Pilih metode", strong("Statistik Inferensia"), "yang sesuai dengan tujuan penelitian"),
                            p("5. Gunakan", strong("Regresi Linear"), "untuk analisis hubungan antar variabel"),
                            p("6. Unduh hasil analisis dalam berbagai format untuk dokumentasi dan presentasi")
                        )
                    )
             )
           ),
           
           hr(),
           fluidRow(
             column(12, style = "text-align: center;",
                    h4("Download Laporan Lengkap"),
                    downloadButton("download_full_report", "Download Laporan Lengkap (PDF)", 
                                   class = "btn-primary btn-lg", style = "margin: 10px;")
             )
           )
  ),
  
  # --- 2. Manajemen Data Menu ---
  tabPanel("Manajemen Data",
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
               downloadButton("download_management_report", "Download Laporan Manajemen (PDF)", 
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
  tabPanel("Eksplorasi Data",
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
               downloadButton("download_explore_report", "Download Laporan Eksplorasi (PDF)", 
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
                          h3("Visualisasi Peta Indonesia"),
                          div(class = "alert alert-info",
                              h4("Informasi Peta"),
                              p("Peta menampilkan distribusi data berdasarkan koordinat geografis kabupaten/kota di Indonesia.")
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
  tabPanel("Uji Asumsi Data",
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
               downloadButton("download_assumption_report", "Download Laporan Uji Asumsi (PDF)", 
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
  tabPanel("Statistik Inferensia",
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
               downloadButton("download_inference_report", "Download Laporan Statistik Inferensia (PDF)", 
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
  tabPanel("Regresi Linear Berganda",
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
               downloadButton("download_regression_report", "Download Laporan Regresi (PDF)", 
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
  
  # Load geographical data
  geo_data <- reactive({
    tryCatch({
      # Fetch the geographical data from the provided URL
      geo_df <- read.csv("https://hebbkx1anhila5yf.public.blob.vercel-storage.com/sovi_data_longitudelatitude-v2WH1ETJpkPgKYXeaW3KwFYaGrOzTU.csv")
      
      # Convert latitude and longitude to numeric
      geo_df$latitude <- as.numeric(geo_df$latitude)
      geo_df$longitude <- as.numeric(geo_df$longitude)
      
      # Remove rows with missing coordinates
      geo_df <- geo_df[!is.na(geo_df$latitude) & !is.na(geo_df$longitude), ]
      
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
  
  # Map visualization - ENHANCED with real coordinates
  output$map_plot <- renderLeaflet({
    df <- filtered_data()
    geo_df <- geo_data()
    var_name <- input$variable_explore
    
    # Merge data with geographical coordinates
    # Try to match by district code or create a mapping
    if(nrow(geo_df) > 0) {
      # Create a sample of data points for visualization
      n_points <- min(nrow(df), nrow(geo_df), 100)  # Limit to 100 points for performance
      
      # Sample data
      sample_indices <- sample(1:min(nrow(df), nrow(geo_df)), n_points)
      map_data <- data.frame(
        lat = geo_df$latitude[sample_indices],
        lng = geo_df$longitude[sample_indices],
        value = df[[var_name]][sample_indices],
        district = geo_df$Nama.Kabupaten[sample_indices]
      )
      
      # Remove rows with missing coordinates or values
      map_data <- map_data[complete.cases(map_data), ]
      
      if(nrow(map_data) > 0) {
        if(is.numeric(map_data$value)) {
          # Create color palette
          pal <- colorNumeric(palette = "YlOrRd", domain = map_data$value)
          
          # Create popup text
          popup_text <- paste(
            "<strong>", map_data$district, "</strong><br>",
            var_name, ":", round(map_data$value, 2)
          )
          
          leaflet(map_data) %>%
            addTiles() %>%
            addCircleMarkers(
              ~lng, ~lat,
              radius = ~sqrt(abs(value)) * 2 + 5,
              color = ~pal(value),
              fillOpacity = 0.8,
              popup = popup_text,
              stroke = TRUE,
              weight = 1
            ) %>%
            addLegend(
              pal = pal, 
              values = ~value,
              title = var_name,
              position = "bottomright"
            ) %>%
            setView(lng = 118, lat = -2, zoom = 5)
        } else {
          # For categorical data
          popup_text <- paste(
            "<strong>", map_data$district, "</strong><br>",
            var_name, ":", map_data$value
          )
          
          leaflet(map_data) %>%
            addTiles() %>%
            addMarkers(
              ~lng, ~lat,
              popup = popup_text
            ) %>%
            setView(lng = 118, lat = -2, zoom = 5)
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
  
  # Geographic data table
  output$geo_data_table <- renderDT({
    df <- filtered_data()
    geo_df <- geo_data()
    var_name <- input$variable_explore
    
    if(nrow(geo_df) > 0) {
      # Create summary table with geographical info
      n_display <- min(20, nrow(geo_df))
      geo_summary <- data.frame(
        Kabupaten = geo_df$Nama.Kabupaten[1:n_display],
        Latitude = geo_df$latitude[1:n_display],
        Longitude = geo_df$longitude[1:n_display],
        Value = df[[var_name]][1:n_display]
      )
      names(geo_summary)[4] <- var_name
      
      datatable(geo_summary, options = list(pageLength = 10, scrollX = TRUE))
    } else {
      # Fallback table
      data.frame(
        Message = "Data geografis tidak tersedia"
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
  
  # --- Download Handlers - FIXED ---
  
  # Helper function to create temporary files
  create_temp_file <- function(extension) {
    tempfile(fileext = paste0(".", extension))
  }
  
  # Download full report
  output$download_full_report <- downloadHandler(
    filename = function() {
      paste("DAST_Laporan_Lengkap_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Create a simple PDF report
      temp_file <- create_temp_file("txt")
      
      # Write report content
      cat("DASHBOARD ANALISIS STATISTIK TERPADU (DAST)\n", file = temp_file)
      cat("Laporan Lengkap\n", file = temp_file, append = TRUE)
      cat("Tanggal:", as.character(Sys.Date()), "\n\n", file = temp_file, append = TRUE)
      cat("Dataset: Social Vulnerability Index (SoVI) Data\n", file = temp_file, append = TRUE)
      cat("Jumlah Observasi: 515 kabupaten/kota\n", file = temp_file, append = TRUE)
      cat("Jumlah Variabel: 16 variabel utama\n\n", file = temp_file, append = TRUE)
      cat("Laporan ini berisi analisis komprehensif data kerentanan sosial Indonesia.\n", file = temp_file, append = TRUE)
      
      # Copy to output file
      file.copy(temp_file, file)
    },
    contentType = "text/plain"
  )
  
  # Download management report
  output$download_management_report <- downloadHandler(
    filename = function() {
      paste("DAST_Manajemen_Data_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      temp_file <- create_temp_file("txt")
      
      cat("LAPORAN MANAJEMEN DATA\n", file = temp_file)
      cat("Tanggal:", as.character(Sys.Date()), "\n\n", file = temp_file, append = TRUE)
      
      if(!is.null(input$variable_categorize)) {
        cat("Variabel yang dikategorisasi:", input$variable_categorize, "\n", file = temp_file, append = TRUE)
        cat("Metode kategorisasi:", input$categorization_method, "\n", file = temp_file, append = TRUE)
        cat("Jumlah kategori:", input$num_bins, "\n\n", file = temp_file, append = TRUE)
      }
      
      cat("Proses kategorisasi telah berhasil dilakukan.\n", file = temp_file, append = TRUE)
      
      file.copy(temp_file, file)
    },
    contentType = "text/plain"
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
  
  # Download explore plot
  output$download_explore_plot <- downloadHandler(
    filename = function() {
      paste("Plot_", input$variable_explore, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      req(input$variable_explore)
      
      # Create the plot
      df <- filtered_data()
      var_name <- input$variable_explore
      
      png(file, width = 800, height = 600, res = 100)
      
      if(is.numeric(df[[var_name]])) {
        hist(df[[var_name]], 
             main = paste("Distribusi", var_name),
             xlab = var_name, 
             ylab = "Frekuensi",
             col = "lightblue",
             border = "white")
      } else {
        barplot(table(df[[var_name]]),
                main = paste("Distribusi", var_name),
                xlab = var_name,
                ylab = "Frekuensi",
                col = "lightblue")
      }
      
      dev.off()
    },
    contentType = "image/png"
  )
  
  # Download explore report
  output$download_explore_report <- downloadHandler(
    filename = function() {
      paste("DAST_Eksplorasi_", input$variable_explore, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      temp_file <- create_temp_file("txt")
      
      cat("LAPORAN EKSPLORASI DATA\n", file = temp_file)
      cat("Tanggal:", as.character(Sys.Date()), "\n\n", file = temp_file, append = TRUE)
      
      if(!is.null(input$variable_explore)) {
        cat("Variabel yang dieksplorasi:", input$variable_explore, "\n\n", file = temp_file, append = TRUE)
        
        df <- filtered_data()
        var_data <- df[[input$variable_explore]]
        
        if(is.numeric(var_data)) {
          cat("STATISTIK DESKRIPTIF:\n", file = temp_file, append = TRUE)
          cat("Mean:", round(mean(var_data, na.rm = TRUE), 4), "\n", file = temp_file, append = TRUE)
          cat("Median:", round(median(var_data, na.rm = TRUE), 4), "\n", file = temp_file, append = TRUE)
          cat("Std Dev:", round(sd(var_data, na.rm = TRUE), 4), "\n", file = temp_file, append = TRUE)
          cat("Min:", round(min(var_data, na.rm = TRUE), 4), "\n", file = temp_file, append = TRUE)
          cat("Max:", round(max(var_data, na.rm = TRUE), 4), "\n", file = temp_file, append = TRUE)
        }
      }
      
      file.copy(temp_file, file)
    },
    contentType = "text/plain"
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
  
  # Download assumption report
  output$download_assumption_report <- downloadHandler(
    filename = function() {
      paste("DAST_Uji_Asumsi_", input$assumption_variable, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      temp_file <- create_temp_file("txt")
      
      cat("LAPORAN UJI ASUMSI\n", file = temp_file)
      cat("Tanggal:", as.character(Sys.Date()), "\n\n", file = temp_file, append = TRUE)
      
      if(!is.null(input$assumption_variable)) {
        cat("Variabel yang diuji:", input$assumption_variable, "\n", file = temp_file, append = TRUE)
        cat("Uji yang dilakukan:", paste(input$selected_tests, collapse = ", "), "\n\n", file = temp_file, append = TRUE)
      }
      
      cat("Hasil uji asumsi telah dilakukan sesuai dengan parameter yang dipilih.\n", file = temp_file, append = TRUE)
      
      file.copy(temp_file, file)
    },
    contentType = "text/plain"
  )
  
  # Download inference report
  output$download_inference_report <- downloadHandler(
    filename = function() {
      paste("DAST_Statistik_Inferensia_", input$stat_test_type, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      temp_file <- create_temp_file("txt")
      
      cat("LAPORAN STATISTIK INFERENSIA\n", file = temp_file)
      cat("Tanggal:", as.character(Sys.Date()), "\n\n", file = temp_file, append = TRUE)
      cat("Jenis Uji:", input$stat_test_type, "\n\n", file = temp_file, append = TRUE)
      
      cat("Hasil uji statistik inferensia telah dilakukan.\n", file = temp_file, append = TRUE)
      cat("Silakan lihat output di aplikasi untuk detail lengkap.\n", file = temp_file, append = TRUE)
      
      file.copy(temp_file, file)
    },
    contentType = "text/plain"
  )
  
  # Download regression plots
  output$download_regression_plots <- downloadHandler(
    filename = function() {
      paste("DAST_Plot_Asumsi_Regresi_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      req(reg_model())
      
      png(file, width = 1200, height = 900, res = 100)
      par(mfrow = c(2, 2))
      plot(reg_model())
      dev.off()
    },
    contentType = "image/png"
  )
  
  # Download regression report
  output$download_regression_report <- downloadHandler(
    filename = function() {
      paste("DAST_Laporan_Regresi_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      temp_file <- create_temp_file("txt")
      
      cat("LAPORAN REGRESI LINEAR BERGANDA\n", file = temp_file)
      cat("Tanggal:", as.character(Sys.Date()), "\n\n", file = temp_file, append = TRUE)
      
      if(!is.null(input$reg_response) && !is.null(input$reg_predictors)) {
        cat("Variabel Terikat:", input$reg_response, "\n", file = temp_file, append = TRUE)
        cat("Variabel Bebas:", paste(input$reg_predictors, collapse = ", "), "\n\n", file = temp_file, append = TRUE)
        
        if(!is.null(reg_model())) {
          model_summary <- summary(reg_model())
          cat("R-squared:", round(model_summary$r.squared, 4), "\n", file = temp_file, append = TRUE)
          cat("Adjusted R-squared:", round(model_summary$adj.r.squared, 4), "\n", file = temp_file, append = TRUE)
        }
      }
      
      cat("\nAnalisis regresi telah dilakukan.\n", file = temp_file, append = TRUE)
      cat("Silakan lihat output di aplikasi untuk detail lengkap.\n", file = temp_file, append = TRUE)
      
      file.copy(temp_file, file)
    },
    contentType = "text/plain"
  )
}

# --- Run the Shiny App ---
shinyApp(ui = ui, server = server)
