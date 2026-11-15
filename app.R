# =====================================================================
# UTS EVDM 26.1 - Eksplorasi & Visualisasi Data Multivariat
# Topik    : Faktor-faktor yang memengaruhi harga rumah di DKI Jakarta
# Nama     : ROSWITA ASMELITA NESTI POTE 
# NIM      : 241064007
# Dosen    : Inggit Fatika, S.Stat., M.Stat
# Instansi : Universitas AKPRIND Indonesia
# =====================================================================


#Memangil package
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(GGally)
library(ggcorrplot)
library(treemap)
library(car)
library(lmtest)
library(mvnormtest)
library(readxl)

# ---------------------------------------------------------------------
# 1. MEMBACA DAN MEMBERSIHKAN DATA
# ---------------------------------------------------------------------
file_path <- "SEMESTER 5/EKSPLORASI DAN VISUALISASI DATA MULTIVARIAT/jakarta_house.xlsx"
raw <- as.data.frame(read_excel(file_path))

selected_vars <- c("city", "district", "bed_rooms", "bath_rooms", "carport", "land_area", "building_area", "price_miliar")
cat_vars      <- c("city", "district", "carport_k", "kamar_k")
num_vars      <- c("price_miliar", "bed_rooms", "bath_rooms", "carport", "land_area", "building_area")

read_house_data <- function(file_path = NULL) {
  candidates <- c(file_path, "jakarta_house.xlsx", "SEMESTER 5/EKSPLORASI DAN VISUALISASI DATA MULTIVARIAT/jakarta_house.xlsx")
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0) return(NULL)
  
  dat <- as.data.frame(read_excel(found[1]))
  
  if ("price" %in% names(dat) && !("price_miliar" %in% names(dat))) {
    dat$price <- as.numeric(gsub(",", ".", as.character(dat$price)))
    dat$price_miliar <- dat$price / 1e9
  }
  
  dat <- dat[!is.na(dat$land_area) & !is.na(dat$building_area), ]
  dat <- dat[dat$price_miliar >= 0.1, ] 
  dat <- dat[dat$land_area >= 10 & dat$building_area >= 10, ]
  dat$city <- factor(dat$city, levels = c("Jakarta Pusat", "Jakarta Barat", "Jakarta Selatan", "Jakarta Timur", "Jakarta Utara"))
  if ("district" %in% names(dat)) dat$district <- as.factor(dat$district)
  
  dat$log_price    <- log(dat$price_miliar)
  dat$log_land     <- log(dat$land_area)
  dat$log_building <- log(dat$building_area)
  return(dat)
}

data_hasil <- read_house_data()

label_var <- c(price_miliar = "Harga (Rp miliar)", bed_rooms = "Kamar tidur",
               bath_rooms = "Kamar mandi", carport = "Carport",
               land_area = "Luas tanah (m2)", building_area = "Luas bangunan (m2)",
               city = "Kota", district = "Kawasan (district)")

cat_uni <- c("city", "district")
cat_biv <- c("city", "district")
is_cat  <- function(v) v %in% cat_vars

pilih_uni <- setNames(c(num_vars, cat_uni), label_var[c(num_vars, cat_uni)])
pilih_biv <- setNames(c(num_vars, cat_biv), label_var[c(num_vars, cat_biv)])
pilih_num <- setNames(num_vars, label_var[num_vars])

# ---------------------------------------------------------------------
# 2. TEMA GRAFIK & PALET WARNA (GAYA UPIN IPIN: CERIA & HANGAT)
# ---------------------------------------------------------------------
p_yellow <- "#FFD166"; p_orange <- "#F4A261"; p_blue <- "#2A9D8F"
p_green  <- "#264653"; p_red    <- "#E76F51"; ink  <- "#2B2D42"

theme_upin <- function() {
  theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", color = ink, size = 15),
          plot.subtitle = element_text(color = "#6C757D"),
          axis.title = element_text(face = "bold", color = ink),
          axis.text = element_text(color = ink),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#E9ECEF"),
          plot.background = element_rect(fill = "#FFFDF9", color = NA),
          legend.position = "top")
}

pal5 <- c(p_orange, p_blue, p_yellow, p_red, p_green)
pick_pal <- function(n) rep_len(pal5, n)

skew_fun <- function(x) {
  x <- x[!is.na(x)]; n <- length(x)
  if (n < 3) return(NA_real_)
  n / ((n - 1) * (n - 2)) * sum(((x - mean(x)) / sd(x))^3)
}

uni_choices <- function(v) {
  if (is_cat(v)) c("Diagram batang", "Diagram lingkaran")
  else c("Histogram", "Histogram + Density", "Density", "Boxplot", "QQ-plot (cek kenormalan)")
}

bi_choices <- function(x, y) {
  xc <- is_cat(x); yc <- is_cat(y)
  if (!xc && !yc) c("Scatter plot + garis tren", "Line plot (rata-rata Y per X)", "Heatmap kepadatan (2D)")
  else if (xc != yc) c("Boxplot", "Violin + boxplot", "Diagram batang rata-rata (+/- SE)")
  else c("Batang berkelompok", "Batang bertumpuk 100%")
}

plot_uni <- function(d, v, type) {
  if (is_cat(v)) {
    if (type == "Diagram lingkaran") {
      tab <- as.data.frame(table(d[[v]]))
      names(tab) <- c("Kategori", "n"); tab$pct <- round(100 * tab$n / sum(tab$n), 1)
      return(ggplot(tab, aes(x = "", y = n, fill = Kategori)) +
               geom_col(width = 1, color = "white") + coord_polar("y", start = 0) +
               geom_text(aes(label = paste0(pct, "%")), position = position_stack(vjust = 0.5), fontface = "bold", color = "white") +
               scale_fill_manual(values = pick_pal(nrow(tab))) + labs(title = paste("Proporsi", v), fill = v) +
               theme_void(base_size = 13))
    }
    return(ggplot(d, aes(x = .data[[v]], fill = .data[[v]])) +
             geom_bar(show.legend = FALSE) +
             geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.4, fontface = "bold", color = ink) +
             scale_fill_manual(values = pick_pal(nlevels(d[[v]]))) +
             labs(title = paste("Frekuensi", v), x = v, y = "Frekuensi") + theme_upin() +
             theme(axis.text.x = element_text(angle = 20, hjust = 1)))
  }
  if (type == "Histogram + Density") {
    return(ggplot(d, aes(x = .data[[v]])) +
             geom_histogram(aes(y = after_stat(density)), bins = 25, fill = p_orange, color = "white") +
             geom_density(color = p_blue, linewidth = 1.2) +
             labs(title = paste("Histogram + Density", v), x = v, y = "Densitas") + theme_upin())
  }
  if (type == "Density") {
    return(ggplot(d, aes(x = .data[[v]])) +
             geom_density(fill = p_yellow, color = p_orange, alpha = 0.6, linewidth = 1) +
             geom_vline(xintercept = mean(d[[v]], na.rm = TRUE), linetype = "dashed", color = p_red) +
             labs(title = paste("Density", v), subtitle = "Garis putus-putus = rata-rata", x = v, y = "Densitas") + theme_upin())
  }
  if (type == "Boxplot") {
    return(ggplot(d, aes(x = "", y = .data[[v]])) +
             geom_boxplot(fill = p_yellow, width = 0.5, outlier.color = p_red) +
             stat_summary(fun = mean, geom = "point", shape = 23, size = 3.5, fill = "white") + coord_flip() +
             labs(title = paste("Boxplot", v), subtitle = "Berlian putih = rata-rata", x = NULL, y = v) + theme_upin())
  }
  if (type == "QQ-plot (cek kenormalan)") {
    return(ggplot(d, aes(sample = .data[[v]])) + stat_qq(color = p_blue, alpha = 0.6) + stat_qq_line(color = p_red, linewidth = 1) +
             labs(title = paste("QQ-plot", v), subtitle = "Titik mengikuti garis = mendekati normal", x = "Kuantil teoretis", y = "Kuantil sampel") + theme_upin())
  }
  ggplot(d, aes(x = .data[[v]])) +
    geom_histogram(bins = 25, fill = p_orange, color = "white") +
    labs(title = paste("Distribusi", v), x = v, y = "Frekuensi") + theme_upin()
}

plot_bi <- function(d, x, y, type) {
  xc <- is_cat(x); yc <- is_cat(y)
  if (!xc && !yc) {
    if (type == "Line plot (rata-rata Y per X)") {
      xv <- d[[x]]; yv <- d[[y]]; grp <- if (length(unique(xv)) <= 12) xv else cut(xv, breaks = 10)
      agg <- data.frame(gx = as.numeric(tapply(xv, grp, mean)), my = as.numeric(tapply(yv, grp, mean)),
                        se = as.numeric(tapply(yv, grp, function(z) sd(z) / sqrt(length(z)))))
      return(ggplot(agg, aes(x = gx, y = my)) +
               geom_ribbon(aes(ymin = my - se, ymax = my + se), fill = p_yellow, alpha = 0.5) +
               geom_line(color = p_blue, linewidth = 1.1) + geom_point(color = p_blue, size = 2.8) +
               labs(x = x, y = paste("Rata-rata", y), title = paste("Rata-rata", y, "menurut", x)) + theme_upin())
    }
    return(ggplot(d, aes(x = .data[[x]], y = .data[[y]])) +
             geom_point(alpha = 0.5, color = p_blue, size = 2) +
             geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = p_red, linewidth = 1) +
             labs(x = x, y = y, title = paste(x, "vs", y)) + theme_upin())
  }
  if (xc != yc) {
    cv <- if (xc) x else y; nv <- if (xc) y else x; ncat <- nlevels(d[[cv]])
    return(ggplot(d, aes(x = .data[[cv]], y = .data[[nv]], fill = .data[[cv]])) +
             geom_boxplot(show.legend = FALSE, alpha = 0.8) + stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
             scale_fill_manual(values = pick_pal(ncat)) + labs(x = cv, y = nv, title = paste(nv, "berdasarkan", cv)) + theme_upin())
  }
  ny <- nlevels(d[[y]])
  ggplot(d, aes(x = .data[[x]], fill = .data[[y]])) + geom_bar(position = "dodge") +
    scale_fill_manual(values = pick_pal(ny)) + labs(x = x, y = "Jumlah observasi", fill = y, title = paste(x, "dan", y)) + theme_upin()
}

# ---------------------------------------------------------------------
# 3. USER INTERFACE (UI) DENGAN TEMA UPIN & IPIN
# ---------------------------------------------------------------------
css <- "
@import url('https://fonts.googleapis.com/css2?family=Comic+Neue:wght@700&family=Nunito:wght@400;700;900&display=swap');
body, h1, h2, h3, h4, p, label { font-family: 'Nunito', sans-serif !important; }
.content-wrapper { background-color: #F8F9FA !important; }
.skin-blue .main-header .logo, .skin-blue .main-header .navbar { background-color: #2A9D8F !important; }
.skin-blue .main-sidebar { background-color: #264653 !important; }
.sidebar-menu>li.active>a, .sidebar-menu>li:hover>a { background-color: #E76F51 !important; color: #fff !important; }
.hero { background: linear-gradient(135deg, #2A9D8F, #264653); border-radius: 20px; padding: 22px; color: white; margin-bottom: 20px; box-shadow: 0 4px 10px rgba(0,0,0,0.1); }
.hero h1 { font-weight: 900; }
.box { border-radius: 15px !important; border-top: 4px solid #E76F51 !important; box-shadow: 0 4px 8px rgba(0,0,0,0.05) !important; }
.box-header { font-weight: 900; }
.upin-card { background: #FFF3B0; border: 2px dashed #E76F51; border-radius: 15px; padding: 15px; text-align: center; margin-bottom: 15px; font-weight: 700; color: #2B2D42; }
"

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "🏡 Rumah Ceria Upin Ipin", titleWidth = 280),
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem("🏠 Beranda", tabName = "beranda"),
      menuItem("🔎 Analisis Univariat", tabName = "uni"),
      menuItem("💘 Analisis Bivariat", tabName = "bi"),
      menuItem("🔥 Peta & Matrix Korelasi", tabName = "cor"),
      menuItem("📊 Regresi & Uji Asumsi Klasik", tabName = "reg"),
      menuItem("👩‍💻 Tentang Nesti", tabName = "dev")
    ),
    div(style = "padding: 15px;",
        div(class = "upin-card", "Betul, betul, betul! 🐔"),
        selectInput("uni_var", "Pilih Variabel Univariat", choices = pilih_uni, selected = "price_miliar"),
        uiOutput("uni_type_ui"),
        hr(),
        selectInput("x_var", "Variabel X (Bivariat)", choices = pilih_biv, selected = "building_area"),
        selectInput("y_var", "Variabel Y (Bivariat)", choices = pilih_biv, selected = "price_miliar"),
        uiOutput("bi_type_ui")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(css))),
    tabItems(
      tabItem("beranda",
              div(class = "hero", h1("Selamat Datang di Portal Rumah Jakarta! 🏘️"), p("Mari kumpulkan data rumah seperti Upin dan Ipin mengumpulkan ayam goreng! - Nesti Pote")),
              fluidRow(
                valueBoxOutput("n_box", width = 3), valueBoxOutput("sales_box", width = 3),
                valueBoxOutput("qty_box", width = 3), valueBoxOutput("rating_box", width = 3)
              ),
              fluidRow(
                box(title = "📊 Sebaran Harga Rumah (Miliar Rp)", status = "primary", solidHeader = TRUE, width = 6, plotOutput("overview_total", height = 300)),
                box(title = "🏙️ Listing per Kota", status = "primary", solidHeader = TRUE, width = 6, plotOutput("overview_product", height = 300))
              ),
              box(title = "👀 Intip Data Rumah Bersih", status = "info", solidHeader = TRUE, width = 12, tableOutput("preview"))
      ),
      tabItem("uni",
              div(class = "hero", h1("Tengok Satu-Satu Dulu 🔍"), p("Pemeriksaan karakteristik tiap variabel properti dengan teliti.")),
              fluidRow(
                box(title = "📊 Grafik Sebaran", status = "primary", solidHeader = TRUE, width = 8, plotOutput("uni_plot", height = 410)),
                box(title = "🧾 Statistik Ringkas", status = "info", solidHeader = TRUE, width = 4, tableOutput("uni_stats"))
              ),
              box(title = "🔢 Tabel Frekuensi & Ukuran Pemusatan", status = "info", solidHeader = TRUE, width = 12, tableOutput("freq_table"))
      ),
      tabItem("bi",
              div(class = "hero", h1("Mencari Kaitan Dua Hal 🤝"), p("Seperti Upin dan Ipin yang selalu bersama, mari lihat hubungan antar variabel!")) ,
              fluidRow(
                box(title = "🧩 Grafik Hubungan", status = "primary", solidHeader = TRUE, width = 8, plotOutput("bi_plot", height = 430)),
                box(title = "💬 Hasil Uji Statistik", status = "info", solidHeader = TRUE, width = 4, verbatimTextOutput("bi_summary"))
              )
      ),
      tabItem("cor",
              div(class = "hero", h1("Matrix Scatter Plot & Korelasi 🔥"), p("Melihat hubungan multivariat sekaligus dalam satu layar kencang.")),
              fluidRow(
                box(title = "🗺️ Matrix Scatter Plot (GGally)", status = "primary", solidHeader = TRUE, width = 12, plotOutput("matrix_scatter", height = 600))
              ),
              fluidRow(
                box(title = "🗺️ Heatmap Korelasi Pearson", status = "primary", solidHeader = TRUE, width = 6, plotOutput("cor_plot", height = 400)),
                box(title = "📋 Tabel Matriks Korelasi", status = "info", solidHeader = TRUE, width = 6, tableOutput("cor_table"))
              )
      ),
      tabItem("reg",
              div(class = "hero", h1("Model Regresi & Uji Normalitas Residual 📊"), p("Memastikan model OLS kita valid dan sisaannya berdistribusi normal.")),
              fluidRow(
                box(title = "📋 Ringkasan Model OLS", status = "primary", solidHeader = TRUE, width = 6, verbatimTextOutput("reg_summary")),
                box(title = "🛡️ Uji Normalitas & Asumsi Klasik Lengkap", status = "warning", solidHeader = TRUE, width = 6, verbatimTextOutput("asumsi_summary"))
              ),
              fluidRow(
                box(title = "📈 Histogram & Q-Q Plot Residual", status = "success", solidHeader = TRUE, width = 6, plotOutput("res_norm_plots", height = 350)),
                box(title = "🖼️ Plot Diagnostik Sisaan 4-in-1", status = "success", solidHeader = TRUE, width = 6, plotOutput("reg_diag_plot", height = 350))
              )
      ),
      tabItem("dev",
              div(class = "hero", h1("Tentang Pembuat Tugas 👩‍🎓"), p("Kawan-kawan, kenalkan perancang aplikasi ini!")),
              box(title = "Identitas Mahasiswa", status = "primary", solidHeader = TRUE, width = 12,
                  h3("ROSWITA ASMELITA NESTI POTE"),
                  p(strong("NIM: "), "241064007"),
                  p(strong("Mata Kuliah: "), "Eksplorasi & Visualisasi Data Multivariat (EVDM)"),
                  p(strong("Dosen Pengampu: "), "Inggit Fatika, S.Stat., M.Stat"),
                  p(strong("Program Studi: "), "Statistika - Universitas AKPRIND Indonesia"),
                  p("Pesan Khas: 'Susah senang kita tanggung bersama, yang penting tugas EVDM tuntas!' 🍗✨")
              )
      )
    )
  )
)

# ---------------------------------------------------------------------
# 4. SERVER LOGIC
# ---------------------------------------------------------------------
server <- function(input, output, session) {
  data_use <- reactive({ data_hasil })
  
  output$n_box <- renderValueBox({ valueBox(format(nrow(data_use()), big.mark=","), "Jumlah Listing", icon=icon("home"), color="blue") })
  output$sales_box <- renderValueBox({ valueBox(paste0("Rp ", round(mean(data_use()$price_miliar, na.rm=T), 2), " M"), "Rata-rata Harga", icon=icon("tag"), color="green") })
  output$qty_box <- renderValueBox({ valueBox(round(mean(data_use()$building_area, na.rm=T), 1), "Rata-rata Luas Bgn (m²)", icon=icon("building"), color="yellow") })
  output$rating_box <- renderValueBox({ valueBox(length(unique(data_use()$city)), "Kota Wilayah", icon=icon("map-marker-alt"), color="red") })
  
  output$overview_total <- renderPlot({
    ggplot(data_use(), aes(x = price_miliar)) + geom_histogram(bins = 25, fill = p_orange, color = "white") +
      labs(x = "Harga (Miliar Rp)", y = "Frekuensi") + theme_upin()
  })
  output$overview_product <- renderPlot({
    ggplot(data_use(), aes(x = city, fill = city)) + geom_bar(show.legend = FALSE) +
      scale_fill_manual(values = pick_pal(5)) + labs(x = NULL, y = "Jumlah Listing") + theme_upin()
  })
  output$preview <- renderTable({ head(data_use()[, c("city", "district", "price_miliar", "building_area", "land_area", "bed_rooms", "bath_rooms")], 8) }, striped=T, bordered=T)
  
  output$uni_type_ui <- renderUI({ selectInput("uni_type", "Jenis Grafik Univariat", choices = uni_choices(input$uni_var)) })
  output$bi_type_ui  <- renderUI({ selectInput("bi_type", "Jenis Grafik Bivariat", choices = bi_choices(input$x_var, input$y_var)) })
  
  output$uni_plot  <- renderPlot({ plot_uni(data_use(), input$uni_var, input$uni_type) })
  output$uni_stats <- renderTable({
    v <- input$uni_var; d <- data_use()
    if (is_cat(v)) {
      tt <- sort(table(d[[v]]), decreasing=TRUE)
      data.frame(Statistik=c("Jumlah Observasi","Kategori Terbanyak"), Nilai=c(as.character(nrow(d)), names(tt)[1]))
    } else {
      x <- d[[v]]
      data.frame(Statistik=c("N","Mean","Median","Std Dev","Min","Max","Skewness"),
                 Nilai=c(sum(!is.na(x)), round(mean(x,na.rm=T),2), round(median(x,na.rm=T),2), round(sd(x,na.rm=T),2), round(min(x,na.rm=T),2), round(max(x,na.rm=T),2), round(skew_fun(x),2)))
    }
  }, striped=T, bordered=T)
  
  output$freq_table <- renderTable({
    v <- input$uni_var; d <- data_use()
    if (is_cat(v)) {
      tab <- as.data.frame(table(d[[v]]))
      names(tab) <- c("Kategori","Frekuensi")
      tab$Persentase <- round(100*tab$Frekuensi/sum(tab$Frekuensi),2); tab
    } else {
      x <- d[[v]]; q <- quantile(x, c(.25,.75), na.rm=T); iqr <- q[2]-q[1]
      data.frame(Ukuran=c("Minimum","Q1","Median","Q3","Maksimum","IQR"), Nilai=c(min(x,na.rm=T), q[1], median(x,na.rm=T), q[2], max(x,na.rm=T), iqr))
    }
  }, striped=T, bordered=T)
  
  output$bi_plot <- renderPlot({ plot_bi(data_use(), input$x_var, input$y_var, input$bi_type) })
  output$bi_summary <- renderPrint({
    d <- data_use(); x <- input$x_var; y <- input$y_var
    if (x == y) return("Pilih dua variabel berbeda, kawan!")
    xc <- is_cat(x); yc <- is_cat(y)
    if (!xc && !yc) print(cor.test(d[[x]], d[[y]]))
    else if (xc != yc) {
      cv <- if (xc) x else y; nv <- if (xc) y else x
      print(summary(aov(d[[nv]] ~ d[[cv]])))
    } else print(chisq.test(table(d[[x]], d[[y]])))
  })
  
  # MATRIX SCATTER PLOT (GGALLY)
  output$matrix_scatter <- renderPlot({
    data_prop <- data_use()[, c("price_miliar", "building_area", "land_area", "bed_rooms", "bath_rooms")]
    ggpairs(data_prop,
            title = "Matrix Scatter Plot: Data Properti Rumah Jakarta",
            aes(color = "skyblue", alpha = 0.5)) + theme_upin()
  })
  
  output$cor_plot <- renderPlot({
    m <- cor(data_use()[, num_vars], use="pairwise.complete.obs")
    ggcorrplot(m, lab = TRUE, colors = c(p_red, "white", p_blue), title = "Korelasi Pearson") + theme_upin()
  })
  output$cor_table <- renderTable({
    m <- round(cor(data_use()[, num_vars], use="pairwise.complete.obs"), 3)
    cbind(Variabel=rownames(m), as.data.frame(m))
  }, striped=T, bordered=T)
  
  # MODEL REGRESI & UJI NORMALITAS RESIDUAL LENGKAP
  fit_model <- reactive({
    lm(price_miliar ~ building_area + land_area + bed_rooms + bath_rooms, data = data_use())
  })
  
  output$reg_summary <- renderPrint({ summary(fit_model()) })
  
  output$asumsi_summary <- renderPrint({
    m <- fit_model(); res <- residuals(m)
    
    cat("=== 1. UJI NORMALITAS KOLMOGOROV-SMIRNOV ===\n")
    print(ks.test(res, "pnorm", mean(res), sd(res)))
    
    cat("\n=== 2. UJI NORMALITAS SHAPIRO-WILK ===\n")
    set.seed(123)
    res_sample <- if(length(res) > 5000) sample(res, 5000) else res
    print(shapiro.test(res_sample))
    
    cat("\n=== 3. UJI AUTOKORELASI (DURBIN-WATSON) ===\n")
    print(dwtest(m))
    
    cat("\n=== 4. UJI MULTIKOLINEARITAS (VIF) ===\n")
    print(vif(m))
  })
  
  output$res_norm_plots <- renderPlot({
    res <- residuals(fit_model())
    par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
    hist(res, breaks = 15, main = "Histogram Residuals", xlab = "Residuals", col = p_yellow, border = "white")
    qqnorm(res, main = "Q-Q Plot Residuals")
    qqline(res, col = p_red, lwd = 2)
    par(mfrow = c(1, 1))
  })
  
  output$reg_diag_plot <- renderPlot({
    par(mfrow = c(2, 2))
    plot(fit_model())
    par(mfrow = c(1, 1))  
  })
  
} 
