library(shiny)
library(neuralnet)
library(dplyr)
library(caret)
library(readxl)

# Fungsi normalisasi
scl <- function(x, min_val, max_val) {
  (x - min_val) / (max_val - min_val)
}

# Shiny UI
ui <- fluidPage(
  titlePanel("Prediksi Anemia Menggunakan Neural Network"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Dataset Excel:", accept = c(".xls", ".xlsx")),
      hr(),
      h4("Input untuk Neural Network:"),
      textInput("hiddenNeurons", "Jumlah Neuron di Hidden Layers:", value = "0"),
      actionButton("trainBtn", "Latih Model"),
      hr(),
      h4("Input untuk Prediksi:"),
      numericInput("Sex", "Jenis Kelamin (1: Laki-laki, 2: Perempuan):", value = 1, min = 1, max = 2),
      numericInput("Red", "Persen Red Pixel:", value = 0),
      numericInput("Green", "Persen Green Pixel:", value = 0),
      numericInput("Blue", "Persen Blue Pixel:", value = 0),
      numericInput("Hb", "Kadar Hb:", value = 1),
      actionButton("predictBtn", "Prediksi")
    ),
    mainPanel(
      verbatimTextOutput("result"),
      verbatimTextOutput("modelInfo"),
      verbatimTextOutput("steps"),
      verbatimTextOutput("confMat"),
      plotOutput("nnPlot")
    )
  )
)

# Shiny Server
server <- function(input, output) {
  dataset <- reactiveVal(NULL)
  NN_model <- reactiveVal(NULL)
  confMat <- reactiveVal(NULL)
  
  # Observasi untuk upload file
  observeEvent(input$file, {
    req(input$file)
    
    # Membaca file Excel
    ext <- tools::file_ext(input$file$name)
    if (ext == "xls" || ext == "xlsx") {
      dataset(read_excel(input$file$datapath))
      output$modelInfo <- renderText({
        paste("Dataset berhasil diimpor dengan", nrow(dataset()), "baris dan", ncol(dataset()), "kolom.")
      })
      output$steps <- renderText({
        "Langkah 1: Dataset berhasil diimpor. Sekarang, pilih jumlah neuron di hidden layer dan latih model."
      })
    } else {
      output$modelInfo <- renderText("File yang diunggah bukan format Excel.")
      output$steps <- renderText("Langkah 1: Pastikan file yang diunggah adalah file Excel (.xls atau .xlsx).")
    }
  })
  
  # Latih model
  observeEvent(input$trainBtn, {
    req(dataset())
    
    # Ubah variabel target menjadi numerik
    data <- dataset()
    data$Anaemic <- as.numeric(factor(data$Anaemic)) - 1
    
    # Normalisasi data
    min_vals <- sapply(data[, -ncol(data)], min)
    max_vals <- sapply(data[, -ncol(data)], max)
    
    normalized_data <- data.frame(lapply(1:(ncol(data)-1), function(i) {
      scl(data[, i], min_vals[i], max_vals[i])
    }))
    colnames(normalized_data) <- colnames(data)[-ncol(data)]
    
    # Gabungkan dengan variabel target
    output_normalized <- cbind(normalized_data, Anaemic = data$Anaemic)
    
    # Split data
    set.seed(123)
    trainIndex <- createDataPartition(output_normalized$Anaemic, p = 0.8, list = FALSE)
    train_data <- output_normalized[trainIndex, ]
    test_data <- output_normalized[-trainIndex, ]
    
    # Ambil input jumlah neuron di hidden layer
    hidden_neurons_input <- input$hiddenNeurons
    hidden_neurons <- as.integer(strsplit(hidden_neurons_input, ",")[[1]])  # Memisahkan dan mengonversi menjadi integer
    
    # Latih model neural network dengan jumlah neuron di hidden layer sesuai input
    model <- neuralnet(
      Anaemic ~ .,
      data = train_data,
      hidden = hidden_neurons,  # Menggunakan jumlah neuron yang dimasukkan oleh pengguna
      act.fct = "logistic",
      linear.output = FALSE
    )
    NN_model(model)
    
    # Prediksi pada data uji
    pred <- predict(model, test_data[, -ncol(test_data)])
    pred_class <- ifelse(pred > 0.5, 1, 0)  # Klasifikasikan sebagai 1 atau 0
    
    # Confusion matrix
    conf <- confusionMatrix(factor(pred_class), factor(test_data$Anaemic))
    confMat(conf)
    
    output$modelInfo <- renderText({
      paste("Model dilatih dengan error terakhir:", model$result.matrix["error", 1])
    })
    
    output$steps <- renderText({
      paste("Langkah 2: Model dilatih. Sekarang Anda bisa memasukkan input untuk prediksi.")
    })
    
    # Render confusion matrix
    output$confMat <- renderPrint({
      confMat()  # Menampilkan confusion matrix
    })
    
    # Render plot jaringan syaraf tiruan setelah model dilatih
    output$nnPlot <- renderPlot({
      plot(model)
    })
  })
  
  # Prediksi
  observeEvent(input$predictBtn, {
    req(NN_model())
    
    # Data input
    input_data <- data.frame(
      Sex = as.numeric(input$Sex),
      Persen_Red_pixel = as.numeric(input$Red),
      Persen_Green_pixel = as.numeric(input$Green),
      Persen_Blue_pixel = as.numeric(input$Blue),
      Hb = as.numeric(input$Hb)
    )
    
    # Pastikan input tidak ada NA
    if(any(is.na(input_data))){
      output$result <- renderText("Input tidak valid, terdapat nilai NA!")
      output$steps <- renderText("Langkah 3: Cek input Anda, pastikan tidak ada nilai NA.")
      return()
    }
    
    # Normalisasi input dengan menggunakan min dan max dari data latih
    min_vals <- sapply(dataset()[, -ncol(dataset())], min)
    max_vals <- sapply(dataset()[, -ncol(dataset())], max)
    normalized_input <- data.frame(lapply(1:(ncol(input_data)), function(i) {
      scl(input_data[, i], min_vals[i], max_vals[i])
    }))
    
    # Periksa apakah model sudah dilatih
    model <- NN_model()
    if (is.null(model)) {
      output$result <- renderText("Model belum dilatih.")
      output$steps <- renderText("Langkah 3: Pastikan model telah dilatih terlebih dahulu.")
      return()
    }
    
    # Prediksi menggunakan fungsi predict() dari neuralnet
    prediction <- predict(model, normalized_input)
    
    # Periksa output prediksi
    if (any(is.na(prediction))) {
      output$result <- renderText("Prediksi gagal, nilai NA pada output!")
      output$steps <- renderText("Langkah 4: Terjadi kesalahan saat melakukan prediksi.")
    } else {
      predicted_class <- ifelse(prediction > 0.5, "Anaemia Detected", "No Anaemia")
      output$result <- renderText({
        paste("Hasil Prediksi:", predicted_class)
      })
      output$steps <- renderText("Langkah 4: Prediksi berhasil. Lihat hasil prediksi di atas.")
    }
  })
}

# Jalankan aplikasi
shinyApp(ui = ui, server = server)
