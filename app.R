
library(plumber)
library(neuralnet)
library(dplyr)
library(caret)
library(readxl)

# Fungsi normalisasi
scl <- function(x, min_val, max_val) {
  (x - min_val) / (max_val - min_val)
}

# Global variabel untuk menyimpan model dan data
dataset <- NULL
NN_model <- NULL
min_vals <- NULL
max_vals <- NULL

#* Upload dataset dan proses
#* @post /upload-dataset
function(req) {
  file <- req$args$file
  if (is.null(file)) {
    return(list(success = FALSE, message = "File tidak ditemukan."))
  }
  
  ext <- tools::file_ext(file$filename)
  if (ext == "xls" || ext == "xlsx") {
    dataset <<- read_excel(file$datapath)
    return(list(success = TRUE, message = "Dataset berhasil diunggah."))
  } else {
    return(list(success = FALSE, message = "Format file tidak didukung."))
  }
}

#* Latih model
#* @post /train-model
function(hidden_neurons = "5,3") {
  if (is.null(dataset)) {
    return(list(success = FALSE, message = "Dataset belum diunggah."))
  }
  
  # Proses dataset
  data <- dataset
  data$Anaemic <- as.numeric(factor(data$Anaemic)) - 1
  
  # Normalisasi data
  min_vals <<- sapply(data[, -ncol(data)], min)
  max_vals <<- sapply(data[, -ncol(data)], max)
  normalized_data <- data.frame(lapply(1:(ncol(data) - 1), function(i) {
    scl(data[, i], min_vals[i], max_vals[i])
  }))
  colnames(normalized_data) <- colnames(data)[-ncol(data)]
  output_normalized <- cbind(normalized_data, Anaemic = data$Anaemic)
  
  # Split data
  set.seed(123)
  trainIndex <- createDataPartition(output_normalized$Anaemic, p = 0.8, list = FALSE)
  train_data <- output_normalized[trainIndex, ]
  test_data <- output_normalized[-trainIndex, ]
  
  # Latih model
  hidden_neurons <- as.integer(strsplit(hidden_neurons, ",")[[1]])
  NN_model <<- neuralnet(
    Anaemic ~ .,
    data = train_data,
    hidden = hidden_neurons,
    act.fct = "logistic",
    linear.output = FALSE
  )
  return(list(success = TRUE, message = "Model berhasil dilatih."))
}

#* Prediksi
#* @post /predict
function(Sex, Red, Green, Blue, Hb) {
  if (is.null(NN_model)) {
    return(list(success = FALSE, message = "Model belum dilatih."))
  }
  
  # Data input
  input_data <- data.frame(
    Sex = as.numeric(Sex),
    Persen_Red_pixel = as.numeric(Red),
    Persen_Green_pixel = as.numeric(Green),
    Persen_Blue_pixel = as.numeric(Blue),
    Hb = as.numeric(Hb)
  )
  
  # Normalisasi
  normalized_input <- data.frame(lapply(1:ncol(input_data), function(i) {
    scl(input_data[, i], min_vals[i], max_vals[i])
  }))
  
  prediction <- predict(NN_model, normalized_input)
  predicted_class <- ifelse(prediction > 0.5, "Anaemia Detected", "No Anaemia")
  return(list(success = TRUE, prediction = predicted_class))
}

#* Status model
#* @get /status
function() {
  if (is.null(NN_model)) {
    return(list(success = FALSE, message = "Model belum dilatih."))
  } else {
    return(list(success = TRUE, message = "Model siap digunakan."))
  }
}

# Jalankan plumber
pr <- plumber::plumb("app.R")
pr$run(host = "0.0.0.0", port = 8000)
