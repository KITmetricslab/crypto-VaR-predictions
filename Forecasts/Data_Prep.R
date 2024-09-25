# Pfad zum Ordner mit den CSV-Dateien
folder_path <- "../../Cleaned_Data/Crypto_Data_Cleaned"
# Pfad zum Ordner für die cleaned Rohdaten
file_path <- "../../Cleaned_Data/Forecasts/"

source('Forecast_Functions.R')

sa_lengths <- c(3,7,30,60)

# Liste zum Speichern der DataFrames
crypto_data <- list()

# Alle CSV-Dateien im Ordner laden und in DataFrames konvertieren
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
for (file in file_list) {
  # Den Dateinamen als Namen für den DataFrame verwenden
  df_name <- gsub(".csv", "", basename(file))
  crypto_data[[df_name]] <- read.csv(file)
}

# Erstelle eine leere Liste für die gereinigten DataFrames
crypto_data_cleaned <- list()
cleaned_names <- vector()
# Iteriere über jedes DataFrame in crypto_data
for (j in seq_along(crypto_data)) {
  df_temp <- crypto_data[[j]]
  
  # Überprüfe, ob das DataFrame NaN-Werte enthält
  if (any(is.na(df_temp))) {
    # Wenn NaN-Werte vorhanden sind, überspringe dieses DataFrame
    next
  } else {
    # Füge das DataFrame zur Liste crypto_data_cleaned hinzu
    crypto_data_cleaned <- append(crypto_data_cleaned, list(df_temp))
    cleaned_names <- append(cleaned_names, names(crypto_data)[j])
  }
}
names(crypto_data_cleaned) <- cleaned_names


save(crypto_data_cleaned, file = paste0(file_path,"Crypto_Data_Raw.RData"))



list_rets <- list()
for (j in seq_along(crypto_data_cleaned)){
  df_temp <- crypto_data_cleaned[[j]]
  df_temp <- fun_get_ret(df_temp, log_ret = TRUE,add_sd=sa_lengths)
  list_rets[[j]] <- df_temp
}
names(list_rets) <- names(crypto_data_cleaned)

save(list_rets, file = paste0(file_path,"Crypto_Data_add.RData"))







