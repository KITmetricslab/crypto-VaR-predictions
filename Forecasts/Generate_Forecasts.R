library(tidyr)
library(quantreg)
library(quantregForest)
library(doParallel)
library(grf)
library(rugarch)



load("../../Cleaned_Data/Forecasts/Crypto_Data_Raw.RData")
source("Forecast_Functions.R")

tau <- 0.05
window_length <- 500
sa_lengths <- c(3,7,30,60)

#Now run it for different FDRs and look at resulting plots
unregister()
cores_size<-min(detectCores()-1,length(crypto_data_cleaned))
#cores_size=10

#register parallel backend
cl<-makeForkCluster(cores_size)
registerDoParallel(cl)

#length(crypto_data_cleaned)
list_final<-foreach(j=1:length(crypto_data_cleaned),.combine = list,.multicombine = TRUE,.maxcombine = 1000,.errorhandling = "stop") %dopar%
  {

    #adds log return and sds and deletes price
    df_crypto<-fun_get_ret(crypto_data_cleaned[[j]],log_ret = TRUE,add_sd=sa_lengths)
    
    dates<-df_crypto$time
    
    # Setze window_length abhängig von der Größe der Kryptowährung
    effective_est_window <- window_length
    oos_period <- nrow(df_crypto) - effective_est_window
    
    # Verkleinere das Schätzzeitfenster und halte oos mindestens auf der ursprünglichen Fensterlänge
    if (nrow(df_crypto) <= 2 * window_length) {
      oos_period <- window_length
      effective_est_window <- nrow(df_crypto) - oos_period
    }
    # Wenn die Größe kleiner als 100 ist, d.h. für 1% VaR, wird erwartet, dass weniger als eine Beobachtung vorliegt, setzen Sie sie auf 100
    # Der schlechteste Wert im Sample gibt ein 101 oos Fenster
    if (effective_est_window < 100) {
      effective_est_window <- 100
      oos_period <- nrow(df_crypto) - effective_est_window
    }
    list_fc <- list()
    for (i in 1:oos_period) {
      print(paste0(i, " out of ", oos_period))
      set.seed(20240422 + i)
      df_fit <- df_crypto[i:(effective_est_window + i - 1), ]
      df_pred <- df_crypto[effective_est_window + i, ]
      
      fc_temp <- compute_model_preds(df_fit, df_pred, tau = tau)
      list_fc[[i]] <- fc_temp
    }
    final_list <- list(fc_ml = list_fc, date = dates)
    return(final_list)
  
  }

stopCluster(cl)
# Benenne die Einträge in list_final nach den Namen der Dataframes ohne NaN-Werte
names(list_final) <- names(crypto_data_cleaned)

name_list <- paste0("List_final_", tau, "_", Sys.Date())
file_path <- "../../Cleaned_Data/Forecasts/"
save(list_final, file = paste0(file_path, name_list, ".RData"))

###########################
#list_final umformen um Evaluation durchführen zu können
###########################
final_data <- vector("list", length(list_final))
exclude_names <- c("Var_imp_qr") #"Var_imp_grf", 
values <- names(list_final[[1]]$fc_ml[[1]])
values_forecasts <- setdiff(values, exclude_names)
values_var_imp <- exclude_names

for (j in seq_along(list_final)) {
  effective_est_window <- window_length
  oos_period <- length(list_final[[j]]$date) - effective_est_window
  
  # Verkleinere das Schätzzeitfenster und halte oos mindestens auf der ursprünglichen Fensterlänge
  if (length(list_final[[j]]$date) <= 2 * window_length) {
    oos_period <- window_length
    effective_est_window <- length(list_final[[j]]$date) - oos_period
  }
  # Wenn die Größe kleiner als 100 ist, d.h. für 1% VaR, wird erwartet, dass weniger als eine Beobachtung vorliegt, setzen Sie sie auf 100
  # Der schlechteste Wert im Sample gibt ein 101 oos Fenster
  if (effective_est_window < 100) {
    effective_est_window <- 100
    oos_period <- length(list_final[[j]]$date) - effective_est_window
  }
  dates_temp <- list_final[[j]]$date[(effective_est_window+1):length(list_final[[j]]$date)]
  fc_ml_temp <- list_final[[j]]$fc_ml
  # Forecast Values
  for (value in seq_along(values_forecasts)) {
    value_list_temp <- list()
    for (i in seq_along(fc_ml_temp)) {
      x_temp <- fc_ml_temp[[i]][[values_forecasts[value]]]
      value_list_temp <- append(value_list_temp, x_temp)
    }
    names(value_list_temp) <- dates_temp
    final_data[[j]] <- append(final_data[[j]], list(value_list_temp))
  }
  # Variable Importance Values
  for (value in seq_along(values_var_imp)) {
    value_list_temp <- list()
    for (i in seq_along(fc_ml_temp)) {
      x_temp <- as.data.frame(fc_ml_temp[[i]][[values_var_imp[value]]])
      value_list_temp <- c(value_list_temp, list(x_temp))
    }
    names(value_list_temp) <- dates_temp
    final_data[[j]] <- append(final_data[[j]], list(value_list_temp))
  }
  names(final_data[[j]]) <- values
  
}
names(final_data) <- names(list_final)

name_list <- paste0("final_data_", tau, "_", Sys.Date())
file_path <- "../../Cleaned_Data/Forecasts/"
save(final_data, file = paste0(file_path, name_list, ".RData"))










