############
#get a new final data frame to include the code from the asymmetric CAVIAR Run:

load("Cleaned_Data/Forecasts/final_data_0.05_2024-05-13_all.RData") #final_data
load("Cleaned_Data/Forecasts/List_final_cav_asym_0.05_500_2024-11-19.RData") #list_final_cav


###########################
#list_final_cav umformen 
final_data_cav <- vector("list", length(list_final_cav))
window_length <- 500

for (j in seq_along(list_final_cav)) {
  effective_est_window <- window_length
  oos_period <- length(list_final_cav[[j]]$date) - effective_est_window
  
  # Verkleinere das Schätzzeitfenster und halte oos mindestens auf der ursprünglichen Fensterlänge
  if (length(list_final_cav[[j]]$date) <= 2 * window_length) {
    oos_period <- window_length
    effective_est_window <- length(list_final_cav[[j]]$date) - oos_period
  }
  # Wenn die Größe kleiner als 100 ist, d.h. für 1% VaR, wird erwartet, dass weniger als eine Beobachtung vorliegt, setzen Sie sie auf 100
  # Der schlechteste Wert im Sample gibt ein 101 oos Fenster
  if (effective_est_window < 100) {
    effective_est_window <- 100
    oos_period <- length(list_final_cav[[j]]$date) - effective_est_window
  }
  dates_temp <- list_final_cav[[j]]$date[(effective_est_window+1):length(list_final_cav[[j]]$date)]
  fc_ml_temp <- list_final_cav[[j]][1:oos_period]
  # Forecast Values
  value_list_temp <- list()
  for (i in seq_along(fc_ml_temp)) {
    x_temp <- fc_ml_temp[[i]]
    value_list_temp <- append(value_list_temp, x_temp)
  }
  names(value_list_temp) <- dates_temp
  final_data_cav[[j]] <- append(final_data_cav[[j]], list(value_list_temp))
  
  # # Variable Importance Values
  #   value_list_temp <- list()
  #   for (i in seq_along(fc_ml_temp)) {
  #     x_temp <- as.data.frame(fc_ml_temp[[i]])
  #     value_list_temp <- c(value_list_temp, list(x_temp))
  #   }
  #   names(value_list_temp) <- dates_temp
  #   final_data_cav[[j]] <- append(final_data_cav[[j]], list(value_list_temp))
  names(final_data_cav[[j]]) <- "CAV_ASY"
}
names(final_data_cav) <- names(list_final_cav)





#############################################################
#for each value of the list, add the forecast of cav_asy
final_data_combined<-final_data
for(i in 1:length(final_data_combined))
{
  list_temp_i<-final_data_combined[[i]]
  list_temp_final<-append(list_temp_i,final_data_cav[[i]],after=8)
  final_data_combined[[i]]<-list_temp_final
}

save(final_data_combined,file="Cleaned_Data/Forecasts/final_data_combined_0.05_500_2024-11-20.RData")
