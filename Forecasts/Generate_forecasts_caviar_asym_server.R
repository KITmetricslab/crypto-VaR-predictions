#Optimization only CAViaR


#Generate forecasts for over 100 different cryptos using parallel processing
############################################################################

#load important packages

library(data.table)
library(foreach)
library(doParallel)
library(grf)
library(quantreg)
library(quantregForest)
library(GAS)
library(rugarch)


#######################
#load and prep data

load("Crypto_Data_Raw.RData")
source("Forecast_Functions_cav.R")


######################

#prep data for run

tau            <- 0.05 #which level do you want (either 0.05 or 0.01)

#length for the rolling window for refitting (either 500 or 1000), out-of-sample
#smaller than estimating window, we keep the window at 500 until we reach 100 observations,
#from where we split the sample equally
window_length  <- 500

sa_lengths     <- c(3,7,30,60) #lengths of return standard deviations to include


###############################
#begin each run


#Now run it for different FDRs and look at resulting plots
unregister()
cores_size<-min(detectCores()-1,length(crypto_data_cleaned))
#cores_size=10

#register parallel backend
# cl <- makeMPIcluster(cores_size) # number of cores
# registerDoSNOW(cl)


#progress <- function(n) setTxtProgressBar(pb, n)
#opts <- list(progress = progress)

list_final_cav<-vector("list",length=length(crypto_data_cleaned))
names(list_final_cav)<-names(crypto_data_cleaned)

#loop over all assets
for(j in 1:length(list_final_cav))
{
  #adds log return and sds and deletes price
  print(paste0("Start with Crypto ",names(list_final_cav)[j]))
  df_crypto<-fun_get_ret(crypto_data_cleaned[[j]],log_ret = TRUE,add_sd=sa_lengths)
  
  dates<-df_crypto$time
  
  #set window length depending on crypto size
  effective_est_window<-window_length
  oos_period<-nrow(df_crypto)-effective_est_window
  
  #for CPA tests, we need that oos_period>effective_est_window
  
  #shrink estimation period and keep oos at least at original window length
  if(nrow(df_crypto)<=2*window_length)
  {
    oos_period<-window_length
    effective_est_window<-nrow(df_crypto)-oos_period
  }
  #if size smaller than 100, i.e. for 1% VaR, less than one observation is expected, set to 100
  #worst value in sample gives 101 oos window
  if(effective_est_window<100)
  {
    effective_est_window<-100
    oos_period<-nrow(df_crypto)-effective_est_window
  }
  
  #Start parallel part
  cl<-makeForkCluster(cores_size)
  registerDoParallel(cl)
  list_temp_cav<-foreach(i=1:oos_period,.combine = list,
                     .multicombine = TRUE,.maxcombine = 5000,.errorhandling = "pass"#,.options.snow = opts
  ) %dopar% {
    print(paste0(i," out of ",oos_period))
    set.seed(20220427+i)
    df_fit   <- df_crypto[i:(effective_est_window+i-1),]
    df_pred  <- df_crypto[effective_est_window+i,]
    
    fc_temp<-compute_caviar_asym(df_fit,df_pred,tau=tau)
    return(fc_temp)
    
  }
  stopCluster(cl)
  list_temp_cav$date <- df_crypto$time
  list_temp_cav$oos_y <- df_crypto$Ret[(effective_est_window+1):(effective_est_window+oos_period)]
  list_final_cav[[j]]<-list_temp_cav
  #save each run in case of crash
  name_list_temp<-paste0("List_temp_cav_asym",tau,"_",names(list_final_cav)[j])
  save(list_temp_cav,file=paste0("../../Results/Temp_files/",name_list_temp,".RData"))
  print(paste0("Finished with Crypto ",names(list_final_cav)[j]," : Run ",j," of ",length(list_final_cav)))
  
}

names(list_final_cav)<-names(crypto_data_cleaned)
name_list<-paste0("List_final_cav_asym_",tau,"_",window_length,"_",Sys.Date())
file_path <- "../../Cleaned_Data/Forecasts/"
save(list_final_cav,file=paste0(file_path, name_list,".RData"))
