
# Liste der benötigten Pakete
required_packages <- c("quantregForest", "grf", "GAS", "DEoptim", "ggplot2", 
                       "reshape2", "PerformanceAnalytics", "rugarch", "foreach", "doParallel", "doSNOW","tvgarch")

# Funktion zum Installieren und Laden der Pakete
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Schleife zum Installieren und Laden der Pakete
for (package in required_packages) {
  install_and_load(package)
}


################################################
# Working Directory setzen und Daten laden
################################################
# Den Pfad der aktuellen Datei ermitteln und als Arbeitsverzeichnis festlegen
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

#source CAVIAR files and functions for model generation
source("Simulation_Functions.R")

#btc data
load(file="../../Cleaned_Data/Forecasts/Crypto_Data_add.RData")
btc_data<-list_rets$btc$Ret

####################################
######### Rolling Forecast #########
####################################
#for (z in 3:5) {
iterations  <- 100 #200
lengthOfObs <- 2000 #2000
tau    <- 0.05
minobs <- 20 #Default: 5
history_lengths <- 500# c(500,1000)
sa_lengths <- c(3,7,30,60)
set.seed(124)
return_models<-c("GARCH_norm","GARCH_btc_asym_t","SAV","GARCH_VaryVola") 
which_model<-return_models[4] 
results    <-  vector("list",length(history_lengths))
otherData  <-  vector("list",length(history_lengths))
Backtest   <-  vector("list",length(history_lengths))

for(iter in 1:length(history_lengths)){
  results[[iter]]   <- vector("list",iterations)
  otherData[[iter]] <- vector("list",iterations)
  Backtest[[iter]]  <- vector("list",iterations)
}
#save.image(file = "Sim_3_20240607.RData")

for(q in 1:length(history_lengths)){
  for(m in 1:iterations){ # or over iterations, i.e. monte carlo samples
    
    set.seed(817263+m)
    
    history_length <- history_lengths[q]
    
    data_frame_raw<-gen_models(dgp=which_model,lengthOfObs = lengthOfObs,btc_data=btc_data,tau=tau)
    data_frame<-data_frame_raw$model_data
    data_ret_raw<-data_frame_raw$raw_ret
    
    X  <- data_frame[,-which(colnames(data_frame) %in% c("Date","Return"))]
    Y  <- data_frame$Return
    
    # Data is now generated and ready to be processed
    
    
    #Data length to iterate over
    calc_length <- nrow(X)-history_length
    
    #setup parallel backend to use many processors
    cores <- detectCores()
    cl <- makeCluster(cores[1]-1) #not to overload your computer
    registerDoSNOW(cl)
    
    pb <- txtProgressBar(max=calc_length, style=3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress=progress)
    
    #.options.snow=opts
    finalMatrix <- foreach(i=1:calc_length, .combine=rbind, .options.snow=opts, .errorhandling ='pass', .packages=c('quantregForest','grf','DEoptim','PerformanceAnalytics','rugarch','quantreg'))  %dopar% {
      
      set.seed(6123873+i)
      
      X_ac   <- X[i:(history_length+i-1),]
      Y_ac   <- Y[i:(history_length+i-1)]
      X_pred <- X[history_length+i,]
      Y_pred <- Y[history_length+i]
      
      #quantregForest
      quantregF <- quantregForest(x=X_ac, y=Y_ac, nodesize = minobs)
      quantregForest_quantiles    <- predict(quantregF, X_pred, what=tau)
      
      #GRF
      q.forest <- quantile_forest(X_ac, Y_ac, quantiles = tau, min.node.size = minobs, alpha = 0.1)
      grf_quantiles  <- predict(q.forest, X_pred,  quantiles= tau)
      
      #Quantile Regression
      rqfit <- rq(Y_ac ~ X_ac$Standard_Dev_2 + X_ac$Standard_Dev_3 + X_ac$Standard_Dev_4 + X_ac$Lagged_Return, tau = tau)
      qr_quantiles <- as.matrix(cbind(rep(1,nrow(X_pred)),cbind(X_pred$Standard_Dev_2,X_pred$Standard_Dev_3,X_pred$Standard_Dev_4,X_pred$Lagged_Return))) %*% matrix(as.numeric(rqfit$coefficients), 5, 1)
      
      #Historical VaR
      #hist_quantiles <- quantile(Y_ac,tau) # identical
      hist_quantiles <- VaR(R = Y_ac,
                            p = 1- tau, 
                            method = "historical")
      
      #Variance/Covariance
      var_cov_quantiles <- VaR(R = Y_ac,
                               p = 1- tau, 
                               method = "gaussian")
      
      #Caviar
      
      var    <- as.numeric(quantile(Y_ac, probs = tau))
      CAViaR <- rep(-var,length(Y_ac))
      
      #SAV
      caviar_quantiles<-NA
      nparms<-3
      res    <- DEoptim(caviar_eval,
                        lower    = rep(-10,nparms),
                        upper    = rep(+10,nparms),
                        control  = list(trace=FALSE),
                        r_t    = Y_ac,#train
                        Var_init   = CAViaR,#CAViaR
                        alpha=tau)
      
      betas  <- res$optim$bestmem
      
      caviar_quantiles <- cav_pred(betas,Y_ac,tau)
      # 
      #Caviar asym:
      nparms<-4
      caviar_quantiles_asym<-NA
      res    <- DEoptim(caviar_eval_asym,
                        lower    = rep(-10,nparms),
                        upper    = rep(+10,nparms),
                        control  = list(trace=FALSE),
                        r_t    = Y_ac,#train
                        Var_init   = CAViaR,#CAViaR
                        alpha=tau)
      
      betas  <- res$optim$bestmem
      
      caviar_quantiles_asym <- cav_pred_asym(betas,Y_ac,tau)
      
      #GARCH
      #take a GARCH (1,1) model with norm distribution
      spec_garch<-ugarchspec(mean.model =list(armaOrder=c(1,1)),
                             variance.model = list(model="sGARCH",
                                                   garchOrder=c(1,1)),
                             distribution.model = "norm")
      
      tryCatch(
        {
          fit_garch<-ugarchfit(spec_garch,
                               Y_ac,
                               out.sample = 1,
                               solver = "hybrid")
          
          forecast_one_ahead<-ugarchforecast(fit_garch,
                                             n.ahead = 1)
          VaR_garch<-quantile(forecast_one_ahead,probs=tau)
          #uses fitted mu instead of predicted mu!
          # VaR_garch <- qdist("norm",
          #                    p=tau,
          #                    mu=fitted(forecast_one_ahead),#fit_garch@fit$coef[1],
          #                    sigma=forecast_one_ahead@forecast$sigmaFor)
        },error=function(cond) {
          VaR_garch <- NA
        })
      
      #take a GARCH (1,1) model with norm distribution
      gjr_garch_spec<-ugarchspec(variance.model = list(model="gjrGARCH"),distribution.model = "norm")
      
      tryCatch({
        garch_fit<-ugarchfit(gjr_garch_spec,data= Y_ac,out.sample = 1,
                             solver = "hybrid")
        fc_one_ahead<-ugarchforecast(garch_fit, n.ahead = 1)
        forecasts_var_gjr<-quantile(fc_one_ahead,probs=tau) #gives back quantile based on normal distribution
      },error=function(cond) {
        forecasts_var_gjr <- NA
      })
      
      df_one_mc_run<-data.frame(quantregF      = quantregForest_quantiles,
                                GRF            = grf_quantiles,
                                QR             = qr_quantiles,
                                Hist           = hist_quantiles,
                                VarCov         = var_cov_quantiles,
                                Caviar_sav     = caviar_quantiles,
                                Caviar_asy     = caviar_quantiles_asym,
                                Garch          = VaR_garch,
                                Garch_GJR      = forecasts_var_gjr)
      
      return(df_one_mc_run)
      
    }
    #stop cluster
    stopCluster(cl)
    close(pb)
    print(paste0("Iteration ", m, " von ", iterations, " abgeschlossen", Sys.time()))
    
    #i<- calc_length
    #X_ac   <- X[i:(history_length+i-1),]
    #Y_ac   <- Y[i:(history_length+i-1)]
    #X_pred <- X[history_length+i,]
    #Y_pred <- Y[history_length+i]
    
    #check for NA and NaN:
    #sum_nan<-apply(finalMatrix,2,function(x) sum(is.nan(x)))
    sum_na<-apply(finalMatrix,2,function(x) sum(is.na(x)))
    which_na<-apply(finalMatrix,2,function(x) (is.na(x)))
    
    finalMatrix_clean<-finalMatrix
    finalMatrix_clean[which_na]<-0
    results[[q]][[m]]   <- finalMatrix_clean
    otherData[[q]][[m]] <- data_ret_raw[(history_length+tail(sa_lengths,n=1)+1):lengthOfObs]
    
    
    #In-sample fit
    
    # #quantregForest
    # quantregF <- quantregForest(x=X_ac, y=Y_ac, nodesize = minobs)
    # quantregForest_quantiles_in    <- predict(quantregF, X_ac, what=tau)
    # 
    # #GRF
    # q.forest <- quantile_forest(X_ac, Y_ac, quantiles = tau, min.node.size = minobs)
    # grf_quantiles_in  <- predict(q.forest, X_ac,  quantiles= tau)
    # 
    # #Quantile Regression
    # rqfit <- rq(Y_ac ~ X_ac$Standard_Dev_2 + X_ac$Standard_Dev_3 + X_ac$Standard_Dev_4 + X_ac$Lagged_Return, tau = tau)
    # qr_quantiles_in <- as.matrix(cbind(rep(1,nrow(X_ac)),cbind(X_ac$Standard_Dev_2,X_ac$Standard_Dev_3,X_ac$Standard_Dev_4,X_ac$Lagged_Return))) %*% matrix(as.numeric(rqfit$coefficients), 5, 1)
    # 
    backtest_df<-backtest_fun(otherData[[q]][[m]], results[[q]][[m]], tau)
    names(backtest_df)<-c("QRF","GRF","QR","Hist","VarCov","CAV_SAV","CAV_ASY","GARCH","GJRGARCH")
    Backtest[[q]][[m]] <- backtest_df
    fileName <- paste0("../../Cleaned_Data/Simulations/history_length_500/Sim_",tau,"_500_",which_model)
    save.image(paste0(fileName,".RData"))
  }
}


fileName <- paste0("../../Cleaned_Data/Simulations/history_length_500/Sim_",tau,"_500_",which_model)
save.image(paste0(fileName,".RData"))
#}