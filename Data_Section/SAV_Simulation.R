
# Liste der benötigten Pakete
required_packages <- c("quantregForest", "grf", "GAS", "DEoptim", "ggplot2", "quantreg",
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



# simulate SAV-model
library(grf)
library(dplyr)
source("Simulations/Simulation_Functions.R")

##############################################################################################
## DQ for many iterations
##############################################################################################
iterations<-100
lengthOfObs <- 2000 #2000
tau    <- 0.05
minobs <- 20 #Default: 5
history_length <- 1000# c(500,1000)
sa_lengths <- c(3,7,30,60)
set.seed(124)
results   <- vector("list",iterations)
otherData <- vector("list",iterations)
Backtest  <- vector("list",iterations)

covs<-c("sd3","sd7","sd30","sd60","Lagged_Return")
cov_list<-list(cov3=covs[c(1,5)],cov7=covs[c(2,5)],cov30=covs[c(3,5)],cov3_7=covs[c(1,2,5)],
               cov3_30=covs[c(1,3,5)],cov7_30=covs[c(2,3,5)],cov3_7_30=covs[c(1,2,3,5)],covs_all=covs)

#save.image(file = "Sim_3_20240607.RData")


for(m in 1:iterations){ # or over iterations, i.e. monte carlo samples
    
    set.seed(817263+m)
    
    data_frame_raw<-gen_models(dgp="SAV",lengthOfObs = lengthOfObs,btc_data=NULL,tau=tau)
    data_frame<-data_frame_raw$model_data
    data_ret_raw<-data_frame_raw$raw_ret
    
    X  <- data_frame[,-which(colnames(data_frame) %in% c("Date","Return"))]
    Y  <- data_frame$Return
    colnames(X)<-covs
    # Data is now generated and ready to be processed
    
    DQ_list<-vector("list",8)
    names(DQ_list)<-names(cov_list)
    
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
      
      #GRF
      q.forest_3 <- quantile_forest(X_ac[,cov_list$cov3], Y_ac, quantiles = tau, min.node.size = minobs, alpha = 0.1)
      grf_quantiles_3  <- predict(q.forest_3, X_pred[,cov_list$cov3],  quantiles= tau)
      
      q.forest_7 <- quantile_forest(X_ac[,cov_list$cov7], Y_ac, quantiles = tau, min.node.size = minobs, alpha = 0.1)
      grf_quantiles_7  <- predict(q.forest_7, X_pred[,cov_list$cov7],  quantiles= tau)
      
      q.forest_30 <- quantile_forest(X_ac[,cov_list$cov30], Y_ac, quantiles = tau, min.node.size = minobs, alpha = 0.1)
      grf_quantiles_30  <- predict(q.forest_30, X_pred[,cov_list$cov30],  quantiles= tau)
      
      q.forest_37 <- quantile_forest(X_ac[,cov_list$cov3_7], Y_ac, quantiles = tau, min.node.size = minobs, alpha = 0.1)
      grf_quantiles_37  <- predict(q.forest_37, X_pred[,cov_list$cov3_7],  quantiles= tau)
      
      q.forest_330 <- quantile_forest(X_ac[,cov_list$cov3_30], Y_ac, quantiles = tau, min.node.size = minobs, alpha = 0.1)
      grf_quantiles_330  <- predict(q.forest_330, X_pred[,cov_list$cov3_30],  quantiles= tau)
      
      q.forest_730 <- quantile_forest(X_ac[,cov_list$cov7_30], Y_ac, quantiles = tau, min.node.size = minobs, alpha = 0.1)
      grf_quantiles_730  <- predict(q.forest_730, X_pred[,cov_list$cov7_30],  quantiles= tau)
      
      q.forest_3730 <- quantile_forest(X_ac[,cov_list$cov3_7_30], Y_ac, quantiles = tau, min.node.size = minobs, alpha = 0.1)
      grf_quantiles_3730  <- predict(q.forest_3730, X_pred[,cov_list$cov3_7_30],  quantiles= tau)
      
      q.forest_all <- quantile_forest(X_ac, Y_ac, quantiles = tau, min.node.size = minobs, alpha = 0.1)
      grf_quantiles_all  <- predict(q.forest_all, X_pred,  quantiles= tau)
      

     
      
      df_one_mc_run<-data.frame(GRF_3          = grf_quantiles_3,
                                GRF_7          = grf_quantiles_7,
                                GRF_30          = grf_quantiles_30,
                                GRF_37          = grf_quantiles_37,
                                GRF_330         = grf_quantiles_330,
                                GRF_730         = grf_quantiles_730,
                                GRF_3730        = grf_quantiles_3730,
                                GRF_all        = grf_quantiles_all)
                                
      return(df_one_mc_run)
      
    }
    #stop cluster
    stopCluster(cl)
    close(pb)
    print(paste0("Iteration ", m, " von ", iterations, " abgeschlossen", Sys.time()))
    
    
    sum_na<-apply(finalMatrix,2,function(x) sum(is.na(x)))
    which_na<-apply(finalMatrix,2,function(x) (is.na(x)))
    
    finalMatrix_clean<-finalMatrix
    finalMatrix_clean[which_na]<-0
    results[[m]]   <- finalMatrix_clean
    otherData[[m]] <- data_ret_raw[(history_length+tail(sa_lengths,n=1)+1):lengthOfObs]
    
    
    backtest_df<-backtest_fun(otherData[[m]], results[[m]], tau)
    names(backtest_df)<-names(cov_list)
    Backtest[[m]] <- backtest_df
    fileName <- paste0("Cleaned_Data/Simulations/DQ_cov_combis_1000hl")
    save.image(paste0(fileName,".RData"))
}



DQ<-vector("list",iterations) # for each iteration, save p-value of DQ-test
for(i in 1:iterations){
  DQ[[i]]<-sapply(Backtest[[i]], function(x){return(x$DQ$pvalue)})
}

# convert to data.frame
#DQ_df<-as.data.frame(do.call(rbind, DQ[c(1:11,13:48)]))
DQ_100it<-DQ[c(1:11,13:100)]
DQ_df<-bind_rows(DQ_100it)
DQ_means<-colMeans(DQ_df)
round(DQ_means, digits = 3)
