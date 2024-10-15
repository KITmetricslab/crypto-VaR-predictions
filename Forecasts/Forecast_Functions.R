library(tidyr)
library(quantregForest)
library(dplyr)
library(rugarch)
library(DEoptim)
library(zoo)




#function to generate returns

fun_get_ret<-function(df_cryptos,log_ret=TRUE,add_sd=NULL,cov_names=NULL){
  if(log_ret){
    ret_daily<-diff(log(df_cryptos$Price),lag=1)
  } else {
    ret_daily<-(df_cryptos$Price[-1]/df_cryptos$Price[-length(df_cryptos$Price)])-1
  }
  if(is.null(cov_names))
  {
    cov_names <- colnames(df_cryptos)[-c(1,2)]
  }
  df_temp<-df_cryptos %>% #with your covariates from the first day, you want to forecast the return from the next day
    mutate(Ret=c(ret_daily,NA),Lagged_ret=c(NA,ret_daily[1:(length(ret_daily))])) %>% 
    select(time,Ret,Lagged_ret,cov_names) #%>% select(-Price)
  if(!is.null(add_sd))
  {
    df_sd<-rbind(rep(NA,length(add_sd)),sapply(add_sd,function(x) {rollapply(ret_daily,width=x,FUN=sd,fill=NA,align="right")})) #add na for first day to avoid issue of using information from same day
    colnames(df_sd)<-paste0("sd_",add_sd)
    df_final<-df_temp %>% cbind(df_sd) %>% drop_na()
  } else {
    df_final<-df_temp
  }
  return(df_final)
}








#function to unregister everything for parallel processing
unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}


#take as input coefficient vector dim(p+3), initial VaR (dim t) and data frame with covariates (dim txp)
#update VaR via functional form with theta
#compute loss
#check whether it is a valid loss or not
#return loss
caviar_eval<-function(theta,r_t,Var_init,alpha=tau)
{
  theta_1<-theta[1]
  theta_2<-theta[2]
  theta_3<-theta[3]
  #theta_rest<-theta[-c(1:3)]
  Var_pred<-Var_init
  for(i in 1:(length(r_t)-1))
  {
    Var_pred[i+1]<-theta_1+theta_2*Var_pred[i]+theta_3*abs(r_t[i]) #+ sum(theta_rest * df_cov[i,])
  }
  q_loss<-mean((alpha-(r_t<Var_pred))*(r_t-Var_pred))
  if(is.na(q_loss)||is.infinite(q_loss)){q_loss<-1e10}
  return(q_loss)
}
caviar_eval_asym<-function(theta,r_t,Var_init,alpha=tau)
{
  theta_1<-theta[1]
  theta_2<-theta[2]
  theta_3<-theta[3]
  theta_4<-theta[4]
  #theta_rest<-theta[-c(1:3)]
  Var_pred<-Var_init
  for(i in 1:(length(r_t)-1))
  {
    Var_pred[i+1]<-theta_1+theta_2*Var_pred[i]+theta_3*max(r_t[i],0) +theta_4*(-1*min(r_t[i],0))
  }
  q_loss<-mean((alpha-(r_t<Var_pred))*(r_t-Var_pred))
  if(is.na(q_loss)||is.infinite(q_loss)){q_loss<-1e10}
  return(q_loss)
}


cav_pred_asym<-function(thetas,r_t,alpha=tau)
{
  Var_fin<-Var_fin<-as.numeric(quantile(r_t,alpha))
  for(k in 1:length(r_t))
  {
    Var_fin<-thetas[1]+thetas[2]*Var_fin+thetas[3]*max(r_t[k],0) +thetas[4]*(-1*min(r_t[k],0))
  }
  caviar_quantiles <- Var_fin
  return(caviar_quantiles)
}

compute_caviar<-function(df_fit,df_pred,tau=0.05){
  #get data in format
  y_fit<-df_fit$Ret
  x_fit<-df_fit[,!colnames(df_fit) %in% c("Ret","time")]
  x_pred<-df_pred[,!colnames(df_pred) %in% c("Ret","time")]
  
  #CAViaR
  print("Caviar start")
  VaR_temp<-rep(quantile(y_fit,0.05),length(y_fit))
  min_obj<-DEoptim(fn=caviar_eval,
                   #lower=rep(-10,ncol(x_caviar)+3),
                   #upper=rep(10,ncol(x_caviar)+3),
                   lower=rep(-25,3),
                   upper=rep(25,3),
                   control=list(trace=FALSE),
                   r_t=y_fit,
                   Var_init=VaR_temp,
                   alpha=tau)
  
  #get best parameters
  thetas  <- min_obj$optim$bestmem
  
  #forecast VaR
  Var_fin<-VaR_temp[1]
  for(k in 2:length(y_fit))
  {
    Var_fin<-thetas[1]+thetas[2]*Var_fin+thetas[3]*abs(y_fit[k-1])# + sum(thetas[-c(1:3)] * x_caviar[i,])
  }
  caviar_quantiles <- Var_fin
  return(caviar_quantiles)
}

compute_caviar_asym<-function(df_fit,df_pred,tau=0.05){
  #get data in format
  y_fit<-df_fit$Ret
  x_fit<-df_fit[,!colnames(df_fit) %in% c("Ret","time")]
  x_pred<-df_pred[,!colnames(df_pred) %in% c("Ret","time")]
  
  #CAViaR
  print("Caviar start")
  VaR_temp<-rep(quantile(y_fit,0.05),length(y_fit))
  min_obj<-DEoptim(fn=caviar_eval_asym,
                   #lower=rep(-10,ncol(x_caviar)+3),
                   #upper=rep(10,ncol(x_caviar)+3),
                   lower=rep(-25,4),
                   upper=rep(25,4),
                   control=list(trace=FALSE),
                   r_t=y_fit,
                   Var_init=VaR_temp,
                   alpha=tau)
  
  #get best parameters
  thetas  <- min_obj$optim$bestmem
  
  #forecast VaR
  Var_fin<-VaR_temp[1]
  for(k in 2:length(y_fit))
  {
    Var_fin<-thetas[1]+thetas[2]*Var_fin+ thetas[3]*max(y_fit[k-1],0) +thetas[4]*(-1*min(y_fit[k-1],0))
  }
  caviar_quantiles <- Var_fin
  return(caviar_quantiles)
}


#df_fit, df_pred are data_frames with variable price and covariates
compute_model_preds<-function(df_fit,df_pred,tau=0.05,compute_cav_hist_gjr=TRUE){
  
  #get data in format
  y_fit<-df_fit$Ret
  x_fit<-df_fit[,!colnames(df_fit) %in% c("Ret","time")]
  x_pred<-df_pred[,!colnames(df_pred) %in% c("Ret","time")]
  y_pred<-df_pred$Ret
  
  x_base_fit<-x_fit %>% select(Lagged_ret,sd_3,sd_7,sd_30,sd_60)
  x_base_pred<-x_pred %>% select(Lagged_ret,sd_3,sd_7,sd_30,sd_60)
  
  #quantregForest
  quantregF <- quantregForest(x=x_fit, y=y_fit, nodesize = 20)
  quantregForest_quantiles    <- predict(quantregF, x_pred, what=tau)
  print("QRF finished")
  #qrf base
  #quantregForest
  qrf_base <- quantregForest(x=x_base_fit, y=y_fit, nodesize = 20)
  qrf_quantiles_base <- predict(qrf_base, x_base_pred, what=tau)
  
  #GRF
  q.forest <- quantile_forest(x_fit, y_fit, quantiles = tau, min.node.size = 20, seed = 7,  alpha = 0.1)
  grf_quantiles  <- predict(q.forest, x_pred,  quantiles= tau)
  print("GRF finished")
  var_imp <- variable_importance(q.forest, decay.exponent = 2, max.depth = 5)
  
  q.forest_base <- quantile_forest(x_base_fit, y_fit, quantiles = tau, min.node.size = 20, seed = 7,  alpha = 0.1)
  grf_quantiles_base  <- predict(q.forest_base, x_base_pred,  quantiles= tau)
  
  #Quantile Regression
  print("QR start")
  qr_quantiles<-NA
  try({  rqfit <- rq(y_fit ~ .,data=x_fit,tau = tau)
  qr_quantiles <- predict(rqfit,x_pred)})
  qr_imp<-NA
  # methods<-c("rank","iid","nid","ker","boot","BLB","conquer","extreme")
  # for(i in methods)
  # {
  #   print(i)
  #   try(print(system.time(qr_imp<-summary(rqfit,se=i))))
  # }
  try({qr_imp<-summary(rqfit,se="iid")$coefficients[,c(1,4)]}) #gives out of memory error for anything besides iid
  #try(qr_imp<-rqfit$coefficients)
  
  qr_quantiles_base<-NA
  try({  rqfit_base <- rq(y_fit ~ .,data=x_base_fit,tau = tau)
  qr_quantiles_base <- predict(rqfit_base,x_base_pred)})
  
  #qr_quantiles <- as.matrix(cbind(rep(1,nrow(x_pred)),cbind(x_pred$Standard_Dev_2,x_pred$Standard_Dev_3,x_pred$Lagged_Return))) %*% matrix(as.numeric(rqfit$coefficients), 4, 1)
  
  #GARCH-X
  
  #make garch forecast df
  df_all<-rbind(df_fit,df_pred)
  x_garch<-df_all[,!colnames(df_fit) %in% c("Ret","time")]
  y_garch<-df_all$Ret
  forecasts_var_gjr<-NA
  forecasts_var_x<-NA
  
  #make specs
  garch_spec_X<-ugarchspec(variance.model = list(model="sGARCH",external.regressors=as.matrix(x_garch)),
                           distribution.model = "norm")
  
  try({
    garch_fit_X<-ugarchfit(garch_spec_X,data= y_garch,out.sample = 1,
                           solver = "hybrid")
    fc_one_ahead_x<-ugarchforecast(garch_fit_X, n.ahead = 1)
    forecasts_var_x<-quantile(fc_one_ahead_x,probs=tau) #gives back quantile based on normal distribution
  })
  
  if(is.na(forecasts_var_x))
  {print("Covariate GJR-Garch NA")}
  
  
  hist_quantiles<-quantiles_gauss<-caviar_quantiles<-forecasts_var_gjr<-NA
  
  #only compute if necessary
  
  if(compute_cav_hist_gjr)
  {
    #GARCH GJR
    
    gjr_garch_spec<-ugarchspec(variance.model = list(model="gjrGARCH"),distribution.model = "norm")
    #try fitting
    try({
      garch_fit<-ugarchfit(gjr_garch_spec,data= y_garch,out.sample = 1,
                           solver = "hybrid")
      fc_one_ahead<-ugarchforecast(garch_fit, n.ahead = 1)
      forecasts_var_gjr<-quantile(fc_one_ahead,probs=tau) #gives back quantile based on normal distribution
    })
    
    if(is.na(forecasts_var_gjr))
    {print("Standard GJR-Garch NA")}
    
    #Historical VaR
    print("Hist start")
    hist_quantiles <- quantile(y_fit,tau)
    
    #Variance/Covariance, ie fit gaussian distribution and take quantile
    var_cov_dist<- fitdist(distribution = "norm",x=y_fit)
    quantiles_gauss <- qnorm(p=tau,mean=var_cov_dist$pars[1],sd=var_cov_dist$pars[2])
    
    ##CAViaR
    #CAViaR
    print("Caviar start")
    VaR_temp<-rep(quantile(y_fit,0.05),length(y_fit))
    min_obj<-DEoptim(fn=caviar_eval,
                     #lower=rep(-10,ncol(x_caviar)+3),
                     #upper=rep(10,ncol(x_caviar)+3),
                     lower=rep(-25,3),
                     upper=rep(25,3),
                     control=list(trace=FALSE),
                     r_t=y_fit,
                     Var_init=VaR_temp,
                     alpha=tau)
    
    #get best parameters
    thetas  <- min_obj$optim$bestmem
    
    #forecast VaR
    Var_fin<-VaR_temp[1]
    for(k in 2:length(y_fit))
    {
      Var_fin<-thetas[1]+thetas[2]*Var_fin+thetas[3]*abs(y_fit[k-1])# + sum(thetas[-c(1:3)] * x_caviar[i,])
    }
    caviar_quantiles <- Var_fin
  }
  
  
  
  
  list_fin<-list(QRF=quantregForest_quantiles,GRF=grf_quantiles,QR=qr_quantiles,
                 CAV=caviar_quantiles,GARCH_plain=forecasts_var_gjr,
                 GARCH_cov=forecasts_var_x,Hist=hist_quantiles,Gauss=quantiles_gauss,
                 QRF_base=qrf_quantiles_base,GRF_base=grf_quantiles_base,QR_base=qr_quantiles_base,
                 Var_imp_grf = var_imp,Var_imp_qr = qr_imp)
  return(list_fin)
}
