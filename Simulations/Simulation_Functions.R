

#####################
#Functions for Simulations
#####################

library(rugarch)
library(DEoptim)
library(MSGARCH)
library(GAS)


generate_random_params <- function() {
  while(TRUE) {
    omega <- rnorm(1, mean = 0.002370133, sd = sqrt(7.179315e-07))  # Zufälliger Wert für omega zwischen 1e-6 und 1e-3
    alpha1 <- rnorm(1, mean = 0.106418998, sd = sqrt(3.663107e-04))  # Zufälliger Wert für alpha1 zwischen 0.05 und 0.3
    beta1 <- rnorm(1, mean = 0.884996656, sd = sqrt(4.710988e-04))   # Zufälliger Wert für beta1 zwischen 0.6 und 0.95
    if (alpha1 + beta1 <= 1) {     # Bedingung überprüfen
      return(list(omega = omega, alpha1 = alpha1, beta1 = beta1))
    }
  }
}


#compute models given input
gen_models<-function(dgp,lengthOfObs,btc_data,tau=0.05)
{
  if(dgp=="GARCH_norm")
  {
    #DGP 1:
    ############
    
    #Normal distribution
    spec = ugarchspec(variance.model = list(model="sGARCH"),
                      mean.model =list(armaOrder = c(0,0), include.mean = TRUE, archm = FALSE,
                                       archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE),
                      distribution.model = "norm",
                      fixed.pars = list(mu=0,omega = 1e-4, alpha1 = 0.1, beta1 = 0.8))
    returns <- ugarchpath(spec,n.sim = lengthOfObs,n.start = 100)@path$seriesSim
    
  }
  
  if(dgp=="GARCH_VaryVola")
  {
    #DGP 1:
    ############
    returns <- numeric()
    for(j in 1:1){
      params <- generate_random_params()
      #Normal distribution
      spec = ugarchspec(variance.model = list(model="sGARCH"),
                        mean.model =list(armaOrder = c(0,0), include.mean = TRUE, archm = FALSE,
                                         archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE),
                        distribution.model = "norm",
                        fixed.pars = list(mu=0,omega = params$omega, alpha1 = params$alpha1, beta1 = params$beta1))
      returns_temp <- ugarchpath(spec,n.sim = 500,n.start = 100)@path$seriesSim
      returns <- c(returns, as.vector(returns_temp))
    }
  }
  
 
  if(dgp=="GARCH_btc_asym_t")
  {
    # normally-distributed
    spec = ugarchspec(variance.model = list(model="sGARCH"),
                      mean.model =list(armaOrder = c(0,0), include.mean = TRUE, archm = FALSE,
                                       archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE),
                      distribution.model = "sstd")
    fit_btc<-ugarchfit(spec,data = btc_data)
    returns <- ugarchsim(fit = fit_btc,n.sim = lengthOfObs,n.start = 100,startMethod = "unconditional")@simulation$seriesSim
  }
 
  
  if(dgp=="SAV")
  {
    stand <- rchisq(lengthOfObs/100,df=2)/65
    
    returns <- rnorm(n=100,mean=0,sd=stand[1])
    for(b in 2:length(stand)){
      returns <- c(returns,rnorm(n=100,mean=0,sd=stand[b]))
    }
    
    #fit SAV model
    var    <- as.numeric(quantile(returns[1:250], probs = tau))
    SAV <- rep(-var,length(returns))
    nparms <- 4
    res    <- DEoptim(SAV_sim,
                      lower=rep(-10,nparms), 
                      upper=rep(+10,nparms),
                      control=list(trace=FALSE),
                      train = returns,
                      SAV = SAV,
                      tau=tau)
    
    betas  <- res$optim$bestmem
    
    #predict SAV model
    
    VaR_sav <- SAV_pred_sim(betas, returns,"SAV")
    
    #calculate modified returns
    sd <- ifelse(VaR_sav >= 0, (-VaR_sav/qnorm(tau,mean=0,sd=1)), 0)
    
    returns_old<-returns
    returns <- VaR_sav
    for(p in 1:length(VaR_sav)){
      returns[p] <- rnorm(1,mean=0,sd=sd[p])
    }
  }
  
  data <- data.frame(count = seq(1,lengthOfObs),Return = returns)
  colnames(data) <- c("count","Return")
  
  sa_lengths <- c(3,7,30,60) #time windows for standard deviation calculations
  SA         <- vector("list",length(sa_lengths))
  k          <- 1 
  for(sa_length in sa_lengths){
    
    #Calculate Standard Deviation 
    standard_deviation <- data$Return[-c(1:sa_length)]
    
    for(i in (sa_length+1):length(data$Return)){
      standard_deviation[i] <- (sum((data$Return[(i-sa_length):(i-1)] - mean(data$Return[(i-sa_length):(i-1)]))^2) / (sa_length -1))^0.5
    }
    standard_deviation[1:sa_length] <- NA
    SA[[k]] <- standard_deviation
    k <- k+1
    
  }
  
  data_frame <- data.frame(Return = data$Return, 
                           Standard_Dev_1 = SA[[1]], 
                           Standard_Dev_2 = SA[[2]], 
                           Standard_Dev_3 = SA[[3]], 
                           Standard_Dev_4 = SA[[4]], 
                           Lagged_Return = c(NA, data$Return[-length(data$Return)]))
  
  
  
  #Data Transformation
  #delete the rows where some NAs are present, i.e. the first 60 since the last sa_lengths entry is 60
  #this is due to the lagged 60 SA values not being present
  data_frame <- data_frame[(sa_length+1):nrow(data_frame),]
  
  return(list(model_data=data_frame,raw_ret=data$Return))
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

cav_pred<-function(thetas,r_t,alpha=tau)
{
  Var_fin<-as.numeric(quantile(r_t,alpha))
  for(k in 1:length(r_t))
  {
    Var_fin<-thetas[1]+thetas[2]*Var_fin+thetas[3]*abs(r_t[k])# + sum(thetas[-c(1:3)] * x_caviar[i,])
  }
  caviar_quantiles <- Var_fin
  return(caviar_quantiles)
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

#Performs backtest given return data, Var prediction vector and tau level, wrapper for BacktestVar()
backtest_fun<-function(returns,Var_pred,tau)
{
  backtest_funs<-apply(Var_pred,2,function(x){BacktestVaR(returns,x,tau)})
  
}



SAV_sim<-function(betas, train, SAV,tau=0.05){
  # Returns the objective funktion value of a SAV process for a given set of betas and return data
  #
  # INPUTS: betas:  a 4x1 vector of parameters
  #         train:  a nx1 vector of training return data
  #         SAV: a nx1 vector of initial values for the SAV process 
  #         tau: the VaR level, usually 0.05
  #
  # OUTPUTS: res: value of the objective function
  beta1<-betas[1]
  beta2<-betas[2]
  beta3<-betas[3]
  beta4<-betas[4]
  for (i in 2:length(train)) {
    SAV[i] <- beta1 + beta2 * SAV[i - 1] + beta3 * abs(train[i - 1] - beta4)
  }
  #Objective Function
  res <- sum((tau-(train< -SAV))*(train+SAV)) 
  if(is.na(res)|is.infinite(res)) res<- 1e+10
  #Objective Function
  return(res)
}

SAV_pred_sim <-function(betas, data, type){
  # Returns the SAV values for given betas based on the given data
  #
  # INPUTS: betas:  a 4x1 vector of parameters
  #         data:   a nx1 vector of return data
  #
  # OUTPUTS: SAV: a (n+1)x1 vector of SAV values (observation n+1 is the forecast)
  beta1<-betas[1]
  beta2<-betas[2]
  beta3<-betas[3]
  beta4<-betas[4]
  
  #Create the SAV vector
  var    <- as.numeric(quantile(data[1:(length(data)/10)], probs = tau))
  SAV <- rep(-var,length(data))
  
  for (i in 2:length(data)) {
    SAV[i] <- beta1 + beta2 * SAV[i - 1] + beta3 * abs(data[i - 1] - beta4)
  }  
  return(SAV)
}