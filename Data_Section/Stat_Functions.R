library(data.table)
library(tidyverse)
library(zoo)
library(scales)
library(tseries)

#FUNCTIONS FOR DATA SECTION
###################################

fun_nonstat_test<-function(x)
{
  kpss_level<-kpss.test(x,"Level") #H1: non-stationarity
  kpss_trend<-kpss.test(x,"Trend") #H1: non-stationarity
  adf<-adf.test(x) #H1: Stationarity
  ret_obj<-c(kpss_level$p.value,kpss_trend$p.value,adf$p.value)
  return(ret_obj)
}

fun_get_stats_by_date<-function(single_date,quantiles=c(0.05,0.5,0.95))
{
  temp_rets<-rep(NA,length(list_log_rets_time))
  for(i in 1:length(list_log_rets_time))
  {
    if(single_date %in% list_log_rets_time[[i]]$date)
    {
      temp_rets[i]<-list_log_rets_time[[i]]$log_ret[which(list_log_rets_time[[i]]$date==single_date)]
    }
  }
  return_quants<-quantile(temp_rets,probs=quantiles,na.rm=TRUE)
  num_non_na<-sum(!is.na(temp_rets))
  fin_ret<-c(return_quants,num_non_na)
  names(fin_ret)<-c(paste0("q_",quantiles),"num_assets")
  return(fin_ret)
}

#plot for the returns over time
fun_ret_time<-function(df_with_cov)
{
  log_ret<-diff(log(df_with_cov$Price),lag=1)
  date<-df_with_cov$time[-1]
  return(data.frame(log_ret,date))
}

#Make summary stats table
fun_sum<-function(data)
{
  quantiles<-quantile(data,c(0,0.01,0.05,0.5,0.95,0.99,1))
  mean_dat<-mean(data)
  no_obs<-length(data)
  kurt<-DistributionUtils::kurtosis(data)
  skew<-DistributionUtils::skewness(data)
  sd<-sd(data)
  
  ret_data<-c(quantiles,skew,kurt,sd,no_obs)
  return(round(ret_data,3))
}


#Make summary of covariates table
fun_quant<-function(data){
  quantiles<-quantile(data,c(0.05,0.5,0.95), na.rm=T)
  return(round(quantiles,3))
}



