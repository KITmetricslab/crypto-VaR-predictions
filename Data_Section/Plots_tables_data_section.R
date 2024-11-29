
library(xtable)
library(tseries)
library(dygraphs)
library(xts)
library(ggplot2)
library(scales)
library(data.table)
library(zoo)
library(dplyr)


source("Data_Section/Stat_Functions.R")

load(file="Cleaned_Data/Forecasts/Crypto_Data_Raw.RData")
load(file="Cleaned_Data/Forecasts/Crypto_Data_add.RData")
load(file="Cleaned_Data/Forecasts/Crypto_Data_MktCap.RData")

#######################################
## Overview of Non-Stationarity Tests 
## Table A.15                         

#check for non-stationarities of log-returns

list_log_rets<-sapply(1:length(crypto_data_cleaned),function(x){diff(log(crypto_data_cleaned[[x]]$Price),lag=1)})
names(list_log_rets)<-names(crypto_data_cleaned)


df_stat_tests<-t(sapply(list_log_rets,fun_nonstat_test))
colnames(df_stat_tests)<-c("KPSS_level","KPSS_trend","ADF")

sum(df_stat_tests[,1]<0.05) 
sum(df_stat_tests[,2]<0.05)
sum(df_stat_tests[,3]>0.01) #zero non-stationary with adf test

df_tests_print<-df_stat_tests[df_stat_tests[,2]<0.1 |df_stat_tests[,1]<0.1,]

caption_c<-"Overview of Non-Stationarity Tests"
tests_print<-xtable(df_tests_print,label = "tab:crypto_tests",caption=caption_c)
print.xtable(tests_print,type="latex",#file="Plots/Crypto_tests.tex",
             include.rownames=TRUE,caption.placement = "top",
             comment=TRUE,booktabs = TRUE)



#####################################################
## Descriptive Statistics of Log-Returns of Cryptos 
## Table 1

summary_table<-sapply(list_log_rets,fun_sum)

summary_compact<-sapply(1:dim(summary_table)[1],function(x)quantile(summary_table[x,],probs=c(0,0.01,0.05,0.25,0.5,0.75,0.95,0.99,1)))

colnames(summary_compact)<-c("Min","1%","5%","Median","95%","99%","Max","Skewness","Excess-Kurtosis","Standard Deviation","Observations")

print(xtable(summary_compact,type="Latex",digits=3))
summary_compact_x<-xtable(summary_compact,label = "Tab:Desc",caption="Descriptive Statistics of Log-Returns of Cryptocurrencies")
print.xtable(summary_compact_x,type="latex",#file="Plots/Crypto_statdesc.tex",
             include.rownames=TRUE,caption.placement = "top",
             comment=TRUE,booktabs = TRUE)


#############################################
## Median returns plot
## Figure 1
##########################
list_log_rets_time<-lapply(crypto_data_cleaned,fun_ret_time)
names(list_log_rets_time)<-names(crypto_data_cleaned)
min_date<-min(do.call("c",lapply(list_log_rets_time,function(x) min(x$date))))
max_date<-max(do.call("c",lapply(list_log_rets_time,function(x) max(x$date))))
min_date<-as.POSIXct(min_date, format = "%Y-%m-%d")
max_date<-as.POSIXct(max_date, format = "%Y-%m-%d")

time_points<-format(seq(min_date,max_date,by="1 day"), "%Y-%m-%d")
ret_over_time<-t(sapply(time_points,fun_get_stats_by_date))


data_plot <- data.frame( time=as.POSIXct(time_points), ret_over_time)

t2015<-as.POSIXct("2015-08-22")
t2018<-as.POSIXct("2017-12-21")
t2020<-as.POSIXct("2020-11-05")
t2023<-as.POSIXct("2023-01-01")


cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cols_periods<-cbPalette[c(5,4,2,8)]
#cols_periods<-c("#CA0020","#F4A582","#0571B0")

ts_plot<-ggplot(data_plot, aes(x = time, y = q_0.5)) +
  geom_ribbon(aes(ymin = q_0.05, ymax = q_0.95), alpha = 0.2,colour="lightblue") +
  geom_line() +
  scale_y_continuous(name="Median Return",limits=c(-0.7,1.2),sec.axis = sec_axis(~. * 100,name="Number of Currencies")) + 
  scale_x_datetime(name="Date",breaks=date_breaks("2 years"),date_labels = "%Y-%m") +
  geom_point(aes(x= time, y=num_assets/100,),colour = "red",alpha=0.2,size=0.5) +
  theme(axis.title.y.right=element_text(color="red"),legend.position = "none",text = element_text(size = 20))+
  geom_vline(xintercept=c(t2015,t2018,t2020,t2023),linetype="dashed")+
  #geom_rect(aes(xmin =t2014 , xmax = t2018, ymin = -Inf, ymax = Inf),alpha = 0.01,fill = "red")
  annotate("rect",xmin = t2015, xmax = t2018, ymin = -Inf, ymax = Inf,
           alpha = .1,fill =cols_periods[1]) +
  annotate("rect",xmin = t2018, xmax = t2020, ymin = -Inf, ymax = Inf,
           alpha = .1,fill = cols_periods[2]) +
  annotate("rect",xmin = t2020, xmax = max(data_plot$time), ymin = -Inf, ymax = Inf,
           alpha = .1,fill = cols_periods[3]) +
  annotate("rect",xmin = t2023, xmax = max(data_plot$time), ymin = -Inf, ymax = Inf,
           alpha = .1,fill = cols_periods[4]) +
  annotate("text",label=c("Period 1","Period 2","Period 3","Period4"),x=c(t2018-(t2018-t2015)/2,
                                                                     t2020-(t2020-t2018)/2,
                                                                     t2023-(t2023-t2020)/2,
                                                                     max(data_plot$time)-(max(data_plot$time)-t2023)/2),
           y=rep(1.2,4))


ts_plot
ggsave("Plots/Median_returns.pdf",plot=ts_plot,width=15,height=10,units="in",scale=1)



#####################################################
## Summary of Covariates for Different Time Periods 
## Table 2

# add Market Cap in Million USD
list_rets_MC<-list()
for(i in seq_along(list_rets)){
  temp_rets<-list_rets[[i]]
  temp_rets$time<-as.IDate(temp_rets$time)
  temp_MC<-list_crypto_MktCap[[i]]
  if(sum(colnames(temp_MC)=="CapMrktCurUSD")<1)
  {
    df_temp<-temp_rets
  } else {
    df_temp <- merge(temp_rets,temp_MC, by="time")  %>% 
       mutate(CapMrktCurMUSD=CapMrktCurUSD/1e6) %>% select(-CapMrktCurUSD)
  }
  list_rets_MC[[i]] <- df_temp
}
names(list_rets_MC)<-names(list_rets)
save(list_rets_MC, file = "~/crypto-VaR-predictions/Cleaned_Data/Forecasts/Crypto_Data_add_plusMktCap.RData")

# keep only covariates
list_covs_only<-list()
covs<-names(list_rets_MC[[1]][c(1,3)])
for (j in seq_along(list_rets_MC)){
  res_temp <- list_rets_MC[[j]][!names(list_rets_MC[[j]]) %in% covs]
  list_covs_only[[j]] <- res_temp
}
names(list_covs_only) <- names(list_rets_MC)

sum_covs<-lapply(list_covs_only, sapply, fun_quant)

load("Cleaned_Data/Tests/Crypto_ind_periods_4.RData")

sum_covs_array<-array(NA, dim=c(3,13,length(sum_covs)))
dimnames(sum_covs_array)[[1]]<-c("5%", "Median", "95%")
dimnames(sum_covs_array)[[2]]<-names(list_rets_MC[[1]][c(2,4:15)])
dimnames(sum_covs_array)[[3]]<-names(sum_covs)

for(i in seq_along(sum_covs)){
  if(dim(sum_covs[[i]])[2]==12){
    sum_covs_array[,,i]<-sum_covs[[i]]
  }
  else {
    for(j in colnames(sum_covs[[i]])){
      cov_val<-sum_covs[[i]][,j]
      sum_covs_array[,j,i]<-cov_val
    }
  }
}


# apply function to full data, and to subsamples of periods 1-4

ind_p1<-list_crypto_period[[1]]$Crypto_ind
p1_array<-sum_covs_array[,,ind_p1]
p2_array<-sum_covs_array[,,list_crypto_period[[2]]$Crypto_ind]
p3_array<-sum_covs_array[,,list_crypto_period[[3]]$Crypto_ind]
p4_array<-sum_covs_array[,,list_crypto_period[[4]]$Crypto_ind]

sum_p1<-apply(p1_array, c(1,2), mean, na.rm=T)
sum_p2<-apply(p2_array, c(1,2), mean, na.rm=T)
sum_p3<-apply(p3_array, c(1,2), mean, na.rm=T)
sum_p4<-apply(p4_array, c(1,2), mean, na.rm=T)
sum_full<-apply(sum_covs_array, c(1,2), mean, na.rm=T)

summary_covariates<-rbind(sum_p1,sum_p2,sum_p3,sum_p4,sum_full)
rownames(summary_covariates)<-rep(c("5", "Median", "95"),5)
#round0<-colnames(summary_covariates)[c(2:5,7)]
#round3<-colnames(summary_covariates)[c(1,6,8:12)]
#summary_covariates_round<-summary_covariates %>% data.frame %>% mutate(across(round0, round, 0)) %>% mutate(across(round3, round, 3))
print(xtable(summary_covariates,type="Latex",digits=c(0,3,0,0,0,0,3,0,3,3,3,3,3,0)))



