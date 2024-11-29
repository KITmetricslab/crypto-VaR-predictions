load("Cleaned_Data/Tests/Crypto_ind_periods_4.RData")
load("Cleaned_Data/Forecasts/final_data_combined_0.05_500_2024-11-20.RData")
load("Cleaned_Data/Forecasts/Crypto_Data_add.RData")
source("Tests/Evaluation_Functions.R")

################################################
# Zeit-Perioden festlegen
################################################
period <- c("2015-08-22", "2017-12-21", "2020-11-05", "2023-01-01", "2024-04-06")

# Liefert eine große Liste wo die Forecasts nach Perioden sortiert enthalten sind
Period_Data <- getPeriodData(final_data_combined, period)

###############################################
# CPA tests over short period and full period
###############################################

# Liste mit p Werten für CPA Tests

list_time_period<-vector("list",4)
names(list_time_period)<-c("Time Period 1","Time Period 2", "Time Period 3", "Time Period 4")
sign_star<-0.05 #from which p-value on should + or - be printed
numbers=TRUE #do you want numbers or latex table
test_lhs<-"GRF_base" #which one do you want to test against; GRF_base(GRF) or GRF(GRF-X)
test_rhs<-setdiff(c("GRF","QRF","GRF_base","QR","CAV","GARCH_plain","GARCH_cov","Hist","QRF_base","QR_base","CAV_ASY"),test_lhs) #what should it be compared to
for(ind_i in 1:length(list_time_period))
{
  mat_p_val<-matrix(NA,nrow=length(list_crypto_period[[ind_i]]$Crypto_ind),ncol=length(test_rhs)) #take QRF, QR, GJR_GARCH, GARCH-X, CAVIAR, Hist,3xbase GRF,QRF,QR, CAV_ASY
  mat_perf<-matrix(NA,nrow=length(list_crypto_period[[ind_i]]$Crypto_ind),ncol=length(test_rhs)) #take QRF, QR, GJR_GARCH, GARCH-X, CAVIAR, Hist,3xbase GRF,QRF,QR, CAV_ASY
  temp_data<-list_crypto_period[[ind_i]]
  colnames(mat_p_val)<-colnames(mat_perf)<-test_rhs
  rownames(mat_p_val)<-rownames(mat_perf)<-names(list_crypto_period[[ind_i]]$Crypto_ind)
  for(ind_k in 1:length(list_crypto_period[[ind_i]]$Crypto_ind))
  {
    ind_temp<-temp_data$Crypto_ind[ind_k]
    temp_pred<-final_data_combined[[ind_temp]]
    true_y<-unlist(temp_pred$True_Ret)
    loss_lhs<-get_loss(loss=tick,true_val=true_y,forecast=unlist(temp_pred[[test_lhs]]), tau=0.05)
    cols_to_keep<-which(names(temp_pred) %in% test_rhs)
    #Make sure that they appear in right order
    ind_order <-  match(test_rhs,names(temp_pred)[cols_to_keep])
    ind_mat <- cols_to_keep[ind_order]
    counter<-1
    for(j in ind_mat) #gives back indices
    {
      loss1 <- get_loss(loss=tick,true_val=true_y,forecast=rem_nan_na(unlist(temp_pred[[j]]),10000), tau=0.05) #change to 0.01 if you want 1% VaR
      cpa_results<-CPAtest(loss1 = loss1,loss2 = loss_lhs,tau=1,alpha=0.05,choice=2,sign_level=sign_star,print_it=FALSE,ret_table=!numbers,ret_numbers=numbers) #change significance level for +/- signs
      mat_p_val[ind_k,counter]<-cpa_results$pval
      mat_perf[ind_k,counter]<-cpa_results$rate
      counter<-counter+1
    }
    print(paste0("Period: ",names(list_time_period)[ind_i]," Finished with Asset: ",names(final_data_combined)[ind_temp]))
  }
  print(paste0("Finished with Time Period: ", names(list_time_period)[ind_i]))
  list_time_period[[ind_i]]<-list(p_vals=mat_p_val,perf_rate=mat_perf)
}


#CPA tests over full time period

mat_p_val<-matrix(NA,nrow=length(final_data_combined),ncol=length(test_rhs)) #take QRF, QR, GJR_GARCH, GARCH-X, CAVIAR, Hist and 3x base, CAV_ASY
mat_perf<-matrix(NA,nrow=length(final_data_combined),ncol=length(test_rhs)) #take QRF, QR, GJR_GARCH, GARCH-X, CAVIAR, Hist, CAV_ASY
colnames(mat_p_val)<-colnames(mat_perf)<-test_rhs
rownames(mat_p_val)<-rownames(mat_perf)<-names(final_data_combined)
for(ind_k in 1:length(final_data_combined))
{
  temp_pred<-final_data_combined[[ind_k]]
  true_y<-unlist(temp_pred$True_Ret)
  loss_lhs<-get_loss(loss=tick,true_val=true_y,forecast=unlist(temp_pred[[test_lhs]]), tau=0.05)
  cols_to_keep<-which(names(temp_pred) %in% test_rhs)
  #Make sure that they appear in right order
  ind_order <-  match(test_rhs,names(temp_pred)[cols_to_keep])
  ind_mat <- cols_to_keep[ind_order]
  counter<-1
  for(j in ind_mat) #gives back indices
  {
    loss1 <- get_loss(loss=tick,true_val=true_y,forecast=rem_nan_na(unlist(temp_pred[[j]]),10000), tau=0.05) #change to 0.01 if you want 1% VaR
    cpa_results<-CPAtest(loss1 = loss1,loss2 = loss_lhs,tau=1,alpha=0.05,choice=2,sign_level=sign_star,print_it=FALSE,ret_table=!numbers,ret_numbers=numbers) #change significance level for +/- signs
    mat_p_val[ind_k,counter]<-cpa_results$pval
    mat_perf[ind_k,counter]<-cpa_results$rate
    counter<-counter+1
  }
  print(paste0(" Finished with Asset: ",names(final_data_combined)[ind_k],", ",ind_k," of ", length(final_data_combined)))
}

cpa_file_name<-paste0("Cleaned_Data/Tests/CPA_tests_full_",test_lhs,"_vs_rest.RData")
save(mat_p_val,mat_perf,list_time_period,test_lhs,test_rhs,file=cpa_file_name)



##########################
## CPA Tests for different time periods
## Table 8
## Table 12
## Table B.18
##########################



#time period GRF vs every method
test_lhs<-"GRF_base" #GRF or GRF_base
load(paste0("Cleaned_Data/Tests/CPA_tests_full_",test_lhs,"_vs_rest.RData"))
list_perf_summary<-lapply(1:4,function(x) fun_make_tab_df(p_val_df=list_time_period[[x]]$p_vals,
                                                          perf_df=list_time_period[[x]]$perf_rate,
                                                          ordering=NULL))
for(i in seq_along(list_perf_summary)){
  rownames(list_perf_summary[[i]]) <- names(Period_Data[[i]])
}

names(list_perf_summary)<-c("P1","P2","P3","P4")
raw_tab_perf_p<-fun_make_tab_df(mat_p_val,mat_perf,ordering=NULL)
list_perf_summary$full<-raw_tab_perf_p
len_periods<-sapply(list_perf_summary,function(x) nrow(x))
num_better<-sapply(1:5,function(x)apply(list_perf_summary[[x]],2,function(y)sum(y>0,na.rm = TRUE)))
num_sig<-sapply(1:5,function(x)apply(list_perf_summary[[x]],2,function(y)sum(y>0.9,na.rm=TRUE)))

if(test_lhs=="GRF_base") {
  num_better_fin<-(t(num_better)/len_periods) %>% as.data.frame() %>%
    select(QRF_base,QR_base,CAV,CAV_ASY,GARCH_plain,Hist,GRF,QRF,QR,GARCH_cov) %>%
    rename("GARCH-X"=GARCH_cov,"GJR-GARCH"=GARCH_plain,"GRF-X"=GRF,"QRF-X"=QRF,"QR-X"=QR)%>%
    rename_with(.fn=function(x){gsub("_base","",x)})
  num_sig_fin<-(t(num_sig)/len_periods) %>% as.data.frame() %>%
    select(QRF_base,QR_base,CAV,CAV_ASY,GARCH_plain,Hist,GRF,QRF,QR,GARCH_cov) %>%
    rename("GARCH-X"=GARCH_cov,"GJR-GARCH"=GARCH_plain,"GRF-X"=GRF,"QRF-X"=QRF,"QR-X"=QR)%>%
    rename_with(.fn=function(x){gsub("_base","",x)})
} else {
  num_better_fin<-(t(num_better)/len_periods) %>% as.data.frame() %>%
    select(GRF_base,QRF_base,QR_base,CAV,CAV_ASY,GARCH_plain,Hist,QRF,QR,GARCH_cov) %>%
    rename("GARCH-X"=GARCH_cov,"GJR-GARCH"=GARCH_plain,"QRF-X"=QRF,"QR-X"=QR)%>%
    rename_with(.fn=function(x){gsub("_base","",x)})
  num_sig_fin<-(t(num_sig)/len_periods) %>% as.data.frame() %>%
    select(GRF_base,QRF_base,QR_base,CAV,CAV_ASY,GARCH_plain,Hist,QRF,QR,GARCH_cov) %>%
    rename("GARCH-X"=GARCH_cov,"GJR-GARCH"=GARCH_plain,"QRF-X"=QRF,"QR-X"=QR)%>%
    rename_with(.fn=function(x){gsub("_base","",x)})
}


sum_cpa_print<-t(rbind(num_better_fin,num_sig_fin))
colnames(sum_cpa_print)<-rep(c(paste0("Period ",1:4),"Full Data"),2)

caption_c<-"Summary of Performance and P-Values of CPA-tests Over Different Time Periods"
sum_cpa_xtab<-xtable(t(sum_cpa_print),label = paste0("tab:crypto_cpa_sum_",test_lhs),caption=caption_c,digits=2)#display = c("s",rep("d",7)))
print.xtable(sum_cpa_xtab,type="latex",#file="Plots/Crypto_dq_1.tex",
             floating=TRUE,include.rownames=TRUE,caption.placement = "top",
             comment=TRUE,booktabs = TRUE)







#################################
## CPA Plots over time
## Table 8
## Figure B.6
## Figure B.7
## Figure B.8
## Table B.18
## Figure B.9
## Figure B.10
#################################

#load the CPA data you want
test_lhs<-"GRF_base" #GRF_base=GRF is default, GRF=GRF-X is the version with covariates
load(paste0("Cleaned_Data/Tests/CPA_tests_full_",test_lhs,"_vs_rest.RData"))

#use rev() to reverse color: rev(brewer.pal(XYZ))
col_perf<- colorRampPalette((RColorBrewer::brewer.pal(9,"RdBu")),)(200)
times<-1:4
adjust<-FALSE #set to true to only display significance for GRF
#perf_rate<-(list_time_period[[tp]]$perf_rate[list_cov_time[[tp]]$TxCnt_sum,])
#p_val<-list_time_period[[tp]]$p_vals[list_cov_time[[tp]]$TxCnt_sum,]

size_stars<-1.3
size_labels<-1.45

#names_sort<-colnames(list_cov_time[[1]])
colnames(list_rets$btc)
#col_to_sort<-3
var_to_order<-"sd_30" # select sd_30 or CapMrktCurMUSD 

ind_rows<-fun_order_one_var(list_rets,Var_to_order =var_to_order,summary_fun=mean)

for(i in 1:4)
{
  #select rows from ordered rows (ind_rows) that are contained in time period i
  ind_row_temp<-ind_rows[ind_rows %in%list_crypto_period[[i]]$Crypto_ind]
  ind_row_t<-match(ind_row_temp,list_crypto_period[[i]]$Crypto_ind)
  tp<-times[i]
  # perf_rate<-(list_time_period[[tp]]$perf_rate[list_cov_time[[tp]][,col_to_sort],])
  # p_val<-list_time_period[[tp]]$p_vals[list_cov_time[[tp]][,col_to_sort],]
  if(test_lhs=="GRF_base")
  {
    perf_rate<-list_time_period[[tp]]$perf_rate[ind_row_t,] %>% rem_nan_na(replace_el = 0.5) %>% as.data.frame() %>%
      select(QRF_base,QR_base,CAV,CAV_ASY,GARCH_plain,Hist,GRF,QRF,QR,GARCH_cov) %>% #FOR GRF_BASE
      rename("GARCH-X"=GARCH_cov,"GJR-GARCH"=GARCH_plain,"GRF-X"=GRF,"QRF-X"=QRF,"QR-X"=QR)%>% #FOR GRF_BASE
      rename_with(.fn=function(x){gsub("_base","",x)})
    rownames(perf_rate) <- names(Period_Data[[i]])[ind_row_t]
    p_val<-list_time_period[[tp]]$p_vals[ind_row_t,]  %>% rem_nan_na(replace_el = 1) %>% as.data.frame() %>%
      select(QRF_base,QR_base,CAV,CAV_ASY,GARCH_plain,Hist,GRF,QRF,QR,GARCH_cov) %>% #FOR GRF_BASE
      rename("GARCH-X"=GARCH_cov,"GJR-GARCH"=GARCH_plain,"GRF-X"=GRF,"QRF-X"=QRF,"QR-X"=QR)%>% #FOR GRF_BASE
      rename_with(.fn=function(x){gsub("_base","",x)})
    rownames(p_val) <- names(Period_Data[[i]])[ind_row_t]
  }else{
    perf_rate<-list_time_period[[tp]]$perf_rate[ind_row_t,] %>% rem_nan_na(replace_el = 0.5) %>% as.data.frame() %>%
      select(GRF_base,QRF_base,QR_base,CAV,CAV_ASY,GARCH_plain,Hist,QRF,QR,GARCH_cov) %>% #FOR GRF-X
      rename("GARCH-X"=GARCH_cov,"GJR-GARCH"=GARCH_plain,"QRF-X"=QRF,"QR-X"=QR)%>% #FOR GRF-X
      rename_with(.fn=function(x){gsub("_base","",x)})
    rownames(perf_rate) <- names(Period_Data[[i]])[ind_row_t]
    p_val<-list_time_period[[tp]]$p_vals[ind_row_t,]  %>% rem_nan_na(replace_el = 1) %>% as.data.frame() %>%
      select(GRF_base,QRF_base,QR_base,CAV,CAV_ASY,GARCH_plain,Hist,QRF,QR,GARCH_cov) %>% #FOR GRF-X
      rename("GARCH-X"=GARCH_cov,"GJR-GARCH"=GARCH_plain,"QRF-X"=QRF,"QR-X"=QR)%>% #FOR GRF-X
      rename_with(.fn=function(x){gsub("_base","",x)}) 
    rownames(p_val) <- names(Period_Data[[i]])[ind_row_t]
  }
  
  if(test_lhs=="GRF_base"){test_plot<-"GRF vs Rest, "}else{if(test_lhs=="GRF"){test_plot<-"GRF-X vs Rest, "}else{test_plot<-test_lhs}}
  if(adjust){
    p_val[perf_rate<0.5]<-1
  }
  if(i==1)
  {
    pdf(paste0("Plots/CPA/P_val_",test_lhs,"_Period_",tp,"_",var_to_order,".pdf"),width=10,height=5)
    corrplot(as.matrix(perf_rate),method="color",is.corr=FALSE,sig.level=c(0.01,0.05,0.1),col=col_perf,
             p.mat = as.matrix(p_val),title = paste0(test_plot," Period ",i,":"," sorted by ",var_to_order),
             col.lim = c(0,1),cl.pos="r",insig="label_sig",pch.col = 'grey20',
             pch.cex = size_stars,tl.srt=45,tl.offset = 1,addgrid.col = "grey",tl.col = "black",
             mar=c(0,0,2,0),tl.cex = size_labels)
    dev.off()
    
  }
  if(i==2)
  {
    pdf(paste0("Plots/CPA/P_val_",test_lhs,"_Period_",tp,"_",var_to_order,".pdf"),width=10,height=5)
    corrplot(t(perf_rate),method="color",is.corr=FALSE,sig.level=c(0.01,0.05,0.1),col=col_perf,
             p.mat = t(p_val),title = paste0(test_plot," Period ",i,":"," sorted by ",var_to_order),
             col.lim = c(0,1),cl.pos="r",insig="label_sig",pch.col = 'grey20',
             pch.cex = size_stars,tl.srt=45,tl.offset = 1,addgrid.col = "grey",tl.col = "black",
             mar=c(0,0,2,0),tl.cex = size_labels)
    dev.off()
    
  }
  
  if(i==3)
  {
    list_seq<-split_fun(1:nrow(perf_rate),n=3)
    for(j in 1:length(list_seq))
    {
      pdf(paste0("Plots/CPA/P_val_",test_lhs,"_Period_",tp,".",j,"_",var_to_order,".pdf"),width=10,height=5)
      corrplot(t(perf_rate[list_seq[[j]],]),method="color",is.corr=FALSE,sig.level=c(0.01,0.05,0.1),col=col_perf,
               p.mat = t(p_val[list_seq[[j]],]),title = paste0(test_plot," Period " ,i,": ",j," of ",length(list_seq),": sorted by ",var_to_order),
               col.lim = c(0,1),cl.pos="b",insig="label_sig",pch.col = 'grey20',
               pch.cex = size_stars,tl.srt=45,tl.offset = 1,addgrid.col = "grey",tl.col = "black",
               mar=c(0,0,2,0),tl.cex = size_labels)
      dev.off()
    }
    
  }
  
  if(i==4)
  {
    list_seq<-split_fun(1:nrow(perf_rate),n=3)
    for(j in 1:length(list_seq))
    {
      pdf(paste0("Plots/CPA/P_val_",test_lhs,"_Period_",tp,".",j,"_",var_to_order,".pdf"),width=10,height=5)
      corrplot(t(perf_rate[list_seq[[j]],]),method="color",is.corr=FALSE,sig.level=c(0.01,0.05,0.1),col=col_perf,
               p.mat = t(p_val[list_seq[[j]],]),title = paste0(test_plot," Period " ,i,": ",j," of ",length(list_seq),": sorted by ",var_to_order),
               col.lim = c(0,1),cl.pos="b",insig="label_sig",pch.col = 'grey20',
               pch.cex = size_stars,tl.srt=45,tl.offset = 1,addgrid.col = "grey",tl.col = "black",
               mar=c(0,0,2,0),tl.cex = size_labels)
      dev.off()
    }
    
  }
}




#################################
## Differences between Covariates where GRF is better vs. worse
## Table 9
## Table 11
## Table B.19
#################################

#GRF_base=GRF or GRF=GRF-X 

ifelse(test_lhs=="GRF",vars_to_compare<-c(3,4,9),vars_to_compare<-c(4,5,9)) # 4=CAV, 9=QR_base, 5=GJR, 3=QR
ind_all<-sapply(list_perf_summary,fun_get_crypto_per,num_better=1,cryptos_better=vars_to_compare)

crypto_inv_names<-names(sort(table(unlist(ind_all)),decreasing=TRUE))

crypto_sum_grf_worse<-fun_sum_crypto(crypto_inv_names)
crypto_sum_rest<-fun_sum_crypto(names(list_rets)[!(names(list_rets)%in%crypto_inv_names)])  


crypto_diff<-round(sapply(crypto_sum_grf_worse,function(x) {x[2,]})/sapply(crypto_sum_rest,function(x) {x[2,]}),3)
colnames(crypto_diff)<-c(paste0("Period ",1:4),"Full Data")
rownames(crypto_diff)<-colnames(list_rets$btc)[c(2,4:14)]#[match(table_fin$Coding_Coinmetrics,rownames(crypto_diff))]<-table_fin$Variable_Name
caption_c<-"Difference Between Covariates of Cryptos Where GRF is Better vs. Worse"
sum_comp_xtab<-xtable(crypto_diff,label = "tab:crypto_dif_rel",caption=caption_c,digits=2)#display = c("s",rep("d",7)))
print.xtable(sum_comp_xtab,type="latex",#file="Plots/Crypto_dq_1.tex",
             floating=TRUE,include.rownames=TRUE,caption.placement = "top",
             comment=TRUE,booktabs = TRUE)


# Der Wert für Ret in Periode 3 ist so negativ da der Median Return von 
#den Cryptos wo grf schlechter ist -0.0041 ist und der Median Return von
#den Cryptos wo grf besser ist 0,0000026. D.h. der Quotient aus beiden
#ist sehr stark negativ und es ist kein Fehler


#################################
#Variable Importance
#################################

# QR

list_var_imp_qr <- vector("list", length(final_data_combined))
for (i in seq_along(final_data_combined)){
  var_imp_qr <- final_data_combined[[i]][[14]]
  list_var_imp_qr[[i]] <- var_imp_qr
}
names(list_var_imp_qr) <- names(final_data_combined)

df_qr_imp_sum<-list()
for(i in 1:length(list_var_imp_qr))
{
  if(i%%10==0)
  {
    print(i)
  }
  df_qr_imp_sum[[i]]<-get_sum_df(list_var_imp_qr[[i]],ind_t=i)
}
names(df_qr_imp_sum)<-names(final_data_combined)

# GRF

list_var_imp_grf <- vector("list", length(final_data_combined))
for (i in seq_along(final_data_combined)){
  var_imp_grf <- do.call(rbind, lapply(final_data_combined[[i]][[11]], t))
  rownames(var_imp_grf)<-names(final_data_combined[[i]][[11]])
  list_var_imp_grf[[i]] <- var_imp_grf
}
names(list_var_imp_grf) <- names(final_data_combined)



library(zoo)
library(dplyr)
library(ggplot2)
library(reshape2)
library(cowplot)
library(tidyr)

which(names(final_data_combined)=="btc")
num_asset<-16 #13,30, 100, btc, eth, xrp
num_assets<-which(names(final_data_combined) %in% c("ada","btc","eth"))
name_assets<-names(final_data_combined)[num_assets]
assets_all<-sapply(num_assets,function(x) order(colMeans(list_var_imp_grf[[x]]),decreasing=TRUE)[1:5])
#manual sorting
assets_ind<-list(c(1,9,10,11,2),c(1,9,10,11,12),c(1,9,10,11,12),c(1,9,10,11,12,2))
set.seed(123)
rainbow_cols<-RColorBrewer::brewer.pal(6,"Dark2")
cols_take<-list(rainbow_cols[c(1:4,6)],rainbow_cols[1:5],rainbow_cols[1:5],rainbow_cols[1:6])
#cnt_i<-1
grid_app<-list()
for(i in 1:(length(num_assets)))
{
  ind_k<-num_assets[i]
  name_asset<-names(final_data_combined)[ind_k]
  imp_grf_wide<-list_var_imp_grf[[ind_k]]
  colnames(imp_grf_wide) <- colnames(list_rets[[ind_k]])[3:length(colnames(list_rets[[ind_k]]))]
  imp_order<-sort(order(colMeans(imp_grf_wide),decreasing=TRUE)[1:5])
  roll_means<-zoo::rollapply(imp_grf_wide,width=30,FUN=mean,partial=TRUE,by.column=TRUE,align="right")[,assets_ind[[i]]]
  max_imp<-max(roll_means)
  df_roll<-data.frame(Date=as.Date(df_qr_imp_sum[[ind_k]]$dates),roll_means)
  df_plot<-pivot_longer(df_roll,cols=2:ncol(df_roll),names_to="variable",values_to = "value")
  df_plot$variable<-factor(df_plot$variable,levels=unique(df_plot$variable))
  # plot(df_qr_imp_sum[[num_asset]]$dates,
  #      rollapply(list_Var_imp_grf[[num_asset]][,1],width=30,FUN=mean,partial=TRUE),
  #      type="n",ylim=c(0,1),n)
  
  grid_app[[i]]<-(ggplot(df_plot, aes(x=Date,y=value,group = variable,color=variable)) + 
                    geom_line(aes(linetype=variable),size=2) +
                    scale_color_manual(values=cols_take[[i]])+# geom_line() +
                    # ggtitle(paste0("GRF Variable Importance - ",crypto_names[i])) +
                    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +  # Nur Jahreszahlen anzeigen
                    ylim(0,max_imp) + 
                    xlab("Date") + 
                    ylab("Variable Importance") +
                    ggtitle(name_asset)+
                    theme(text = element_text(size = 35),axis.text=element_text(size=28),legend.position = "n",
                          #egend.key.size = unit(1.7, "cm"),legend.key.width = unit(4.0,"cm"),
                          legend.title = element_blank(),) #+
                  # guides(color = guide_legend(override.aes = list(size = 3)))
  )
  #make legend
  if(i==length(num_assets))
  {
    ind_k<-num_assets[i]
    name_asset<-names(final_data_combined)[ind_k]
    imp_grf_wide<-list_var_imp_grf[[ind_k]]
    colnames(imp_grf_wide) <- colnames(list_rets[[ind_k]])[3:length(colnames(list_rets[[ind_k]]))]
    names_legend<-colnames(imp_grf_wide)[assets_ind[[i+1]]]
    #imp_order<-sort(order(colMeans(imp_grf_wide),decreasing=TRUE)[1:5])
    roll_means<-zoo::rollapply(imp_grf_wide,width=30,FUN=mean,partial=TRUE,by.column=TRUE,align="right")[,assets_ind[[i+1]]]
    max_imp<-max(roll_means)
    df_roll<-data.frame(Date=as.Date(df_qr_imp_sum[[ind_k]]$dates),roll_means)
    df_plot<-pivot_longer(df_roll,cols=2:ncol(df_roll),names_to="variable",values_to = "value")
    df_plot$variable<-factor(df_plot$variable,levels=unique(df_plot$variable))
    
    grid_app[[i+1]]<-(ggplot(df_plot, aes(x=Date,y=value,group = variable,color=variable)) +
                        scale_color_manual(values=cols_take[[i+1]])+
                        geom_line(aes(linetype=variable),size=2) +# geom_line() +
                        # ggtitle(paste0("GRF Variable Importance - ",crypto_names[i])) + 
                        scale_x_date(date_breaks = "2 years", date_labels = "%Y") +  # Nur Jahreszahlen anzeigen
                        ylim(0,max_imp) + 
                        xlab("Date") + 
                        ylab("Variable Importance") +
                        ggtitle(name_asset)+
                        theme(text = element_text(size = 35),axis.text=element_text(size=28),legend.position = "bottom",
                              legend.key.size = unit(1.7, "cm"),legend.key.width = unit(4.0,"cm"),
                              legend.title = element_blank()) +
                        guides(col = guide_legend(nrow=1))
    )
  }
  
}

legend_plot<-get_legend(grid_app[[4]])
top_part<-plot_grid(grid_app[[1]],grid_app[[2]],nrow=1)
bottom_part<-plot_grid(NULL,grid_app[[3]],NULL,rel_widths = c(1,2,1),nrow=1)
legend_part<-plot_grid(legend_plot)
#plot_grid(top_part,bottom_part,legend_part,ncol=1,align="vh",axis="tb",lables="AUTO")
plot_grid(top_part,NULL,bottom_part,NULL,legend_part,NULL,ncol=1,rel_heights = c(1,0.1,1,-0.3,1,-1.3),align="vh",axis="tb",lables="AUTO")
ggsave(paste0("Plots/GRF_var_imp_",paste0(name_assets,collapse="_"),".pdf"),width=24,height = 18,units = "in")

