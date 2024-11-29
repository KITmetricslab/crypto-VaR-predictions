###########################################
## CPA tests full and sorted by Market Cap
## Code for Figures B.8, B.9, B.10
##################################


load("Cleaned_Data/Tests/Crypto_ind_periods_4.RData")
load("Cleaned_Data/Forecasts/final_data_combined_0.05_500_2024-11-20.RData")
load("Cleaned_Data/Forecasts/Crypto_Data_add.RData")
load("Cleaned_Data/Forecasts/Crypto_Data_add_plusMktCap.RData")
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




##########################
## CPA Tests for different time periods

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



#################################
## CPA Plots over time
## Figure B.8
## Figure B.9
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
colnames(list_rets_MC$btc)
#col_to_sort<-3
var_to_order<-"CapMrktCurMUSD" # select sd_30 or CapMrktCurMUSD 

# eos and kcs do not have Market Cap
list_rets_MC_noNA<-list_rets_MC[!(names(list_rets_MC) %in% c("eos","kcs"))]
ind_rows<-fun_order_one_var(list_rets_MC_noNA,Var_to_order =var_to_order,summary_fun=mean)

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


###########################################
## CPA-tests of all cryptos for full data
## Figure B.10

col_brew<-RColorBrewer::brewer.pal(4,"Blues")[4:1]

color.palette <- function(steps, n.steps.between=NULL, ...){
  
  if(is.null(n.steps.between)) n.steps.between <- rep(0, (length(steps)-1))
  if(length(n.steps.between) != length(steps)-1) stop("Must have one less n.steps.between value than steps")
  
  fill.steps <- cumsum(rep(1, length(steps))+c(0,n.steps.between))
  RGB <- matrix(NA, nrow=3, ncol=fill.steps[length(fill.steps)])
  RGB[,fill.steps] <- col2rgb(steps)
  
  for(i in which(n.steps.between>0)){
    col.start=RGB[,fill.steps[i]]
    col.end=RGB[,fill.steps[i+1]]
    for(j in seq(3)){
      vals <- seq(col.start[j], col.end[j], length.out=n.steps.between[i]+2)[2:(2+n.steps.between[i]-1)]  
      RGB[j,(fill.steps[i]+1):(fill.steps[i+1]-1)] <- vals
    }
  }
  
  new.steps <- rgb(RGB[1,], RGB[2,], RGB[3,], maxColorValue = 255)
  pal <- colorRampPalette(new.steps, ...)
  return(pal)
}

pal<-color.palette(col_brew,n.steps.between = c(10,2,1),space="rgb")


col_perf2<- pal(200)
col_to_sort<-2

if(test_lhs=="GRF")
{
  perf_rate<-mat_perf[,] %>% rem_nan_na(replace_el = 0.5) %>% as.data.frame() %>%
    select(GRF_base,QRF_base,QR_base,CAV,CAV_ASY,GARCH_plain,Hist,QRF,QR,GARCH_cov) %>% #FOR GRF-X
    rename("GARCH-X"=GARCH_cov,"GJR-GARCH"=GARCH_plain,"QRF-X"=QRF,"QR-X"=QR)%>% #FOR GRF-X
    rename_with(.fn=function(x){gsub("_base","",x)})
  p_val<-mat_p_val[,] %>% rem_nan_na(replace_el = 1) %>% as.data.frame() %>%
    select(GRF_base,QRF_base,QR_base,CAV,CAV_ASY,GARCH_plain,Hist,QRF,QR,GARCH_cov) %>% #FOR GRF-X
    rename("GARCH-X"=GARCH_cov,"GJR-GARCH"=GARCH_plain,"QRF-X"=QRF,"QR-X"=QR)%>% #FOR GRF-X
    rename_with(.fn=function(x){gsub("_base","",x)})
  
}
if(test_lhs=="GRF_base")
{
  perf_rate<-mat_perf[,] %>% as.data.frame() %>%
    select(QRF_base,QR_base,CAV,CAV_ASY,GARCH_plain,Hist,GRF,QRF,QR,GARCH_cov) %>% #FOR GRF_BASE
    rename("GARCH-X"=GARCH_cov,"GJR-GARCH"=GARCH_plain,"GRF-X"=GRF,"QRF-X"=QRF,"QR-X"=QR)%>% #FOR GRF_BASE
    rename_with(.fn=function(x){gsub("_base","",x)})
  p_val<-mat_p_val[,] %>% as.data.frame() %>%
    select(QRF_base,QR_base,CAV,CAV_ASY,GARCH_plain,Hist,GRF,QRF,QR,GARCH_cov) %>% #FOR GRF_BASE
    rename("GARCH-X"=GARCH_cov,"GJR-GARCH"=GARCH_plain,"GRF-X"=GRF,"QRF-X"=QRF,"QR-X"=QR)%>% #FOR GRF_BASE
    rename_with(.fn=function(x){gsub("_base","",x)})
  
}


list_seq<-split_fun(1:nrow(perf_rate),n=3)
if(test_lhs=="GRF_base"){test_plot<-"GRF vs Rest, "}else{if(test_lhs=="GRF"){test_plot<-"GRF-X vs Rest, "}else{test_plot<-test_lhs}}

for(j in 1:length(list_seq))
{
  
  pdf(paste0("Plots/CPA/P_val_full_",test_lhs,"_",j,".pdf"),width=10,height=5)
  corrplot(t(perf_rate[list_seq[[j]],]),method="color",is.corr=FALSE,sig.level=c(0.01,0.05,0.1),col=col_perf,
           p.mat = t(p_val[list_seq[[j]],]),title = paste0(test_plot," Full Data: " ,j," of ",length(list_seq)),#,": sorted by ",var_to_order),
           col.lim = c(0,1),cl.pos="b",insig="label_sig",pch.col = 'grey20',
           pch.cex = 0.95,tl.srt=45,tl.offset = 1,addgrid.col = "grey",tl.col = "black",
           mar=c(0,0,2,0))
  dev.off()
}
