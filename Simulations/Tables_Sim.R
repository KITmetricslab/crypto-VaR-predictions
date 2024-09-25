

library(xtable)

source("../Tests/Evaluation_Functions.R")

######################
#Simulation Table new
######################

#load data

Backtest_all<-vector("list",4)
true_y<-vector("list",4)
losses<-vector("list",4)
names(Backtest_all)<-names(true_y)<-names(losses)<-c("GARCH_norm","GARCH_btc_asym_t","SAV","GARCH_VaryVola")
load("../../Cleaned_Data/Simulations/history_length_500/Sim_0.05_GARCH_norm.RData")
Backtest_all[[1]]<-list(Backtest)
true_y[[1]]<-list(otherData)
losses[[1]]<-list(results)
load("../../Cleaned_Data/Simulations/history_length_500/Sim_0.05_GARCH_btc_asym_t.RData")
Backtest_all[[2]]<-list(Backtest)
true_y[[2]]<-list(otherData)
losses[[2]]<-list(results)
load("../../Cleaned_Data/Simulations/history_length_500/Sim_0.05_SAV.RData")
Backtest_all[[3]]<-list(Backtest)
true_y[[3]]<-list(otherData)
losses[[3]]<-list(results)
load("../../Cleaned_Data/Simulations/history_length_500/Sim_0.05_500_GARCH_norm_Niklas_random_periods.RData")
Backtest_all[[4]]<-list(Backtest[[1]][1:63])
true_y[[4]]<-list(otherData[[1]][1:63])
losses[[4]]<-list(results[[1]][1:63])



load("../../Cleaned_Data/Simulations/history_length_1000/Sim_0.05_1000_GARCH_norm.RData")
Backtest_all[[1]][[2]]<-Backtest[[1]]
true_y[[1]][[2]]<-otherData[[1]]
losses[[1]][[2]]<-results[[1]]
load("../../Cleaned_Data/Simulations/history_length_1000/Sim_0.05_1000_GARCH_btc_asym_t.RData")
Backtest_all[[2]][[2]]<-Backtest[[1]]
true_y[[2]][[2]]<-otherData[[1]]
losses[[2]][[2]]<-results[[1]]
load("../../Cleaned_Data/Simulations/history_length_1000/Sim_0.05_1000_SAV.RData")
Backtest_all[[3]][[2]]<-Backtest[[1]]
true_y[[3]][[2]]<-otherData[[1]]
losses[[3]][[2]]<-results[[1]]
load("../../Cleaned_Data/Simulations/history_length_1000/Sim_0.05_1000_GARCH_norm_Niklas.RData")
Backtest_all[[4]][[2]]<-Backtest[[1]]
true_y[[4]][[2]]<-otherData[[1]]
losses[[4]][[2]]<-results[[1]]

for(j in 1:length(Backtest_all)){
  names(Backtest_all[[j]])<-c(500,1000)
  names(losses[[j]])<-c(500,1000)
  names(true_y[[j]])<-c(500,1000)
}
tau<-0.05 # for Rejection Rates
##
#BACKTESTS

#function to extract only Backtest values from a list from simulation
fun_bt_extr<-function(list_bt)
{
  fin_vec<-rep(NA,4)
  names(fin_vec)<-c("DQ","Kupiec","Christoffersen","AoE")
  fin_vec[1]<-list_bt$DQ$pvalue
  fin_vec[2]<-list_bt$LRuc[2]
  fin_vec[3]<-list_bt$LRcc[2]
  fin_vec[4]<-list_bt$AE
  return(fin_vec)
}

#Make one result table for DQ test
num_proc<-length(Backtest_all[[2]][[1]][[1]])
names(Backtest_all[[2]][[1]][[1]])
n_iterations <- list(sapply(true_y[1:6], function(x) length(x[[1]])),
                     sapply(true_y[1:6], function(x) length(x[[2]])))

Final_pval<-Final_rejRate<-vector(mode="list",4)
Final_GW<-vector(mode="list",4)
Final_GW_perf<-vector(mode="list",4)
names(Final_pval)<-names(Final_rejRate)<-names(Final_GW)<-names(Final_GW_perf)<-c("GARCH_norm","GARCH_btc_asym_t","SAV","GARCH_VaryVola") 

history_lengths<-c(500, 1000) #,1000

#GW Test stuff
sign_star<-0.05 #from which p-value on should + or - be printed
numbers=TRUE #do you want numbers or latex table
test_lhs<-"GRF_base" #which one do you want to test against; GRF_base or GRF
test_rhs<-c("QR","Hist","CAV_SAV") #"QR",

for(i in 1:4)#length(Backtest_all)) #all different simulation setups
{
  Backtest<-Backtest_all[[i]]
  Backtest_mean <- vector("list", length(history_lengths))
  GW_all<-vector("list", length(history_lengths))
  GW_all_perf<-vector("list", length(history_lengths))
  #Backtest_var <- vector("list", length(history_lengths))
  Backtest_rejRate <- vector("list", length(history_lengths))
  
  for(u in 1:length(history_lengths)){
    
    Backtest_mean[[u]] <- matrix(NA,nrow=num_proc,ncol=4)
    #Backtest_var[[u]]  <- matrix(NA,nrow=num_proc,ncol=4)
    Backtest_rejRate[[u]]  <- matrix(NA,nrow=num_proc,ncol=3)
    GW_all[[u]]<-matrix(NA,ncol=3,nrow=n_iterations[[u]][i])
    GW_all_perf[[u]]<-matrix(NA,ncol=3,nrow=n_iterations[[u]][i])
    
    rownames(Backtest_mean[[u]])<-rownames(Backtest_rejRate[[u]])<-names(Backtest[[1]][[1]])
    colnames(GW_all[[u]])<-colnames(GW_all_perf[[u]])<-c("QR","Hist","CAV_SAV")
    colnames(Backtest_mean[[u]])<-c("DQ","Kupiec","Christoffersen","AoE")
    colnames(Backtest_rejRate[[u]])<-c("DQ","Kupiec","Christoffersen")
    
    for(j in 1:num_proc){
      temp_bt<-sapply(1:n_iterations[[u]][i],function(x)fun_bt_extr(Backtest[[u]][[x]][[j]])) #get list only with backtest values over all MC samples
      temp_loss<-sapply(1:n_iterations[[u]][i],function(x) Backtest[[u]][[x]][[j]]$Loss$LossSeries)
      Backtest_mean[[u]][j,]<-rowMeans(temp_bt)
      Backtest_rejRate[[u]][j,]<-apply(temp_bt[-4,],1,function(x) sum(x<tau) /length(x))
    }
    for(j in 1:n_iterations[[u]][i])
    {
      #do GW test here
      true_y_temp<-true_y[[i]][[u]][[j]]
      #change to 0.01 if you want 1% VaR
      loss_grf<-get_loss(loss=tick,true_val=true_y_temp,forecast=rem_nan_na(losses[[i]][[u]][[j]][[2]]), tau=0.05) 
      
      loss_qr <- get_loss(loss=tick,true_val=true_y_temp,forecast=rem_nan_na(losses[[i]][[u]][[j]][[3]]), tau=0.05) 
      loss_hist <- get_loss(loss=tick,true_val=true_y_temp,forecast=rem_nan_na(losses[[i]][[u]][[j]][[4]]), tau=0.05)
      loss_cav <- get_loss(loss=tick,true_val=true_y_temp,forecast=rem_nan_na(losses[[i]][[u]][[j]][[6]]), tau=0.05)
      
      cpa_qr<-CPAtest(loss1 = loss_qr,loss2 = loss_grf,tau=1,alpha=0.05,choice=2,sign_level=sign_star,print_it=FALSE,ret_table=!numbers,ret_numbers=numbers) #change significance level for +/- signs
      cpa_hist<-CPAtest(loss1 = loss_hist,loss2 = loss_grf,tau=1,alpha=0.05,choice=2,sign_level=sign_star,print_it=FALSE,ret_table=!numbers,ret_numbers=numbers)
      cpa_cav<-CPAtest(loss1 = loss_cav,loss2 = loss_grf,tau=1,alpha=0.05,choice=2,sign_level=sign_star,print_it=FALSE,ret_table=!numbers,ret_numbers=numbers)
      
      GW_all[[u]][j,1]<-cpa_qr$pval
      GW_all[[u]][j,2]<-cpa_hist$pval
      GW_all[[u]][j,3]<-cpa_cav$pval
      
      GW_all_perf[[u]][j,1]<-cpa_qr$rate
      GW_all_perf[[u]][j,2]<-cpa_hist$rate
      GW_all_perf[[u]][j,3]<-cpa_cav$rate
    }
    print(paste0("Finished with: ",i," of 4"))
  }
  Final_pval[[i]]<-Backtest_mean
  Final_rejRate[[i]]<-Backtest_rejRate
  
  Final_GW[[i]]<-GW_all
  Final_GW_perf[[i]]<-GW_all_perf
}

#create Backtest Table with parentheses
Backtest_mat<-matrix(NA,nrow=num_proc*4,ncol=4*2)
rowcount<-1
for(proc in 1:4)
{
  for(i in 1:num_proc)
  {
    for(j in 1:3)
    {
      Backtest_mat[rowcount,j]<-paste0(format(round(Final_rejRate[[proc]][[1]][i,j], digits=3), nsmall = 3)," $( ",format(round(Final_pval[[proc]][[1]][i,j], digits=3), nsmall = 3)," )$")
    }
    Backtest_mat[rowcount,4]<-paste0(format(round(Final_pval[[proc]][[1]][i,4], digits=3), nsmall = 3))
    
    for(j in 5:7)
    {
      Backtest_mat[rowcount,j]<-paste0(format(round(Final_rejRate[[proc]][[2]][i,j-4], digits=3), nsmall = 3)," $( ",format(round(Final_pval[[proc]][[2]][i,j-4], digits=3), nsmall = 3)," )$")
    }
    Backtest_mat[rowcount,8]<-paste0(format(round(Final_pval[[proc]][[2]][i,4], digits=3), nsmall = 3))
    rowcount<-rowcount+1
    print(rowcount)
  }
  
}
names_proc<-rep(c("QRF","GRF","QR","Hist","NormFit","CAV","CAV\\_ASY","GARCH(1,1)","GJR-GARCH"),6)
Backtest_print<-data.frame(Names=names_proc,Backtest_mat)
Backtest_print<-rbind(c("NAMES",rep(c("DQ","Kupiec","Christoffersen","AoE"),2)),Backtest_print)
print(xtable(Backtest_print),sanitize.text.function = identity,include.rownames = FALSE,include.colnames = FALSE)



##
#GW-Test Table
final_tab<-matrix(NA,ncol=6,nrow=18)
for(i in 1:6)
{
  for(j in 1:2)#2
  {
    final_pvals<-apply(Final_GW[[i]][[j]],2,mean,na.rm=TRUE)
    final_perf<-apply(Final_GW_perf[[i]][[j]],2,mean,na.rm=TRUE)
    no_sig<-apply(Final_GW[[i]][[j]],2,function(x) sum(rem_nan_na(x)<0.1))
    final_tab[(i-1)*3+1,((j-1)*3+1):((j-1)*3+3)]<-final_pvals
    final_tab[(i-1)*3+2,((j-1)*3+1):((j-1)*3+3)]<-no_sig
    final_tab[(i-1)*3+3,((j-1)*3+1):((j-1)*3+3)]<-final_perf
  }
  
}
final_tab
names_rows<-rep(c("Mean P-Value", "No. P-values < 0.1","GRF-Performance"),6)
names_col<-rep(c("QR","Hist","CAV\\_SAV"),2)
final_print<-data.frame(Names=names_rows,round(final_tab,3))
final_print<-rbind(c("GRF vs.",names_col),final_print)
print(xtable(final_print,digits=3),sanitize.text.function = identity,include.rownames = FALSE,include.colnames = FALSE)
