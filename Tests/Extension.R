load("Cleaned_Data/Forecasts/final_data_combined_0.05_500_2024-11-20.RData")
load("Cleaned_Data/Forecasts/Crypto_Data_add.RData")
source("Tests/Evaluation_Functions.R")

#################
#Do the same CPA-tests but only for selected cryptos and return loss series and returns

test_lhs<-"GRF_base" #GRF or GRF_base
load(paste0("Cleaned_Data/Tests/CPA_tests_full_",test_lhs,"_vs_rest.RData"))
name_cryptos<-sort(c("ada","btc","usdt_omni"))

## truncate data after 2022-03-21 for plots in Extension 5.2.1
fun_trunc2022<-function(x){
  return(x[1:which(names(x)=="2022-03-21")])
}
fun_truncbtc<-function(x){
  return(x[which(names(x)=="2012-09-01"):which(names(x)=="2022-03-21")])
}
trunc_ada<-lapply(final_data_combined$ada, fun_trunc2022)
trunc_btc<-lapply(final_data_combined$btc, fun_truncbtc)
trunc_usdt_omni<-lapply(final_data_combined$usdt_omni, fun_trunc2022)
final_data_trunc<-list(trunc_ada,trunc_btc,trunc_usdt_omni)
names(final_data_trunc)<-name_cryptos

##################
list_pred_losses<-list() #take QRF, QR, GJR_GARCH, GARCH-X, CAVIAR, Hist and 3x base
count_outer<-1
for(ind_k in seq_along(final_data_trunc))
{
  temp_pred<-final_data_trunc[[ind_k]]
  true_y<-unlist(temp_pred$True_Ret)
  temp_dates<-names(temp_pred$True_Ret)
  date_pred<-temp_dates[-length(temp_dates)]
  loss_lhs<-get_loss(loss=tick,true_val=true_y,forecast=unlist(temp_pred[[test_lhs]]), tau=0.05)
  ind_mat<-which(names(temp_pred) %in% test_rhs)
  temp_mat<-matrix(ncol=length(ind_mat)+2,nrow=length(true_y)-1,NA)
  colnames(temp_mat)<-c(names(temp_pred)[ind_mat],"Date","ret")
  temp_mat<-as.data.frame(temp_mat)
  temp_mat$Date<-as.Date(date_pred)
  temp_mat$ret<-true_y[-length(true_y)]
  counter<-1
  for(j in ind_mat) #gives back indices
  {
    loss1 <- get_loss(loss=tick,true_val=true_y,forecast=rem_nan_na(unlist(temp_pred[[j]]),10000), tau=0.05) #change to 0.01 if you want 1% VaR
    cpa_results<-CPAtest(loss1 = loss1,loss2 = loss_lhs,tau=1,alpha=0.05,choice=2,ret_rate=TRUE,print_it = FALSE) #this gives rates
    temp_mat[,counter]<-cpa_results$Full_rate
    counter<-counter+1
  }
  list_pred_losses[[count_outer]]<-temp_mat
  print(paste0(" Finished with Asset: ",names(final_data_trunc)[ind_k],", ",count_outer," of ",length(final_data_trunc) ))
  count_outer<-count_outer+1
}
names(list_pred_losses)<-name_cryptos
cpa_file_name_loss<-paste0("Cleaned_Data/Tests/CPA_tests_loss_series_",paste0(name_cryptos,collapse="_"),".RData")
save(list_pred_losses,test_lhs,test_rhs,file=cpa_file_name_loss)



###############################################
#Plot predicted loss series for certain methods
###############################################

########################
## PLOT TOGETHER
## Figure 4

#name_cryptos<-c("ada","btc","usdt_omni")
load(paste0("Cleaned_Data/Tests/CPA_tests_loss_series_",paste0(name_cryptos,collapse="_"),".RData"))
window_size_moving<-c(30,180,30)#c(180,60,30)
rem_rhs<-c("GARCH_cov","QRF","QRF_base","QR","Hist","GRF","CAV_ASY") ###
loss_scale=100
ret_width=10
#loads list_pred_losses

# Prepare data for plot
rainbow_cols<-RColorBrewer::brewer.pal(6,"Dark2")
cols_together<-rainbow_cols[1:6]

scaling_factor<-c(100,100,100)
#each crypto gets one plot
#make sure scales are the same

#
i<-1 # 1 for ada or 3 for usdt_omni
#
scale_plots<-sapply(list_pred_losses,function(x) range(x[,!(colnames(x) %in% c(rem_rhs,"ret","Date"))]))
#for(i in 1:length(list_pred_losses))
#{
  scale=scaling_factor[i]
  #make temp_df
  if(test_lhs=="GRF_base") {name_lhs<-"GRF"} else {name_lhs<- "GRF-x"}
  temp_df<-list_pred_losses[[i]]
  Ret<-temp_df$ret
  range_ret<-range(Ret)
  temp_df<-temp_df[,!(colnames(temp_df) %in% c(rem_rhs,"ret"))]
  
  moving_avg<-zoo::rollapply(temp_df[,-ncol(temp_df)],width=window_size_moving[i],
                             FUN=mean,by.column=TRUE,fill=NA,partial=TRUE,align="right")%>% 
    data.frame() %>% rename("GJR-GARCH"=GARCH_plain)%>%
    rename_with(.fn=function(x){gsub("_base","",x)})
  moving_log_ret<-zoo::rollapply(Ret,width=ret_width,FUN=mean,fill=NA,partial=TRUE,align="right")
  range_all<-range(cbind(moving_avg,moving_log_ret/scale))
  scale_fin<-(range(range_all)+0.01*range(range_all))*loss_scale
  for(k in 1) # change to j in 1:ncol(moving_avg) for single plots
  {
    j=list(1:ncol(moving_avg))
    cols_take<- cols_together
    name_var<-colnames(moving_avg)[j[[1]]]
    wide_df<-data.frame(Date=temp_df$Date,"Log-Return"=moving_log_ret/scale*loss_scale,name_method=moving_avg[,j[[1]]]*loss_scale)
    which_wide<-which(grepl("name_method",colnames(wide_df)))
    names(wide_df)[grepl("name_method",colnames(wide_df))] <- name_var
    long_df<-reshape2::melt(wide_df,id.vars="Date",variable.factor=TRUE,variable.name="Method")
    
    print(ggplot(long_df,aes(x=Date,y=value,group=Method,color=Method)) + 
            geom_line(aes(linetype=Method)) +
            scale_color_manual(values = c("grey",cols_take[1:length(j[[1]])])) + 
            scale_linetype_manual(values=c("twodash",rep("solid",length(j[[1]])))) +
            scale_y_continuous(
              name = paste0("Predicted Quantile-Loss Difference x",loss_scale),
              sec.axis = sec_axis(~.*scale/loss_scale, name=paste0(ret_width,"-day Mean Log-Return")),
              limits=scale_fin,
              labels = function(x) format(x, big.mark = ",",scientific=F)
            ) + geom_hline(yintercept = 0,linetype=2)+
            ggtitle(names(list_pred_losses)[[i]])+
            # ggtitle(paste0(names(list_pred_losses)[[i]],": ",paste0(colnames(wide_df)[which_wide],collapse="/"),
            #                " - ",name_lhs)) + 
            xlab("Date") + 
            theme(legend.position = "n",text = element_text(size = 20))) #right for usdt_omni and n for ada
    #ggsave(paste0("Loss_difference_",names(list_pred_losses)[[i]],
    #              "_GRF_",paste0(colnames(wide_df)[which_wide],collapse="_"),"_no_legend.pdf"),
    #      path ="Plots/CPA",width=15,height=10,units="in",scale=0.7 )
     ggsave(paste0("Loss_difference_",names(list_pred_losses)[[i]],
                   "_GRF_",paste0(colnames(wide_df)[which_wide],collapse="_"),".pdf"),
            path ="Plots/CPA",width=15,height=10,units="in",scale=0.7 )
  }
  
#}

###############################
## PLOT BITCOIN SEPERATELY
## Figure 3

scale=scaling_factor[1]
loss_scale=100
ret_width=10
#make temp_df
if(test_lhs=="GRF_base") {name_lhs<-"GRF"} else {name_lhs<- "GRF-x"}
temp_df<-list_pred_losses$btc
Ret<-temp_df$ret
range_ret<-range(Ret)
temp_df<-temp_df[,!(colnames(temp_df) %in% c(rem_rhs,"ret"))]

moving_avg<-zoo::rollapply(temp_df[,-ncol(temp_df)],width=window_size_moving[2],
                           FUN=mean,by.column=TRUE,fill=NA,partial=TRUE,align="right")%>%  # partial FALSE
  data.frame() %>% rename("GJR-GARCH"=GARCH_plain)%>%
  rename_with(.fn=function(x){gsub("_base","",x)})
moving_log_ret<-zoo::rollapply(Ret,width=ret_width,FUN=mean,fill=NA,partial=TRUE,align="right") # partial FALSE
range_all<-range(cbind(moving_avg,moving_log_ret/scale))
scale_fin<-(range_all+0.01*range_all)*loss_scale
for(j in 1:ncol(moving_avg)) # change to j in 1:ncol(moving_avg) for single plots
{
  cols_take<-c("red","darkgreen","blue")
  name_var<-colnames(moving_avg)[j[[1]]]
  wide_df<-data.frame(Date=temp_df$Date,"Log-Return"=moving_log_ret/scale*loss_scale,name_method=moving_avg[,j[[1]]]*loss_scale) %>%
    drop_na()
  which_wide<-which(grepl("name_method",colnames(wide_df)))
  names(wide_df)[grepl("name_method",colnames(wide_df))] <- name_var
  long_df<-reshape2::melt(wide_df,id.vars="Date",variable.factor=TRUE,variable.name="Method")
  
  print(ggplot(long_df,aes(x=Date,y=value,group=Method,color=Method)) + 
          geom_line(aes(linetype=Method)) +
          scale_color_manual(values = c("grey",cols_take[1:length(j[[1]])]))+ 
          scale_linetype_manual(values=c("twodash",rep("solid",length(j[[1]])))) +
          scale_y_continuous(
            name = paste0("Predicted Quantile-Loss Difference x ",loss_scale),
            sec.axis = sec_axis(~.*scale/loss_scale, name=paste0(ret_width,"-day Mean Log-Return")),
            limits=scale_fin,
            labels = function(x) format(x, big.mark = ",",scientific=F)
          ) + geom_hline(yintercept = 0,linetype=2)+
          ggtitle(paste0(names(list_pred_losses)[[2]],": ",paste0(colnames(wide_df)[which_wide],collapse="/"),
                         " - ",name_lhs)) + 
          xlab("Date") + 
          theme(legend.position = "n",text = element_text(size = 20)))
  ggsave(paste0("Loss_difference_BTC_",paste0(colnames(wide_df)[which_wide],collapse="_"),".pdf"),
         path ="Plots/CPA",width=15,height=10,units="in",scale=0.7 )
}


##########################################
## Plot log returns for the three currencies
## Figure 11

library(lubridate)
name_cryptos<-c("ada","btc","usdt_omni")
num_assets<-which(names(final_data_combined) %in% name_cryptos)

create_pdf<-TRUE
if(create_pdf) {
  pdf(file="Plots/Log_returns_crypto_new.pdf",width=16,height=12)
  par(mfrow=c(2,2))
}
for(i in 1:length(num_assets))
{
  data_select<-list_rets[[num_assets[i]]]
  plot(as.Date(data_select$time),data_select$Ret,type="l",xaxt="n",ylab="log-return",main=name_cryptos[i],
       xlab="Dates",cex.lab=1.5, cex.axis=1.2,cex.main=1.2)
  dates_plot3<-seq(from=min(as.Date(data_select$time)),
                   to=max(as.Date(data_select$time)),by=ifelse(i<2,"quarter","year"))
  dates_final<-ceiling_date(dates_plot3,unit=ifelse(i<2,"month","year"))
  axis(1,at=dates_final,labels=format(dates_final,"%Y/%m"),cex.axis=1.5,cex.lab=1.2)
  
  print(paste0("Finished with ",name_cryptos[i]))
}
if(create_pdf){dev.off()}


###############################################
# Variable Importance of ada, btc, usdt_omni
### Figure 5

################################
# get list of variable importance:
list_Var_imp_grf<-list()
#list_Var_imp_qr<-list()

for(i in 1:length(final_data_trunc))
{
  list_temp<-final_data_trunc[[i]] #load in results for crypto
  
  #save variable importance with name
  var_imp_temp_grf_list<-list_temp$Var_imp_grf
  var_imp_temp_grf<-t(as.data.frame(do.call(cbind, var_imp_temp_grf_list)))
  #var_imp_temp_qr<-list_temp$Var_imp_qr
  
  #names_selected_covar<-colnames(list_rets[[i]])[colnames(list_rets[[i]]) %in%c(
  #  "Active_Users","Total_Users","Total_Users_USD100",
  #  "Total_Users_USD_10","SER","Transactions","Velocity")]
  names_covar<-c("Lagged_ret","Active_Users","Total_Users","Total_Users_USD100",
                 "Total_Users_USD_10","SER","Transactions","Velocity",paste0("sd_",c(3,7,30,60)))
  colnames(var_imp_temp_grf)<-names_covar
  rownames(var_imp_temp_grf)<-names(var_imp_temp_grf_list)
  list_Var_imp_grf[[i]]<-var_imp_temp_grf
  #list_Var_imp_qr[[i]]<-var_imp_temp_qr
}
names(list_Var_imp_grf)<-names(final_data_trunc)

###################################
library(zoo)
library(dplyr)
library(ggplot2)
library(reshape2)
#install.packages("cowplot")
library(cowplot)
num_assets<-which(names(final_data_trunc) %in% c("ada","btc","usdt_omni"))
name_assets<-names(final_data_trunc)[num_assets]
assets_all<-sapply(num_assets,function(x) order(colMeans(list_Var_imp_grf[[x]]),decreasing=TRUE)[1:5])
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
  name_asset<-names(final_data_trunc)[ind_k]
  imp_grf_wide<-list_Var_imp_grf[[ind_k]]
  #colnames(imp_grf_wide)[match(table_fin$Coding_Coinmetrics,colnames(imp_grf_wide))]<-table_fin$Variable_Name
  imp_order<-sort(order(colMeans(imp_grf_wide),decreasing=TRUE)[1:5])
  roll_means<-zoo::rollapply(imp_grf_wide,width=30,FUN=mean,partial=TRUE,by.column=TRUE,align="right")[,assets_ind[[i]]]
  max_imp<-max(roll_means)
  df_roll<-data.frame(Date=as.Date(rownames(imp_grf_wide)),roll_means)
  df_plot<-pivot_longer(df_roll,cols=2:ncol(df_roll),names_to="variable",values_to = "value")
  df_plot$variable<-factor(df_plot$variable,levels=unique(df_plot$variable))
  # plot(df_qr_imp_sum[[num_asset]]$dates,
  #      rollapply(list_Var_imp_grf[[num_asset]][,1],width=30,FUN=mean,partial=TRUE),
  #      type="n",ylim=c(0,1),n)
  
  grid_app[[i]]<-(ggplot(df_plot, aes(x=Date,y=value,group = variable,color=variable)) + 
                    geom_line(aes(linetype=variable),size=2) +
                    scale_color_manual(values=cols_take[[i]])+# geom_line() +
                    # ggtitle(paste0("GRF Variable Importance - ",crypto_names[i])) + 
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
    name_asset<-names(final_data_trunc)[ind_k]
    imp_grf_wide<-list_Var_imp_grf[[ind_k]]
    #colnames(imp_grf_wide)[match(table_fin$Coding_Coinmetrics,colnames(imp_grf_wide))]<-table_fin$Variable_Name
    names_legend<-colnames(imp_grf_wide)[assets_ind[[i+1]]]  ###
    #imp_order<-sort(order(colMeans(imp_grf_wide),decreasing=TRUE)[1:5])
    roll_means<-zoo::rollapply(imp_grf_wide,width=30,FUN=mean,partial=TRUE,by.column=TRUE,align="right")[,assets_ind[[i+1]]] ###
    max_imp<-max(roll_means)
    df_roll<-data.frame(Date=as.Date(rownames(imp_grf_wide)),roll_means)
    df_plot<-pivot_longer(df_roll,cols=2:ncol(df_roll),names_to="variable",values_to = "value")
    df_plot$variable<-factor(df_plot$variable,levels=unique(df_plot$variable))
    
    grid_app[[i+1]]<-(ggplot(df_plot, aes(x=Date,y=value,group = variable,color=variable)) +
                        scale_color_manual(values=cols_take[[i+1]])+
                        geom_line(aes(linetype=variable),size=2) +# geom_line() +
                        # ggtitle(paste0("GRF Variable Importance - ",crypto_names[i])) + 
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
  
  #ggsave(paste0("./Plots/",name_asset,"_var_imp_new.pdf"),width=22,height = 15,units = "in")
  #cnt_i<-cnt_i+1
}

legend_plot<-get_legend(grid_app[[4]])
top_part<-plot_grid(grid_app[[1]],grid_app[[2]],nrow=1)
bottom_part<-plot_grid(NULL,grid_app[[3]],NULL,rel_widths = c(1,2,1),nrow=1)
legend_part<-plot_grid(legend_plot)
#plot_grid(top_part,bottom_part,legend_part,ncol=1,align="vh",axis="tb",lables="AUTO")
plot_grid(top_part,NULL,bottom_part,NULL,legend_part,NULL,ncol=1,rel_heights = c(1,0.1,1,-0.3,1,-1.3),align="vh",axis="tb",lables="AUTO")
ggsave(paste0("Plots/GRF_var_imp_",paste0(name_assets,collapse="_"),".pdf"),width=24,height = 18,units = "in")

