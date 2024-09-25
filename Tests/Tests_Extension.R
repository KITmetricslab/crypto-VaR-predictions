###########################
#Make plots for Cryptos
###########################

#load data and libraries

library(xtable)
library(corrplot)

source("./Code/New_Code/Functions_evaluation_plot.R")

load(file="./Data/New_Data/data_to_test.RData") #test objects with predictions (ser_preds,bt_obj)
load("./Data/New_Data/ret_over_time.RData") #returns over time
load("./Data/New_Data/dq_tests_subperiod.RData") #dq tests for subperiods and aoe for subperiods (aoe_1,..,dq_1,...)
load("./Data/New_Data/Time_periods.RData") #Sub periods
load("./Data/New_Data/data_matrix_add.RData") #load list_rets, important for returns
load(file="./Data/New_Data/var_imp.RData") #For Variable Importances
load("./New_Main_Results/List_final_0.05_500_2022-05-07.RData") # for dates in Variable Importance
load(file="./Data/New_Data/description_paper.RData") # for new covariate names, table_fin

#define color-blind friendly palettes
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cols_periods<-cbPalette[c(2,4,8)]



###############################################
#Plot predicted loss series for certain methods
###############################################

#PLOT TOGETHER!
name_cryptos<-c("ada","btc","usdt")
load(paste0("./Data/New_Data/CPA_tests_loss_series_",paste0(name_cryptos,collapse="_"),".RData"))
window_size_moving<-c(30,180,30)#c(180,60,30)
rem_rhs<-c("GARCH_cov","QRF","QRF_base","QR","Hist","GRF")
loss_scale=100
ret_width=10
#loads list_pred_losses

# Prepare data for plot

scaling_factor<-c(100,100,100)
#each crypto gets one plot
#make sure scales are the same
scale_plots<-sapply(list_pred_losses,function(x) range(x[,!(colnames(x) %in% c(rem_rhs,"ret","Date"))]))
for(i in 1:length(list_pred_losses))
{
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
    cols_take<- cols_periods
    name_var<-colnames(moving_avg)[j[[1]]]
    wide_df<-data.frame(Date=temp_df$Date,"Log-Return"=moving_log_ret/scale*loss_scale,name_method=moving_avg[,j[[1]]]*loss_scale)
    which_wide<-which(grepl("name_method",colnames(wide_df)))
    names(wide_df)[grepl("name_method",colnames(wide_df))] <- name_var
    long_df<-reshape2::melt(wide_df,id.vars="Date",variable.factor=TRUE,variable.name="Method")
    
    print(ggplot(long_df,aes(x=Date,y=value,group=Method,color=Method)) + 
            geom_line(aes(linetype=Method)) +
            scale_color_manual(values = c("grey",cols_take[1:length(j[[1]])]))+ 
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
             theme(legend.position = "n",text = element_text(size = 20))) #right and n
    ggsave(paste0("Loss_difference_",names(list_pred_losses)[[i]],
                  "_GRF_",paste0(colnames(wide_df)[which_wide],collapse="_"),"_no_legend.pdf"),
           path ="./Plots/CPA/New/Loss_diff",width=15,height=10,units="in",scale=0.7 )
    # ggsave(paste0("Loss_difference_",names(list_pred_losses)[[i]],
    #               "_GRF_",paste0(colnames(wide_df)[which_wide],collapse="_"),".pdf"),
    #        path ="./Plots/CPA/New/Loss_diff",width=15,height=10,units="in",scale=0.7 )
  }
 
}


#PLOT BITCOIN SEPERATELY

scale=scaling_factor[1]
loss_scale=100
ret_width=10
#make temp_df
if(test_lhs=="GRF_base") {name_lhs<-"GRF"} else {name_lhs<- "GRF-x"}
temp_df<-list_pred_losses[[1]]
Ret<-temp_df$ret
range_ret<-range(Ret)
temp_df<-temp_df[,!(colnames(temp_df) %in% c(rem_rhs,"ret"))]

moving_avg<-zoo::rollapply(temp_df[,-ncol(temp_df)],width=window_size_moving[1],
                           FUN=mean,by.column=TRUE,fill=NA,partial=FALSE,align="right")%>% 
  data.frame() %>% rename("GJR-GARCH"=GARCH_plain)%>%
  rename_with(.fn=function(x){gsub("_base","",x)})
moving_log_ret<-zoo::rollapply(Ret,width=ret_width,FUN=mean,fill=NA,partial=FALSE,align="right")
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
          ggtitle(paste0(names(list_pred_losses)[[1]],": ",paste0(colnames(wide_df)[which_wide],collapse="/"),
                         " - ",name_lhs)) + 
          xlab("Date") + 
          theme(legend.position = "n",text = element_text(size = 20)))
  ggsave(paste0("Loss_difference_BTC_",paste0(colnames(wide_df)[which_wide],collapse="_"),".pdf"),
         path ="./Plots/CPA/New/Loss_diff",width=15,height=10,units="in",scale=0.7 )
}
  

####
#Plot log returns for the three currencies
library(lubridate)
name_cryptos<-c("ada","btc","usdt")
num_assets<-which(names(list_final) %in% name_cryptos)

create_pdf<-TRUE
if(create_pdf) {
  pdf(file="./Plots/Log_returns_crypto_new.pdf",width=16,height=12)
  par(mfrow=c(2,2))
}
for(i in 1:length(num_assets))
{
  data_select<-list_rets[[num_assets[i]]]
  plot(data_select$time,data_select$Ret,type="l",xaxt="n",ylab="log-return",main=name_cryptos[i],
       xlab="Dates",cex.lab=1.5, cex.axis=1.2,cex.main=1.2)
  dates_plot3<-seq(from=min(data_select$time),
                          to=max(data_select$time),by=ifelse(i<2,"quarter","year"))
  dates_final<-ceiling_date(dates_plot3,unit=ifelse(i<2,"month","year"))
  axis(1,at=dates_final,labels=format(dates_final,"%Y/%m"),cex.axis=1.5,cex.lab=1.2)
  
  print(paste0("Finished with ",name_cryptos[i]))
}
if(create_pdf){dev.off()}





###############################################
#Variable Importance
###############################################
#var imp of QR and QRF
###############################################

#df_qr_imp_sum<-lapply(list_Var_imp_qr,get_sum_df)
df_qr_imp_sum<-list()
for(i in 1:length(list_Var_imp_qr))
{
  if(i%%10==0)
  {
    print(i)
  }
  df_qr_imp_sum[[i]]<-get_sum_df(list_Var_imp_qr[[i]],ind_t=i)
}
#var imp qrf is already good
library(zoo)
library(dplyr)
library(ggplot2)
library(reshape2)
#install.packages("cowplot")
library(cowplot)
which(names(list_final)=="usdt")
num_asset<-13 #13,30, 100, btc, eth, xrp
num_assets<-which(names(list_final) %in% c("ada","btc","usdt"))
name_assets<-names(list_final)[num_assets]
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
  name_asset<-names(list_final)[ind_k]
  imp_grf_wide<-list_Var_imp_grf[[ind_k]]
  colnames(imp_grf_wide)[match(table_fin$Coding_Coinmetrics,colnames(imp_grf_wide))]<-table_fin$Variable_Name
  imp_order<-sort(order(colMeans(imp_grf_wide),decreasing=TRUE)[1:5])
  roll_means<-zoo::rollapply(imp_grf_wide,width=30,FUN=mean,partial=TRUE,by.column=TRUE,align="right")[,assets_ind[[i]]]
  max_imp<-max(roll_means)
  df_roll<-data.frame(Date=df_qr_imp_sum[[ind_k]]$dates,roll_means)
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
    name_asset<-names(list_final)[ind_k]
    imp_grf_wide<-list_Var_imp_grf[[ind_k]]
    colnames(imp_grf_wide)[match(table_fin$Coding_Coinmetrics,colnames(imp_grf_wide))]<-table_fin$Variable_Name
    names_legend<-colnames(imp_grf_wide)[assets_ind[[i+1]]]
    #imp_order<-sort(order(colMeans(imp_grf_wide),decreasing=TRUE)[1:5])
    roll_means<-zoo::rollapply(imp_grf_wide,width=30,FUN=mean,partial=TRUE,by.column=TRUE,align="right")[,assets_ind[[i+1]]]
    max_imp<-max(roll_means)
    df_roll<-data.frame(Date=df_qr_imp_sum[[ind_k]]$dates,roll_means)
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
ggsave(paste0("./Plots/GRF_var_imp_",paste0(name_assets,collapse="_"),".pdf"),width=24,height = 18,units = "in")




