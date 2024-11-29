################################################
# Alle nötigen Packages laden
################################################
packages <- c("xtable", "ggplot2", "corrplot", "rstudioapi", "dplyr", "tidyr","doParallel")

# Schleife durch die Liste der Pakete
for (pkg in packages) {
  # Überprüfen, ob das Paket installiert ist
  if (!requireNamespace(pkg, quietly = TRUE)) {
    # Wenn das Paket nicht installiert ist, installieren Sie es
    install.packages(pkg)
  }
  # Das Paket laden
  library(pkg, character.only = TRUE)
}

################################################

# Load crypto data
load("Cleaned_Data/Forecasts/final_data_combined_0.05_500_2024-11-20.RData") #final_data
load("Cleaned_Data/Forecasts/Crypto_Data_add.RData") #list_rets
source("Tests/Evaluation_Functions.R")


################################################
# Zeit-Perioden festlegen
################################################
period <- c("2015-08-22", "2017-12-21", "2020-11-05", "2023-01-01", "2024-04-06")

# Liefert eine große Liste wo die Forecasts nach Perioden sortiert enthalten sind
Period_Data <- getPeriodData(final_data_combined, period)

# Perioden als Vektoren von Datumsangaben
period1 <- as.Date(seq(as.Date(period[1]), as.Date(period[2]), by = "1 day"))[-length(as.Date(seq(as.Date(period[1]), as.Date(period[2]), by = "1 day")))]
period2 <- as.Date(seq(as.Date(period[2]), as.Date(period[3]), by = "1 day"))[-length(as.Date(seq(as.Date(period[2]), as.Date(period[3]), by = "1 day")))]
period3 <- as.Date(seq(as.Date(period[3]), as.Date(period[4]), by = "1 day"))[-length(as.Date(seq(as.Date(period[3]), as.Date(period[4]), by = "1 day")))]
period4 <- as.Date(seq(as.Date(period[4]), as.Date(period[5]), by = "1 day"))


# Daten in einzelne Listen schreiben
period_1_final_data <- Period_Data$`Period 1`
period_2_final_data <- Period_Data$`Period 2`
period_3_final_data <- Period_Data$`Period 3`
period_4_final_data <- Period_Data$`Period 4`
# Signifikanzniveau tau
tau <- 0.05
n_cryptos <- length(final_data_combined)

################################################
# Tests durchführen DQ-Test, AoE-Test
################################################

# Liste in der die Testergebnisse gespeichert werden
Test_results_periods <- list()
All_Tests_Results_Periods <- list()

# Schleife die für jede Periode die Testergebnisse speichert
for(i in 1:5){
  #Auswählen welcher Datensatz benutzt wird
  period_data <- switch(i,
                        period_1_final_data,
                        period_2_final_data,
                        period_3_final_data,
                        period_4_final_data,
                        final_data_combined)
  # Hier werden die Testergebnisse für die entsprechende Periode gespeichert
  test_results <- vector("list", length(period_data))
  
  for (j in seq_along(period_data)){
    list_p_values <- list()
    crypto <- period_data[[j]]
    #GRF-X
    GRF_base <- list(DQ_Custom = as.numeric(BacktestVaR_DQ_Custom(crypto$True_Ret, rem_nan_na(unlist(crypto$GRF_base)), tau)$DQ$pvalue),
                     DQ = as.numeric(BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$GRF_base)), tau)$DQ$pvalue),
                     Kupiec = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$GRF_base)), tau)$LRuc[2],
                     Christoffesen = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$GRF_base)), tau)$LRcc[2],
                     AoE = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$GRF_base)), tau)$AE)
    
    #QRF-X
    QRF_base <- list(DQ_Custom = as.numeric(BacktestVaR_DQ_Custom(crypto$True_Ret, rem_nan_na(unlist(crypto$QRF_base)), tau)$DQ$pvalue),
                     DQ = as.numeric(BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$QRF_base)), tau)$DQ$pvalue),
                     Kupiec = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$QRF_base)), tau)$LRuc[2],
                     Christoffesen = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$QRF_base)), tau)$LRcc[2],
                     AoE = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$QRF_base)), tau)$AE)
    
    
    #QR-X
    tryCatch({
      QR_base <- list(DQ_Custom = as.numeric(BacktestVaR_DQ_Custom(crypto$True_Ret, rem_nan_na(unlist(crypto$QR_base)), tau)$DQ$pvalue),
                      DQ = as.numeric(BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$QR_base)), tau)$DQ$pvalue),
                      Kupiec = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$QR_base)), tau)$LRuc[2],
                      Christoffesen = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$QR_base)), tau)$LRcc[2],
                      AoE = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$QR_base)), tau)$AE)
    }, error = function(e) {
      cat("Fehler aufgetreten für crypto ", names(period_data)[j], " bei der Berechnung von QR_base: ", conditionMessage(e), "\n")
      return()
    })
    
    #CAV
    CAV <- list(DQ_Custom = as.numeric(BacktestVaR_DQ_Custom(crypto$True_Ret, rem_nan_na(unlist(crypto$CAV)), tau)$DQ$pvalue),
                DQ = as.numeric(BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$CAV)), tau)$DQ$pvalue),
                Kupiec = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$CAV)), tau)$LRuc[2],
                Christoffesen = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$CAV)), tau)$LRcc[2],
                AoE = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$CAV)), tau)$AE)
    
    #CAV_ASY
    tryCatch({
    CAV_ASY <- list(DQ_Custom = as.numeric(BacktestVaR_DQ_Custom(crypto$True_Ret, rem_nan_na(unlist(crypto$CAV_ASY)), tau)$DQ$pvalue),
                DQ = as.numeric(BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$CAV_ASY)), tau)$DQ$pvalue),
                Kupiec = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$CAV_ASY)), tau)$LRuc[2],
                Christoffesen = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$CAV_ASY)), tau)$LRcc[2],
                AoE = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$CAV_ASY)), tau)$AE)
    }, error = function(e) {
      cat("Fehler aufgetreten für crypto ", names(period_data)[j], " bei der Berechnung von CAV_ASY: ", conditionMessage(e), "\n")
      return()
    })
    
    #GJR-GARCH
    GARCH_plain <- list(DQ_Custom = as.numeric(BacktestVaR_DQ_Custom(crypto$True_Ret, rem_nan_na(unlist(crypto$GARCH_plain)), tau)$DQ$pvalue),
                DQ = as.numeric(BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$GARCH_plain)), tau)$DQ$pvalue),
                Kupiec = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$GARCH_plain)), tau)$LRuc[2],
                Christoffesen = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$GARCH_plain)), tau)$LRcc[2],
                AoE = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$GARCH_plain)), tau)$AE)
    
    
    
    
    #Hist
    Hist <- list(DQ_Custom = as.numeric(BacktestVaR_DQ_Custom(crypto$True_Ret, rem_nan_na(unlist(crypto$Hist)), tau)$DQ$pvalue),
                DQ = as.numeric(BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$Hist)), tau)$DQ$pvalue),
                Kupiec = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$Hist)), tau)$LRuc[2],
                Christoffesen = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$Hist)), tau)$LRcc[2],
                AoE = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$Hist)), tau)$AE)
    
    #QRF
    QRF <- list(DQ_Custom = as.numeric(BacktestVaR_DQ_Custom(crypto$True_Ret, rem_nan_na(unlist(crypto$QRF)), tau)$DQ$pvalue),
                DQ = as.numeric(BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$QRF)), tau)$DQ$pvalue),
                Kupiec = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$QRF)), tau)$LRuc[2],
                Christoffesen = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$QRF)), tau)$LRcc[2],
                AoE = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$QRF)), tau)$AE)
    
    #GRF
    GRF <- list(DQ_Custom = as.numeric(BacktestVaR_DQ_Custom(crypto$True_Ret, rem_nan_na(unlist(crypto$GRF)), tau)$DQ$pvalue),
                DQ = as.numeric(BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$GRF)), tau)$DQ$pvalue),
                Kupiec = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$GRF)), tau)$LRuc[2],
                Christoffesen = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$GRF)), tau)$LRcc[2],
                AoE = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$GRF)), tau)$AE)
    
    #QR
    tryCatch({
      QR <- list(DQ_Custom = as.numeric(BacktestVaR_DQ_Custom(crypto$True_Ret, rem_nan_na(unlist(crypto$QR)), tau)$DQ$pvalue),
                 DQ = as.numeric(BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$QR)), tau)$DQ$pvalue),
                 Kupiec = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$QR)), tau)$LRuc[2],
                 Christoffesen = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$QR)), tau)$LRcc[2],
                 AoE = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$QR)), tau)$AE)
    }, error = function(e) {
      cat("Fehler aufgetreten für crypto ", names(period_data)[j], " bei der Berechnung von QR: ", conditionMessage(e), "\n")
      return()
    })    
    
    
    #GARCH-X
    GARCH_cov <- list(DQ_Custom = as.numeric(BacktestVaR_DQ_Custom(crypto$True_Ret, rem_nan_na(unlist(crypto$GARCH_cov)), tau)$DQ$pvalue),
                     DQ = as.numeric(BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$GARCH_cov)), tau)$DQ$pvalue),
                     Kupiec = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$GARCH_cov)), tau)$LRuc[2],
                     Christoffesen = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$GARCH_cov)), tau)$LRcc[2],
                     AoE = BacktestVaR(crypto$True_Ret, rem_nan_na(unlist(crypto$GARCH_cov)), tau)$AE)
    
    
    # Speichern der berechneten p-Werte in einer Liste für jede Methode
    list_p_values$GRF_base <- GRF_base
    list_p_values$QRF_base <- QRF_base
    list_p_values$QR_base <- QR_base
    list_p_values$CAV <- CAV
    list_p_values$CAV_ASY <- CAV_ASY
    list_p_values$GARCH_plain <- GARCH_plain
    list_p_values$Hist <- Hist
    list_p_values$GRF <- GRF
    list_p_values$QRF <- QRF
    list_p_values$QR <- QR
    list_p_values$GARCH_cov <- GARCH_cov
    
    test_results[[j]] <- list_p_values
  }
  names(test_results) <- names(period_data)
  
  All_Tests_Results_Periods[[i]] <- test_results
  
  # Hier wird für jeden Test der Median berechnet und in einer Liste gespeichert
  
  Tests_Median <- list()
  methods <- c("GRF_base", "QRF_base", "QR_base", "CAV","CAV_ASY", "GARCH_plain", "Hist", "GRF", "QRF", "QR", "GARCH_cov")
  tests <- c("DQ_Custom", "DQ", "Kupiec", "Christoffesen", "AoE")
  
  for (method in methods) {
    methods_list <- list()
    for (test in tests) {
      tests_list <- c()
      for (crypto in seq_along(test_results)) {
        tests_list <- c(tests_list, test_results[[crypto]][[method]][[test]])
      }
      methods_list[[test]] <- median(tests_list)
    }
    Tests_Median[[method]] <- methods_list
  }
  names(Tests_Median) <- methods
  Test_results_periods <- append(Test_results_periods, list(Tests_Median))
}
names(Test_results_periods) <- names(All_Tests_Results_Periods) <- c("Period_1", "Period_2", "Period_3", "Period_4", "All")

file_name <- paste0("Backtests_4_Periods_",tau,"_",n_cryptos,"_", Sys.Date())
save(Test_results_periods, file=paste0("Cleaned_Data/Tests/",file_name,".RData"))

save(All_Tests_Results_Periods, file=paste0("Cleaned_Data/Tests/All_Tests_Results_Periods.RData"))


###########################
# Liste mit allen Testergebnissen umformen (Alle Tests)
###########################

for (i in 1:5){
  for (j in 1:length(All_Tests_Results_Periods[[i]])){
    list_temp <- All_Tests_Results_Periods[[i]][[j]]
    df_temp = data.frame(matrix(nrow=length(list_temp[[1]]), ncol=length(list_temp)))
    for (s in 1:length(list_temp)){
      for (k in 1:length(list_temp[[1]])){
        df_temp[k,s] = list_temp[[s]][[k]]
      }
    }
    colnames(df_temp) <- names(list_temp)
    rownames(df_temp) <- names(list_temp[[1]])
    All_Tests_Results_Periods[[i]][[j]] = df_temp
  }
}

###########################
# DQ-Test Liste erstellen
###########################

DQ_Results_Periods <- list()

for (i in 1:5) {
  # Erstellen eines leeren DataFrames für jede Testergebnisperiode
  df_combined <- data.frame()
  
  for (j in 1:length(All_Tests_Results_Periods[[i]])) {
    df_temp <- All_Tests_Results_Periods[[i]][[j]]
    df_combined <- rbind(df_combined, df_temp[2, ])
  }
  
  # Die Spaltennamen des kombinierten DataFrames festlegen
  rownames(df_combined) <- names(All_Tests_Results_Periods[[i]])
  
  # Das kombinierte DataFrame zur Liste hinzufügen
  DQ_Results_Periods[[i]] <- df_combined
}

# Die Namen der Liste DQ_Results_Periods festlegen
names(DQ_Results_Periods) <- names(All_Tests_Results_Periods)

###########################
## Medians of P-Values for DQ-Tests in Different Time Periods
## Table 6
## Table 10
###########################


# Wenden Sie unlist() auf jedes Element der Liste an
df_period_1 <- lapply(Test_results_periods$Period_1, unlist)
df_period_2 <- lapply(Test_results_periods$Period_2, unlist)
df_period_3 <- lapply(Test_results_periods$Period_3, unlist)
df_period_4 <- lapply(Test_results_periods$Period_4, unlist)
df_full <- lapply(Test_results_periods$All, unlist)

# Erstellen Sie Dataframes aus den Listen
df_period_1 <- as.data.frame(do.call(cbind, df_period_1))
df_period_2 <- as.data.frame(do.call(cbind, df_period_2))
df_period_3 <- as.data.frame(do.call(cbind, df_period_3))
df_period_4 <- as.data.frame(do.call(cbind, df_period_4))
df_full <- as.data.frame(do.call(cbind, df_full))

# Runden Sie die Werte im Dataframe auf 3 Dezimalstellen
table_pvalues <- rbind(df_period_1[2,], df_period_2[2,], df_period_3[2,], df_period_4[2,], df_full[2,])
rownames(table_pvalues) = c("period 1", "period 2", "period 3", "period 4","full")
table_pvalues <- round(table_pvalues, 3)

table_pvalues_latex <- xtable(table_pvalues)
print(table_pvalues_latex, include.rownames = TRUE)


##############################################################

# Liste mit Indikatoren, welche Crypto in welcher Periode ist
list_crypto_period <- vector(mode="list", 4)
for (period in seq_along(list_crypto_period)){
  temp <- vector()
  for (crypto in seq_along(Period_Data[[period]])){
    crypto_name <- names(Period_Data[[period]])[crypto]
    temp[crypto] <- which(names(list_rets) == crypto_name)
  }
  list_crypto_period[[period]]$Crypto_ind <- temp
  names(list_crypto_period[[period]]$Crypto_ind) <- names(Period_Data[[period]])
}
names(list_crypto_period) <- names(Period_Data)
list_crypto_period$`Period 1`$Time <- period1
list_crypto_period$`Period 2`$Time <- period2
list_crypto_period$`Period 3`$Time <- period3
list_crypto_period$`Period 4`$Time <- period4
save(list_crypto_period, file="Cleaned_Data/Tests/Crypto_ind_periods_4.RData")

##############################################
## Boxplots of p-values of DQ-tests
## Figure 2
####################################
list_nmax<-list(c(1,5,15,15),c(1,5,15,15), c(1,5,15,15)) #Anzahl der jeweils größten Werte
name_vars<-c("SER","Active_Users", "sd_7") #Covariates nach denen sortiert werden soll
list_periods<-vector(mode="list",4)
sort_val <- 1
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cols_periods<-cbPalette[c(2,4,8)]

for (period in 1:4){
  
  ind_period<-list_crypto_period[[period]]$Crypto_ind
  len_period<-length(ind_period)
  ind_temp_period<-matrix(NA,nrow=len_period,ncol=length(name_vars))
  vals_temp_period<-matrix(NA,nrow=len_period,ncol=length(name_vars))
  colnames(ind_temp_period)<-colnames(vals_temp_period)<-name_vars
  cnt_temp<-1
  for(i in name_vars)
  {
    ind_rows<-fun_order_one_var(list_rets,Var_to_order =i,summary_fun=mean,ret_sorted = FALSE,subsample=ind_period)
    sorted_rows<-fun_order_one_var(list_rets,Var_to_order =i,summary_fun=mean,ret_sorted = TRUE,subsample=ind_period)
    # print(paste0(i,"highest inds: ",ind_rows[1:20]))
    ind_temp_period[,cnt_temp]<-ind_rows[1:len_period]
    vals_temp_period[,cnt_temp]<-sorted_rows
    cnt_temp<-cnt_temp+1
  }
  
  list_vars<-vector("list",length(name_vars))
  names(list_vars)<-name_vars
  
  for (var_inds in 1:length(list_vars)){
    
    num_vals_temp<-list_nmax[[var_inds]][period]
    grp_high<-DQ_Results_Periods[[period]][ind_temp_period[1:num_vals_temp,var_inds],,drop=FALSE]
    grp_low<-DQ_Results_Periods[[period]][ind_temp_period[(num_vals_temp+1):nrow(ind_temp_period),var_inds],,drop=FALSE]
    df_temp<-data.frame(grp_high=apply(grp_high,2,median),grp_low=apply(grp_low,2,median))
    colnames(df_temp)<-c(paste0(num_vals_temp," Highest"),paste0(nrow(DQ_Results_Periods[[period]])-num_vals_temp," Lowest"))
    list_vars[[var_inds]]<-round(df_temp,3)
  
  }
  list_periods[[period]]<-list_vars
  
  
  num_max<-list_nmax[[sort_val]]
  num_vals<-num_max[period]
  high_name<-paste0(num_vals," Highest Sorted")
  low_name<-paste0(nrow(DQ_Results_Periods[[period]])-num_vals," Lowest Sorted")
  dq_boxplot_sel_df<-data.frame(DQ_Results_Periods[[period]])[c(ind_temp_period[,sort_val],setdiff(1:nrow(DQ_Results_Periods[[period]]),ind_temp_period[,sort_val])),] %>%
    mutate(Group=factor(c(rep(high_name,num_vals),rep(low_name,nrow(DQ_Results_Periods[[period]])-num_vals)),
                        levels=c(high_name,low_name))) %>% 
    select(GRF_base,QRF_base,QR_base,CAV,CAV_ASY,GARCH_plain,Hist,GRF,QRF,QR,GARCH_cov,Group) %>%
    rename("GARCH-X"=GARCH_cov,"GJR-GARCH"=GARCH_plain,"GRF-X"=GRF,"QRF-X"=QRF,"QR-X"=QR)%>%
    rename_with(.fn=function(x){gsub("_base","",x)}) %>%
    pivot_longer(cols=!last_col(),names_to="Method",values_to = "Value") %>%
    mutate(Method=factor(Method,levels = as.character(unique(Method))))
  
  p2_sel <- ggplot(dq_boxplot_sel_df, aes(x=Method, y=Value, fill=Group)) + 
    geom_boxplot() + geom_hline(yintercept=0.1,linetype = 2,size=1)+
    scale_fill_manual(values=cols_periods[1:2])+
    ggtitle(paste0("Period ",period))+
    ylab("P-Value") +  guides(fill=guide_legend(title=paste0("Sorted by ",colnames(ind_temp_period)[sort_val]))) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          text = element_text(size = 15))
  
  ggsave(paste0("Boxplot_4_Period_",period,"_",colnames(ind_temp_period)[sort_val],".pdf"),
         path ="Plots/Boxplots",width=15,height=10,units="in",scale=0.7 )
  
}
names(list_periods) <- names(DQ_Results_Periods[1:4])

##########################
## Medians of P-Values for DQ-Tests sorted by SER (and Active Users and SD_7)
## Table 7
## Table 10
##########################

SER_sorted_median <- rbind(t(list_periods[[1]][[1]]),t(list_periods[[2]][[1]]),t(list_periods[[3]][[1]]),t(list_periods[[4]][[1]]))
Active_Users_sorted_median <- rbind(t(list_periods[[1]][[2]]),t(list_periods[[2]][[2]]),t(list_periods[[3]][[2]]),t(list_periods[[4]][[2]]))                          
SD_7_sorted_median <- rbind(t(list_periods[[1]][[3]]),t(list_periods[[2]][[3]]),t(list_periods[[3]][[3]]),t(list_periods[[4]][[3]]))                          

SER_sorted_median_latex <- xtable(SER_sorted_median)
print(SER_sorted_median_latex, include.rownames = TRUE)

#Active_Users_sorted_median_latex <- xtable(Active_Users_sorted_median)
#print(Active_Users_sorted_median_latex, include.rownames = TRUE)

#SD_7_sorted_median_latex <- xtable(SD_7_sorted_median)
#print(SD_7_sorted_median_latex, include.rownames = TRUE)


################################
## summary table with DQ-values over time periods, and sorted by SER, Number of Users, SD_7
## Table B.17
#########
identical(colnames(SER_sorted_median),colnames(Active_Users_sorted_median))
all_sorted_median<- rbind(SER_sorted_median[1:6,],Active_Users_sorted_median[1:6,],SD_7_sorted_median[1:6,])
high_group_ind<-seq(1,nrow(all_sorted_median),by=2)
low_group_ind<-seq(2,nrow(all_sorted_median),by=2)
all_median_hilo<-all_sorted_median[c(high_group_ind,low_group_ind),]

caption_tab <-"Medians of P-Values for DQ-Tests Sorted by Covariate Values in Different Time Periods"
tab_print_2<-xtable(all_median_hilo,label = "tab:median_pvals_sorted",caption=caption_tab)#,digits=c(0,0,3,rep(0,4),3,0,rep(3,5),0))#display = c("s",rep("d",7)))
print.xtable(tab_print_2,type="latex",
             floating=TRUE,include.rownames=FALSE,caption.placement = "top",
             comment=TRUE,booktabs = TRUE)


