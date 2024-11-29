library(GAS)
############################
# Custom Funktion für den DQ Test
dq_custom <- function(y, VaR, tau, cLags) {
  
  cT = length(y)
  vHit = numeric(cT)
  vHit[y < VaR] = 1 - tau
  vHit[y > VaR] = -tau
  
  vConstant = rep(1, (cT - cLags))
  vHIT = vHit[(cLags + 1):cT]
  vVaRforecast = VaR[(cLags + 1):cT]
  mZ = matrix(0, cT - cLags, cLags)
  
  for (st in 1:cLags) {
    
    mZ[, st] = vHit[st:(cT - (cLags + 1L - st))]
    
  }
  
  mX = cbind(vConstant, vVaRforecast, mZ)
  
  dDQstatOut = (t(vHIT) %*% mX %*% MASS::ginv(t(mX) %*% mX) %*% t(mX) %*% (vHIT))/(tau * (1 - tau))
  
  dDQpvalueOut = 1 - pchisq(dDQstatOut, ncol(mX))
  
  out = list(stat = dDQstatOut, pvalue = dDQpvalueOut)
}

# Funktion um einen Backtest für VaR DQ Custom zu machen
BacktestVaR_DQ_Custom <- function(y, VaR, tau){
  data_select <- unlist(y)
  data_predict <- unlist(VaR)
  
  DQ <- dq_custom(data_select, data_predict, tau=tau, cLags=4)
  
  out <- list(DQ = DQ)
}

# Funktion, um aus dem Final_Data Datensatz die daten in Zeitperioden einzuteilen
getPeriodData <- function(final_data, dates_vector) {
  if (length(dates_vector) != 5) {
    stop("Der Eingabevektor muss genau fünf Elemente enthalten.")
  }
  
  # Sortieren des Vektors, um sicherzustellen, dass die Daten in aufsteigender Reihenfolge sind
  sorted_dates <- sort(dates_vector)
  
  # Berechnung der Grenzen für die drei Perioden
  period1 <- c(sorted_dates[1], sorted_dates[2])
  period2 <- c(sorted_dates[2], sorted_dates[3])
  period3 <- c(sorted_dates[3], sorted_dates[4])
  period4 <- c(sorted_dates[4], sorted_dates[5])

  period_1_data <- list()
  period_2_data <- list()
  period_3_data <- list()
  period_4_data <- list()
  
  all_periods <- list()
  for (period in list(period1, period2, period3, period4)) {
    period_data <- list()
    for (crypto_index in seq_along(final_data)){
      crypto_data <- final_data[[crypto_index]]
      crypto_name <- names(final_data)[crypto_index]
      crypto_filtered_data <- vector("list", length(crypto_data))
      names(crypto_filtered_data) <- names(crypto_data)
      for (method_index in seq_along(crypto_data)){
        method_data <- crypto_data[[method_index]]
        method_filtered_data <- vector("list", length(method_data))
        start_index <- which(names(method_data) == period[1])
        end_index <- which(names(method_data) == period[2])-1
        if(length(start_index) == 0 & length(end_index) == 0){crypto_name <- "stop"
        next}
        if(length(start_index) == 0 & length(end_index) != 0){crypto_name <- "stop"
        next} #start_index <- 1}
        if(length(start_index) != 0 & length(end_index) == 0){crypto_name <- "stop"
        next}#end_index <- length(method_data)}
        method_filtered_data <- method_data[start_index:end_index]
        crypto_filtered_data[[method_index]] <- method_filtered_data
      }
      if(crypto_name=="stop"){next}
      period_data[[crypto_name]] <- crypto_filtered_data
    }
    all_periods <- append(all_periods, list(period_data))
  }
  names(all_periods) <- c('Period 1','Period 2','Period 3','Period 4')
  
  return(all_periods)
}



#remove NA values
rem_nan_na<-function(vec,replace_el=0)
{
  vec[is.nan(vec)]<-replace_el
  vec[is.na(vec)]<-replace_el
  ret_vec<-vec
  return(ret_vec)
}

#order one variable by aggregating with a summary
fun_order_one_var<-function(list_ret=list_rets,Var_to_order,summary_fun=mean,ret_sorted=FALSE,subsample=1:length(list_ret))
{
  Var_vals<-rem_nan_na(sapply(subsample,function(x) summary_fun(list_ret[[x]][,colnames(list_ret[[x]]) %in% Var_to_order])))
  var_sum<-order(Var_vals,decreasing = TRUE)
  if(ret_sorted)
  {
    return(sort(Var_vals,decreasing = TRUE))
  } else {
    return(var_sum)
  }
}


get_loss<-function(loss=tick,true_val,forecast,...)
{
  return(tick(true_val-forecast,...))
}


# Define Tick Funciton
tick <- function(res,tau){
  
  return((tau - (res < 0))*res)
  
}


#summarize dq tests
#requires ser_preds object (i.e. predictions for each function)
fun_dq_df<-function(time_period,cryptos_contained,aoe=FALSE)
{
  preds_time<-lapply(cryptos_contained,function(x) ser_preds[[x]][ser_preds[[x]]$Dates %in%time_period,-1])
  if(aoe)
  {
    dq_pred<-sapply(preds_time,function(x) {sapply(2:ncol(x),function(y){
      unlist(BacktestVaR(data=x$true_y,VaR=rem_nan_na(x[,y]),alpha=0.05,Lags=4)$AE)})})
  } else {
    dq_pred<-sapply(preds_time,function(x) {sapply(2:ncol(x),function(y){
      unlist(BacktestVaR(data=x$true_y,VaR=rem_nan_na(x[,y]),alpha=0.05,Lags=4)$DQ[2])})})
  }
  rownames(dq_pred)<-colnames(preds_time[[1]])[-1]
  return(t(data.frame(dq_pred))[,c(2,1,3:nrow(dq_pred))])
}





CPAtest <- function(loss1,loss2,tau,alpha,choice,sign_level=0.05,ret_rate=FALSE,ret_table=FALSE,print_it=TRUE,ret_numbers=FALSE){
  
  #  This function performs the asymptotic Conditional Predictive Ability Test and is adapted from Giacomini and White
  #
  #  INPUTS: loss1 and loss2: Tx1 vectors of losses over the out of sample period for the two models under consideration
  #          tau:             the forecast horizon
  #          alpha:           niminal risk level: 1%, 5%, 10%
  #          choice:          1 if unconditional ; 2 if conditional
  #          sign_level:      the level of significance for the + or - to be added
  #          ret_rate:        If true, output will be changed to a list containing one entry with the output and one with the full rates, i.e. losses
  #          ret_table:       If true, output will be changed to a list containing one entry with p-values and one with the rate of outperformance 
  #          print_it:           If true, output will be printed
  #          ret_numbers      If true, will return numeric numbers of pvalue and convergence rate
  
  # 
  #  OUTPUTS: teststat: the test statistic of the conditional predictive ability test
  #           critval:  the critical value of the test for a 5% level (the test is a chi square)
  #           pval:     the p-value of the test
  
  
  #loss diferential 
  lossdiff1 = loss1-loss2                                                     
  
  TT = length(lossdiff1)
  
  if(choice==1){
    
    instruments = as.matrix(rep(1,TT))
    lossdiff    = lossdiff1
    t           = TT
    
  }else{
    
    instruments = cbind(rep(1,TT-tau),lossdiff1[1:(length(lossdiff1)-tau)])
    lossdiff    = lossdiff1[(tau+1):length(lossdiff1)]                                     
    t           = TT-tau
  }  
  
  # create the regressor matrix given by lossdiff*ht', where ht is the matrix of instruments
  reg = matrix(nrow = nrow(instruments),
               ncol = ncol(instruments),
               rep(-999, nrow(instruments)*ncol(instruments)))
  for(jj in 1:ncol(instruments)){
    reg[,jj] = instruments[,jj]*lossdiff
  }
  
  if(tau == 1){
    #calculate the test stat as nR^2 from the regression of one on lossdiff*ht
    res.beta = lm(as.matrix(rep(1,t)) ~ reg -1)$coefficients
    err      = rep(1,t)-reg%*%res.beta
    r2       = 1-mean(err^2)
    teststat = t*r2
    q        = ncol(reg)
    critval  = qchisq(1-alpha,q)
    pval     = 1 - pchisq(abs(teststat),q)
  }else{
    zbar = matrix(nrow = ncol(reg),
                  ncol = 1)
    for(p in 1:ncol(reg)){
      zbar[p,] <- mean(reg[,p])
    }
    nlags    = tau-1
    omega    = NeweyWest(reg,nlags)
    teststat = t*t(zbar)%*%solve(omega)%*%zbar
    q        = ncol(reg)
    critval  = qchisq(1-alpha,q)
    pval     = 1 - pchisq(abs(teststat),q)
  }
  
  coeffs <- lm(lossdiff ~ instruments -1)$coefficients
  av_diff_loss = mean(loss1-loss2)
  
  if(av_diff_loss < 0){
    sign='(-)'
  }else if(av_diff_loss > 0){
    sign='(+)'
  }
  
  if(print_it)
  {
    if(choice==1){ 
      print('Choice: Unconditional Test')
    }else{
      print('Choice: Conditional Test' )
    }
    
    print(paste('Forecast Horizon: ' , tau ))
    print(paste('Nominal Risk level: ' , alpha) )
    print('-----------------------------------------------') 
    print( paste('Test-statistic: ' , teststat, sign))
    print( paste('Critical Value: ' , critval ) )
    print( paste('P-value: ' , pval ) )
  }
  
  rate <- instruments%*%coeffs
  rate_ind <- sum(rate > 0 )/length(rate)
  sign <- ifelse(pval <= sign_level,ifelse(rate_ind > 0.5,'^+','^-'),' ')
  ret <- paste('$',round(pval, digits =3),sign,'$','(',round(rate_ind, digits=3),')')
  p_val_ret<- paste('& $',format(round(pval, digits =3),nsmall=3),sign,'$')
  rate_ret<-  paste('& $',format(round(rate_ind, digits =3),nsmall=3),'$')
  if(ret_rate){
    return(list(Table=ret,Full_rate=rate))
  } else {
    if(ret_numbers)
    {
      return(list(pval=pval,rate=rate_ind))
    }
  }
  if(ret_table)
  {
    return(list(pval=p_val_ret,rate=rate_ret))
  }
  return(ret)
  
}



NeweyWest <- function(Z,nlags){
  
  # Returns the Newey-West estimator of the asymptotic variance matrix, needed for Conditional Predictive Ability Test 
  #
  # INPUTS: Z:     a nxk matrix with rows the vector zt'
  #         nlags: the number of lags
  #
  # OUTPUTS: omegahat: the Newey-West estimator of the covariance matrix
  
  
  n = nrow(Z)
  k = ncol(Z)
  
  #de-mean the variables
  for(i in 1:ncol(Z)){
    Z[,i] = Z[,i] - rep(1,nrow(Z))*mean(Z[,i])
  }
  
  
  gamma = matrix(nrow = nlags,
                 ncol = k,
                 rep(-999, nlags*k))
  
  samplevar = (t(Z)%*%Z)/n # sample variance
  omegahat = samplevar;
  
  if(nlags > 0){
    #sample autocovariances
    for(ii in 1:nlags){
      if((n-ii) > 0){
        Zlag = rbind(matrix(nrow = ii,ncol = k,rep(0,ii*k)),
                     as.matrix(Z[1:(n-ii),]))
      }else{
        Zlag = matrix(nrow = ii,ncol = k,rep(0,ii*k))
      }
      gamma = ((t(Z)%*%Zlag +t(Zlag)%*%Z)/n)
      weights = 1 - (ii/(nlags+1))
      omegahat = omegahat + weights*gamma
      
    }
  }
  return(omegahat)
}


#make nice summary table
fun_make_tab_df<-function(p_val_df,perf_df,ordering=NULL)
{
  if(is.null(ordering))
  {
    ordering<-1:nrow(p_val_df)
  }
  fin_df<-(1-p_val_df)*ifelse(perf_df>0.5,1,-1)
  return(fin_df[ordering,])
}


#split vector into n equal parts
split_fun<-function(x,n=3) split(x, cut(seq_along(x), n, labels = FALSE)) 




#Function to summarize covariates of crypto currencies given names of the currencies
#used for table in "Difference Between Covariates of Cryptos Where GRF is Better vs. Worse"
#needs stuff to be executed before, list_rets, list_crypto_period,
fun_sum_crypto<-function(names_of_ind=NULL)
{
  crypto_sum<-list()
  if(is.null(names_of_ind))
  {
    names_of_ind<-names(list_rets)
  }
  for(i in 1:(length(list_crypto_period)+1))
  {
    if(i<5)
    {
      temp_names<-names(list_crypto_period[[i]]$Crypto_ind) %in% names_of_ind
      temp_ind<-list_crypto_period[[i]]$Crypto_ind[temp_names]
      sum_list<-matrix(0,ncol=12,nrow=3)
      colnames(sum_list)<-colnames(list_rets[[12]])[-c(1,3)]
      temp_time<-list_crypto_period[[i]]$Time
      for(j in temp_ind)
      {
        #print(j)
        rets_temp<-list_rets[[j]][,-c(1,3)]
        log_time<-temp_time %in% as.Date(list_rets[[j]]$time)
        log_vars<-match(colnames(rets_temp),colnames(sum_list))
        #colnames(list_rets[[j]])[-c(1,3)] %in% colnames(sum_list)
        #match(colnames(list_rets[[j]])[-c(1,3)],colnames(sum_list))
        
        sum_list[,log_vars]<-sum_list[,log_vars]+ apply(rets_temp[log_time,],2,quantile,probs=c(0.05,0.5,0.95))
      } 
    } else {
      temp_names<- names(list_rets) %in% names_of_ind
      temp_ind<-(1:length(list_rets))[temp_names]
      sum_list<-matrix(0,ncol=12,nrow=3)#13 mit Marktkapitalisierung
      colnames(sum_list)<-colnames(list_rets[[12]])[-c(1,3)]
      for(j in temp_ind)
      {
        #print(j)
        rets_temp<-list_rets[[j]][,-c(1,3)]
        log_vars<-match(colnames(rets_temp),colnames(sum_list))
        #colnames(list_rets[[j]])[-c(1,3)] %in% colnames(sum_list)
        #match(colnames(list_rets[[j]])[-c(1,3)],colnames(sum_list))
        
        sum_list[,log_vars]<-sum_list[,log_vars]+ apply(rets_temp,2,quantile,probs=c(0.05,0.5,0.95))
      }
    }
    
    sum_list<-sum_list/length(temp_ind)
    
    crypto_sum[[i]]<-sum_list
  }
  return(crypto_sum)
}



#where is GRF worse than CAV, GJR, or QR
fun_get_crypto_per<-function(perf_trans_mat,num_better=0,cryptos_better=c(4,5,9))
{
  perf_ind<-rowSums(perf_trans_mat[,cryptos_better]<0)
  names(perf_ind)<-rownames(perf_trans_mat)
  perf_cryptos<-names(perf_ind)[perf_ind>num_better]
  return(perf_cryptos)
}


#look at variable importance measures
#depends on list_final being loaded
get_sum_df<-function(list_one_cur,ind_t)
{
  rows_one<-nrow(list_one_cur[[1]])
  cols_one<-ncol(list_one_cur[[1]])
  final_list<-list(coefs=list(),pval=list())
  for(i in 1:cols_one)
  {
    temp_df<-t(sapply(list_one_cur,function(x){if(any(is.na(x))){return(rep(NA,rows_one))}else{return(x[,i])}}))
    final_list[[i]]<-temp_df
  }
  final_list$dates<-names(list_one_cur)
  return(final_list)
}
