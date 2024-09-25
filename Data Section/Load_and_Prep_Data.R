library(data.table)
library(tidyverse)
library(scales)

path_list<-list.files(path="../../Raw_Data/csv",full.names = TRUE)
path_names_raw<-list.files(path="../../Raw_Data/csv",full.names = FALSE)
path_names<-substr(path_names_raw,start=1,stop=nchar(path_names_raw)-4)
max_na_allowed<-10

#variable descriptions at https://docs.coinmetrics.io/info/metrics
list_cov<-c("time",#"CapMrktCurUSD",#"PriceUSD","ReferenceRateUSD",
            "VelCur1yr", #velocity
            "AdrActCnt","AdrBalCnt", "AdrBalUSD10Cnt", "AdrBalUSD100Cnt", #number of active users
            "SER","TxCnt") #exposure to market risk

list_crypto_raw <- vector("list",length=length(path_list))
names(list_crypto_raw)<-path_names
for(i in 1:length(list_crypto_raw))
{
  temp_df<-fread(path_list[[i]],data.table=FALSE)
  #print(dim(temp_df))
  list_crypto_raw[[i]]<-temp_df
}


list_crypto_clean <- vector("list",length=length(path_list))
names(list_crypto_clean)<-path_names
cnt<-0
for(i in 1:length(list_crypto_clean))
{
  temp_raw<-list_crypto_raw[[i]]
  #check whether price USD is there
  if(sum(colnames(temp_raw)=="PriceUSD")<1)
  {
    price_ind<-which(colnames(temp_raw)=="ReferenceRateUSD")
  } else {
    cnt<-cnt+1
    #print(paste0(i," : ",names(list_crypto_raw)[i]," : "))
    price_ind<-which(colnames(temp_raw)=="PriceUSD")
  }
  
  #select columns in set of covariates
  ind_cont<-colnames(temp_raw) %in% list_cov
  if(!(length(price_ind)==0)){
    #delete all NAs in price
    temp_raw<-temp_raw[!is.na(temp_raw[,price_ind]),]
  }

  temp_fin<-temp_raw[,ind_cont,drop=FALSE]
  temp_fin$Price <- temp_raw[,price_ind]
  temp_fin<-temp_fin %>% drop_na() %>% select(time,Price,everything())
  
  list_crypto_clean[[i]]<-temp_fin
}

cnt #number of times we have closing price, otherwise use Reference rate
#test if time and USD price is always contained

#remove variables without price:
no_price<-which(sapply(1:length(list_crypto_clean),function(x) length(list_crypto_clean[[x]]$Price)==0))
list_crypto_clean[no_price]<-NULL

no_extra_columns <- which(sapply(1:length(list_crypto_clean), function(x) {
  # Überprüfen, ob nur die Spalten "time" und "price" vorhanden sind
  all_cols <- colnames(list_crypto_clean[[x]])
  extra_cols <- setdiff(all_cols, c("time", "Price"))
  length(extra_cols) == 0
}))

# Entfernen der DataFrames ohne zusätzliche Spalten
list_crypto_clean[no_extra_columns] <- NULL



for(j in 1:length(list_crypto_clean)){
  temp_raw <- list_crypto_clean[[j]]
  #delete columns with too many NAs
  not_na<-!(colSums(is.na(temp_raw))>max_na_allowed)
  temp_fin<-temp_raw[,not_na,drop=FALSE]
}

setdiff(names(list_crypto_clean), names(crypto_data_cleaned))
setdiff(names(crypto_data_cleaned), names(list_crypto_clean))
