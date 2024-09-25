# RUN  source("testing_Exchange.r")
rm(list=ls())
require(graphics)
library("matrixStats") # needed for function "colSds" to work
source("functions_Exchange.r")

#----------------------------------------------------------------
#  H0: et=sqrt(ht)zt	H1: et=sqrt(ht)sqrt(gt)zt, where gt=1+d*Gt
#  Test 1:	H0: gamma=0 i.e d0=d1=d2=d3=0
#----------------------------------------------------------------
#  H0: et=zt	H1: et=sqrt(ht)sqrt(gt)zt, where gt=1+d*Gt and ht=1
#  Test 2:	H0: gamma=0 i.e d1=d2=d3=0
#----------------------------------------------------------------



# -------------------------------------------------------------------------------------------------
# READ IN DATA
# -------------------------------------------------------------------------------------------------

# Meine eigenen Daten reinladen
ex.Data <- read.csv("/Users/niklaskorn/Desktop/Bachelorarbeit/Data Analytics/Results/Data Section/price_data.csv", header = TRUE)
# Erstellen der Ergebnistabelle
Results <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(Results) <- c("p(Test1)", "p(Test1 robust)", "p(Test2)")

for (i in 2:90) {
  
  data_column <- ex.Data[,c(1,i)]
  data_column <- na.omit(data_column)
  column.name <- colnames(data_column)[2]  # Spaltenname von data_column extrahieren
  if (column.name == 'X1inch') {
    column.name <- '1inch'
  }
  dates <- data_column[2:NROW(data_column),1]
  data_column <- as.vector(data_column[,2])
  e.DATA <- (log(data_column[2:length(data_column)]) - log(data_column[1:(length(data_column)-1)])) * 100
  dates <- as.Date(dates, "%Y-%m-%d")
  
  
  # -------------------------------------------------------------------------------------------------
  # CHOOSE DATA (Indonesia / Korea / Taiwan)
  # -------------------------------------------------------------------------------------------------

  e <- e.DATA
  
  # -------------------------------------------------------------------------------------------------
  # PLOT DATA
  # -------------------------------------------------------------------------------------------------
  plot(dates, e, type="l", xaxt = "n", ann=FALSE)
  axis.Date(side = 1, dates, format = "%Y")
  
  # -------------------------------------------------------------------------------------------------
  # TEST 1 - assume GARCH, then test if TV=1
  # -------------------------------------------------------------------------------------------------
  T <- NROW(e)
  #startpar = c(0.05,0.03,0.8)
  startpar = c(0.05,0.06,0.8) # indonesia
  mycontrol = list(trace=0,fnscale=-1,parscale=rep(1,NROW(startpar)),ndeps=rep(1e-5,NROW(startpar)),maxit=500,reltol=1e-7,REPORT=1)
  result <- myEstimate.GARCH(startpar,myLogLik.GARCH,myScore.GARCH,e,"Nelder-Mead",mycontrol)
  final.par1 <- result[2:4]
  final.par1 <- cbind(t(final.par1),result[3]+result[4])
  estimation1.ok = (result[1]==0)
  
  test1 = 0
  test1 <- myTest.TV.TR2(final.par1[1:3],e)[1:2]
  if (estimation1.ok==0) cat("\n Estimation failed. ")
  
  test3 <- myTest.TV.robust(final.par1[1:3],e)[1:2]
  
  # -------------------------------------------------------------------------------------------------
  # TEST 2 - assume no GARCH, test if TV=1 directly
  # -------------------------------------------------------------------------------------------------
  hBAR <- 1
  psi2 <- e^2  # psi2 <- e^2/hBAR
  final.par2 <- (1/T)*sum(psi2)
  estimation2.ok <- is.finite(final.par2)
  
  test2 = 0
  test2 <- myTest.TVfix.TR2(final.par2,psi2)[1:2]
  if (estimation2.ok==0) cat("\n Estimation failed.")
  
  
  # -------------------------------------------------------------------------------------------------
  # PRINT RESULTS TO SCREEN
  # -------------------------------------------------------------------------------------------------
  
  cat("\n T = ",NROW(e))
  cat("\n Analytical Score - rejection frequency")
  cat("\n TEST 1 - assume GARCH, then test if TV=1")
  cat("\n omega = ",final.par1[1],"  alpha = ",final.par1[2],"  beta = ",final.par1[3],"  alpha+beta = ",final.par1[4])
  cat("\n estimation ok? : ",estimation1.ok)
  cat("\n test statistic TR2 = ",test1)
  cat("\n test statistic TR2 (robust) = ",test3)
  cat("\n ")
  cat("\n TEST 2 - assume no GARCH, test if TV=1 directly")
  cat("\n delta0 = ",final.par2)
  cat("\n estimation ok? : ",estimation2.ok)
  cat("\n test statistic TR2 = ",test2)
  cat("\n ")
  
  Results[nrow(Results) + 1, ] <- c(test1[2], test3[2], test2[2])
  row.names(Results)[nrow(Results)] <- column.name
}
Results['eos_eth',] <- c(0,0,0)
Results <- Results[order(rownames(Results)), ]
write.csv(Results, file = "LM_Test_Results.csv", row.names = TRUE)
