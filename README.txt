Code for "Predicting Value at Risk for Cryptocurrencies With Generalized Random Forests" by Buse, R., Goergen, K. and Schienle, M.


Folder structure and main files include:
	- Folder Data Section: Data_Prep.R: Preparation of data files, computation of returns and covariates, run once before to obtain necessary files for cryptocurrency application. Resulting Datasets Crypto_Data_Raw and Crypto_Data_add are stored in Cleaned_Data/Forecasts/.
	- Folder Simulations: contains code for all simulation data with seeds (to be run in parallel on linux System) as well as backtests and CPA-tests
	- Folder Forecasts: computes forecasts of cryptocurrencies for all models (to be run in parallel on linux System)
	- Folder Tests: contains code for all backtesting and CPA-test results for the cryptocurrency application
	- Various xx_Functions files: contains functions that are required for respective scripts

DATA
Data for cryptocurrency application can be downloaded from coinmetrics.io. This data is free to share and adapt under the licence CC BY-NC 4.0  (ATTRIBUTION-NONCOMMERCIAL 4.0 INTERNATIONAL, available at https://creativecommons.org/licenses/by-nc/4.0/). 
The data of the 106 cryptocurrencies from the paper that was downloaded and processed on 2024-04-07 is in the folder Cleaned_Data/Crypto_Data_Cleaned. 


COMPUTATION
The analyses were run on R 4.1.2 and we use the following packages: quantreg (5.99.1), quantregForest(7.1), doParallel (1.0.17), doSNOW (1.0.20), grf (2.4.0), DEoptim(2.2-8), ggplot2(3.5.1), reshape2(1.4.4), PerformanceAnalytics(2.0.4), rugarch(1.5-3), MSGARCH(2.51), foreach(1.5.2), GAS(0.3.4.1), tidyr(1.3.1), dplyr(1.1.4), zoo(1.8-12), xts(0.14.1), scales(1.3.0), data.table(1.16.2), xtable(1.8-4), tseries(0.10-58), dygraphs(1.1.1.6).
The paths in the files are relative to the root, so the working directory needs to be set accordingly or used as .Rproj.
The simulations and the forecasts were computed on a standard CPU with 48 kernels using parallel computing. 
The approximate runtime for the simulations with history length 500 is 2,5 days.
The approximate runtime for the simulations with history length 1000 is 4 days.
The approximate runtime for the forecasts without Caviar is 2 days.
The approximate runtime for the forecasts for Caviar and Caviar asymmetric is 1,5 days each.
The approximate runtime for the SAV Simulation with differnet covariate combinations (Table 3) is 1 day.

CLONING REPO WITH GIT LFS
Two files are uploaded with Git LFS: Cleaned_Data/Forecasts/final_data_combined_0.05_500_2024-11-20.RData and Cleaned_Data/Forecasts/final_data_0.05_2024-05-13_all.RData. In order to clone the repository correctly, it is necessary install Git LFS first.

FIGURES AND TABLES
The code for each figure and table of the paper can be found in the R-file (with path) named in brackets. The order of tables and figures in the list below is according to their appearance in the paper. 

	- Table 1: Descriptive Statistics of Cryptocurrencies (Data_Section\Plots_tables_data_section.R)
	- Figure 1: Returns of all currencies (Data_Section\Plots_tables_data_section.R)
	- Table 2: Summary of Covariates (Data_Section\Plots_tables_data_section.R)
	- Table 3: MSE Prediction Error for Covariate Combinations (Data_Section\SAV_Simulation.R)
	- Table 4: Simulation 5% VaR (Simulations\Tables_Sim.R)
	- Table 5: CPA-Tests for Simulations  (Simulations\Tables_Sim.R)
	- Table 6: P-Values for DQ-Tests for Different Time Periods (Tests\Evaluation_Main.R)
	- Figure 2: Boxplots of p-values of DQ-Tests (Tests\Evaluation_Main.R)
	- Table 7: P-Values for DQ-Tests sorted by SER (Tests\Evaluation_Main.R)
	- Table 8: CPA-Tests over different time periods without covariates (Tests\CPA_Tests.R)
	- Table 9: Difference between Covariates where GRF is better vs. worse (Tests\CPA_Tests.R)
	- Figure 3: predicted loss difference series of CPA Tests Bitcoin: CAV, GJR-GARCH, QR (Tests\Extension.R)
	- Figure 4: predicted loss difference series of CPA Tests Cardano/Tether (Tests\Extension.R)
	- Figure 5: GRF variable importance of ada, btc, usdt (Tests\Extension.R)
	- Table 10: P-Values for DQ-Tests (Tests\Evaluation_Main.R)
	- Table 11: CPA-Tests without covariates (Tests\CPA_Tests.R)
	- Table 12: Difference between Covariates where GRF is better vs. worse (Tests\CPA_Tests.R)
	Appendix:
	- Table 13+14: Overview all Cryptos (from coinmetrics.io)
	- Table 15: Non-Stationarity Tests (Data_Section\Plots_tables_data_section.R)
	- Table 16: External Covariates and Descriptions (from coinmetrics.io)
	- Figure 6: CPA-Tests of GRF-X (Tests\CPA_Tests.R)
	- Table 17: P-Values for DQ-Tests sorted by covariates (Tests\Evaluation_Main.R)
	- Table 18: CPA-Tests with GRF-X (Tests\CPA_Tests.R)
	- Table 19: Difference Covariates where GRF-X is better vs. worse (Tests\CPA_Tests.R)
	- Figure 7: CPA-Tests fourth period (Tests\CPA_Tests.R)
	- Figure 8: CPA-Tests first and second period ordered by market cap (Tests\CPA_Tests_Appendix.R)
	- Figure 9: CPA-Tests third period ordered by market cap(Tests\CPA_Tests_Appendix.R)
	- Figure 10: CPA-Tests full data set (Tests\CPA_Tests_Appendix.R)
	- Figure 11: Log-Returns for cryptos in Subsection 5.2 (Tests\Extension.R)
