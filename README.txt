Code for "Predicting Value at Risk for Cryptocurrencies With Generalized Random Forests" by Buse, R., Goergen, K. and Schienle, M.


Folder structure and main files include:
	- Folder Data Section: Load_and_prep_data.R: Preparation of data files, run once before to obtain necessary files for cryptocurrency application
	- Folder Simulations: contains code for all simulation data with seeds (to be run in parallel on linux System) as well as backtests and CPA-tests
	- Folder Forecasts: computes forecasts of cryptocurrencies for all models (to be run in parallel on linux System)
	- Folder Tests: contains code for all backtesting and CPA-test results for the cryptocurrency application
	- Various functions_xx files: contains functions that are required for respective scripts


Data for cryptocurrency application can be downloaded from coinmetrics.io.
Data for simulations is replicable with given seed.


The code for each figure and table of the paper can be in the Folder named in brackets. The order of tables and figures is according to their appearance in the paper. 

	- Table 1: Descriptive Statistics of Cryptocurrencies (from coinmetrics.io)
	- Figure 1: Returns of all currencies (from coinmetrics.io)
	- Table 2: Summary of Covariates (from coinmetrics.io)
	- Table 3: MSE Prediction Error for Covariate Combinations (Simulations)
	- Table 4: Simulation 5% VaR (Simulations\Tables_Sim)
	- Table 5: CPA-Tests for Simulations  (Simulations\Tables_Sim)
	- Table 6: P-Values for DQ-Tests for Different Time Periods (Tests\Evaluation_Main)
	- Figure 2: Boxplots of p-values of DQ-Tests (Tests\Evaluation_Main)
	- Table 7: P-Values for DQ-Tests sorted by SER (Tests\Evaluation_Main)
	- Table 8: CPA-Tests over different time periods without covariates (Tests\CPA_Tests)
	- Table 9: Difference between Covariates where GRF is better vs. worse (Tests\CPA_Tests)
	- Figure 3: predicted loss difference series of CPA Tests Bitcoin: CAG, GJR-GARCH, QR (Tests\Tests_Extension)
	- Figure 4: predicted loss difference series of CPA Tests Cardano/Tether (Tests\Tests_Extension)
	- Figure 5: GRF variable importance of ada, btc, usdt (Tests\Tests_Extension)
	- Table 10: P-Values for DQ-Tests (Tests\Evaluation_Main)
	- Table 11: CPA-Tests without covariates (Tests\CPA_Tests)
	- Table 12: Difference between Covariates where GRF is better vs. worse (Tests\CPA_Tests)
	Appendix:
	- Table 13: Non-Stationarity Tests (standard commands on coinmetrics.io-data)
	- Figure 6: CPA-Tests of GRF-X (Tests\CPA_Tests)
	- Table 14: Overview all Cryptos (from coinmetrics.io)
	- Table 15: External Covariates and Descriptions (from coinmetrics.io)
	- Table 16: P-Values for DQ-Tests sorted by covariates (Tests\Evaluation_Main)
	- Table 17: CPA-Tests with GRF-X (Tests\CPA_Tests)
	- Table 18: Difference Covariates where GRF-X is better vs. worse (Tests\CPA_Tests)
	- Figure 7: CPA-Tests fourth period (Tests\CPA_Tests)
	- Figure 8: CPA-Tests first and second period (Tests\CPA_Tests)
	- Figure 9: CPA-Tests third period (Tests\CPA_Tests)
	- Figure 10: CPA-Tests full data set (Tests\CPA_Tests)
	- Figure 11: Log-Returns for cryptos in Subsection 5.2 (from coinmetrics.io)
