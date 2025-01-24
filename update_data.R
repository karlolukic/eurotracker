# Packages ----------------
library(data.table)
library(ecb)
library(magrittr)
library(xts)
library(readr)
library(here)

# Load data (outside the server) function -----------------
load_data <- function(key, is_quarterly = FALSE) {
  data <- get_data(key, list(detail = "dataonly")) %>%
    setDT()
  if (is_quarterly) {
    # Use as.yearqtr for quarterly data
    data$obstime <- as.yearqtr(data$obstime, format = "%Y-Q%q")
  } else {
    data$obstime <- convert_dates(data$obstime)
  }
  return(data)
}

# Data keys -----
inflation_key <- "ICP.M.U2.N.000000.4.ANR"
m3_key <- "BSI.M.U2.Y.V.M30.X.I.U2.2300.Z01.A"
gdp_key <- "MNA.Q.Y.I9.W2.S1.S1.B.B1GQ._Z._Z._Z.EUR.LR.GY"
unemployment_key <- "LFSI.M.I9.S.UNEHRT.TOTAL0.15_74.T"
usd_key <- "EXR.D.USD.EUR.SP00.A"
gov_debt_key <- "GFS.Q.N.I9.W0.S13.S1.C.L.LE.GD.T._Z.XDC_R_B1GQ_CY._T.F.V.N._T"

euro_rate_key <- "EST.B.EU000A2X2A25.WT" # daily
gov_bond_yield_key <- "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_10Y"
loans_to_corp_key <- "BSI.M.U2.Y.U.A20T.A.I.U2.2240.Z01.A"
loans_to_households_key <- "BSI.M.U2.Y.U.A20T.A.I.U2.2250.Z01.A"
cost_borrowing_house_key <- "MIR.M.U2.B.A2C.AM.R.A.2250.EUR.N"
cost_borrowing_corp_key <- "MIR.M.U2.B.A2I.AM.R.A.2240.EUR.N"

current_account_key <- "BPS.M.N.I9.W1.S1.S1.T.B.CA._Z._Z._Z.EUR._T._X.N.ALL"
direct_investment_key <- "BPS.M.N.I9.W1.S1.S1.T.N.FA.D.F._Z.EUR._T._X.N.ALL"
portfolio_investment_key <- "BPS.M.N.I9.W1.S1.S1.T.N.FA.P.F._Z.EUR._T.M.N.ALL"
real_effective_key <- "EXR.M.E03.EUR.ERC0.A"
reserve_assets_key <- "RAS.M.N.U2.W1.S121.S1.LE.A.FA.R.F._Z.EUR.X1._X.N.ALL"
nominal_effective_key <- "EXR.M.E03.EUR.EN00.A"

total_assets_key <- "SUP.Q.B01.W0._Z.A0000._T.SII._Z.ALL.LE.E.C"
non_perf_loans_key <- "SUP.Q.B01.W0._Z.I7005._T.SII._Z._Z._Z.PCT.C"
significant_inst_key <- "SUP.Q.B01._Z._Z.R0104._T.SII._Z._Z._Z.Z.C"
return_on_equity_key <- "SUP.Q.B01.W0._Z.I2003._T.SII._Z._Z._Z.PCT.C"
cet1_ratio_key <- "SUP.Q.B01.W0._Z.I4008._T.SII._Z._Z._Z.PCT.C"
liquidity_coverage_key <- "SUP.Q.B01.W0._Z.I3017._T.SII._Z._Z._Z.PCT.C"

# Load updated data ------
inflation_data <- load_data(inflation_key)
m3_data <- load_data(m3_key)
gdp_data <- load_data(gdp_key, is_quarterly = TRUE)
unemployment_data <- load_data(unemployment_key)
usd_data <- load_data(usd_key)
gov_debt_data <- load_data(gov_debt_key, is_quarterly = TRUE)

euro_rate_data <- load_data(euro_rate_key)
gov_bond_yield_data <- load_data(gov_bond_yield_key)
loans_to_corp_data <- load_data(loans_to_corp_key)
loans_to_households_data <- load_data(loans_to_households_key)
cost_borrowing_house_data <- load_data(cost_borrowing_house_key)
cost_borrowing_corp_data <- load_data(cost_borrowing_corp_key)

current_account_data <- load_data(current_account_key)
direct_investment_data <- load_data(direct_investment_key)
portfolio_investment_data <- load_data(portfolio_investment_key)
real_effective_data <- load_data(real_effective_key)
reserve_assets_data <- load_data(reserve_assets_key)
nominal_effective_data <- load_data(nominal_effective_key)

total_assets_data <- load_data(total_assets_key, is_quarterly = TRUE)
non_perf_loans_data <- load_data(non_perf_loans_key, is_quarterly = TRUE)
significant_inst_data <- load_data(significant_inst_key, is_quarterly = TRUE)
return_on_equity_data <- load_data(return_on_equity_key, is_quarterly = TRUE)
cet1_ratio_data <- load_data(cet1_ratio_key, is_quarterly = TRUE)
liquidity_coverage_data <- load_data(liquidity_coverage_key, is_quarterly = TRUE)

# Save the updated data -------
write_rds(inflation_data, file = here("data", "inflation_data.rds"), compress = "gz")
write_rds(m3_data, file = here("data", "m3_data.rds"), compress = "gz")
write_rds(gdp_data, file = here("data", "gdp_data.rds"), compress = "gz")
write_rds(unemployment_data, file = here("data", "unemployment_data.rds"), compress = "gz")
write_rds(usd_data, file = here("data", "usd_data.rds"), compress = "gz")
write_rds(gov_debt_data, file = here("data", "gov_debt_data.rds"), compress = "gz")

write_rds(euro_rate_data, file = here("data", "euro_rate_data.rds"), compress = "gz")
write_rds(gov_bond_yield_data, file = here("data", "gov_bond_yield_data.rds"), compress = "gz")
write_rds(loans_to_corp_data, file = here("data", "loans_to_corp_data.rds"), compress = "gz")
write_rds(loans_to_households_data, file = here("data", "loans_to_households_data.rds"), compress = "gz")
write_rds(cost_borrowing_house_data, file = here("data", "cost_borrowing_house_data.rds"), compress = "gz")
write_rds(cost_borrowing_corp_data, file = here("data", "cost_borrowing_corp_data.rds"), compress = "gz")

write_rds(current_account_data, file = here("data", "current_account_data.rds"), compress = "gz")
write_rds(direct_investment_data, file = here("data", "direct_investment_data.rds"), compress = "gz")
write_rds(portfolio_investment_data, file = here("data", "portfolio_investment_data.rds"), compress = "gz")
write_rds(real_effective_data, file = here("data", "real_effective_data.rds"), compress = "gz")
write_rds(reserve_assets_data, file = here("data", "reserve_assets_data.rds"), compress = "gz")
write_rds(nominal_effective_data, file = here("data", "nominal_effective_data.rds"), compress = "gz")

write_rds(total_assets_data, file = here("data", "total_assets_data.rds"), compress = "gz")
write_rds(non_perf_loans_data, file = here("data", "non_perf_loans_data.rds"), compress = "gz")
write_rds(significant_inst_data, file = here("data", "significant_inst_data.rds"), compress = "gz")
write_rds(return_on_equity_data, file = here("data", "return_on_equity_data.rds"), compress = "gz")
write_rds(cet1_ratio_data, file = here("data", "cet1_ratio_data.rds"), compress = "gz")
write_rds(liquidity_coverage_data, file = here("data", "liquidity_coverage_data.rds"), compress = "gz")
