library(plotly)
library(TSstudio)
library(tsbox)
library(forecast)
library(dynlm)
library(corrplot)
library(readxl)
#library(tidyverse)
library(zoo)

dt <- read_excel("dt_usa.xlsx")
dt$...1 <- NULL

dt$inf_defl_ld <- c(NA,diff(log(dt$inf_defl))*100)
dt$inf_cpi_nofe_ld <- c(NA,diff(log(dt$inf_cpi_nofe))*100)
dt$rgdpld <- c(NA,diff(log(dt$rgdp))*100)
dt$ngdpld <- c(NA,diff(log(dt$ngdp))*100)
dt$prod_roh1_ld <- c(NA,diff(log(dt$prod_roh1))*100)
dt$prod_tfp_ld <- c(NA,diff(log(dt$prod_tfp))*100)
dt$wages_ld <- c(NA,diff(log(dt$wages))*100)
dt$m2_ld <- c(NA,diff(log(dt$m2))*100)
dt$unemplev_ld <- c(NA,diff(log(dt$unemplev))*100)

inf_defl <- ts(dt$inf_defl, start=1968, end=2019)
inf_cpi_nofe <- ts(dt$inf_cpi_nofe, start=1968, end=2019)
inf_cpi_stick <- ts(dt$inf_cpi_stick[-1], start=1969, end=2019)
rgdp <- ts(dt$rgdp, start=1968, end=2019)
ngdp <- ts(dt$ngdp, start=1968, end=2019)
prod_roh1 <- ts(dt$prod_roh1, start=1968, end=2019)
prod_roh2 <- ts(dt$prod_roh2[-1], start=1969, end=2019)
prod_tfp <- ts(dt$prod_tfp, start=1968, end=2019)
wages <- ts(dt$wages, start=1968, end=2019)
m2 <- ts(dt$m2, start=1968, end=2019)
unemplev <- ts(dt$unemplev, start=1968, end=2019)
unemprate_civ <- ts(dt$unemprate_civ[-1], start=1969, end=2019)

date <- ts(dt$date[2:52], start=1969, end=2019)
inf_defl_ld <- ts(dt$inf_defl_ld[-1], start=1969, end=2019)
inf_cpi_nofe_ld <- ts(dt$inf_cpi_nofe_ld[-1], start=1969, end=2019)
rgdpld <- ts(dt$rgdpld[-1], start=1969, end=2019)
ngdpld <- ts(dt$ngdpld[-1], start=1969, end=2019)
prod_roh1_ld <- ts(dt$prod_roh1_ld[-1], start=1969, end=2019)
wages_ld <- ts(dt$wages_ld[-1], start=1969, end=2019)
m2_ld <- ts(dt$m2_ld[-1], start=1969, end=2019)
prod_tfp_ld <- ts(dt$prod_tfp_ld[-1], start=1969, end=2019)
unemplev_ld <- ts(dt$unemplev_ld[-1], start=1969, end=2019)

TSstudio::ts_plot(ts_c(inf_defl_ld, inf_cpi_nofe_ld, inf_cpi_stick, rgdpld, wages_ld, ngdpld,
                       m2_ld, lag(m2_ld,-1), lag(m2_ld,-2), lag(m2_ld,-3), lag(m2_ld,-4), lag(m2_ld,-5))
                  )

cor_m2_inf <- ccf(m2_ld, inf_defl_ld, lag.max = 10)
# notice the negative lags only, positive ones don't have sense in this case
# inflation is higher 3 years later than a growth in M2
cor_m2_inf[-3,1]


cor_prod_inf <- ccf(prod_roh1_ld, inf_defl_ld, lag.max = 10)
# negative correlation at lag 0
cor_prod_inf[0,1]

cor_wag_prod <- ccf(wages_ld, inf_defl_ld, lag.max = 10) # nominal wages
cor_wag_prod[0,1]

cor_wag_prod <- ccf(wages_ld, prod_roh1_ld, lag.max = 10)
cor_wag_prod[0,1]

cor_ngdpld_prod <- ccf(ngdpld, prod_roh1_ld, lag.max = 10) # nominal income growth
cor_ngdpld_prod[0,1]

cor_unr_inf <- ccf(unemprate_civ, inf_defl_ld, lag.max = 10)
cor_unr_inf[0,1]

infl <- tslm(inf_defl_ld~prod_roh1_ld+lag(m2_ld, -3))
summary(infl)

infl <- tslm(window(inf_defl_ld, start = 1972, end = 2000)~window(prod_roh1_ld, start = 1972,end = 2000)+window(lag(m2_ld, -3), end = 2000))
summary(infl)

cor_m2_prod <- ccf(window(m2_ld, start = 2000, end = 2019), window(inf_defl_ld, start = 2000, end = 2019), lag.max = 10)
infl <- tslm(window(inf_defl_ld, start = 1995, end = 2008)~window(prod_roh1_ld, start = 1995, end = 2008)+window(lag(m2_ld, -2), start = 1995, end = 2008))
summary(infl)


unempl <- dynlm(inf_defl_ld~., data.frame(lag(unemprate_civ,2)))
summary(unempl)



ma_prod <- rollmean(prod_roh1_ld, 3, align = "right")
ma_M2 <- rollmean(m2_ld, 3, align = "right")

TSstudio::ts_plot(ts_c(inf_defl_ld, ma_prod,prod_roh1_ld, m2_ld, ma_M2))


# TABLE 1

case1.1a <- tslm(window(inf_defl_ld, start = 1971)~ma_prod)
summary(case1.1a)

case1.1b <- tslm(window(inf_defl_ld, start = 1971, end = 2000)~window(ma_prod, end = 2000))
summary(case1.1b) # same results as Kiley paper

case1.1c <- tslm(window(inf_defl_ld, start = 2000)~window(ma_prod, start = 2000))
summary(case1.1c)

case1.2 <- tslm(window(inf_defl_ld, start = 1971)~window(unemprate_civ, start = 1970))
summary(case1.2)

case1.2b <- tslm(window(inf_defl_ld, start = 1971, end = 2000)~window(unemprate_civ, start = 1970, end = 2000))
summary(case1.2b) # same results as Kiley paper

case1.2c <- tslm(window(inf_defl_ld, start = 2000)~window(unemprate_civ, start = 2000))
summary(case1.2c)

case1.3 <- tslm(window(inf_defl_ld, start = 1972)~window(lag(ma_M2, -2), end = 2019))
summary(case1.3)

case1.3b <- tslm(window(inf_defl_ld, start = 1972, end = 2000)~window(lag(ma_M2, -2), end = 2000))
summary(case1.3b) # not same result because of lag on ma_M2 that Kiley did not consider

case1.3c <- tslm(window(inf_defl_ld, start = 2000)~window(lag(ma_M2, -4), start = 2000, end = 2019))
summary(case1.3c)

# TABLE 2

case2.2 <- tslm(window(inf_defl_ld, start = 1971, end = 2000)~window(ma_prod, start = 1970, end = 2000)+window(lag(inf_defl_ld,-1), end = 2000))
summary(case2.2)

case2.3a <- tslm(window(inf_defl_ld, start = 1971)~ma_prod+window(lag(inf_defl_ld,-1), end = 2019)+window(unemprate_civ, start = 1971))
summary(case2.3a)

case2.3b <- tslm(window(inf_defl_ld, start = 1971, end = 2000)~window(ma_prod, start = 1971, end = 2000)+window(lag(inf_defl_ld,-1), end = 2000)+window(unemprate_civ, start = 1970, end =2000))
summary(case2.3b)

case2.3c <- tslm(window(inf_defl_ld, start = 2000, end = 2019)~window(ma_prod, start = 2000, end = 2019)+window(lag(inf_defl_ld,-1), start = 2000, end = 2019)+window(unemprate_civ, start = 2000, end = 2019))
summary(case2.3c)

tpc <- tslm(window(inf_defl_ld, start = 1971)~window(lag(inf_defl_ld,-1), end = 2019)+window(unemprate_civ, start = 1970))
summary(tpc)

tpc <- tslm(window(inf_defl_ld, start = 1971, end = 2000)~window(lag(inf_defl_ld,-1), end = 2000)+window(unemprate_civ, start = 1971, end = 2000))
summary(tpc)

tpc <- tslm(window(inf_defl_ld, start = 2000)~window(lag(inf_defl_ld,-1), start = 2000, end = 2019)+window(unemprate_civ, start = 2000))
summary(tpc)


# Empirical Implication 2

wag <- tslm(window(wages_ld, start = 1971)~ma_prod)
summary(wag)

wag <- tslm(window(wages_ld, start = 1971, end = 2000)~window(ma_prod, end = 2000))
summary(wag)

cor_ma_XBH_PLld <- ccf(window(ma_prod, start = 2000), window(wages_ld, start = 2000), lag.max = 10)

wag <- tslm(window(wages_ld, start = 2000)~window(ma_prod, start = 2000))
summary(wag)

out <- tslm(window(ngdpld, start = 1971)~ma_prod)
summary(out)

out <- tslm(window(ngdpld, start = 1971, end = 2000)~window(ma_prod, end = 2000))
summary(out)

out <- tslm(window(ngdpld, start = 2000)~window(ma_prod, start = 2000))
summary(out)

