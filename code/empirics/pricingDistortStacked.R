.libPaths(file.path("bin/R/libs"))

library(data.table)
library(zoo)
library(fixest)
library(haven)
pacman::p_load(foreach, iterators, doSNOW)


# read in parent aggregates
parents <- readRDS("data/nielsen_extracts/parents_states_aggregate.Rds")
setkeyv(parents, c("year","quarter","parent_code","fips_state_code"))
parents[, in_cali := max(fips_state_code==6), by = list(year, quarter, parent_code)]
parents[, in_flor := max(fips_state_code==12), by = list(year, quarter, parent_code)]
parents[, in_tex := max(fips_state_code==48), by = list(year, quarter, parent_code)]
parents[, in_wash := max(fips_state_code==53), by = list(year, quarter, parent_code)]
parents[, in_oreg := max(fips_state_code==41), by = list(year, quarter, parent_code)]
parents[, only_in_cali := in_cali & state_sales_share ==1]
parents[, parent_flag := fips_state_code==first(fips_state_code), by = list(year, quarter, parent_code)]
parents[, cali_sales_share := -1]
parents[, cali_sales_share := ifelse(in_cali,state_sales_share[fips_state_code==6],0), by = list(year, quarter, parent_code)]
parents[, tot_rev := sum(revenue), by = list(year, quarter, parent_code)]

# read in tax rates
taxrates <- as.data.table(read_dta("data/taxrates/taxrates.dta"))
setkeyv(taxrates,c("statefips","year"))
taxrates[, dt_x := round(t_x - shift(t_x),2), by = statefips]  
taxrates[, dt_corp := round(t_corp - shift(t_corp),2), by = statefips]  
taxrates[, dwgt_sales := round(wgt_sales - shift(wgt_sales),2), by = statefips]  

zero_tax <- taxrates[year==2011, list(fips_state_code = statefips,zerotax = t_corp==0)]

taxrate_changes <- taxrates[dwgt_sales>=10 & year>=2008]