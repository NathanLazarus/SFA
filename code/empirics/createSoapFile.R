# This file creates an .Rds file with soap sales by store and year. It takes the
# most common extras groupings for soap, and merges in the state tax data and parent data

.libPaths(file.path("/nfs/econlab001/lazarus-factor/bin/R/libs"))

library(data.table)
library(haven)
library(zoo)
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

# category numbers
category_numbers <- unique(foreach(year = 2006:2016, .combine = append) %do% {
  substr(list.files(paste0("data/nielsen_extracts/RMS/",year,"/Clean_Files")), 7, 10)
})


zero_tax <- taxrates[year==2011, list(fips_state_code = statefips,zerotax = t_corp==0)]

# soap data
extras_7035 <- readRDS("data/nielsen_extracts/product_extras_collapse/extras_7035.Rds")
extras_7035[, count := .N, by = extras_group]
extras_7035 <- extras_7035[count>700000 & year>=2009 & year<=2013]
extras_7035[,count:=NULL]


extras_7035[, yearqtr := as.yearqtr(paste0(year,"-",quarter))]
setkeyv(extras_7035, c("store_code_uc","yearqtr","extras_group"))
extras_7035[, price := revenue/quantity]
extras_7035[, lprice := log(price)]
extras_7035[, dlprice_pre := lprice - log(price[year == 2010 & quarter == 4]), .(store_code_uc, extras_group)]

extras_7035[parents, on = c("year", "quarter", "parent_code", "fips_state_code"),`:=`(state_sales_share = i.state_sales_share,
                                                                                   presence_count = i.presence_count,
                                                                                   in_cali = i.in_cali,
                                                                                   only_in_cali = i.only_in_cali,
                                                                                   gross_revenue = i.revenue,
                                                                                   cali_sales_share = i.cali_sales_share)]
extras_7035[zero_tax, on = "fips_state_code", `:=`(zerotax = i.zerotax)]

extras_7035[, cali_sales_share_pre := mean(cali_sales_share[year<2011]), .(store_code_uc)]

# tags
extras_7035[, post2011 := year>=2011]
extras_7035[, parent_quarter_tag := store_code_uc == first(store_code_uc), by = list(year, quarter, parent_code)]

saveRDS(extras_7035, "data/nielsen_extracts/soap_test_file.Rds") 