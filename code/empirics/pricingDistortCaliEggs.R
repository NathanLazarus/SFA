.libPaths(file.path("bin/R/libs"))

library(data.table)
library(zoo)
library(fixest)
pacman::p_load(foreach, iterators, doSNOW)

# read in parent aggregates
parents <- readRDS("data/nielsen_extracts/parents_states_aggregate.Rds")
parents[, in_cali := max(fips_state_code==6), by = list(year, quarter, parent_code)]
parents[, in_flor := max(fips_state_code==12), by = list(year, quarter, parent_code)]
parents[, in_tex := max(fips_state_code==48), by = list(year, quarter, parent_code)]
parents[, in_wash := max(fips_state_code==53), by = list(year, quarter, parent_code)]
parents[, in_oreg := max(fips_state_code==41), by = list(year, quarter, parent_code)]
parents[, only_in_cali := in_cali & state_sales_share ==1]
parents[, parent_flag := fips_state_code==first(fips_state_code), by = list(year, quarter, parent_code)]

# read in tax rates
taxrates <- as.data.table(read_dta("data/taxrates/taxrates.dta"))
setkeyv(taxrates,c("statefips","year"))
taxrates[, dt_x := round(t_x - shift(t_x),2), by = statefips]  
taxrates[, dt_corp := round(t_corp - shift(t_corp),2), by = statefips]  
taxrates[, dwgt_sales := round(wgt_sales - shift(wgt_sales),2), by = statefips]  

# read in price data
stores_groups <- readRDS("data/nielsen_extracts/stores_groups_aggregate.Rds")
eggs <- stores_groups[group==4100 & year>=2009 & year<=2013]
eggs[, yearqtr := as.yearqtr(paste0(year,"-",quarter))]
setkeyv(eggs, c("store_code_uc","yearqtr"))
eggs[, price := revenue/quantity]
eggs[, lprice := log(price)]
eggs[, dlprice_pre := lprice - log(price[year == 2010 & quarter == 4]), .(store_code_uc)]

eggs[parents, on = c("year", "quarter", "parent_code", "fips_state_code"),`:=`(state_sales_share = i.state_sales_share,
                                                                               presence_count = i.presence_count,
                                                                               in_cali = i.in_cali,
                                                                               in_flor = i.in_flor,
                                                                               in_tex = i.in_tex,
                                                                               in_wash = i.in_wash,
                                                                               in_oreg = i.in_oreg,
                                                                               only_in_cali = i.only_in_cali,
                                                                               gross_revenue = i.revenue)]
eggs[, state_sales_share2010 := state_sales_share[year==2010 & quarter==4], .(store_code_uc)]


# tags
eggs[, post2011 := year>=2011]
eggs[, parent_quarter_tag := store_code_uc == first(store_code_uc), by = list(year, quarter, parent_code)]
#eggs[, store_tag := , by=list(store_code_uc)]

# distribution of stores in/out of cali: very small control group relative to treatment
table(eggs[year==2011 & quarter==1 & fips_state_code==6]$only_in_cali) 
table(eggs[year==2011  & quarter==1  & fips_state_code==6]$only_in_cali)/nrow(eggs[year==2011 & fips_state_code==6 & quarter==1])

# pre-2011 mean of prices is very different (oof)
mean(eggs[year==2010 & only_in_cali & fips_state_code==6]$price)
mean(eggs[year==2010 & !only_in_cali  & fips_state_code==6]$price)

# very few parents despite many stores
length(unique(eggs[fips_state_code==6]$store_code_uc))
length(unique(eggs[only_in_cali==1 & fips_state_code==6]$store_code_uc))
length(unique(eggs[only_in_cali!=1 & fips_state_code==6]$store_code_uc))

length(unique(eggs[fips_state_code==6]$parent_code))
length(unique(eggs[fips_state_code==6 & only_in_cali==1]$parent_code))
length(unique(eggs[fips_state_code==6 & only_in_cali!=1]$parent_code))

length(unique(eggs[fips_state_code==12]$parent_code))
length(unique(eggs[fips_state_code==12 & in_cali==1]$parent_code))

# similar revenues?
mean(eggs[year==2010 & quarter==4 & parent_quarter_tag==1 & only_in_cali & fips_state_code==6]$gross_revenue)
mean(eggs[year==2010 & quarter==4 & parent_quarter_tag==1 & !only_in_cali & fips_state_code==6]$gross_revenue)

mean(eggs[year==2010 & quarter==4 & parent_quarter_tag==1 & in_cali & fips_state_code==12]$gross_revenue)
mean(eggs[year==2010 & quarter==4 & parent_quarter_tag==1 & !in_cali & fips_state_code==12]$gross_revenue)

# table of firms in cali and florida
table(eggs[year==2010 & quarter==4 & parent_quarter_tag==1]$in_cali, eggs[year==2010 & quarter==4 & parent_quarter_tag==1]$in_flor)

# store appearances
eggs[, appear := .N, by = store_code_uc]

# median sales share
median_sales_share <- median(eggs[year==2010 & quarter==4 & parent_quarter_tag==1 & !only_in_cali]$state_sales_share2010)

eggs[, abv_median_ss_2010 := state_sales_share2010>=median_sales_share]

# regressions
reg1 <- feols(dlprice_pre ~ post2011 + only_in_cali + post2011*only_in_cali |  yearqtr, eggs[fips_state_code==6], cluster = "store_code_uc")
reg2 <- feols(dlprice_pre ~ post2011 + in_cali + post2011*in_cali |  yearqtr, eggs[fips_state_code!=6], cluster = "store_code_uc")
# regression with only tau=0 states
# regression with only mutli-state firms >3
reg3 <- feols(dlprice_pre ~ post2011 + (fips_state_code==6) + post2011*(fips_state_code==6) |  yearqtr,
              eggs[in_cali & !only_in_cali], cluster = "store_code_uc")

# reg with bigger treatment/control def + parent 