# summary statistics on distribution of stores, parents and geography

.libPaths(file.path("bin/R/libs"))

library(data.table)
library(fixest)
pacman::p_load(foreach, iterators, doSNOW)

stores <- foreach(year = 2006:2016, .combine = rbind) %do% {
  fread(paste0("data/nielsen_extracts/RMS/",year,"/Annual_Files/stores_",year,".tsv"))
}

# Out of 2449 chains in the data in 2011, 65 appear in california. 18 of those are single-state firms
# For the other 47, they make on average 32% of their sales in california and are present in ~24 other states
# Firms with presence in California account for 42% of total revenue, and california itself is 13% of revenue

parents <- readRDS("data/nielsen_extracts/parents_states_aggregate.Rds")
parents[, in_cali := max(fips_state_code==6), by = list(year, quarter, parent_code)]
parents[, only_in_cali := in_cali & state_sales_share ==1]
parents[, parent_flag := fips_state_code==first(fips_state_code), by = list(year, quarter, parent_code)]

nrow(parents[year==2011 & quarter==1])
nrow(parents[year==2011 & quarter==1 & fips_state_code==6])
nrow(parents[year==2011 & quarter==1 &  only_in_cali])

sum(parents[year==2011 & quarter==1 & in_cali]$revenue)/sum(parents[year==2011 & quarter==1]$revenue)
sum(parents[year==2011 & quarter==1 & fips_state_code==6]$revenue)/sum(parents[year==2011 & quarter==1]$revenue)


mean(parents[year==2011 & quarter==1 & in_cali & !only_in_cali & parent_flag]$presence_count)
mean(parents[year==2011 & quarter==1 & fips_state_code==6 & !only_in_cali]$state_sales_share)


# 82% of stores appear in all years
stores[, appearances := .N, by = list(store_code_uc)]
table(stores$appearances)/nrow(stores)

# 60% of parent-state pairs in each year have persistence in the number of stores within a state over time. 82% are within 5 stores of average.
setkeyv(stores, c("parent_code","year","store_code_uc"))
    
stores[, avg_num_stores_in_state := mean(num_stores_in_state), by = list(parent_code, fips_state_code)]
stores[, persistence_in_state := num_stores_in_state/avg_num_stores_in_state]

nrow(stores[persistence_in_state>0.95 & persistence_in_state<1.05 & parent_state_flag])/nrow(stores[parent_state_flag==1])
nrow(stores[abs(num_stores_in_state-avg_num_stores_in_state)<5 & parent_state_flag])/nrow(stores[parent_state_flag==1])

# florida & texas are high sales states for cali firms
x <- parents[in_cali==1 & fips_state_code!=6 ,list(tot_sales = sum(revenue)), by = list(year, quarter, fips_state_code)][
  , sales_share := tot_sales/sum(tot_sales), by=list(year, quarter)]

