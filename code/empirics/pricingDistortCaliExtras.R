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


# category numbers
category_numbers <- unique(foreach(year = 2006:2016, .combine = append) %do% {
  substr(list.files(paste0("data/nielsen_extracts/RMS/",year,"/Clean_Files")), 7, 10)
})

# read in tax rates
taxrates <- as.data.table(read_dta("data/taxrates/taxrates.dta"))
setkeyv(taxrates,c("statefips","year"))
taxrates[, dt_x := round(t_x - shift(t_x),2), by = statefips]  
taxrates[, dt_corp := round(t_corp - shift(t_corp),2), by = statefips]  
taxrates[, dwgt_sales := round(wgt_sales - shift(wgt_sales),2), by = statefips]  

zero_tax <- taxrates[year==2011, list(fips_state_code = statefips,zerotax = t_corp==0)]

#uniform price check
unif <- foreach(cn = category_numbers, .combine = rbind) %do% {
  unif <- readRDS(paste0("data/nielsen_extracts/uniform_check/uni_",cn,".Rds"))[, group := cn]
  unif <- unif[year>=2009 & year<=2013,list(hhi = weighted.mean(hhi,total_revenue)), by = list(year, quarter, group, extras_group)]
  unif[, list(hhi = mean(hhi)), by = list(group,extras_group)]
}
unif[, quartile :=  cut(hhi, breaks = quantile(hhi, probs = 0:5/5),
                        labels = FALSE, include.lowest = TRUE)]


# reg results
clusters = makeCluster(12)
registerDoSNOW(clusters)
results_group <- foreach(cn = category_numbers, .packages = c('data.table','jtools','fixest', 'zoo'), .combine = rbind) %dopar% {
  products <- readRDS(paste0("data/nielsen_extracts/product_extras_collapse/extras_",cn,".Rds"))[year>=2009 & year<=2013]
  
  products[, yearqtr := as.yearqtr(paste0(year,"-",quarter))]
  setkeyv(products, c("store_code_uc","yearqtr","extras_group"))
  products[, price := revenue/quantity]
  products[, lprice := log(price)]
  products[, dlprice_pre := lprice - log(price[year == 2010 & quarter == 4]), .(store_code_uc, extras_group)]
  
  products[parents, on = c("year", "quarter", "parent_code", "fips_state_code"),`:=`(state_sales_share = i.state_sales_share,
                                                                                     presence_count = i.presence_count,
                                                                                     in_cali = i.in_cali,
                                                                                     only_in_cali = i.only_in_cali,
                                                                                     gross_revenue = i.revenue,
                                                                                     cali_sales_share = i.cali_sales_share)]
  products[zero_tax, on = "fips_state_code", `:=`(zerotax = i.zerotax)]
  
  products[, cali_sales_share_pre := mean(cali_sales_share[year<2011]), .(store_code_uc)]
  
  # tags
  products[, post2011 := year>=2011]
  products[, parent_quarter_tag := store_code_uc == first(store_code_uc), by = list(year, quarter, parent_code)]
  
  
  reg1 <- NULL
  reg2 <- NULL
  reg3 <- NULL
  reg4 <- NULL
  reg2a <- NULL
  reg3a <- NULL
  reg4a <- NULL
  reg5 <- NULL
  reg6 <- NULL
  reg7 <- NULL
  
  
  tryCatch(
    {
      reg1 <- feols(dlprice_pre ~ post2011 + only_in_cali + post2011*only_in_cali | extras_group^yearqtr, products[fips_state_code==6], cluster = "store_code_uc")
    },
    error=function(cond) {
      reg1 <- NULL
      return(NULL)
    },
    warning=function(cond) {
      return(NULL)
    }
  )    
  
  tryCatch(
    {
      reg2 <- feols(dlprice_pre ~ post2011 + in_cali + post2011*in_cali |  extras_group^yearqtr, products[fips_state_code!=6], cluster = "store_code_uc")
    },
    error=function(cond) {
      reg2 <- NULL
      return(NULL)
    },
    warning=function(cond) {
      return(NULL)
    }
  )    
  
  tryCatch(
    {
      reg3 <- feols(dlprice_pre ~ post2011 + in_cali + post2011*in_cali | extras_group^yearqtr, products[fips_state_code!=6 & zerotax], cluster = "store_code_uc")
    },
    error=function(cond) {
      reg3 <- NULL
      return(NULL)
    },
    warning=function(cond) {
      return(NULL)
    }
  )    
  
  tryCatch(
    {
      reg4 <- feols(dlprice_pre ~ post2011 + in_cali + post2011*in_cali | extras_group^yearqtr, products[fips_state_code!=6 & presence_count>=2 & presence_count<=5], cluster = "store_code_uc")
    },
    error=function(cond) {
      reg4 <- NULL
      return(NULL)
    },
    warning=function(cond) {
      return(NULL)
    }
  )    
  
  tryCatch(
    {
      reg2a <- feols(dlprice_pre ~ post2011 + cali_sales_share_pre + post2011*cali_sales_share_pre | extras_group^yearqtr, products[fips_state_code!=6], cluster = "store_code_uc")
    },
    error=function(cond) {
      reg2a <- NULL
      return(NULL)
    },
    warning=function(cond) {
      return(NULL)
    }
  )    
  
  tryCatch(
    {
      reg3a <- feols(dlprice_pre ~ post2011 + cali_sales_share_pre + post2011*cali_sales_share_pre | extras_group^yearqtr, products[fips_state_code!=6 & zerotax], cluster = "store_code_uc")
    },
    error=function(cond) {
      reg3a <- NULL
      return(NULL)
    },
    warning=function(cond) {
      return(NULL)
    }
  )    
  
  tryCatch(
    {
      reg4a <- feols(dlprice_pre ~ post2011 + cali_sales_share_pre + post2011*cali_sales_share_pre | extras_group^yearqtr, products[fips_state_code!=6 & presence_count>=2 & presence_count<=5], cluster = "store_code_uc")
    },
    error=function(cond) {
      reg4a <- NULL
      return(NULL)
    },
    warning=function(cond) {
      return(NULL)
    }
  )    
  
  tryCatch(
    {
      reg5 <- feols(dlprice_pre ~ post2011 + in_cali + post2011*in_cali | extras_group^yearqtr, products[fips_state_code!=6 & zerotax & presence_count>=2 & presence_count<=5], cluster = "store_code_uc")
    },
    error=function(cond) {
      reg5 <- NULL
      return(NULL)
    },
    warning=function(cond) {
      return(NULL)
    }
  )        
  tryCatch(
    {
      reg6 <- feols(dlprice_pre ~ post2011 + (fips_state_code==6) + post2011*(fips_state_code==6) | extras_group^parent_code^yearqtr,
                    products[in_cali & !only_in_cali], cluster = "store_code_uc")
    },
    error=function(cond) {
      reg6 <- NULL
      return(NULL)
    },
    warning=function(cond) {
      return(NULL)
    }
  )
  
  tryCatch(
    {
      reg7 <- feols(dlprice_pre ~ post2011 + (fips_state_code==6) + post2011*(fips_state_code==6) | extras_group^parent_code^yearqtr,
                    products[in_cali & !only_in_cali & presence_count<=5], cluster = "store_code_uc")
    },
    error=function(cond) {
      reg7 <- NULL
      return(NULL)
    },
    warning=function(cond) {
      return(NULL)
    }
  )    
  
  results_group <- c()
  results_group$group <- cn
  
  if (!is.null(reg1)) {
    results_group$r1_b <- as.numeric(reg1$coefficients[2])
    results_group$r1_se <- as.numeric(reg1$se[2])
    results_group$r1_N <- reg1$nobs
    results_group$r1_r2 <- as.numeric(r2(reg1, "war2"))
  } else {
    results_group$r1_b <- NA
    results_group$r1_se <- NA
    results_group$r1_N <- NA
    results_group$r1_r2 <- NA
  }
  
  if (!is.null(reg2)) {
    results_group$r2_b <- as.numeric(reg2$coefficients[2])
    results_group$r2_se <- as.numeric(reg2$se[2])
    results_group$r2_N <- reg2$nobs
    results_group$r2_r2 <- as.numeric(r2(reg2, "war2"))
  } else {
    results_group$r2_b <- NA
    results_group$r2_se <- NA
    results_group$r2_N <- NA
    results_group$r2_r2 <- NA
  }
  
  if (!is.null(reg3)) {
    results_group$r3_b <- as.numeric(reg3$coefficients[2])
    results_group$r3_se <- as.numeric(reg3$se[2])
    results_group$r3_N <- reg3$nobs
    results_group$r3_r2 <- as.numeric(r2(reg3, "war2"))
  } else {
    results_group$r3_b <- NA
    results_group$r3_se <- NA
    results_group$r3_N <- NA
    results_group$r3_r2 <- NA
  }
  
  if (!is.null(reg4)) {
    results_group$r4_b <- as.numeric(reg4$coefficients[2])
    results_group$r4_se <- as.numeric(reg4$se[2])
    results_group$r4_N <- reg4$nobs
    results_group$r4_r2 <- as.numeric(r2(reg4, "war2"))
  } else {
    results_group$r4_b <- NA
    results_group$r4_se <- NA
    results_group$r4_N <- NA
    results_group$r4_r2 <- NA
  }
  
  if (!is.null(reg2a)) {
    results_group$r2a_b <- as.numeric(reg2a$coefficients[2])
    results_group$r2a_se <- as.numeric(reg2a$se[2])
    results_group$r2a_N <- reg2a$nobs
    results_group$r2a_r2 <- as.numeric(r2(reg2a, "war2"))
  } else {
    results_group$r2a_b <- NA
    results_group$r2a_se <- NA
    results_group$r2a_N <- NA
    results_group$r2a_r2 <- NA
  }
  
  if (!is.null(reg3a)) {
    results_group$r3a_b <- as.numeric(reg3a$coefficients[2])
    results_group$r3a_se <- as.numeric(reg3a$se[2])
    results_group$r3a_N <- reg3a$nobs
    results_group$r3a_r2 <- as.numeric(r2(reg3a, "war2"))
  } else {
    results_group$r3a_b <- NA
    results_group$r3a_se <- NA
    results_group$r3a_N <- NA
    results_group$r3a_r2 <- NA
  }
  
  if (!is.null(reg4a)) {
    results_group$r4a_b <- as.numeric(reg4a$coefficients[2])
    results_group$r4a_se <- as.numeric(reg4a$se[2])
    results_group$r4a_N <- reg4a$nobs
    results_group$r4a_r2 <- as.numeric(r2(reg4a, "war2"))
  } else {
    results_group$r4a_b <- NA
    results_group$r4a_se <- NA
    results_group$r4a_N <- NA
    results_group$r4a_r2 <- NA
  }
  
  if (!is.null(reg5)) {
    results_group$r5_b <- as.numeric(reg5$coefficients[2])
    results_group$r5_se <- as.numeric(reg5$se[2])
    results_group$r5_N <- reg5$nobs
    results_group$r5_r2 <- as.numeric(r2(reg5, "war2"))
  } else {
    results_group$r5_b <- NA
    results_group$r5_se <- NA
    results_group$r5_N <- NA
    results_group$r5_r2 <- NA
  }
  
  if (!is.null(reg6)) {
    results_group$r6_b <- as.numeric(reg6$coefficients[2])
    results_group$r6_se <- as.numeric(reg6$se[2])
    results_group$r6_N <- reg6$nobs
    results_group$r6_r2 <- as.numeric(r2(reg6, "war2"))
  } else {
    results_group$r6_b <- NA
    results_group$r6_se <- NA
    results_group$r6_N <- NA
    results_group$r6_r2 <- NA
  }
  
  if (!is.null(reg7)) {
    results_group$r7_b <- as.numeric(reg7$coefficients[2])
    results_group$r7_se <- as.numeric(reg7$se[2])
    results_group$r7_N <- reg7$nobs
    results_group$r7_r2 <- as.numeric(r2(reg7, "war2"))
  } else {
    results_group$r7_b <- NA
    results_group$r7_se <- NA
    results_group$r7_N <- NA
    results_group$r7_r2 <- NA
  }
  
  
  
  results_group
  
}

stopCluster(clusters)

results_group <- as.data.table(results_group)
results_group <- results_group[, lapply(.SD, as.numeric)]
saveRDS(results_group, "reg_results/results_extras_group.Rds")


print("Done with the group-level regressions, now onto the split by hhi quartiles")

# 
# # big regressions
# clusters = makeCluster(5)
# registerDoSNOW(clusters)
# results_hhi_q <- foreach(qt = 1:5, .packages = c('data.table','jtools','fixest', 'zoo'), .combine = rbind) %dopar% {
#   products <- stores_groups[hhi_quartile==qt]
#   products[, yearqtr := as.yearqtr(paste0(year,"-",quarter))]
#   setkeyv(products, c("store_code_uc","yearqtr","group"))
#   products[, price := revenue/quantity]
#   products[, lprice := log(price)]
#   products[, dlprice_pre := lprice - log(price[year == 2010 & quarter == 4]), .(store_code_uc, group )]
#   
#   products[parents, on = c("year", "quarter", "parent_code", "fips_state_code"),`:=`(state_sales_share = i.state_sales_share,
#                                                                                      presence_count = i.presence_count,
#                                                                                      in_cali = i.in_cali,
#                                                                                      only_in_cali = i.only_in_cali,
#                                                                                      gross_revenue = i.revenue)]
#   products[zero_tax, on = "fips_state_code", `:=`(zerotax = i.zerotax)]
#   
#   # tags
#   products[, post2011 := year>=2011]
#   
#   
#   # regressions
#   reg1 <- feols(dlprice_pre ~ post2011 + only_in_cali + post2011*only_in_cali | group^yearqtr, products[fips_state_code==6], cluster = "store_code_uc")
#   reg2 <- feols(dlprice_pre ~ post2011 + in_cali + post2011*in_cali | group^yearqtr, products[fips_state_code!=6], cluster = "store_code_uc")
#   reg3 <- feols(dlprice_pre ~ post2011 + in_cali + post2011*in_cali | group^yearqtr, products[fips_state_code!=6 & zerotax], cluster = "store_code_uc")
#   reg4 <- feols(dlprice_pre ~ post2011 + in_cali + post2011*in_cali | group^yearqtr, products[fips_state_code!=6 & presence_count>=2 & presence_count<=5], cluster = "store_code_uc")
#   reg5 <- feols(dlprice_pre ~ post2011 + in_cali + post2011*in_cali | group^yearqtr, products[fips_state_code!=6 & zerotax & presence_count>=2 & presence_count<=5], cluster = "store_code_uc")
#   reg6 <- feols(dlprice_pre ~ post2011 + (fips_state_code==6) + post2011*(fips_state_code==6) | parent_code^group^yearqtr,
#                 products[in_cali & !only_in_cali], cluster = "store_code_uc")
#   reg7 <- feols(dlprice_pre ~ post2011 + (fips_state_code==6) + post2011*(fips_state_code==6) | parent_code^group^yearqtr,
#                 products[in_cali & !only_in_cali & presence_count<=5], cluster = "store_code_uc")
#   
#   results_hhi_q <- c()
#   results_hhi_q$quartile <- qt
#   
#   for (i in 1:7) {
#     reg <- get(paste0("reg", i))
#     target <- paste0("r", i)
#     
#     if (!is.null(reg)) {
#       results_hhi_q[[paste0(target, "_b")]] <- as.numeric(reg$coefficients[2])
#       results_hhi_q[[paste0(target, "_se")]] <- as.numeric(reg$se[2])
#       results_hhi_q[[paste0(target, "_N")]] <- reg$nobs
#       results_hhi_q[[paste0(target, "_r2")]] <- as.numeric(r2(reg, "war2"))
#     } else {
#       results_hhi_q[[paste0(target, "_b")]] <- NA
#       results_hhi_q[[paste0(target, "_se")]] <- NA
#       results_hhi_q[[paste0(target, "_N")]] <- NA
#       results_hhi_q[[paste0(target, "_r2")]] <- NA
#     }
#   }
#   
#   
#   
#   results_hhi_q
# }
# 
# results_hhi_q <- as.data.table(results_hhi_q)
# results_hhi_q <- results_hhi_q[, lapply(.SD, as.numeric)]
# 
# stopCluster(clusters)
# 
# 
# saveRDS(results_hhi_q, "reg_results/results_hhi_q4.Rds")
# 
# 
# print("Done with the split by hhi quartiles, now onto the big regression")
# 
# results_big <- data.table(reg = 1:10, 
#                           b = numeric(), se = numeric(), N = numeric(), r2 = numeric())
# 
# stores_groups[, yearqtr := as.yearqtr(paste0(year,"-",quarter))]
# setkeyv(stores_groups, c("store_code_uc","yearqtr","group"))
# stores_groups[, price := revenue/quantity]
# stores_groups[, lprice := log(price)]
# stores_groups[, dlprice_pre := lprice - log(price[year == 2010 & quarter == 4]), .(store_code_uc, group )]
# 
# stores_groups[parents, on = c("year", "quarter", "parent_code", "fips_state_code"),`:=`(state_sales_share = i.state_sales_share,
#                                                                                         presence_count = i.presence_count,
#                                                                                         in_cali = i.in_cali,
#                                                                                         only_in_cali = i.only_in_cali,
#                                                                                         gross_revenue = i.revenue,
#                                                                                         cali_sales_share = i.cali_sales_share)]
# stores_groups[zero_tax, on = "fips_state_code", `:=`(zerotax = i.zerotax)]
# 
# # tags
# stores_groups[, post2011 := year>=2011]
# stores_groups[, cali_sales_share_pre := mean(cali_sales_share[year<2011]), .(store_code_uc, group)]
# 
# stores_groups[, post2010 := year>2010]
# stores_groups[, post2012 := year>2012]
# 
# # regressions
# reg1 <- feols(dlprice_pre ~ post2011 + only_in_cali + post2011*only_in_cali | group^yearqtr, stores_groups[fips_state_code==6], cluster = "store_code_uc")
# reg2 <- feols(dlprice_pre ~ post2011 + in_cali + post2011*in_cali | group^yearqtr, stores_groups[fips_state_code!=6], cluster = "store_code_uc")
# reg3 <- feols(dlprice_pre ~ post2011 + in_cali + post2011*in_cali | group^yearqtr, stores_groups[fips_state_code!=6 & zerotax], cluster = "store_code_uc")
# reg4 <- feols(dlprice_pre ~ post2011 + in_cali + post2011*in_cali | group^yearqtr, stores_groups[fips_state_code!=6 & presence_count>=2 & presence_count<=5], cluster = "store_code_uc")
# reg5 <- feols(dlprice_pre ~ post2011 + in_cali + post2011*in_cali | group^yearqtr, stores_groups[fips_state_code!=6 & zerotax & presence_count>=2 & presence_count<=5], cluster = "store_code_uc")
# reg6 <- feols(dlprice_pre ~ post2011 + (fips_state_code==6) + post2011*(fips_state_code==6) | parent_code^group^yearqtr,
#               stores_groups[in_cali & !only_in_cali], cluster = "store_code_uc")
# reg7 <- feols(dlprice_pre ~ post2011 + (fips_state_code==6) + post2011*(fips_state_code==6) | parent_code^group^yearqtr,
#               stores_groups[in_cali & !only_in_cali & presence_count<=5], cluster = "store_code_uc")
# reg8 <- feols(dlprice_pre ~ post2011 + cali_sales_share_pre + post2011*cali_sales_share_pre | group^yearqtr, stores_groups[fips_state_code!=6], cluster = "store_code_uc")
# reg9 <- feols(dlprice_pre ~ post2011 + cali_sales_share_pre + post2011*cali_sales_share_pre | group^yearqtr, stores_groups[fips_state_code!=6 & zerotax], cluster = "store_code_uc")
# reg10 <- feols(dlprice_pre ~ post2011 + cali_sales_share_pre + post2011*cali_sales_share_pre | group^yearqtr, stores_groups[fips_state_code!=6 & presence_count>=2 & presence_count<=5], cluster = "store_code_uc")
# reg11 <- feols(dlprice_pre ~ post2011 + cali_sales_share_pre + post2011*cali_sales_share_pre | group^yearqtr, stores_groups[fips_state_code!=6 & zerotax & presence_count>=2 & presence_count<=5], cluster = "store_code_uc")
# reg12 <- feols(dlprice_pre ~ post2010 + cali_sales_share_pre + post2010*cali_sales_share_pre | group^yearqtr, stores_groups[fips_state_code!=6 & zerotax & presence_count>=2 & presence_count<=5], cluster = "store_code_uc")
# reg13 <- feols(dlprice_pre ~ post2012 + cali_sales_share_pre + post2012*cali_sales_share_pre | group^yearqtr, stores_groups[fips_state_code!=6 & zerotax & presence_count>=2 & presence_count<=5], cluster = "store_code_uc")
# 
# 
# results_big[reg == 1, b := reg1$coefficients[2]]
# results_big[reg == 1, se := reg1$se[2]]
# results_big[reg == 1, N := reg1$nobs]
# results_big[reg == 1, r2 := r2(reg1, "war2")]
# 
# results_big[reg == 2, b := reg2$coefficients[2]]
# results_big[reg == 2, se := reg2$se[2]]
# results_big[reg == 2, N := reg2$nobs]
# results_big[reg == 2, r2 := r2(reg2, "war2")]
# 
# results_big[reg == 3, b := reg3$coefficients[2]]
# results_big[reg == 3, se := reg3$se[2]]
# results_big[reg == 3, N := reg3$nobs]
# results_big[reg == 3, r2 := r2(reg3, "war2")]
# 
# results_big[reg == 4, b := reg4$coefficients[2]]
# results_big[reg == 4, se := reg4$se[2]]
# results_big[reg == 4, N := reg4$nobs]
# results_big[reg == 4, r2 := r2(reg4, "war2")]
# 
# results_big[reg == 5, b := reg5$coefficients[2]]
# results_big[reg == 5, se := reg5$se[2]]
# results_big[reg == 5, N := reg5$nobs]
# results_big[reg == 5, r2 := r2(reg5, "war2")]
# 
# results_big[reg == 6, b := reg6$coefficients[2]]
# results_big[reg == 6, se := reg6$se[2]]
# results_big[reg == 6, N := reg6$nobs]
# results_big[reg == 6, r2 := r2(reg6, "war2")]
# 
# results_big[reg == 7, b := reg7$coefficients[2]]
# results_big[reg == 7, se := reg7$se[2]]
# results_big[reg == 7, N := reg7$nobs]
# results_big[reg == 7, r2 := r2(reg7, "war2")]
# 
# results_big[reg == 8, b := reg8$coefficients[2]]
# results_big[reg == 8, se := reg8$se[2]]
# results_big[reg == 8, N := reg8$nobs]
# results_big[reg == 8, r2 := r2(reg8, "war2")]
# 
# results_big[reg == 9, b := reg9$coefficients[2]]
# results_big[reg == 9, se := reg9$se[2]]
# results_big[reg == 9, N := reg9$nobs]
# results_big[reg == 9, r2 := r2(reg9, "war2")]
# 
# results_big[reg == 10, b := reg10$coefficients[2]]
# results_big[reg == 10, se := reg10$se[2]]
# results_big[reg == 10, N := reg10$nobs]
# results_big[reg == 10, r2 := r2(reg10, "war2")]
# 
# results_big <- as.data.table(results_big)
# results_big <- results_big[, lapply(.SD, as.numeric)]
# 
# saveRDS(results_big, "reg_results/results_big4.Rds")
