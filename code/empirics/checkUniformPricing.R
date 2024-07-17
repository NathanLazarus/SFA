.libPaths(file.path("/nfs/econlab001/lazarus-factor/bin/R/libs"))
library(data.table)
library(jtools)
pacman::p_load(foreach, iterators, doSNOW)


# get all category numbers
category_numbers <- unique(foreach(year = 2006:2016, .combine = append) %do% {
  substr(list.files(paste0("data/nielsen_extracts/RMS/",year,"/Clean_Files")), 7, 10)
})

clusters = makeCluster(12)
registerDoSNOW(clusters)
foreach(cn = category_numbers, .packages = c('data.table','jtools')) %dopar% {

  # load file
  products <- readRDS(paste0("data/nielsen_extracts/product_extras_collapse/extras_",cn,".Rds"))
  
  # round price to 2 decimals
  products[, price := round(revenue/quantity,2)]
  
  # mode price
  products[, freq := .N, by = list(year,quarter,parent_code,extras_group) ]
  products[, freq_p := .N, by = list(year,quarter,parent_code,extras_group,price)]
  products[, is_highest_freq_p := freq_p==max(freq_p), by = list(year, quarter, parent_code, extras_group)]
  

  # collapse average and sd log price within parent, extras_group and time
  products_1 <- products[,list(price_mean = unlist(lapply(.SD,weighted.mean,w=quantity)),
                             price_sd = unlist(lapply(.SD,wtd.sd,weights=quantity)),
                             total_revenue = sum(revenue)), by = list(parent_code,extras_group,year,quarter), .SDcols = "price"]
  
  products_1[, coef_var := price_sd/price_mean]
  
  # collapse mode
  products_2 <- products[, list(mode_share = mean(is_highest_freq_p)), by = list(parent_code,extras_group,year,quarter)]
  
  # hhi collapse
  products_3 <- unique(products[, list(ms2 = (freq_p/freq)^2), by = list(parent_code,extras_group,year,quarter,price)])
  products_3 <- products_3[, list(hhi = sum(ms2)), by = list(parent_code,extras_group,year,quarter)]
  
  products <- products_1[products_2[products_3, on = list(parent_code,extras_group,year,quarter)], on = list(parent_code,extras_group,year,quarter)]
    
  saveRDS(products, paste0("data/nielsen_extracts/uniform_check/uni_",cn,".Rds"))
  

}
stopCluster(clusters)


