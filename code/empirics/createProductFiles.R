# merge movement files into product files that span years

.libPaths(file.path("/nfs/econlab001/lazarus-factor/bin/R/libs"))
library(data.table)
library(readxl)

pacman::p_load(foreach, iterators, doSNOW)

# select range of years
year_range = 2006:2016

# load store data
stores <- foreach(year = year_range, .combine = rbind) %do% {
  fread(paste0("data/nielsen_extracts/RMS/",year,"/Annual_Files/stores_",year,".tsv"))
}

stores <- stores[,list(year,parent_code,store_code_uc,fips_state_code)]

# get all category numbers
category_numbers <- unique(foreach(year = year_range, .combine = append) %do% {
  substr(list.files(paste0("data/nielsen_extracts/RMS/",year,"/Clean_Files")), 7, 10)
})

# get UPC versions
rms_versions <- foreach(year = year_range, .combine = rbind) %do% {
  fread(paste0("data/nielsen_extracts/RMS/",year,"/Annual_Files/rms_versions_",year,".tsv"))
}
setnames(rms_versions, old = c('upc','upc_ver_uc','panel_year'), 
         new = c('upc','upc_ver_uc','year'))
rms_versions$upc <- as.double(rms_versions$upc)

clusters = makeCluster(16)
registerDoSNOW(clusters)
foreach(cn = category_numbers, .packages = c('data.table','foreach')) %dopar% {
  
  
  movement <- foreach(year = year_range, .combine = rbind) %do% {
    tryCatch(
      {
        readRDS(paste0("data/nielsen_extracts/RMS/",year,"/Clean_Files/clean_", cn, "_",year,".Rds"))[,year := year]
      },
      warning=function(cond) {
        return(NULL)
      },
      error=function(cond) {
        return(NULL)
      }
    )    
  }
  
  movement[stores, on = c("store_code_uc", "year"), `:=`(parent_code = i.parent_code, store_code_uc = i.store_code_uc, fips_state_code = i.fips_state_code)]
  movement[rms_versions, on = c("upc", "year"), `:=`(upc_ver_uc = i.upc_ver_uc)]
  
  movement[, price := revenue/quantity]
  

  saveRDS(movement, paste0("data/nielsen_extracts/product_files/products_",cn,".Rds"))
  
  print(paste0("all done code ",cn))

  
}

stopCluster(clusters)




