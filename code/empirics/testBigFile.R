.libPaths(file.path("bin/R/libs"))

library(data.table)
pacman::p_load(foreach, iterators, doSNOW)


# category numbers
category_numbers <- unique(foreach(year = 2006:2016, .combine = append) %do% {
  substr(list.files(paste0("data/nielsen_extracts/RMS/",year,"/Clean_Files")), 7, 10)
})

chunks <- 10

category_numbers <- split(category_numbers,  cut(seq_along(category_numbers), chunks, labels = FALSE))

foreach(cat_num_list = category_numbers, i=icount(), .packages = c('data.table', 'foreach', 'iterators', 'doSNOW')) %do% {
  clusters = makeCluster(12)
  registerDoSNOW(clusters)
  big_file <- foreach(cn = cat_num_list, .packages = c('data.table'), .combine = rbind) %dopar% {
    readRDS(paste0("data/nielsen_extracts/product_extras_collapse/extras_",cn,".Rds"))[year>=2009 & year<=2013]
  }
  saveRDS(big_file,paste0("data/nielsen_extracts/stores_groups_extras_",i,".Rds"))
  stopCluster(clusters)
  rm(big_file)
  gc()
}

