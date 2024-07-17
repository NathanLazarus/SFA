# Collapse movement files into quarter by state by store file for each year

.libPaths(file.path("/nfs/econlab001/lazarus-factor/bin/R/libs"))
library(data.table)

pacman::p_load(foreach, iterators, doSNOW)

setwd("/nfs/econlab001/lazarus-factor/")

year_range = 2006:2016

# get all category numbers
category_numbers <- unique(foreach(year = year_range, .combine = append) %do% {
  substr(list.files(paste0("data/nielsen_extracts/RMS/",year,"/Clean_Files")), 7, 10)
})

# read in all the extras files
extras <- readRDS("data/nielsen_extracts/extras.Rds")


# read in each file and collapse into revenue, bind them all together
clusters = makeCluster(11)
registerDoSNOW(clusters)
foreach(cn = category_numbers, .packages = c("data.table")) %dopar% {
movement <- readRDS(paste0("data/nielsen_extracts/product_files/products_",cn,".Rds"))[
  , group := as.numeric(cn)][
    extras, on = c("upc","year"), `:=`(flavor_code = flavor_code, form_code = form_code, formula_code = formula_code, container_code = container_code,
                                       salt_content_code = salt_content_code, style_code = style_code, type_code = type_code, product_code = product_code,
                                       variety_code = variety_code, organic_claim_code = organic_claim_code, common_consumer_name_code = common_consumer_name_code,
                                       strength_code = strength_code, scent_code = scent_code, dosage_code = dosage_code, gender_code = gender_code, use_code = use_code,
                                       size_code = size2_code)][
                                         , extras_group := .GRP, by = .(flavor_code, form_code, formula_code, container_code, salt_content_code, style_code, type_code,
                                                                        product_code, variety_code, organic_claim_code, common_consumer_name_code, strength_code,
                                                                        scent_code, dosage_code, gender_code, use_code, size_code)][
                                         , list(revenue = sum(revenue), quantity = sum(quantity)), by = list(year, quarter, fips_state_code, parent_code,
                                                                                                             store_code_uc, group, extras_group)]
saveRDS(movement, paste0("data/nielsen_extracts/product_extras_collapse/extras_",cn,".Rds"))
print(paste0("Saved extras file ", cn))
}
stopCluster(clusters)
print("made it to here!")

clusters = makeCluster(11)
registerDoSNOW(clusters)
movement <- foreach(cn = category_numbers, .combine = rbind, .packages = c("data.table")) %dopar% {
  readRDS(paste0("data/nielsen_extracts/product_extras_collapse/extras_",cn,".Rds"))
}
stopCluster(clusters)
  
# save a file at the store by product group extras by year level
saveRDS(movement, "data/nielsen_extracts/stores_groups_extras_aggregate.Rds")
print("saved extras!")


# collapse again to store by product group by year, without extras group
movement <- movement[, list(revenue = sum(revenue), quantity = sum(quantity)), by = list(year, quarter, fips_state_code, parent_code, store_code_uc, group)]

# save a file at the store by product group by year level
saveRDS(movement, "data/nielsen_extracts/stores_groups_aggregate2.Rds") # version 2 temporary, want to make sure the file is unchanged after above edits which added the extras grouping

# collapse again to get from revenue by product group and store to total revenue by parent and state
movement <- movement[, list(revenue = sum(revenue), quantity = sum(quantity)), by = list(year, quarter, fips_state_code, parent_code)]

# share of sales in each state by firm
movement[, state_sales_share := revenue/sum(revenue), by = list(year, quarter, parent_code)]

# number of states a firm is present in 
movement[, presence_count := .N, by = list(year, quarter, parent_code)]

setkeyv(movement,c("year","quarter","parent_code","fips_state_code"))

saveRDS(movement, "data/nielsen_extracts/parents_states_aggregate2.Rds")


