.libPaths(file.path("/nfs/econlab001/lazarus-factor/bin/R/libs"))
library(data.table)
library(readxl)
library(haven)


taxrates <- as.data.table(read_dta("data/taxrates/taxrates.dta"))
setkeyv(taxrates,c("statefips","year"))
taxrates[, dt_x := round(t_x - shift(t_x),2), by = statefips]  
taxrates[, dt_corp := round(t_corp - shift(t_corp),2), by = statefips]  
taxrates[, dwgt_sales := round(wgt_sales - shift(wgt_sales),2), by = statefips]  

# get changes in tax rates post 2006
taxrates_changes <- taxrates[year>=2006 & abs(dt_x)>0]
taxrates_changes[, mag := abs(dt_x)]
setorder(taxrates_changes, "mag")