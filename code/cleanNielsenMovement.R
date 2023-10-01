args = commandArgs(TRUE)
file = file.path(args[1])

# Read in a raw Nielsen movement data file at the weekly level,
# collapse to quarter and drop variables not needed. Output to Rds

# packages
library(data.table)

# load file
movement <- fread("../data/fake_Neilsen.tsv")

# generate quarter
movement[, quarter := quarter(as.Date(as.character(week_end),format = '%Y%m%d'))]

# collapse
movement[, quantity := units * prmult]
movement[, revenue := price * quantity]
movement <- movement[, list(revenue = sum(revenue), quantity = sum(quantity)), by = list(store_code_uc,upc,quarter)]

# write to Rds
fwrite(movement,"../data/fake_Neilsen.Rds")

# delete tsv file
file.remove(file)