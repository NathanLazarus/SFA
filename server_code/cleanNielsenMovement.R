# Read in a raw Nielsen movement data file at the weekly level,
# collapse to quarter and drop variables not needed. Output to Rds


# take in argument from command line, assert that it is a tsv file
args = commandArgs(TRUE)
year = args[1]
array_no = args[2]

# packages
.libPaths(file.path("/nfs/econlab001/lazarus-factor/bin/R/libs"))
library(tools)
library(data.table)
library(stringr)

# get list of files and obtain the specific 6 for this job. Get rid of NAs
files <- readLines(paste0("/nfs/econlab001/lazarus-factor/data/nielsen_extracts/RMS",year, "/files.txt", sep = ""))
files <- files[((array_no-1)*6+1):((array_no-1)*6+6)]
files <- files[!is.na(files)]

for (i in 1:length(files) {
  file <- files[i]
  movement <- fread(file)
  
  # generate quarter
  movement[, quarter := quarter(as.Date(as.character(week_end),format = '%Y%m%d'))]

  # collapse
  movement[, quantity := units]
  movement[, revenue := (price/prmult) * quantity]
  movement <- movement[, list(revenue = sum(revenue), quantity = sum(quantity)), by = list(store_code_uc,upc,quarter)]

  movement$upc <- as.double(movement$upc)


  # write to Rds
  new_file <- paste0("/nfs/econlab001/lazarus-factor/data/nielsen_extracts/RMS/",year,"/Clean_Files/clean_",gsub('.{4}$', '',str_extract(file,"[0-9]+_[0-9]+.tsv$")),".Rds")

  saveRDS(movement,new_file)
}


print(paste("Successfully collapsed ",file))