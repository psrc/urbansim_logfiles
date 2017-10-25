# This script will read log files and tabulate county 'demand'

library(stringr)

base.dir <- "//MODELSRV8/d$/opusgit/urbansim_data/data/psrc_parcel/runs"
run.dir <- c("run_4.run_2017_10_18_22_50")
out.dir <- "C:/Users/CLam/Desktop/urbansim_logfiles/demand"

years <- seq(2015, 2040)

# patterns to search for in regex format
pattern.file.str <- "year_\\d+{4}_log.txt"

# format run names for column names in table
runs <- lapply(run.dir, function(x) unlist(strsplit(x,"[.]"))[[1]])

# set-up table
df <- NULL

for (r in 1:length(run.dir)){ # loop through each run
  log.files.all <- list.files(file.path(base.dir, run.dir[r]), pattern = pattern.file.str)
  log.files <- unique(grep(paste(years,collapse="|"), log.files.all, value=TRUE))
  for (l in 1:length(log.files)){ # loop through each log file
    txt <- readLines(file.path(base.dir, run.dir[r], log.files[l]))
    print(paste("reading", runs[r], str_match(log.files[l], "\\d+"), "log"))
    
  } # end log.files loop
  
} # end run.dir loop


print("Processing complete")