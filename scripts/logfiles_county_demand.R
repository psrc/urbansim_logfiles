# This script will read log files and tabulate county 'demand'

library(magrittr)
library(stringr)

base.dir <- "//MODELSRV8/d$/opusgit/urbansim_data/data/psrc_parcel/runs"
run.dir <- c("run_4.run_2017_10_18_22_50")
out.dir <- "C:/Users/CLam/Desktop/urbansim_logfiles/demand"

years <- seq(2015, 2040)

counties <- c("33", "35", "53", "61")

# patterns to search for in regex format
pattern.file.str <- "year_\\d+{4}_log.txt"

# format run names for column names in table
runs <- lapply(run.dir, function(x) unlist(strsplit(x,"[.]"))[[1]])

# set-up table
df <- NULL

for (r in 1:length(run.dir)) { # loop through each run
  log.files.all <- list.files(file.path(base.dir, run.dir[r]), pattern = pattern.file.str)
  log.files <- unique(grep(paste(years,collapse="|"), log.files.all, value=TRUE))
  for (l in 1:length(log.files)) { # loop through each log file
    txt <- readLines(file.path(base.dir, run.dir[r], log.files[l]))
    year <- str_extract(log.files[l], "\\d+")
    print(paste("reading", runs[r], str_match(log.files[l], "\\d+"), "log"))
    for (county in counties) {
      ind.dpsm <- grep("\\s+DPSM\\sfor\\sarea\\s", txt) # find line indices where 'DPSM for area ' occurs
      ind.cnty <- grep(paste0("\\s+DPSM\\sfor\\sarea\\s", county), txt) # index of target county
      ord.ind.dpsm <- which(ind.cnty == ind.dpsm) # which index is it in ind.dpsm?
      if (which(ind.cnty == ind.dpsm) != 4) { 
        cnty.txt <- txt[ind.cnty:(ind.dpsm[ord.ind.dpsm + 1]-2)]
      } else {
        ind.endline <- grep("DevelopmentProposalSamplingModelBySubarea:\\scompleted.", txt)
        cnty.txt <- txt[ind.cnty:(ind.endline-1)]
      }
      ind.headers <-  grep("\\s+building_type_id", cnty.txt) # find line index where 'building_type_id' occurs
      headers <- str_split(cnty.txt[ind.headers], "\t") %>% unlist()
      cnty.df <- data.frame(matrix(ncol = length(headers), nrow = 0)) %>% setNames(headers) # initialize empty df
      cnty.tblines <- cnty.txt[(ind.headers+1):length(cnty.txt)]
      for (i in 1:length(cnty.tblines)) {
        aline <- str_split(cnty.tblines[i], "\t") %>% unlist() %>% as.data.frame() %>% t() 
        colnames(aline) <- headers
        cnty.df <- rbind(cnty.df, aline)
      }
      cnty.df$county <- county
      cnty.df$year <- year
      ifelse(is.null(df), df <- cnty.df, df <- rbind(df, cnty.df))
    } # end counties loop
  } # end log.files loop
} # end run.dir loop


print("Processing complete")
