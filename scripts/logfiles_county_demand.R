# This script will read log files and tabulate county 'demand'

library(tidyverse)

base.dir <- "//MODELSRV8/d$/opusgit/urbansim_data/data/psrc_parcel/runs"
run.dir <- c("run_4.run_2017_10_18_22_50") # can have multiple runs, separated by comma
out.dir <- "C:/Users/CLam/Desktop/urbansim_logfiles/output"

years <- seq(2015, 2040)

counties <- c("33", "35", "53", "61")

# patterns to search for in regex format
pattern.file.str <- "year_\\d+{4}_log.txt"

# format run names for column names in table
runs <- lapply(run.dir, function(x) unlist(strsplit(x,"[.]"))[[1]])

# set-up master table (wide form)
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
      for (i in 1:length(cnty.tblines)) { # compile county lines into a mini table
        aline <- str_split(cnty.tblines[i], "\t") %>% unlist() %>% data.frame() %>% t() 
        colnames(aline) <- headers
        cnty.df <- rbind(cnty.df, aline)
      }
      # add additional columns
      cnty.df$county <- county
      cnty.df$year <- year
      cnty.df$run <- runs[r] %>% unlist()
      ifelse(is.null(df), df <- cnty.df, df <- rbind(df, cnty.df)) # bind to master table
    } # end counties loop
  } # end log.files loop
} # end run.dir loop

# tidy df
df <- df %>% rename(Difference = `Difference(T-C)`, Action = `Action(P-D)`)
colnames(df)[1] <- 'building_type_id'
df <- df %>% 
  mutate_each_(funs(as.character), c('Target', 'Current', 'Difference', 'Proposed', 'Demolished', 'Action')) %>%
  mutate_each_(funs(as.numeric), c('Target', 'Current', 'Difference', 'Proposed', 'Demolished'))
df$Action <- gsub("\\+", "", df$Action) # remove symbol
df <- df %>% mutate(Action = as.numeric(Action))

# create long form df
dflong <- df %>% gather(attribute, value, -building_type_id, -county, -year, -run)

# option to export as .csv
concat.runs <- paste(runs %>% unlist(), collapse = "_")
write_csv(df, file.path(out.dir, paste0("county_demand_wide_", concat.runs, ".csv")))
write_csv(dflong, file.path(out.dir, paste0("county_demand_long_", concat.runs, ".csv")))

print("Processing complete")
