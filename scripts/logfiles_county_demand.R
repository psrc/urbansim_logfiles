# This script will read log files and tabulate county 'demand'

library(tidyverse)
library(stringr)

# user input
run.dir <- c("run_8.run_2018_05_08_16_46", "run_7.run_2018_05_08_12_57") # can have multiple runs
out.dir <- "C:/Users/CLam/Desktop/urbansim_logfiles/output"
years <- seq(2015, 2050)

# all modelservers
base <- list(Modelsrv5 = "//modelsrv5/d$/opusgit/urbansim_data/data/psrc_parcel/runs",
             Modelsrv6 = "//modelsrv6/d$/opusgit/urbansim_data/data/psrc_parcel/runs",
             Modelsrv8 = "//MODELSRV8/d$/opusgit/urbansim_data/data/psrc_parcel/runs",
             Modelsrv3 = "//modelsrv3/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
             )

# scan all modelservers for runs and setnames
allruns <- list()
for (b in 1:length(base)) {
  fdirlist <- list.dirs(base[[b]], full.names = TRUE, recursive = FALSE)
  ndirlist <- list.dirs(base[[b]], full.names = FALSE, recursive = FALSE)
  dirlist <- setNames(fdirlist, ndirlist)
  ifelse(is.null(allruns), allruns <- dirlist, allruns <- append(allruns, dirlist))
}

counties <- c("33", "35", "53", "61")

# patterns to search for in regex format
pattern.file.str <- "year_\\d+{4}_log.txt"
pattern.dpsm <- "\\s+DPSM\\sfor\\sarea\\s"
pattern.end.line <- "DevelopmentProposalSamplingModelBySubarea:\\scompleted."
pattern.tbl.hdr <- "\\s+building_type_id"

# format run names for table
runs <- lapply(run.dir, function(x) unlist(strsplit(x,"[.]"))[[1]])

# set-up master table (wide form)
df <- NULL

for (r in 1:length(run.dir)) { # for each run
  base.dir <- pluck(allruns, run.dir[r])
  log.files.all <- list.files(base.dir, pattern = pattern.file.str)
  log.files <- unique(grep(paste(years, collapse = "|"), log.files.all, value=TRUE))
  for (l in 1:length(log.files)) { # for each log file
    txt <- readLines(file.path(base.dir, log.files[l]))
    year <- str_extract(log.files[l], "\\d+")
    print(paste("reading", runs[r], str_match(log.files[l], "\\d+"), "log"))
    for (county in counties) { # for each county
      ind.dpsm <- grep(pattern.dpsm, txt) # find line indices where 'DPSM for area ' occurs
      ind.cnty <- grep(paste0(pattern.dpsm, county), txt) # index of target county
      ord.ind.dpsm <- which(ind.cnty == ind.dpsm) # which index is it in ind.dpsm?
      # isolate county's set of text
      if (ord.ind.dpsm != 4) { 
        cnty.txt <- txt[ind.cnty:(ind.dpsm[ord.ind.dpsm + 1]-2)]
      } else {
        ind.endline <- grep(pattern.end.line, txt)
        cnty.txt <- txt[ind.cnty:(ind.endline-1)]
      }
      ind.headers <-  grep(pattern.tbl.hdr, cnty.txt) # find line index where 'building_type_id' occurs
      headers <- str_split(cnty.txt[ind.headers], "\t") %>% unlist()
      cnty.df <- data.frame(matrix(ncol = length(headers), nrow = 0)) %>% setNames(headers) # initialize empty df
      cnty.tblines <- cnty.txt[(ind.headers+1):length(cnty.txt)]
      for (i in 1:length(cnty.tblines)) { # compile county lines into a mini table
        aline <- str_split(cnty.tblines[i], "\t") %>% unlist() %>% data.frame() %>% t() 
        colnames(aline) <- headers
        cnty.df <- rbind(cnty.df, aline)
      }
      colnames(cnty.df) <- lapply(colnames(cnty.df), trimws)
      # query for county
      cnty.df$county_id <- as.character(cnty.df$county_id) %>% trimws()
      cnty.df <- subset(cnty.df, county_id == county)
      # add additional columns
      cnty.df$county <- county
      cnty.df$year <- year
      cnty.df$run <- runs[r] %>% unlist()
      # remove county_id
      cnty.df <- cnty.df[, 2:ncol(cnty.df)]
      ifelse(is.null(df), df <- cnty.df, df <- rbind(df, cnty.df)) # bind to master table
    } # end counties loop
  } # end log.files loop
} # end run.dir loop

# tidy df
df <- df %>% rename(Difference = `Difference(T-C)`, Action = `Action(P-D)`)
colnames(df)[1] <- 'building_type_id' # rename to trim whitespace
df <- df %>% 
  mutate_at(c('Target', 'Current', 'Difference', 'Proposed', 'Demolished', 'Action'), funs(as.character)) %>%
  mutate_at(c('Target', 'Current', 'Difference', 'Proposed', 'Demolished'), funs(as.numeric))
df$Action <- gsub("\\+", "", df$Action) # remove symbol
df <- df %>% mutate(Action = as.numeric(Action))

# create long form df
dflong <- df %>% gather(attribute, value, -building_type_id, -county, -year, -run)

# option to export as .csv
concat.runs <- paste(runs %>% unlist(), collapse = "_")
write_csv(df, file.path(out.dir, paste0("county_demand_wide_", concat.runs, ".csv")))
write_csv(dflong, file.path(out.dir, paste0("county_demand_long_", concat.runs, ".csv")))

print("Processing complete")
