# This script will read log files in 1 or more run directories and tabulate the amount of time
# (in minutes) to complete an UrbanSim submodel

library(stringr)
library(magrittr)
library(lubridate)
library(reshape2)

# base.dir <- "//MODELSRV8/d$/opusgit/urbansim_data/data/psrc_parcel/runs"
base.dir <-'T:/2017September/Peter/Cloud Test Log Files'
run.dir <- c("LUV Test 1") # can have multiple runs, separated by comma
out.dir <- "C:/Users/CLam/Desktop/scripts/modeltime_logfiles"

years <- seq(2014, 2040)

submodel <- c("Real Estate Price Model" = "Running Real Estate Price Model.", 
              "Creating proposals for new development" = "Creating proposals for new development:.", 
              "Creating proposals for re-development" = "Creating proposals for re-development:.",
              "Selecting proposals within parcels" = "Selecting proposals within parcels.", 
              "DevelopmentProjectProposalSamplingModel" = "DevelopmentProjectProposalSamplingModel:.", 
              "BuildingConstructionModel" = "Running BuildingConstructionModel.",
              "Household Transition Model" = "Running Household Transition Model.", 
              "Employment Transition Model" = "Running Employment Transition Model.", 
              "PersonDatasetConsistencyKeeperModel" = "PersonDatasetConsistencyKeeperModel:.", ##does it have completed?
              "Household Relocation Model" = "Running Household Relocation Model.",
              "Household Location Choice Model"= "Running Household Location Choice Model.", 
              "Employment Relocation Model" = "Running Employment Relocation Model.", 
              "Non_home_based Employment Location Choice Model" = "Running Non_home_based Employment Location Choice Model.",
              "Scaling Jobs Model"= "Running Scaling Jobs Model.", 
              "Work At Home Choice Model" = "Running Work At Home Choice Model.", 
              "Non-home-based Workplace Choice Model for residents" = "Running Non-home-based Workplace Choice Model for residents.",
              "Simulate year" = "Simulate year\\s\\d+:")

# submodel number lookup
submodel.num <- structure(1:17, names = names(submodel)) 

# patterns to search for in regex format
pattern.file.str <- "year_\\d+{4}_log.txt"

# regex patterns for each submodel
submodel.rgx <- unname(submodel)

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
    for (s in 1:length(submodel.rgx)){ # loop through each submodel
      sub.txt0 <- grep(submodel.rgx[s], txt, value = TRUE) # search in file for pattern
      sub.txt <- sub.txt0[length(sub.txt0)] # extract the second option, or the only option
      if (length(sub.txt) == 0) { # if log file doesn't contain that pattern, update table w/0 mins
        arow <- NULL
        arow <- list(run = runs[[r]], submodel = submodel.rgx[s], year = str_match(log.files[l], "\\d+"),
                     minutes = 0) %>% data.frame()
        ifelse(is.null(df), df <- arow, df <- rbind(df, arow))
      } else { # if log file contains pattern, calculate mins
        time.str <- str_match(sub.txt, "\\.{2,}(\\d+.*)")[2]
        fmt.time <- str_split(time.str, ",") %>% unlist() %>% length()
        if (fmt.time == 1) { # seconds
          t <- unlist(str_split(time.str, " ")[[1]])[1] %>% as.numeric()
          mins <- t/60
        } else if (fmt.time == 2) { # minutes + seconds
          t <- ms(time.str)
          mins <- minute(t) + second(t)/60
        } else if (fmt.time == 4){ # days + hr + min + secs (use regex to extract pieces and calc into minutes)
          # days
          d.str <- str_extract(time.str, "\\d+\\s+days") %>% str_split(" ") %>% unlist()
          d <- d.str[1] %>% as.numeric()
          d <- d * 1440
          # hours
          h.str <- str_extract(time.str, "\\d+\\s+hrs") %>% str_split(" ") %>% unlist()
          h <- h.str[1] %>% as.numeric()
          h <- h*60
          # minutes
          m.str <- str_extract(time.str, "\\d+\\s+min") %>% str_split(" ") %>% unlist()
          m <- m.str[1] %>% as.numeric()
          # seconds
          sec.str <- str_extract(time.str, "(\\d+(\\.\\d+)*\\s+sec$)") %>% str_split(" ") %>% unlist()
          sec <- sec.str[1] %>% as.numeric()
          sec <- s/60
          # total minutes
          mins <- d + h + m + sec
        } else { # hr + min + secs
          t <- hms(time.str)
          mins <- hour(t)*60 + minute(t) + second(t)/60
        }
        arow <- NULL
        arow <- list(run = runs[[r]], submodel = submodel.rgx[s], year = str_match(log.files[l], "\\d+"),
                     minutes = mins) %>% data.frame()
        ifelse(is.null(df), df <- arow, df <- rbind(df, arow))
    } # end first if/else statement
    } # end submodel.rgx loop
  } # end log.files loop
} # end run.dir loop

# format table (long form)
df$submodel <- names(submodel[df$submodel])
df$num <- submodel.num[df$submodel]
df <- df[, c("run", "num", "submodel", "year", "minutes")]

# format table (wide form)
df.wide <- dcast(df, run + num + submodel ~ year, value.var = "minutes")
df.wide[,c(4:ncol(df.wide))] <- round(df.wide[,c(4:ncol(df.wide))], digits = 0)
# export to csv
write.csv(df.wide, file.path(out.dir, "submodel_runtime_logfiles.csv"))

print("Processing complete")