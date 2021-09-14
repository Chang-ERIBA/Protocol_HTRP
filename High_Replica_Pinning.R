#!/usr/local/bin/Rscript
###################################################################################
# Write by: 
# Last Update: 13-09-2021
#
# Usage: Rscript ./High_Replica_Pinning.R [OPTIONS]
#
###################################################################################

suppressPackageStartupMessages(library(optparse))
options(warn = -1) # Suppress Warnings

# Options list
option_list <- list(
  make_option(c("-i", "--inputfile"), 
              help = "Input file with colony areas (required)", type = "character"),
  make_option(opt_str = c("-k", "--keyfile"),
              help = "Keyfile mutant library (required): Containing the mutant library data in csv format", type = "character"),
  make_option(opt_str = c("-t", "--tsfile"),
              help = "TS keyfile library (required): Containg the ts mutant library version data in csv format", type = "character"),
  make_option(opt_str = c("-p", "--position"),
              help = "CSV file with the positons to remove (required)", type = "character"),
  make_option(opt_str = c("-s", "--set"),
              help = "Plates set. It could be 'A' or 'B'. [default %default]", default = "A", type = "character"),
  make_option(opt_str = c("-o", "--output"),
              help = "Output directory (optional): Directory where the results will be save. [default %default]", default = getwd(), type = "character")
)

# Parse the arguments
args <- parse_args(OptionParser(option_list = option_list))

# Set working directory

# Validate directory output exists
if (args$output == getwd()) {
  dir.create(paste0(args$output,"/Output"))
  args$output <- paste0(args$output,"/Output/")
} else if (length(list.dirs(args$output)) == 0) {
  stop(paste0(args$output, " is not valid a output directory."))
}

# Validate Existence of Input files / key files
files <- c(args$inputfile, args$keyfile, args$tsfile, args$position)
files <- files[!is.na(files)]
valid.files <- file.exists(files)
if (sum(!valid.files) >0 ){
  invalid.file = files[!valid.files][1]
  stop(paste0(invalid.file, " is not valid a file path."))
}

# Validate Set
if (!args$set %in% c("A", "B")){
  stop("Set plates invalid.")
}

# Load Source files
if(file.exists("High_Replica_Pinning_Tools.R")) {
  source("High_Replica_Pinning_Tools.R")
} else {
  stop(paste0("High_Replica_Pinning_Tools.R file is not present in the same directory."))
}

# Plates Formatting -------------------------------------------------------

## For YKO
cat("Matrix Preparation YKO.\n")
cat(args$keyfile, "\n")
data_lib <- matrix_preparation(file_lib= args$keyfile)

# Preparing the data library
data_lib <-  data_lib %>% 
  select(Plate, rowname, New_column, ORF) %>% 
  rename("New_row" = "rowname",
         "New_plate" = "Plate") %>%
  mutate(New_plate = as.character(New_plate))

# Data frame with old and new Coordinates alongside with the others columns from key file.
data_lib_YKO <- merging_files(file_lib = args$keyfile, data_gene = data_lib)


## For TS
cat("Matrix Preparation TS.\n")
cat(args$tsfile, "\n")
data_lib <- matrix_preparation(file_lib= args$tsfile)

# Preparing the data library
data_lib <-  data_lib %>% 
  select(Plate, rowname, New_column, ORF) %>% 
  rename("New_row" = "rowname",
         "New_plate" = "Plate") %>%
  mutate(New_plate = as.character(New_plate))

# Data frame with old and new Coordinates alongside with the others columns from key file.
data_lib_TS <- merging_files(file_lib = args$tsfile, data_gene = data_lib)

# Colony Areas ------------------------------------------------------------
cat("Colony Areas filling.\n")
cat("Output directory: ", args$output, "\n")
cat("Colony File: ", args$inputfile, "\n")
## Read colony areas file
screen_data <- Colony_areas(filename = args$inputfile)

out_name <- strsplit(args$inputfile, "/")[[1]]
out_name <- str_replace(out_name[length(out_name)], "\\.txt", "")

## YKO process
cat("YKO Data process.\n")

YKO_data <- Screen_processing(data = screen_data,
                              screen_pattern = "YKO",
                              letter = args$set)

screen_YKO <- Colony_format(data = YKO_data,
                            type_letter = args$set)

out_name_YKO <- paste0(args$output, out_name, "_YKO_")
cat(out_name_YKO, "\n")

DataColony_Filling(fileScreen = data_lib_YKO,
                   data = screen_YKO,
                   fileName = out_name_YKO,
                   type_file = args$set,
                   data_positions = args$position,
                   isTS = FALSE)

### TS Process
cat("TS Data process.\n")

ts_data <- Screen_processing(data = screen_data,
                              screen_pattern = "TS",
                              letter = args$set)

screen_ts <- Colony_format(data = ts_data,
                            type_letter = args$set)

out_name_ts <- paste0(args$output, out_name, "_TS_")

DataColony_Filling(fileScreen = data_lib_TS,
                   data = screen_ts,
                   fileName = out_name_ts,
                   type_file = args$set,
                   isTS = TRUE)
