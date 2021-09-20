#!/usr/local/bin/Rscript
###################################################################################
# High-throughput Replica Pinning
# Last Update: 17-09-2021
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
  make_option(opt_str = c("--rep"), 
              help = "Numeric value to multiply the colonies from Non-Selective Plates. [default=%default]", type = "numeric", default = 1),
  make_option(opt_str = c("--Filter"),
              help = "Condition to filter the colonies in the Non-Selective Plates. [default=%default]", type = "numeric", default = 12),
  make_option(opt_str = c("--Median_NSP"),
              help = "Value in a range of 0-1 to modify the Median criteria in Non-Selective Plates. [default=%default]", type = "numeric", default = 0.5),
  make_option(opt_str = c("--Median_SP"),
              help = "Value in a range of 0-1 to modify the Median criteria in Selective Plates. [default=%default]", type = "numeric", default = 0.2),
  make_option(opt_str = c("-O", "--output"),
              help = "Output directory (optional): Directory where the results will be save. [default=%default]", default = getwd(), type = "character")
)

# Parse the arguments
args <- parse_args(OptionParser(option_list = option_list))

#
cat("
#######################################
High-throughput Replica Pinning
#######################################\n")

# Validate directory output exists
if (args$output == getwd()) {
  dir.create(paste0(args$output,"/Output"))
  args$output <- paste0(args$output,"/Output/")
} else if (length(list.dirs(args$output)) == 0) {
  stop(paste0(args$output, " is not valid a output directory."))
}

# Validate Existence of Input files / key files
files <- c(args$inputfile, args$keyfile)
files <- files[!is.na(files)]
valid.files <- file.exists(files)
if (sum(!valid.files) >0 ){
  invalid.file = files[!valid.files][1]
  stop(paste0(invalid.file, " is not valid a file path."))
}

# Validate Range in Median
if (!((args$Median_NSP >= 0 & args$Median_NSP <= 1) & (args$Median_SP >= 0 & args$Median_SP <= 1))){
  stop("Values in Median arguments are not valid. Only range between 0-1 are allowed.")
}

# Load Source files
if(file.exists("High_Replica_Pinning_Tools.R")) {
  source("High_Replica_Pinning_Tools.R")
} else {
  stop(paste0("High_Replica_Pinning_Tools.R file is not present in the same directory."))
}

# Plates Formatting -------------------------------------------------------

cat("Matrix Preparation Of 1564 Cells.\n\n")
tryCatch({data_lib <- matrix_preparation(file_lib= args$keyfile,
                                        rows_plate = rows_plate, 
                                        columns_plate=columns_plate)},
         error = function (x) {"There is an error in the keyfile."})

# Preparing the data library
data_lib <-  data_lib %>% 
  select(Plate, rowname, New_column, ORF) %>% 
  rename("New_row" = "rowname",
         "New_plate" = "Plate") %>%
  mutate(New_plate = as.character(New_plate))

# Data frame with old and new Coordinates alongside with the others columns from key file.
data_lib_YKO <- merging_files(file_lib = args$keyfile, data_gene = data_lib,
                              rows_plate = rows_plate, 
                              columns_plate=columns_plate)

# ## For TS
# cat("Matrix Preparation TS.\n")
# cat(args$tsfile, "\n")
# data_lib <- matrix_preparation(file_lib= args$tsfile)
# 
# # Preparing the data library
# data_lib <-  data_lib %>% 
#   select(Plate, rowname, New_column, ORF) %>% 
#   rename("New_row" = "rowname",
#          "New_plate" = "Plate") %>%
#   mutate(New_plate = as.character(New_plate))
# 
# # Data frame with old and new Coordinates alongside with the others columns from key file.
# data_lib_TS <- merging_files(file_lib = args$tsfile, data_gene = data_lib)

# Colony Areas ------------------------------------------------------------

# Conditions Output
cat("------------------------------------------\n")
cat("Conditions:\n")
cat("Colony Input File: ", args$inputfile, "\n")
cat(paste0("Keyfile Used: ",args$keyfile, "\n"))
cat(paste0("Colony times from Non-Selective Plates: ", args$rep, "\n"))
cat(paste0("% Median for Non-Selective Plates: ", args$Median_NSP*100, "\n"))
cat(paste0("% Median for Selective Plates: ", args$Median_SP*100, "\n"))
cat(paste0("Threshold for Total colonies Filtering conditions in Non-Selective plates: ", args$Filter, "\n"))
cat("Output Directory: ", args$output, "\n")
out_name <- strsplit(args$inputfile, "/")[[1]]
out_name <- str_replace(out_name[length(out_name)], "\\.txt", "")
out_name_YKO <- paste0(args$output, out_name, "_", args$Median_NSP*100 ,"_", args$Median_SP*100, "_", args$Filter)
cat(paste0("Output filename: ", out_name_YKO, "\n\n"))
cat("------------------------------------------\n")

cat("Colony Areas Filling.\n\n")
## Read colony areas file
screen_data <- Colony_areas(filename = args$inputfile)

## YKO process
cat("Data processing.\n\n")

YKO_data <- Screen_processing(data = screen_data)

screen_YKO <- Colony_format(data = YKO_data)

DataColony_Filling(fileScreen = data_lib_YKO,
                   data = screen_YKO,
                   fileName = out_name_YKO,
                   times = args$rep,
                   Med_higher = args$Median_NSP,
                   Med_lower = args$Median_SP,
                   threshold = args$Filter
                   )

cat("Data Preparation Complete.\n")
cat(paste0(Sys.time(),"\n"))

# ### TS Process
# cat("TS Data process.\n")
# 
# ts_data <- Screen_processing(data = screen_data,
#                               screen_pattern = "TS",
#                               letter = args$set)
# 
# screen_ts <- Colony_format(data = ts_data,
#                             type_letter = args$set)
# 
# out_name_ts <- paste0(args$output, out_name, "_TS_")
# 
# DataColony_Filling(fileScreen = data_lib_TS,
#                    data = screen_ts,
#                    fileName = out_name_ts,
#                    type_file = args$set,
#                    isTS = TRUE)
