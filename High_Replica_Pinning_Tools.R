###################################################################################
# Write by: 
# Last Update: 13-09-2021
#
# This scripts contains all the functions necessary for High_Replica_Pinning.R
# Save this script alongside with High_Replica_Pinning.R in the same directory.
#
###################################################################################


## Check packages
if (!all(c("tidyverse", "openxlsx") %in% installed.packages())) {
  stop("Some packages are not installed or updated.\n")
}

# Load required libraries
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(openxlsx))


# Variables ---------------------------------------------------------------

### Unique ID for 1564 Plates
rows_plate <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","A'","B'","C'","D'","E'","F'")
columns_plate <- c(1:48)

# Plates Formatting --------------------------------------------------------
# Matrix_preparation takes the library and arrange the genes in a 1564 plate
# format to extract then the coordinates. It returns a data frame with all the
# genes and coordinates from the matrix.
# @file_lib: Path to the library to read and convert.
#
matrix_preparation <- function(file_lib, rows_plate, columns_plate, cells = 1536){
  
  # Data library
  df_lib <- suppressMessages(read_csv(file_lib, show_col_types = FALSE, col_types = "c")) %>%
    drop_na(`Plate #`, Column, Row) %>% 
    arrange(`Plate #`, Column, Row) ## Ordering by Plate , Column and Row.
  
  plates_lib <- unique(df_lib$`Plate #`) ## Work by each plate from this document.
  
  ## Correction and filling the Key file
  key_full <- df_lib %>% 
    group_by(`Plate #`) %>% 
    tally() %>% 
    distinct(n) %>% 
    min()
  
  if (key_full < 384) {
    cat("Coordinates in Key file incomplete. Proceeding to complete it.\n")
    ## New Database with old and new coordinates
    nrow_plate <- cells # CHANGE IF NECCESARY
    old_rows_plate <- rep(unique(df_lib$Row), each = 2, times = nrow_plate/length(rows_plate))
    old_cols_plate <- rep(unique(df_lib$Column), each = nrow_plate/length(columns_plate)*2) # Necessary multiply for the replicates, IN this case 2
    nplates <- length(plates_lib)
    
    df_lib <- tibble(
      "Plate #" = as.character(rep(1:nplates, each = nrow_plate)),
      "Row" = rep(old_rows_plate, times = nplates),
      "Column" = rep(old_cols_plate, times = nplates),
    )  %>%
      distinct() %>%
      left_join(df_lib, by = c("Row", "Column", "Plate #")) %>%
      arrange(`Plate #`, Column, Row) ## Ordering by Plate , Column and Row.
  }
  
  ## Constant values
  nrows_plate <- length(rows_plate)
  cols_plate <- seq(1, length(columns_plate), by = 2) # By two as the replicates are 2 x 2
  selected <- seq(1, nrow(df_lib), by = 16) # As the library has 16 rows each plate
  
  # Empty objects to fill for each plate.
  database <- data.frame()
  list.plates <- list() # Each plate will be an element in a list
  
  for (p in plates_lib) {
    # Extract genes from plate
    genes <- df_lib %>% 
      filter(`Plate #` == p) %>% 
      pull(ORF)
    
    ### Create a Empty matrix (Plate)
    plate <- matrix(nrow = length(rows_plate), 
                    ncol = length(columns_plate),
                    dimnames = list(rows_plate,
                                    columns_plate))
    
    ## Extract the ORF and fill in order
    for (i in 1:length(cols_plate)) {
      # Duplicate the genes
      genes_col <- genes[selected[i]:(selected[i]+15)]
      genes_col <- rep(genes_col, each = 2)
      
      # Adding to a plate
      plate[,c(cols_plate[i], cols_plate[i] +1)] <- genes_col
    }
    ## Save each plate
    list.plates[[p]] <- plate
    ## Save the plate in a data frame
    plate_df <- as.data.frame(plate, row.names = rownames(plate), stringsAsFactors = FALSE) %>% 
      rownames_to_column() %>% 
      gather(key="New_column", value = "ORF", -rowname) %>% 
      mutate(Plate = p)
    database <- rbind(database, plate_df)
  }
  
  return(database)
}

# merging_files function reads the key file and the database created in
# matrix_preparation() to create the new coordinates and merge both files to get 
# a data frame with New and old (original) columns.
# @ file_lib: File path for the key file
# @ data_gene: data frame created from matrix_preparation() function
# @ cells: plate size. For the moment it is only adapted to plate with 1536 cells.
merging_files <- function(file_lib, data_gene, cells = 1536, rows_plate, columns_plate) {
  
  # Data library
  df_lib <- suppressMessages(read_csv(file_lib, show_col_types = FALSE)) %>%
    drop_na(`Plate #`, Column, Row) %>% 
    arrange(`Plate #`, Column, Row) %>%  ## Ordering by Plate, Column and Row.
    mutate(`Plate #` = as.character(`Plate #`),
           Column = as.character(Column)) %>% 
    select(`Plate #`, Row, Column, ORF, Gene, contains("mutation", ignore.case = TRUE))
  
  plates_lib <- unique(df_lib$`Plate #`)
  
  ## New Database with old and new coordinates
  nrow_plate <- cells # CHANGE IF NECCESARY 
  new_rows_plate <- rep(rows_plate, times = nrow_plate/length(rows_plate))
  new_cols_plate <- rep(columns_plate, each = nrow_plate/length(columns_plate))
  old_rows_plate <- rep(unique(df_lib$Row), each = 2, times = nrow_plate/length(rows_plate))
  old_cols_plate <- rep(unique(df_lib$Column), each = nrow_plate/length(columns_plate)*2) # Necessary multiply for the replicates, IN this case 2
  nplates <- length(plates_lib)
  
  new_data <- data.frame(
    New_plate = as.character(rep(1:nplates, each = nrow_plate)),
    New_row = rep(new_rows_plate, times = nplates),
    New_column = as.character(rep(new_cols_plate, times = nplates)),
    Old_plate = as.character(rep(1:nplates, each = nrow_plate)),
    Old_row = rep(old_rows_plate, times = nplates),
    Old_column = rep(old_cols_plate, times = nplates),
    stringsAsFactors = FALSE
  ) %>% 
    left_join(data_gene, by = c("New_plate", "New_row", "New_column")) %>% # Join the Plates format
    left_join(df_lib, by = c("ORF"= "ORF", "Old_row" = "Row", 
                             "Old_column" = "Column", "Old_plate" = "Plate #"))
  
  return(new_data)
}


# Colony Areas function ---------------------------------------------------
assign_names <- function(x, names){colnames(x) <- names;return(x)}

cbind.fill <- function(...){
  # Credits: https://gist.github.com/abelsonlive/4112423
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(nrow = n-nrow(x), ncol = ncol(x))))) 
}

# Colony_areas function that read the ColonyAreas file from the screening and
# prepare a better format to work.
# @filename: Character. File with the colony areas from the software.
Colony_areas <- function(filename){
  # Read Colony File
  lines <- readLines(filename)
  
  ## Parse and clean plate
  plates <- grep('[A-Za-z]', lines, value = TRUE)
  
  ## Drop plate and parse measurements
  cm <- lines[-grep("[A-Za-z]", lines)]
  
  tbl <- do.call(rbind, strsplit(cm, split = "\t"))
  
  data_colony <- tbl %>% 
    assign_names(c("size", "circ")) %>% 
    as.data.frame(stringsAsFactors = FALSE) %>%
    mutate(size = as.numeric(.data$size),
           circ = as.numeric(.data$circ))
  
  # Extract values
  nobs <- nrow(data_colony) #Number of Observations
  density <- nobs/length(plates) # Colonies for plate
  
  df_colony <- data_colony %>% 
    mutate(
      id = rep(plates, length.out = nobs, each = density)
    ) %>%
    mutate(id = str_replace_all(id, '"\"', ''),
           id = gsub('[\r\n\t]', '', id)) %>%
    separate(id, c("Plate", "replicate", NA), sep = ",") %>%
    separate(Plate, c("plate_name", "trat"), sep = "_") %>% 
    mutate(plate_name = gsub('\"', '', plate_name),
           replicate = gsub('\"', '', replicate))
  
  return(df_colony)
}

# Screen_processing function to clean screen Data and separate  by
# a screen pattern and select the plate letter (Updated)
# @data: Data frame. Screen data that contain the colony areas. Direct input from 
# @Colony_areas function.
# @screen_pattern: Character. It can be used "YKO" or "ts" (depleted)
# @letter: Character. Select a letter that describe the screening like "A" or "B" (depleted)
Screen_processing <- function(data) {
  df <- data %>% 
    mutate(
      plate_name = str_replace_all(plate_name, " ", ""),
      Plate_number = str_extract(plate_name, "(\\d)+"),
      plate_set = str_replace(plate_name, "(\\d)+", ""),
      Plate_number = as.numeric(Plate_number)
    ) %>% 
    unite("Trat_code", trat, replicate, plate_set, sep = "_") %>% 
    select(-circ)
  return(df)
}

# Version 0 ---------------------------------------------------------------

# Colony_format function that works with the Colony areas table to convert in a 
# matrix as the library data is filled.
# @data:
# @type_letter: Character. Only two possible options, "A" or "B"
# Colony_format <- function(data, type_letter){
#   
#   nrows <- data %>% 
#     group_by(Trat_code) %>% 
#     summarise(n = n()) %>% 
#     filter(n == max(n)) %>%
#     distinct(n) %>% 
#     pull(n)
#   colony_col <- unique(data$Trat_code)
#   nplates <- length(unique(data$plate_name))
#   
#   cat(paste0("Screen File with: ", nrows, " rows, ", length(colony_col), " columns and ", nplates, " plates\n"))
#   
#   colony_database <- matrix(nrow = nrows, ncol = length(colony_col))
#   
#   ## Filling Matrix
#   for (t in 1:length(colony_col)) {
#     replicate_data <- vector(mode = "numeric") # For each plate
#     for (i in 1:nplates) {
#       replicate <- data %>% 
#         filter(Trat_code == colony_col[t], Plate_number == i) %>% 
#         pull(size)
#       if (length(replicate) == 0) {
#         replicate <- rep(NA, 1536)
#         replicate_data <- c(replicate_data, replicate)
#       } else {
#         replicate_data <- c(replicate_data, replicate)
#       }
#     }
#     colony_database[,t] <- replicate_data
#   }
#   correct_names <- str_replace_all(colony_col, "CanFoa", "CanFOA")
#   correct_names <- str_replace(correct_names, '\"', '')
#   colnames(colony_database) <- correct_names
#   
#   if (type_letter == "B") {
#     df <- as.data.frame(colony_database) %>% 
#       select(starts_with("URA"), CanFOA_1:CanFOA_6) 
#   } else {
#     df <- as.data.frame(colony_database) %>% 
#       select(YPD_1:YPD_6, CanFOA_1:CanFOA_6)
#   }
#   
#   return(df)
# }

# DataColony_Filling function to process the data alongside with Library screen information
# and generate a excel file with the next sheet:
# @fileScreen: Character. Database created with the genenames, new and old columns
# @data: Data frame. Data Colony in a table format. Output from Colony_format function
# @type_file: Character. Only two possible options, "A", "B" 
# @fileName: Character. Path and file name for the Excel File
# @isTS: Logical. If TRUE the files are from TS and contains the column "mutation"
# @data_positions: Character. Path to file that have positions to remove
# DataColony_Filling <- function(fileScreen, data, type_file, fileName, isTS = FALSE, data_positions) {
#   screen_file <- fileScreen#read_csv(fileScreen, show_col_types = FALSE)
#   
#   database <- cbind(screen_file, data)
#   
#   OutFile <- createWorkbook() # Excel File
#   
#   # Raw Data
#   
#   raw_data <- database %>% 
#     mutate(
#       Gene = if_else(is.na(Gene) | Gene == "-", ORF, Gene))
#   
#   if ( type_file == "B" ) {
#     raw_data <- raw_data %>% 
#       group_by(New_plate) %>% 
#       mutate(Median_URA = median(URA_1)) %>% 
#       ungroup() %>% 
#       mutate(
#         URA_median_50 = Median_URA*0.5,
#         URA_median_20 = Median_URA*0.2,
#         URA_colonies = if_else(URA_1 > URA_median_50, 1*6,0)
#       ) %>% 
#       rowwise() %>% 
#       mutate(CanFOA_colonies = sum(c(CanFOA_1,CanFOA_2,CanFOA_3,CanFOA_4,CanFOA_5,CanFOA_6) > URA_median_20, na.rm = TRUE),
#              perc_GCR = CanFOA_colonies/URA_colonies) 
#   } else {
#     raw_data <-  raw_data %>% 
#       rowwise() %>% 
#       mutate(YPD_median = median(c(YPD_1,YPD_2,YPD_3,YPD_4,YPD_5,YPD_6), na.rm = TRUE),
#              Median_50 = YPD_median*0.5,
#              Median_20 = YPD_median*0.2,
#              YPD_colonies = sum(c(YPD_1,YPD_2,YPD_3,YPD_4,YPD_5,YPD_6) > Median_50, na.rm = TRUE),
#              CanFOA_colonies = sum(c(CanFOA_1,CanFOA_2,CanFOA_3,CanFOA_4,CanFOA_5,CanFOA_6) > Median_20, na.rm = TRUE),
#              perc_GCR = CanFOA_colonies/YPD_colonies)
#   }
#   
#   ## Sheet Raw Data
#   addWorksheet(OutFile, "Raw Data")
#   writeData(OutFile, sheet = "Raw Data", x = raw_data)
#   cat("Raw Data processing complete.\n")
#   
#   # Ordering and Grouping
#   if (isTS){
#     grouped_data <- raw_data %>% 
#       group_by(Old_plate, Old_row, Old_column, ORF, Gene, mutation)
#   } else {
#     grouped_data <- raw_data %>% 
#       group_by(Old_plate, Old_row, Old_column, ORF, Gene)
#   }
#   
#   if (type_file == "B") {
#     grouped_data <- grouped_data %>% 
#       summarise(
#         Total = n(),
#         URA_colonies_total = sum(URA_colonies, na.rm = TRUE),
#         CanFOA_colonies_total = sum(CanFOA_colonies, na.rm = TRUE),
#         .groups = "drop"
#       ) %>% 
#       ungroup() %>% 
#       mutate(
#         perc_GCR = round((CanFOA_colonies_total/URA_colonies_total)*100, 3)
#       )
#   } else {
#     grouped_data <- grouped_data %>% 
#       summarise(
#         Total = n(),
#         YPD_colonies_total = sum(YPD_colonies, na.rm = TRUE),
#         CanFOA_colonies_total = sum(CanFOA_colonies, na.rm = TRUE),
#         .groups = "drop"
#       ) %>% 
#       ungroup() %>% 
#       mutate(
#         perc_GCR = round((CanFOA_colonies_total/YPD_colonies_total)*100, 3)
#       )
#   }
#   
#   ordered_data  <-grouped_data %>%
#     arrange(desc(Gene), ORF) 
#   
#   ## Sheet Ordered Data
#   addWorksheet(OutFile, "Ordered")
#   writeData(OutFile, sheet = "Ordered", x = ordered_data)
#   cat("Ordered Data Complete.\n")
#   
#   ## Ranked
#   ranked_data <- grouped_data %>% 
#     arrange(desc(perc_GCR)) 
#   
#   ## Sheet Ranked Data
#   addWorksheet(OutFile, "Ranked")
#   writeData(OutFile, sheet = "Ranked", x = ranked_data)
#   cat("Ranked Data Complete.\n")
#   
#   # Filtered
#   filtered_data <- grouped_data %>% 
#     filter(YPD_colonies_total >= 12)
#   
#   if (isTS){
#     filtered_data <- filtered_data %>% 
#       arrange(desc(perc_GCR))
#   } else {
#     positions <- read_csv(data_positions, show_col_types = FALSE) %>% 
#       pull(Position)
#     # positions <- c("1A18","1A20","1A22","1A24","1C2","1C4","1C6","1C8","1C10","1C12","1C14", "1C16", "1C20") ## From a File?
#     filtered_data <- filtered_data %>% 
#       unite(Position, Old_plate, Old_row, Old_column, sep = "", remove = FALSE) %>% 
#       filter(!(Position %in% positions)) %>% 
#       select(-Position) %>% 
#       arrange(desc(perc_GCR))
#   }
#   
#   ## Sheet Filtered Data
#   addWorksheet(OutFile, "Filtered")
#   writeData(OutFile, sheet = "Filtered", x = filtered_data)
#   cat("Filtered Data Complete.\n")
#   
#   ## Counting
#   counting_genes <- raw_data %>% 
#     group_by(Gene, ORF,) %>% 
#     summarise(n = n(),.groups = "drop")
#   
#   ## Sheet Counting Data
#   addWorksheet(OutFile, "Counting")
#   writeData(OutFile, sheet = "Counting", x = counting_genes)
#   cat("Counting Data Complete.\n")
#   
#   # if (MakeCalc) {
#   # }
#   saveWorkbook(OutFile, paste0(fileName, Sys.Date(),".xlsx"))
#   cat(paste("Data processing complete. Excell file with the name ", fileName, ".xlsx\n", sep = ""))
# }

# Version 01 --------------------------------------------------------------
# Colony_format function that works with the Colony areas table to convert in a 
# matrix as the library data is filled.
# Changes: Now reads the plates by SP and NSP
# @data:
# @type_letter: Character. Only two possible options, "A" or "B" (depleted)
Colony_format <- function(data) {
  nrows <- data %>% 
    group_by(Trat_code) %>% 
    summarise(n = n()) %>% 
    filter(n == max(n)) %>% 
    distinct(n) %>% 
    pull(n)
  colony_col <- unique(data$Trat_code)
  nplates <- length(unique(data$Plate_number))
  
  cat(paste0("Screen File with: ", nrows, " rows, ", length(colony_col), " columns and ", nplates, " plates.\n"))
  
  if (all(str_detect(colony_col, "^SP|^NSP"))) {
    cat("All plates have  a correct name: SP or NSP.\n")
  } else {
    stop("Some plates does not have a correct name: SP or NSP.\n")
  }
  
  colony_database <- matrix(nrow = nrows, ncol = length(colony_col))
  
  ## Filling Matrix
  for (t in 1:length(colony_col)) {
    replicate_data <- vector(mode = "numeric") # For each plate
    for (i in 1:nplates) {
      replicate <- data %>% 
        filter(Trat_code == colony_col[t], Plate_number == i) %>% 
        pull(size)
      if (length(replicate) == 0) {
        replicate <- rep(NA, 1536)
        replicate_data <- c(replicate_data, replicate)
      } else {
        replicate_data <- c(replicate_data, replicate)
      }
    }
    colony_database[,t] <- replicate_data
  }
  
  colnames(colony_database) <- colony_col
  
  df <- as.data.frame(colony_database) %>% 
    select(starts_with("NSP"), starts_with("SP"))
  
  return(df)
}


# DataColony_Filling function to process the data alongside with Library screen information
# and generate a excel file with the next sheet:
# @fileScreen: Character. Database created with the genenames, new and old columns
# @data: Data frame. Data Colony in a table format. Output from Colony_format function
# @type_file: Character. Only two possible options, "A", "B" (depleted)
# @isTS: Logical. (depleted)
# @fileName: Character. Path and file name for the Excel File
# @Med_higher: Numeric. Value to change the MEDIAN for Non-Selective Plates. By default 0.5
# @Med_lower: Numeric. Value to change the lower MEDIAN for Selective Plates. By default 0.2
# @threshold: Numeric. Minimal value to filter by de colonies Non-selective plates. By default 12
DataColony_Filling <- function(fileScreen, 
                               data, 
                               fileName, 
                               times = 1,
                               Med_higher = 0.5,
                               Med_lower = 0.2,
                               threshold = 12) {
  screen_file <- fileScreen
  
  database <- cbind.fill(screen_file, data) %>% 
    data.frame(stringsAsFactors = FALSE) %>% 
    mutate_at(vars(starts_with("NSP"), starts_with("SP")), as.numeric)
  
  OutFile <- createWorkbook() # Excel File
  
  # Raw Data
  
  ## Conditions
  Med_high_Char <- paste0("Median_", Med_higher*100)
  Med_low_Char <- paste0("Median_", Med_lower*100)
  cat(paste0("Processing the colonies using the parameters: \n"))
  cat(paste0(Med_higher*100, " % of the Median Non-Selective Plates.\n"))
  cat(paste0(Med_lower*100, " % of the Median Selective Plates.\n"))
  cat(paste0("Multiply colonies from Non-Selective Plates by: ", times, "\n"))
  
  raw_data <-  database %>% 
    mutate(
      Gene = if_else(is.na(Gene) | Gene == "-", ORF, Gene)
    ) %>% 
    rowwise() %>% 
    mutate(
      Median_NSP = median(c_across(starts_with("NSP")), na.rm = TRUE),
      "Median_{Med_higher*100}" := Median_NSP*Med_higher,
      "Median_{Med_lower*100}" := Median_NSP*Med_lower,
      colonies_NSP = sum(c_across(starts_with("NSP")) > c_across(starts_with(Med_high_Char)), na.rm = TRUE)*times,
      colonies_SP = sum(c_across(starts_with("SP")) > c_across(starts_with(Med_low_Char)), na.rm = TRUE),
      Freq_percent = colonies_SP/colonies_NSP
    ) 
  
  ## Sheet Raw Data
  addWorksheet(OutFile, "Raw Data")
  writeData(OutFile, sheet = "Raw Data", x = raw_data)
  cat("Raw Data Complete.\n\n")
  
  raw_ordered <-  raw_data %>% 
    arrange(as.numeric(New_plate), desc(ORF))
  
  ## Sheet Raw Data
  addWorksheet(OutFile, "Raw-Ordered Data")
  writeData(OutFile, sheet = "Raw-Ordered Data", x = raw_ordered)
  cat("Raw Data Ordered Complete.\n\n")
  
  # Ordering and Grouping

  x <- raw_data %>% 
    select(starts_with("Old"), contains("ORF"), contains("Gene"), starts_with("mutation", ignore.case = TRUE)) %>% 
    names()
  cat("Grouping colonies by the next variables: \n")
  cat(x, "\n\n")
  
  grouped_data <- raw_data %>% 
    group_by(Old_plate, Old_row, Old_column, ORF, Gene, c_across(starts_with("mutation", ignore.case = TRUE)))

  
  grouped_data <- grouped_data %>% 
    summarise(
      Total = n(),
      colonies_total_NSP = sum(colonies_NSP, na.rm = TRUE),
      colonies_total_SP = sum(colonies_SP, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    ungroup() %>% 
    mutate(
      Freq_percent = round((colonies_total_SP/colonies_total_NSP)*100, 3)
    )
  
  names(grouped_data)[grepl("mutation", names(grouped_data))] <- "Mutation"
  
  ordered_data  <-grouped_data %>%
    arrange(desc(Gene), ORF) 
  
  ## Sheet Ordered Data
  addWorksheet(OutFile, "Ordered")
  writeData(OutFile, sheet = "Ordered", x = ordered_data)
  cat("Order Complete.\n")
  
  ## Ranked
  ranked_data <- grouped_data %>% 
    arrange(desc(Freq_percent)) 
  
  ## Sheet Ranked Data
  addWorksheet(OutFile, "Ranked")
  writeData(OutFile, sheet = "Ranked", x = ranked_data)
  cat("Ranked Complete.\n")
  
  # Filtered
  cat(paste0("Filtering Data with the treshold ", threshold, " in total colonies for Non-Selective plates.\n"))
  filtered_data <- grouped_data %>% 
    filter(colonies_total_NSP >= threshold | (Gene != "" & ORF != "")) %>% 
    arrange(desc(Freq_percent)) %>% 
    ungroup()
  
  ## Sheet Filtered Data
  addWorksheet(OutFile, "Filtered")
  writeData(OutFile, sheet = "Filtered", x = filtered_data)
  cat("Filter Complete.\n")
  
  ## Counting filtered: Using the filtered data and calculate the Frec percentage
  counting_genes <- filtered_data %>% # or row data
    group_by(Gene, ORF, c_across(starts_with("mutation", ignore.case = TRUE))) %>% 
    summarise(n = n(),
              colonies_total_NSP = sum(colonies_total_NSP, na.rm = TRUE),
              colonies_total_SP = sum(colonies_total_SP, na.rm = TRUE),
              .groups = "drop") %>% 
    ungroup() %>% 
    mutate(
      Freq_percent = round((colonies_total_SP/colonies_total_NSP)*100, 3)
    ) %>% 
    arrange(desc(Freq_percent))
  names(counting_genes)[grepl("mutation", names(counting_genes))] <- "Mutation"
  
  ## Sheet Counting Data
  addWorksheet(OutFile, "Counting")
  writeData(OutFile, sheet = "Counting", x = counting_genes)
  cat("Count Complete.\n")
  
  saveWorkbook(OutFile, paste0(fileName, "_", Sys.Date(), ".xlsx"))
  cat(paste("Excell file created with the name: ", fileName, "_", Sys.Date(),".xlsx\n", sep = ""))
}
