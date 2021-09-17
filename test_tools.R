# Colony_format function that works with the Colony areas table to convert in a 
# matrix as the library data is filled.
# Changes: Now reads the plates by SP and NSP
# @data:
# @type_letter: Character. Only two possible options, "A" or "B"
Colony_format <- function(data, type_letter) {
  nrows <- data %>% 
    group_by(Trat_code) %>% 
    summarise(n = n()) %>% 
    filter(n == max(n)) %>% 
    distinct(n) %>% 
    pull(n)
  colony_col <- unique(data$Trat_code)
  nplates <- length(unique(data$plate_name))
  
  cat(paste0("Screen File with: ", nrows, " rows, ", length(colony_col), " columns and ", nplates, " plates\n"))
  
  if (all(str_detect(colony_col, "^SP|^NSP"))) {
    cat("All plates have  a correct name: SP or NSP")
  } else {
    stop("Some plates does not have a correct name: SP or NSP")
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
# @type_file: Character. Only two possible options, "A", "B" 
# @fileName: Character. Path and file name for the Excel File
# @Med_NSP: Numeric. Value to change the MEDIAN for Non-Selective Plates. By default 0.5
# @Med_SP: Numeric. Value to change the MEDIAN for Selective Plates. By default 0.2
# @Threshold: Numeric. Minima value to filter by de colonies Non-selective plates. By default 12
DataColony_Filling <- function(fileScreen, 
                               data, 
                               type_file, 
                               fileName, 
                               Med_higher = 0.5,
                               Med_lower = 0.2,
                               threshold = 12) {
  screen_file <- fileScreen#read_csv(fileScreen, show_col_types = FALSE)
  
  database <- cbind(screen_file, data)
  
  OutFile <- createWorkbook() # Excel File
  
  # Raw Data
  
  ## Conditions
  Med_high_Char <- paste0("Median_", Med_higher*100)
  Med_low_Char <- paste0("Median_", Med_lower*100)
  cat(paste0("Processing the colonies using the parameters:"))
  cat(paste0(Med_higher*100, " of the Median Non-Selective Plates"))
  cat(paste0(Med_lower*100, " of the Median Selective Plates"))
  
  raw_data <-  database %>% 
    mutate(
      Gene = if_else(is.na(Gene) | Gene == "-", ORF, Gene)
    ) %>% 
    rowwise() %>% 
    mutate(
      Median_NSP = median(c_across(starts_with("NSP")), na.rm = TRUE),
      "Median_{Med_higher*100}" := Median_NSP*Med_higher,
      "Median_{Med_lower*100}" := Median_NSP*Med_lower,
      colonies_NSP = sum(c_across(starts_with("NSP")) > c_across(starts_with(Med_high_Char)), na.rm = TRUE),
      colonies_SP = sum(c_across(starts_with("SP")) > c_across(starts_with(Med_low_Char)), na.rm = TRUE),
      perc_GCR = colonies_SP/colonies_NSP
    )

  ## Sheet Raw Data
  addWorksheet(OutFile, "Raw Data")
  writeData(OutFile, sheet = "Raw Data", x = raw_data)
  cat("Raw Data processing complete.\n")
  
  # Ordering and Grouping
  if (isTS){
    grouped_data <- raw_data %>% 
      group_by(Old_plate, Old_row, Old_column, ORF, Gene, mutation)
  } else {
    grouped_data <- raw_data %>% 
      group_by(Old_plate, Old_row, Old_column, ORF, Gene)
  }
  
  grouped_data <- grouped_data %>% 
    summarise(
      Total = n(),
      colonies_total_NSP = sum(colonies_NSP, na.rm = TRUE),
      colonies_total_SP = sum(colonies_SP, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    ungroup() %>% 
    mutate(
      perc_GCR = round((colonies_total_SP/colonies_total_NSP)*100, 3)
    )
  
  ordered_data  <-grouped_data %>%
    arrange(desc(Gene), ORF) 
  
  ## Sheet Ordered Data
  addWorksheet(OutFile, "Ordered")
  writeData(OutFile, sheet = "Ordered", x = ordered_data)
  cat("Ordered Data Complete.\n")
  
  ## Ranked
  ranked_data <- grouped_data %>% 
    arrange(desc(perc_GCR)) 
  
  ## Sheet Ranked Data
  addWorksheet(OutFile, "Ranked")
  writeData(OutFile, sheet = "Ranked", x = ranked_data)
  cat("Ranked Data Complete.\n")
  
  # Filtered
  cat(paste0("Filtering Data with the treshold ", threshold, " in total colonies for Non-Selective plates."))
  filtered_data <- grouped_data %>% 
    filter(colonies_total_NSP >= threshold) %>% 
    arrange(desc(perc_GCR))
  
  # if (isTS){
  #   filtered_data <- filtered_data %>% 
  #     arrange(desc(perc_GCR))
  # } else {
  #   positions <- read_csv(data_positions, show_col_types = FALSE) %>% 
  #     pull(Position)
  #   # positions <- c("1A18","1A20","1A22","1A24","1C2","1C4","1C6","1C8","1C10","1C12","1C14", "1C16", "1C20") ## From a File?
  #   filtered_data <- filtered_data %>% 
  #     unite(Position, Old_plate, Old_row, Old_column, sep = "", remove = FALSE) %>% 
  #     filter(!(Position %in% positions)) %>% 
  #     select(-Position) %>% 
  #     arrange(desc(perc_GCR))
  # }
  
  ## Sheet Filtered Data
  addWorksheet(OutFile, "Filtered")
  writeData(OutFile, sheet = "Filtered", x = filtered_data)
  cat("Filtered Data Complete.\n")
  
  ## Counting
  counting_genes <- raw_data %>% 
    group_by(Gene, ORF,) %>% 
    summarise(n = n(),.groups = "drop")
  
  ## Sheet Counting Data
  addWorksheet(OutFile, "Counting")
  writeData(OutFile, sheet = "Counting", x = counting_genes)
  cat("Counting Data Complete.\n")
  
  saveWorkbook(OutFile, paste0(fileName, Sys.Date(),".xlsx"))
  cat(paste("Data processing complete. Excell file with the name ", fileName, ".xlsx\n", sep = ""))
}



