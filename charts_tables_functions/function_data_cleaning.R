################################################################################
# Script name: function_data_cleaning.R
# Created by: Calli Dougall
# Date created: 21/10/2026
# 
# Description: creates different functions to clean excel workbooks on import.
# These are used in the census_data_process script.
#
# Functions created in this script:
# 1. remove_until_source
# 2. clean_data
# 3. clean_headers
# 4. clean_cells
# 5. read_and_process_sheet
# 6. round_df
#
################################################################################

# 1. remove_until_source -------------------------------------------------------

# Function to remove rows until the first occurrence of "Source:" in the first column
remove_until_source <- function(data) {
  
  # Remove the first row (which is now the original header row)
  data <- data[-1, ]
  
  # Now remove rows until the first occurrence of "Source:"
  source_row <- which(str_detect(data[[1]], "^Source:"))
  if (length(source_row) > 0) {
    data <- data[(source_row + 1):nrow(data), ]
  }
  
  return(data)
}


# 2. clean_data ----------------------------------------------------------------

# Function to remove columns and rows that are all NAs
clean_data <- function(data) {
  data <- data[, colSums(is.na(data)) < nrow(data)]  # Remove columns with all NAs
  data <- data[rowSums(is.na(data)) < ncol(data), ]  # Remove rows with all NAs
  return(data)
}


# 3. clean_headers -------------------------------------------------------------

# Function to clean header names by removing text within brackets, any '\r\n', extra spaces, and specific region names
clean_headers <- function(headers) {
  headers <- str_replace_all(headers, "\\s*\\([^\\)]+\\)", "")  # Remove text within brackets
  headers <- str_replace_all(headers, "\r\n", " ")  # Remove \r\n
  headers <- str_replace_all(headers, "\\s+", " ")  # Replace multiple spaces with a single space
  headers <- str_replace_all(headers, "\\b(North West|North East|South East|South West)\\b", "")  # Remove specific region names
  headers <- str_trim(headers)  # Trim again to remove any resulting leading or trailing spaces
  
  # Make headers unique
  headers <- make.unique(headers, sep = "_")
  
  return(headers)
}


# 4. clean_cells ---------------------------------------------------------------

# Function to clean cell values by replacing multiple spaces with a single space
clean_cells <- function(data) {
  data <- data %>% mutate(across(where(is.character), ~str_replace_all(.x, "\\s+", " ")))
  return(data)
}


# 5. read_and_process_sheet ----------------------------------------------------

# Function to read and process each sheet
read_and_process_sheet <- function(sheet) {
  df <- read_excel(file_path, sheet = sheet, skip = 5)  # Skip the first 5 rows
  colnames(df) <- clean_headers(colnames(df))  # Clean the headers
  return(df)
}

# 6. round_df ------------------------------------------------------------------

# Function to round all numeric columns to 2 decimal places
round_df <- function(df) {
  df[] <- lapply(df, function(x) if(is.numeric(x)) round(x, 2) else x)
  return(df)
}


# End of script #