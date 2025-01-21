# Lim Wei Lun - TP069058
# Goh Xin Tong - TP069712
# Chang Qi Eue - TP065956
# Lee Jun Zhe - TP065879

# Purpose: Clean the dataset by removing duplicate rows

# Data Overview:
# - data: Original CSV data
# - data_unique: Cleaned data after removing duplicate rows
# - duplicate_rows: Subset containing only the duplicate rows
# - duplicates: Logical vector indicating duplicate rows

#-----------------------------------------------------------------

# Import required library
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl") # Install the readxl package if not already installed
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("DBI", quietly = TRUE)) {
  install.packages("DBI")
}
if (!requireNamespace("RSQLite", quietly = TRUE)) {
  install.packages("RSQLite")
}

library(readxl) # Load the readxl library
library(dplyr)
library(ggplot2)
library(DBI)
library(RSQLite)


#-----------------------------------------------------------------

# Declare file path
FILE_PATH <- "C:\\Users\\user\\New folder (2)\\OneDrive - Asia Pacific University\\APU Degree\\YEAR2 SEM1\\PFDA\\PFDA Assigment\\4.hackingdata.csv"

# Import the 4.hackingdata.csv file
data <- read.csv(FILE_PATH, stringsAsFactors = FALSE) # Read CSV without converting strings to factors

#-----------------------------------------------------------------

# Output the 4.hackingdata.csv file
View(data)

#-----------------------------------------------------------------


# Display summary of the original data
cat("Summary of the original data:\n")
print(summary(data))

# Check for duplicate rows
cat("\nChecking for duplicate rows...\n")
duplicates <- duplicated(data) # Create a logical vector to mark duplicate rows

# Identify and display duplicate rows if present
if (any(duplicates)) {
  cat("\nDuplicate rows found:\n")
  duplicate_rows <- data[duplicates, ] # Extract the duplicate rows
  print(duplicate_rows)
} else {
  cat("\nNo duplicate rows found.\n")
}

# Remove duplicate rows
data_unique <- data[!duplicates, ] # Retain only unique rows

# Display summary of the cleaned data
cat("\nSummary of the cleaned data:\n")
print(summary(data_unique))


# End of script
cat("\nDuplicate removal process completed successfully.\n")

sum(is.na(data_unique))
