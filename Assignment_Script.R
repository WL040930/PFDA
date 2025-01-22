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
data$OS_Cleaned <- case_when(
  grepl("Windows|Win|Microsoft", data$OS, ignore.case = TRUE) & !grepl("Phone|Mobile", data$OS, ignore.case = TRUE) ~ "Windows", 
  grepl("Linux|Ubuntu|Debian|Red Hat|CentOS|Fedora|Mint", data$OS, ignore.case = TRUE) ~ "Linux",  
  grepl("BSD|FreeBSD|OpenBSD|NetBSD|Tru64|AIX|Solaris|HP-UX|IRIX|Unix", data$OS, ignore.case = TRUE) ~ "Unix",  
  grepl("MacOS|OS X", data$OS, ignore.case = TRUE) ~ "MacOS",  
  grepl("Android|iOS|Windows Phone|iPXE|F5|Cisco|Juniper|Netgear|HP", data$OS, ignore.case = TRUE) ~ "Embedded/Other",  
  TRUE ~ "Others"
)

cat("Unique values in the cleaned OS field:\n")
print(unique(data$OS_Cleaned))

os_summary <- table(data$OS_Cleaned)
cat("\nSummary of OS categories:\n")
print(os_summary)
