# Lim Wei Lun - TP069058
# Goh Xin Tong - TP069712
# Chang Qi Eue - TP065956
# Lee Jun Zhe - TP065879

# Import required libraries
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("DBI", quietly = TRUE)) install.packages("DBI")
if (!requireNamespace("RSQLite", quietly = TRUE)) install.packages("RSQLite")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
if (!requireNamespace("reshape2", quietly = TRUE)) install.packages("reshape2")

library(readxl)
library(dplyr)
library(ggplot2)
library(DBI)
library(RSQLite)
library(lubridate)
library(reshape2)

# Declare file path for the dataset

FILE_PATH <- "C:\\Users\\user\\New folder (2)\\OneDrive - Asia Pacific University\\APU Degree\\YEAR2 SEM1\\PFDA\\PFDA Assigment\\4.hackingdata.csv"
# Import the CSV file
data <- read.csv(FILE_PATH, stringsAsFactors = FALSE)

# Change all column titles to lowercase
colnames(data) <- tolower(colnames(data))

# Replace missing values in other columns
data <- replace(data, data == "", NA)

# Replace missing values for specific columns
data$notify[is.na(data$notify)] <- "Unknown"
data$url[is.na(data$url)] <- "Unknown"
data$ip[is.na(data$ip)] <- "Unknown"
data$country[is.na(data$country)] <- "Unknown"
data$os[is.na(data$os)] <- "Unknown"
data$webserver[is.na(data$webserver)] <- "Unknown"

# For certain columns, replace NA with more specific values
data$encoding[is.na(data$encoding)] <- "NULL"
data$lang[is.na(data$lang)] <- "NULL"
data$ransom[is.na(data$ransom)] <- NA
data$downtime[is.na(data$downtime)] <- NA
data$loss[is.na(data$loss)] <- NA

# Standardize incorrect entries
data$os[data$os == "Unkno"] <- "Unknown"
data$country[data$country == "UNKNOWN"] <- "Unknown"

# Check for duplicate rows and remove them
data <- data[!duplicated(data), ]

# Categorize OS field into broader categories and replace in the original data
data$os_category <- case_when(
  grepl("Windows|Win|Microsoft", data$os, ignore.case = TRUE) & !grepl("Phone|Mobile", data$os, ignore.case = TRUE) ~ "Windows", 
  grepl("Linux|Ubuntu|Debian|Red Hat|CentOS|Fedora|Mint", data$os, ignore.case = TRUE) ~ "Linux",  
  grepl("BSD|FreeBSD|OpenBSD|NetBSD|Tru64|AIX|Solaris|HP-UX|IRIX|Unix", data$os, ignore.case = TRUE) ~ "Unix",  
  grepl("MacOS|OS X", data$os, ignore.case = TRUE) ~ "MacOS",  
  grepl("Android|iOS|Windows Phone|iPXE|F5|Cisco|Juniper|Netgear|HP", data$os, ignore.case = TRUE) ~ "Embedded",  
  TRUE ~ "Others"
)

# Convert incomplete year-only entries (e.g., 2015) to full date (e.g., 2015-01-01)
data$date <- ifelse(grepl("^\\d{4}$", data$date), paste0(data$date, "-01-01"), data$date)

 data$date <- parse_date_time(data$date, orders = c("dmy", "mdy", "ymd", "b d Y"))

# Handle invalid dates by setting them to 'Unknown'
invalid_dates <- is.na(data$date)
if (any(invalid_dates)) {
  print("unknown")
  cat("There are invalid dates. Replacing them with 'Unknown'.\n")
  data$date <- as.character(data$date)  # Convert to character to allow "Unknown"
  data$date[invalid_dates] <- "Unknown"
} else {
  data$date <- as.character(data$date)
}

# categorize the date into time periods
data$time_period <- case_when(
  grepl("Unknown", data$date) ~ "Unknown",  # For 'Unknown' dates
  as.numeric(format(as.Date(data$date), "%Y")) >= 1900 & as.numeric(format(as.Date(data$date), "%Y")) <= 1999 ~ "1900 - 1999",
  as.numeric(format(as.Date(data$date), "%Y")) >= 2000 & as.numeric(format(as.Date(data$date), "%Y")) <= 2009 ~ "2000 - 2009", 
  as.numeric(format(as.Date(data$date), "%Y")) >= 2010 & as.numeric(format(as.Date(data$date), "%Y")) <= 2019 ~ "2010 - 2019", 
  as.numeric(format(as.Date(data$date), "%Y")) >= 2020 ~ "2020 - Present", 
  TRUE ~ "Unknown"  
)

View(data)


# Lim Wei Lun
# RQ 1 - 1.	Which operating system categories experience the most significant downtime fluctuations across different time periods?
aggregated_data <- data %>%
  group_by(time_period, os_category) %>%
  summarise(avg_downtime = mean(downtime, na.rm = TRUE))

# Clustered bar chart of average system downtime by time period and OS category
ggplot(aggregated_data, aes(x = time_period, y = avg_downtime, fill = os_category)) +
  geom_bar(stat = "identity", position = "dodge") +  # Position the bars next to each other
  labs(title = "Average System Downtime by Time Period and OS Category",
       x = "Time Period",
       y = "Average Downtime (in hours)",
       fill = "Operating System Category") +
  theme_minimal() +
  scale_fill_manual(values = c("Windows" = "red", "Linux" = "blue", "Unix" = "green", "MacOS" = "purple", "Embedded" = "orange", "Others" = "grey"))  # Customize colors for OS categories

# Extra Feature 
# Create the heatmap with a red color gradient

ggplot(aggregated_data, aes(x = time_period, y = os_category, fill = avg_downtime)) +
  geom_tile() +
  labs(title = "Heatmap of Average System Downtime by Time Period and OS Category",
       x = "Time Period",
       y = "Operating System Category",
       fill = "Average Downtime") +
  theme_minimal() +
  scale_fill_gradientn(colors = c("white", "lightcoral", "red", "darkred"))











# Box plot: Time periods associated with the highest system downtime across different OS categories
ggplot(data, aes(x = time_period, y = downtime, fill = os_category)) +
  geom_boxplot() +
  labs(title = "System Downtime Across Time Periods by Operating System Category",
       x = "Time Period",
       y = "System Downtime (in hours)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Facet grid: System downtime by operating system and time period
ggplot(data, aes(x = time_period, y = downtime, fill = os_category)) +
  geom_boxplot() +
  facet_wrap(~ os_category, scales = "free_y") +
  labs(title = "System Downtime by Operating System Across Time Periods",
       x = "Time Period",
       y = "System Downtime (in hours)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Line plot: Time-based factors influencing downtime patterns across OS categories
ggplot(data, aes(x = as.factor(time_period), y = downtime, group = os_category, color = os_category)) +
  geom_line() +
  geom_point() +
  labs(title = "System Downtime Trends by Time Period and Operating System",
       x = "Time Period",
       y = "System Downtime (in hours)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Violin plot: System downtime by time period
ggplot(data, aes(x = time_period, y = downtime, fill = time_period)) +
  geom_violin(trim = FALSE) +
  labs(title = "Distribution of System Downtime Across Time Periods",
       x = "Time Period",
       y = "System Downtime (in hours)") +
  theme_minimal() +
  theme(legend.position = "none")

# Violin plot: System downtime by operating system category
ggplot(data, aes(x = os_category, y = downtime, fill = os_category)) +
  geom_violin(trim = FALSE) +
  labs(title = "Distribution of System Downtime by Operating System Category",
       x = "Operating System Category",
       y = "System Downtime (in hours)") +
  theme_minimal() +
  theme(legend.position = "none")
