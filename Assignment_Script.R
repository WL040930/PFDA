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
if (!requireNamespace("mice", quietly = TRUE)) install.packages("mice")

library(readxl)
library(dplyr)
library(ggplot2)
library(DBI)
library(RSQLite)
library(lubridate)
library(reshape2)
library(mice)
library(scales)

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
data$encoding[data$encoding == "N"] <- "NULL"

# Check for duplicate rows and remove them
data <- data[!duplicated(data), ]

# Categorize OS field into broader categories and replace in the original data

data$os_category <- case_when(
  grepl("Windows|Win|Microsoft", data$os, ignore.case = TRUE) & !grepl("Phone|Mobile", data$os, ignore.case = TRUE) ~ "Windows", 
  grepl("Linux|Ubuntu|Debian|Red Hat|CentOS|Fedora|Mint", data$os, ignore.case = TRUE) ~ "Linux",  
  grepl("BSD|FreeBSD|OpenBSD|NetBSD|Tru64|AIX|Solaris|HP-UX|IRIX|Unix", data$os, ignore.case = TRUE) ~ "Unix",  
  grepl("MacOS|OS X", data$os, ignore.case = TRUE) ~ "MacOS",  
  grepl("Android|iOS|Windows Phone|iPXE|F5|Cisco|Juniper|Netgear|HP", data$os, ignore.case = TRUE) ~ "Embedded", 
  grepl("Unknown|unknown", data$os, ignore.case = TRUE) ~ "Unknown",  # Group 'Unknown' entries
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

data$year <- as.numeric(format(as.Date(data$date), "%Y"))

# ensure downtime is numeric 
data$downtime <- as.numeric(data$downtime)

View(data)


# Lim Wei Lun

# Check how many data is unknown
sum(data$os_category == "Unknown") / nrow(data)

# Although only 4.6% of data is unknown, we choose to ignore these column 
# while conducting analysis. To avoid affecting others analysis.

# RQ 1 - Which year has the highest system downtime across different operating system categories?

# Calculate the total downtime for each year
sum_data <- data %>%
  filter(os_category != "Unknown") %>%
  group_by(year, os_category) %>%
  summarise(total_downtime = sum(downtime, na.rm = TRUE), .groups = 'drop')

ggplot(sum_data, aes(x = factor(year), y = total_downtime, fill = os_category)) +
  geom_bar(stat = "identity", position = "stack") +  # Create stacked bars
  labs(title = "Total System Downtime by Year and OS Category",
       x = "Year",
       y = "Total Downtime",
       fill = "OS Category") +
  scale_y_continuous(labels = number_format(accuracy = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# RQ 2 - What is the relationship between downtime and time periods across different OS categories?
aggregated_data <- data %>%
  filter(os_category != "Unknown") %>%
  group_by(year, os_category) %>%
  summarise(avg_downtime = mean(downtime, na.rm = TRUE))

ggplot(aggregated_data, aes(x = year, y = avg_downtime, color = os_category)) +
  geom_line() +  # Line graph showing trends
  facet_wrap(~ os_category) +  # Facet by OS category
  labs(title = "Downtime Trends by OS Category Across Years",
       x = "Year",
       y = "Average Downtime",
       color = "OS Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# RQ 3 -	Which operating system categories experience the most significant downtime fluctuations across different year?
# Calculate average downtime per year for each OS category
avg_data <- data %>%
  filter(os_category != "Unknown") %>%
  group_by(year, os_category) %>%
  summarise(avg_downtime = mean(downtime, na.rm = TRUE), .groups = 'drop')

# Create the grouped bar chart
# Custom colors using scale_fill_manual
ggplot(avg_data, aes(x = factor(year), y = avg_downtime, fill = os_category)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Grouped bars
  labs(title = "Average System Downtime by Year and OS Category",
       x = "Year",
       y = "Average Downtime (in hours)",
       fill = "OS Category") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +  # Format y-axis labels
  scale_x_discrete(breaks = seq(min(avg_data$year), max(avg_data$year), by = 1)) +  # X-axis breaks for each year
  scale_fill_manual(values = c("Embedded" = "#F39C12", "Linux" = "#27AE60", 
                               "Windows" = "#2980B9", "MacOS" = "#8E44AD", 
                               "Unix" = "#E74C3C", "Others" = "#F1C40F")) +  # Manually assign colors
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        plot.title = element_text(hjust = 0.5),  # Center title
        axis.text = element_text(size = 12),  # Adjust axis text size
        axis.title = element_text(size = 14))  # Adjust axis title size


# Extra Feature 1
# Create the heatmap with a red color gradient

ggplot(aggregated_data, aes(x = year, y = os_category, fill = avg_downtime)) +
  geom_tile() +
  labs(title = "Heatmap of Average System Downtime by Years and OS Category",
       x = "Years",
       y = "Operating System Category",
       fill = "Average Downtime") +
  theme_minimal() +
  scale_fill_gradientn(colors = c("white", "lightcoral", "red", "darkred"))



write.csv(data, "output.csv", row.names = FALSE)














#######################################
ggplot(aggregated_data, aes(x = year, y = avg_downtime, color = os_category)) +
  geom_line() +  # Line plot for each OS category over time
  geom_point() +  # Points to highlight each data point
  labs(title = "System Downtime Trends by Year and OS Category",
       x = "Year",
       y = "Average Downtime",
       color = "Operating System Category") +
  theme_minimal()

overall_avg <- data %>%
  group_by(os_category) %>%
  summarise(overall_avg_downtime = mean(downtime, na.rm = TRUE))

# Create the bar plot with labels
ggplot(overall_avg, aes(x = os_category, y = overall_avg_downtime, fill = os_category)) +
  geom_bar(stat = "identity", width = 0.7) +  # Adjusted bar width for thinner bars
  labs(title = "Overall Average System Downtime by OS Category",
       x = "Operating System Category",
       y = "Average Downtime (in hours)",
       fill = "OS Category") +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +  # Format y-axis labels to one decimal place
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate and adjust x-axis label position
        axis.text = element_text(size = 10),  # Adjust the size of the axis labels
        axis.title = element_text(size = 12)) +  # Adjust axis title size
  geom_text(aes(label = round(overall_avg_downtime, 1)),  # Display the value with 1 decimal place
            vjust = -0.5,  # Adjust the position of the label above the bar
            size = 4)  # Adjust label size
