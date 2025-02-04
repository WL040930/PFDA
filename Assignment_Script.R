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
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")

library(readxl)
library(dplyr)
library(ggplot2)
library(DBI)
library(RSQLite)
library(lubridate)
library(reshape2)
library(mice)
library(scales)
library(tidyr)

##############################################################################
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

# DATA CLEANING - COUNTRY
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

# Convert country names to lowercase
data$country <- tolower(data$country)

# DATA CLEANING - COUNTRY
# Initialize the 'continent ' column
data$continent  <- NA

# Define the countries for each continent 
north_america <- c("united states", "canada", "mexico", "costarica", "panama", "guatemala", "belize", 
                   "jamaica", "dominican republic", "cuba", "haiti")
south_america <- c("argentina", "brazil", "colombia", "peru", "venezuela", "chile", "ecuador", "bolivia", 
                   "paraguay", "suriname", "guyana")
europe <- c("united kingdom", "germany", "france", "italy", "spain", "sweden", "netherlands", "belgium", 
            "denmark", "romania", "poland", "norway", "portugal", "greece", "switzerland", "austria", 
            "finland", "ireland", "czech republic", "hungary", "bulgaria", "slovenia", "slovakia", 
            "moldova", "lithuania", "latvia", "estonia", "liechtenstein", "monaco", "san marino", "andorra", 
            "albania", "kosovo", "malta", "armenia", "georgia")
asia <- c("india", "china", "japan", "south korea", "taiwan", "hong kong", "thailand", "malaysia", 
          "singapore", "indonesia", "philippines", "pakistan", "bangladesh", "nepal", "afghanistan", 
          "israel", "lebanon", "sri lanka", "myanmar", "vietnam", "mongolia", "bangkok", "brunei darussalam", 
          "maldives")
africa <- c("south africa", "egypt", "algeria", "kenya", "nigeria", "tunisia", "ghana", "ethiopia", 
            "uganda", "tanzania", "zambia", "namibia", "gabon", "liberia", "congo", "mozambique", "senegal", 
            "mauritius", "zimbabwe", "botswana", "mali", "cameroon", "sierra leone", "burundi", "burkina faso", 
            "mauritania", "gambia", "reunion", "seychelles")
middle_east <- c("saudi arabia", "united arab emirates", "oman", "qatar", "bahrain", "kuwait", "syrian arab republic", 
                 "iraq", "yemen")
oceania <- c("australia", "new zealand", "papua new guinea", "fiji", "samoa", "vanuatu", "tonga", "solomon islands", 
             "micronesia", "marshall islands", "palau", "nauru")

# Apply categorization
data$continent [data$country %in% north_america] <- "North America"
data$continent [data$country %in% south_america] <- "South America"
data$continent [data$country %in% europe] <- "Europe"
data$continent [data$country %in% asia] <- "Asia"
data$continent [data$country %in% africa] <- "Africa"
data$continent [data$country %in% middle_east] <- "Middle East"
data$continent [data$country %in% oceania] <- "Oceania"

# Handle ambiguous or special cases
data$continent [data$country %in% c("europe", "asia/pacific region", "unknown", "anonymous proxy", 
                               "satellite provider", "palastinian territory", "americansamoa", 
                               "virginislands(british)", "virginislands(u.s.)")] <- NA

# Clean up trailing spaces
data$continent  <- trimws(data$continent )

# Count non-NA values in the 'continent ' column
sum(!is.na(data$continent ))


# DATA CLEANING - DATE
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

##############################################################################
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

# Calculate average downtime by year and OS category
avg_data <- data %>%
  filter(os_category != "Unknown") %>%
  group_by(year, os_category) %>%
  summarise(avg_downtime = mean(downtime, na.rm = TRUE), .groups = 'drop')

# Calculate the correlation between year and average downtime for each OS category
correlation_by_os <- avg_data %>%
  group_by(os_category) %>%
  summarise(correlation = cor(year, avg_downtime, use = "complete.obs"))

# Create scatter plot with linear regression line
ggplot(avg_data, aes(x = year, y = avg_downtime, color = os_category)) +
  geom_point(alpha = 0.7, size = 3) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  facet_wrap(~ os_category, scales = "free") +  # Facet by OS category
  labs(title = "Average System Downtime by Year and OS Category",
       x = "Year",
       y = "Average Downtime (hours)",
       color = "Operating System Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

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

# RQ 4
# Calculate the average downtime for each OS category by year
avg_downtime_per_os <- data %>%
  filter(os_category != "Unknown") %>%
  group_by(year, os_category) %>%
  summarise(avg_downtime = mean(downtime, na.rm = TRUE), .groups = 'drop')

# Calculate the change in average downtime (difference between first and last year)
downtime_change <- avg_downtime_per_os %>%
  group_by(os_category) %>%
  summarise(
    first_year_downtime = avg_downtime[which.min(year)],  # Downtime in the first year
    last_year_downtime = avg_downtime[which.max(year)],   # Downtime in the last year
    change_in_downtime = last_year_downtime - first_year_downtime
  )

# Sort the data by change in downtime to highlight the most improvement
downtime_change <- downtime_change %>%
  arrange(desc(change_in_downtime))

# Plot the change in downtime for each OS category
ggplot(downtime_change, aes(x = reorder(os_category, change_in_downtime), y = change_in_downtime, fill = change_in_downtime > 0)) +
  geom_bar(stat = "identity", width = 0.7) +  # Bar chart
  labs(title = "Change in Average System Downtime Across Years by OS Category",
       x = "Operating System Category",
       y = "Change in Average Downtime (hours)",
       fill = "Improvement") +
  scale_fill_manual(values = c("TRUE" = "#27AE60", "FALSE" = "#E74C3C")) +  # Green for improvement, red for decline
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# RQ 5
variance_data <- data %>%
  filter(os_category != "Unknown") %>%
  group_by(os_category) %>%
  summarise(avg_downtime = mean(downtime, na.rm = TRUE),
            sd_downtime = sd(downtime, na.rm = TRUE), .groups = 'drop') %>%
  arrange(avg_downtime)  # Arrange by average downtime in ascending order

ggplot(variance_data, aes(x = reorder(os_category, avg_downtime), y = avg_downtime, fill = os_category)) +
  geom_bar(stat = "identity", width = 0.6) +  # Bar chart for average downtime
  geom_errorbar(aes(ymin = avg_downtime - sd_downtime, ymax = avg_downtime + sd_downtime), 
                width = 0.2, color = "black") +  # Error bars for variance (standard deviation)
  labs(title = "Average Downtime with Variance by OS Category",
       x = "Operating System Category",
       y = "Average Downtime (hours)",
       fill = "OS Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##############################################################################
# Goh Xin Tong 
# RQ 1
# Calculate variance of downtime for each continent 
state_variance <- data %>%
  filter(!is.na(continent )) %>%
  group_by(continent ) %>%
  summarise(downtime_variance = var(downtime, na.rm = TRUE), .groups = 'drop')

# Sort by variance (lowest first, most consistent continent  first)
state_variance <- state_variance %>%
  arrange(downtime_variance)

# Visualize which continent  has the most consistent downtime using variance
ggplot(state_variance, aes(x = reorder(continent , downtime_variance), y = downtime_variance, fill = continent )) +
  geom_bar(stat = "identity") +
  labs(title = "Downtime Consistency by Region (Variance)", x = "continent ", y = "Variance of Downtime") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# RQ 2
# Group data by 'year' and 'continent ' and calculate the total downtime for each year and continent 
total_downtime_per_year_state <- data %>%
  filter(!is.na(continent )) %>%
  group_by(year, continent ) %>%
  summarise(total_downtime = sum(downtime, na.rm = TRUE)) %>%
  arrange(year, continent )


# Create a heatmap to visualize the total downtime per state over the years
ggplot(total_downtime_per_year_state, aes(x = year, y = continent , fill = total_downtime)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +  # Set color gradient for heatmap
  labs(
    title = "Total Downtime per State Over the Years",
    x = "Year",
    y = "State",
    fill = "Total Downtime"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Aggregate total downtime by state
state_downtime <- data %>%
  filter(!is.na(continent) & !is.na(downtime)) %>%
  group_by(continent) %>%
  summarise(total_downtime = sum(downtime, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(total_downtime))

# Plot the top 10 states with highest downtime
ggplot(state_downtime, aes(x = reorder(continent, total_downtime), y = total_downtime, fill = total_downtime)) +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +  # Flip for better readability
  scale_fill_gradient(low = "blue", high = "red") +  # Color gradient
  labs(title = "Total System Downtime by State",
       x = "State",
       y = "Total Downtime (hours)",
       fill = "Total Downtime") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# RQ 4
# Summarize the data: calculate average downtime by continent and year
avg_downtime_trends <- data %>%
  filter(!is.na(continent )) %>%
  group_by(continent, year) %>%
  summarise(avg_downtime = mean(downtime, na.rm = TRUE)) %>%
  arrange(continent, year)

# Visualization of the trends using faceting
ggplot(avg_downtime_trends, aes(x = year, y = avg_downtime)) +
  geom_line(aes(color = continent)) +
  geom_point(aes(color = continent)) +
  facet_wrap(~ continent, scales = "free_y") +  # Facet by continent
  labs(title = "Trends in Average Downtime by Continent and Year",
       x = "Year",
       y = "Average Downtime (hours)") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide legend since it's already facet-based

downtime_by_continent_year <- data %>%
  filter(!is.na(continent )) %>%
  group_by(continent, year) %>%
  summarise(average_downtime = mean(downtime, na.rm = TRUE))

# Calculate the change in downtime from the first to the last year for each continent
downtime_change <- downtime_by_continent_year %>%
  group_by(continent) %>%
  summarise(first_year_downtime = first(average_downtime),
            last_year_downtime = last(average_downtime),
            change_in_downtime = last_year_downtime - first_year_downtime)

# Bar Chart for Change in Downtime (First to Last Year)
ggplot(downtime_change, aes(x = reorder(continent, change_in_downtime), y = change_in_downtime, fill = continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Change in Downtime by Continent (First to Last Year)",
       x = "Continent", y = "Change in Downtime") +
  coord_flip() +  # Flip to make it horizontal
  theme_minimal() +
  theme(legend.position = "none")

##############################################################################


write.csv(data, "output.csv", row.names = FALSE)


#######################################