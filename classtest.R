
#Installing the required libraries
if (!require(readr)) {
  install.packages("readr")
  library(readr)
}
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}


#Q1Creating the Retest_data dataframe
Retest_data <- read_csv("/Users/sunilsharma/Desktop/untitled folder/big data architecture/Retest.csv")
# to view the data frame in table format
View(Retest_data)
str(Retest_data)
head(Retest_data, 15)
num_rows <- nrow(Retest_data)
cat("Number of rows in Retest_data:", num_rows)

#Q2changing Datandtime variable from chr to date format
Retest_data$dateandtime <- as.Date(Retest_data$dateandtime, format = "%m/%d/%Y")
str(Retest_data)
names(Retest_data)

#q3changing the specific columns names
old_names <- c("dateandtime", "duration (hours/min).", "duration (seconds)", "value1", "value2")
new_names <- c("DateTime", "TotalDuration", "DurationSeconds", "MeanValue_1", "MeanValue_2")
names(Retest_data) <- ifelse(names(Retest_data) %in% old_names, new_names[match(names(Retest_data), old_names)], names(Retest_data))
names(Retest_data)
View(Retest_data)

#Q4 Changing the MeanValue_2 to numeric from chr
Retest_data <- mutate(Retest_data, MeanValue_2 = as.numeric(MeanValue_2))
str(Retest_data)
View(Retest_data)

#q5 installing mice and Vim libraries
install.packages("mice")
install.packages("VIM")
library(mice)
library(VIM)

# Display the number of missing variables
missing_summary <- aggr(Retest_data, numbers=TRUE)

#records which have no missing data content
no_missing_records <- sum(complete.cases(Retest_data))

#variables who have the DateTime records missing
missing_date_time <- sum(is.na(Retest_data$DateTime))

#variable which has the largest number of missing data points
largest_missing_variable <- colnames(Retest_data)[which.max(colSums(is.na(Retest_data)))]

#percent of data which is available without missing data points
percent_complete <- (no_missing_records / nrow(Retest_data)) * 100

#results
cat("Number of records with no missing data:", no_missing_records, "\n")
cat("Number of missing DateTime records:", missing_date_time, "\n")
cat("Variable with the largest number of missing data points:", largest_missing_variable, "\n")
cat("Percentage of data available without missing data points:", percent_complete, "%\n")

#q6
#Removing records with missing values
cleaned_data <- na.omit(Retest_data)

# Counting the number of records deleted
records_deleted <- nrow(Retest_data) - nrow(cleaned_data)

# Print the number of records deleted
cat("Number of records deleted from the Retest data frame:", records_deleted)

#q7  plotting the chart 
# summarizing the data using table function
summary_table <- table(Retest_data$ID)

# Plot the summary using a bar plot
plot(summary_table, type = "o", 
     main = "Summary of Retest Information",
     xlab = "ID",
     ylab = "MeanValue_1")

#q8 
#sorting the data frame using arrange on shape then by city
sorted_Retest_data <- Retest_data %>%
  arrange(shape, city)

# Extracting only the specified columns
sorted_Retest_data <- sorted_Retest_data %>%
  select(DateTime, city, country, shape)

# Displaying the first 15 rows of data in the new data frame
head(sorted_Retest_data, 15)
str(sorted_Retest_data)                                                            

#q9Using the subset filter to filter where the country variable values are 'gb' and shape is 'disk'
Retest_sub <- subset(Retest_data, country == "gb" & shape == "disk")

# Count the total number of records in Retest_sub data frame
num_records <- nrow(Retest_sub)
cat("Total number of records in Retest_sub data frame:", num_records)


#q10 
#writing modified Retest data frame to CSV file
write.csv(Retest_data, "/Users/sunilsharma/Desktop/DataScienceTest/modified_Retest.csv", row.names = FALSE)

#Writing Retest_sub data frame to CSV file
write.csv(Retest_sub, "/Users/sunilsharma/Desktop/DataScienceTest/Retest_sub.csv", row.names = FALSE)

#Writing sorted_Retest_data data frame to CSV file
write.csv(sorted_Retest_data, "/Users/sunilsharma/Desktop/DataScienceTest/sorted_Retest.csv", row.names = FALSE)


# Adding all modified files to Github
system("git add /Users/sunilsharma/Desktop/DataScienceTest/modified_Retest.csv")
system("git add /Users/sunilsharma/Desktop/DataScienceTest/Retest_sub.csv")
system("git add /Users/sunilsharma/Desktop/DataScienceTest/sorted_Retest.csv")
system("git add YourScriptFileName.R")

# Commit changes
system("git commit -m 'Data Science class test'")

# Push changes to remote GitHub repository
system("git push origin master")
