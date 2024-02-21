#!/usr/bin/env Rscript
# Read vfld data and save it in sqlite format
# Set the working directory to the folder containing the text file
setwd("/perm/sp3c/deode_verif/station_lists/")
options(error=traceback)

#dataframes 0 2 and 4 are similar so I skip merging these ones

# Set the file path
file_path <- "allsynop.list_cy46h_2023"

# Read the lines from the text file
lines <- readLines(file_path)

# Initialize empty vectors for each column
number1 <- vector()
number2 <- vector()
number3 <- vector()
number4 <- vector()
string <- vector()

# Iterate over each line
for (line in lines) {
  # Extract the numbers and string using regular expressions
  pattern <- "\\s*(-?\\d+\\.?\\d*)\\s+(-?\\d+\\.?\\d*)\\s+(-?\\d+\\.?\\d*)\\s+(-?\\d+\\.?\\d*)\\s+(.*)"
  matches <- regexec(pattern, line)
  
  # Check if there is a match
  if (length(matches[[1]]) > 1) {
    # Extract the matched values
    num1 <- as.numeric(regmatches(line, matches)[[1]][2])
    num2 <- as.numeric(regmatches(line, matches)[[1]][3])
    num3 <- as.numeric(regmatches(line, matches)[[1]][4])
    num4 <- as.numeric(regmatches(line, matches)[[1]][5])
    str <- regmatches(line, matches)[[1]][6]
    
    # Append the values to the corresponding vectors
    number1 <- c(number1, num1)
    number2 <- c(number2, num2)
    number3 <- c(number3, num3)
    number4 <- c(number4, num4)
    string <- c(string, str)
  }
}

# Create a data frame from the vectors
data0 <- data.frame(sid = number1, lat = number2, lon = number3, elev = number4, name = string)

file_path <- "allsynop.list_dmi"
# Read the lines from the text file
lines <- readLines(file_path)

# Initialize empty vectors for each column
number1 <- vector()
number2 <- vector()
number3 <- vector()
number4 <- vector()
string <- vector()

# Iterate over each line
for (line in lines) {
  # Extract the numbers and string using regular expressions
  pattern <- "\\s*(-?\\d+\\.?\\d*)\\s+(-?\\d+\\.?\\d*)\\s+(-?\\d+\\.?\\d*)\\s+(-?\\d+\\.?\\d*)\\s+(.*)"
  matches <- regexec(pattern, line)
  
  # Check if there is a match
  if (length(matches[[1]]) > 1) {
    # Extract the matched values
    num1 <- as.numeric(regmatches(line, matches)[[1]][2])
    num2 <- as.numeric(regmatches(line, matches)[[1]][3])
    num3 <- as.numeric(regmatches(line, matches)[[1]][4])
    num4 <- as.numeric(regmatches(line, matches)[[1]][5])
    str <- regmatches(line, matches)[[1]][6]
    
    # Append the values to the corresponding vectors
    number1 <- c(number1, num1)
    number2 <- c(number2, num2)
    number3 <- c(number3, num3)
    number4 <- c(number4, num4)
    string <- c(string, str)
  }
}

# Create a data frame from the vectors
data1 <- data.frame(sid = number1, lat = number2, lon = number3, elev = number4, name = string,row.names = NULL, check.rows = TRUE)

file_path <- "allsynop.list_metcoop"
# Read the lines from the text file
lines <- readLines(file_path)

# Initialize empty vectors for each column
number1 <- vector()
number2 <- vector()
number3 <- vector()
number4 <- vector()
string <- vector()

# Iterate over each line
for (line in lines) {
  # Extract the numbers and string using regular expressions
  pattern <- "\\s*(-?\\d+\\.?\\d*)\\s+(-?\\d+\\.?\\d*)\\s+(-?\\d+\\.?\\d*)\\s+(-?\\d+\\.?\\d*)\\s+(.*)"
  matches <- regexec(pattern, line)
  
  # Check if there is a match
  if (length(matches[[1]]) > 1) {
    # Extract the matched values
    num1 <- as.numeric(regmatches(line, matches)[[1]][2])
    num2 <- as.numeric(regmatches(line, matches)[[1]][3])
    num3 <- as.numeric(regmatches(line, matches)[[1]][4])
    num4 <- as.numeric(regmatches(line, matches)[[1]][5])
    str <- regmatches(line, matches)[[1]][6]
    
    # Append the values to the corresponding vectors
    number1 <- c(number1, num1)
    number2 <- c(number2, num2)
    number3 <- c(number3, num3)
    number4 <- c(number4, num4)
    string <- c(string, str)
  }
}

# Create a data frame from the vectors
data2<- data.frame(sid = number1, lat = number2, lon = number3, elev = number4, name = string,row.names = NULL, check.rows = TRUE)



file_path <- "allsynop.list_meteirann"

# Read the lines from the text file
lines <- readLines(file_path)

# Initialize empty vectors for each column
number1 <- vector()
number2 <- vector()
number3 <- vector()
number4 <- vector()
string <- vector()

# Iterate over each line
for (line in lines) {
  # Extract the numbers and string using regular expressions
  pattern <- "\\s*(-?\\d+\\.?\\d*)\\s+(-?\\d+\\.?\\d*)\\s+(-?\\d+\\.?\\d*)\\s+(-?\\d+\\.?\\d*)\\s+(.*)"
  matches <- regexec(pattern, line)
  
  # Check if there is a match
  if (length(matches[[1]]) > 1) {
    # Extract the matched values
    num1 <- as.numeric(regmatches(line, matches)[[1]][2])
    num2 <- as.numeric(regmatches(line, matches)[[1]][3])
    num3 <- as.numeric(regmatches(line, matches)[[1]][4])
    num4 <- as.numeric(regmatches(line, matches)[[1]][5])
    str <- regmatches(line, matches)[[1]][6]
    
    # Append the values to the corresponding vectors
    number1 <- c(number1, num1)
    number2 <- c(number2, num2)
    number3 <- c(number3, num3)
    number4 <- c(number4, num4)
    string <- c(string, str)
  }
}

# Create a data frame from the vectors
data3<- data.frame(sid = number1, lat = number2, lon = number3, elev = number4, name = string,row.names = NULL, check.rows = TRUE)


file_path <- "allsynop.list_shmu"

# Read the lines from the text file
lines <- readLines(file_path)

# Initialize empty vectors for each column
number1 <- vector()
number2 <- vector()
number3 <- vector()
number4 <- vector()
string <- vector()

# Iterate over each line
for (line in lines) {
  # Extract the numbers and string using regular expressions
  pattern <- "\\s*(-?\\d+\\.?\\d*)\\s+(-?\\d+\\.?\\d*)\\s+(-?\\d+\\.?\\d*)\\s+(-?\\d+\\.?\\d*)\\s+(.*)"
  matches <- regexec(pattern, line)
  
  # Check if there is a match
  if (length(matches[[1]]) > 1) {
    # Extract the matched values
    num1 <- as.numeric(regmatches(line, matches)[[1]][2])
    num2 <- as.numeric(regmatches(line, matches)[[1]][3])
    num3 <- as.numeric(regmatches(line, matches)[[1]][4])
    num4 <- as.numeric(regmatches(line, matches)[[1]][5])
    str <- regmatches(line, matches)[[1]][6]
    
    # Append the values to the corresponding vectors
    number1 <- c(number1, num1)
    number2 <- c(number2, num2)
    number3 <- c(number3, num3)
    number4 <- c(number4, num4)
    string <- c(string, str)
  }
}

# Create a data frame from the vectors
data4<- data.frame(sid = number1, lat = number2, lon = number3, elev = number4, name = string,row.names = NULL, check.rows = TRUE)


file_path <- "allsynop.list_aemet_after20220601"

# Read the lines from the text file
lines <- readLines(file_path)

# Initialize empty vectors for each column
number1 <- vector()
number2 <- vector()
number3 <- vector()
number4 <- vector()
string <- vector()

# Iterate over each line
for (line in lines) {
  # Extract the numbers and string using regular expressions
  pattern <- "\\s*(-?\\d+\\.?\\d*)\\s+(-?\\d+\\.?\\d*)\\s+(-?\\d+\\.?\\d*)\\s+(-?\\d+\\.?\\d*)\\s+(.*)"
  matches <- regexec(pattern, line)
  
  # Check if there is a match
  if (length(matches[[1]]) > 1) {
    # Extract the matched values
    num1 <- as.numeric(regmatches(line, matches)[[1]][2])
    num2 <- as.numeric(regmatches(line, matches)[[1]][3])
    num3 <- as.numeric(regmatches(line, matches)[[1]][4])
    num4 <- as.numeric(regmatches(line, matches)[[1]][5])
    str <- regmatches(line, matches)[[1]][6]
    
    # Append the values to the corresponding vectors
    number1 <- c(number1, num1)
    number2 <- c(number2, num2)
    number3 <- c(number3, num3)
    number4 <- c(number4, num4)
    string <- c(string, str)
  }
}

# Create a data frame from the vectors
data5<- data.frame(sid = number1, lat = number2, lon = number3, elev = number4, name = string,row.names = NULL, check.rows = TRUE)

#Renumber AEMET local stations to comply with new numbering convention for DEODE
data5$sid[data5$sid>99000 & data5$sid<99999]=data5$sid[data5$sid>99000 & data5$sid<99999]+8000000

library(dplyr)

merged_df <- rbind(data0, data1)
merged_df <- distinct(merged_df, sid, .keep_all = TRUE)

merged_df <- rbind(merged_df, data2)
merged_df <- distinct(merged_df, sid, .keep_all = TRUE)

merged_df <- rbind(merged_df, data3)
merged_df <- distinct(merged_df, sid, .keep_all = TRUE)

merged_df <- rbind(merged_df, data4)
merged_df <- distinct(merged_df, sid, .keep_all = TRUE)

merged_df <- rbind(merged_df, data5)
merged_df <- distinct(merged_df, sid, .keep_all = TRUE)

merged_df <- merged_df[order(merged_df$sid),]

save(merged_df, file = "station_list_deode.rda")
library(gdata)
write.fwf(merged_df, file = "allsynop.list_deode", width = c(11, 11, 11, 11, 80), colnames = FALSE, rownames = FALSE)
