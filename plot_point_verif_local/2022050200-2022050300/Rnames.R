# Set the directory containing the files
dir_path <- "/perm/sp3c/deode_verif/cases/Spain20220503/output/2022050200-2022050300/"

# Get the list of files in the directory
file_list <- list.files(path = dir_path, pattern = ".png", full.names = TRUE)

# Initialize empty dictionaries
dict1 <- list()
dict2 <- list()
dict3 <- list()
dict4 <- list()
dict5 <- list()

# Loop through each file in the directory
for (file_path in file_list) {
  
  # Split the filename into its three parts
  filename <- basename(file_path)
  parts <- strsplit(filename, "-")[[1]]
  
  # Add each part to its corresponding dictionary
  dict1[[parts[1]]] <- 1
  dict2[[parts[2]]] <- 1
  dict3[[parts[3]]] <- 1
  dict4[[parts[4]]] <- 1
  dict5[[parts[5]]] <- 1

}

# Print the contents of each dictionary
cat("Dictionary 1:\n")
print(names(dict1))
cat("\n")

cat("Dictionary 2:\n")
print(names(dict2))
cat("\n")

cat("Dictionary 3:\n")
print(names(dict3))
cat("\n")

cat("Dictionary 4:\n")
print(names(dict4))
cat("\n")

cat("Dictionary 5:\n")
print(names(dict5))
cat("\n")


