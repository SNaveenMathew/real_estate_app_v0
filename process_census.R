library(data.table)
library(plyr)

read_zip <- function(zip_file, relevant_cols = NULL) {
  folder <- gsub(pattern = "\\.zip$", replacement = "", x = zip_file)
  unzip(zip_file, exdir = folder)
  files <- list.files(folder, full.names = T)
  data_files <- files[grep(pattern = "\\-Data.csv", x = files, ignore.case = T)]
  file_names <- strsplit(data_files, "/")
  file_names <- sapply(file_names, function(x) x[length(x)])
  full_data <- data.frame()
  
  for(i in 1:length(data_files)) {
    data_file <- data_files[i]
    file_name <- file_names[i]
    file_name1 <- strsplit(x = file_name, split = "[a-zA-Z][0-9]Y[0-9]{4}")[[1]]
    for(part in file_name1) {
      file_name <- gsub(pattern = part, replacement = "", x = file_name)
    }
    metadata_file <- gsub(pattern = "\\-Data.csv", replacement = "-Column-Metadata.csv", x = data_file)
    file_data <- as.data.frame(fread(data_file, skip = 1))
    file_metadata <- as.data.frame(fread(metadata_file))
    metadata_idx <- match(colnames(file_data), file_metadata$Label)
    file_data <- as.data.frame(fread(data_file, skip = 2))[, !is.na(metadata_idx)]
    colnames(file_data) <- gsub(pattern = " {1,}", replacement = " ", x = tolower(file_metadata$Label[metadata_idx[!is.na(metadata_idx)]]))
    file_data$data_type <- gsub(pattern = "^[a-zA-Z]{1}", replacement = "", x = file_name)
    if(i == 1) {
      full_data <- file_data
      prev_ncol <- ncol(file_data)
    } else {
      full_data <- rbind.fill(full_data, file_data)
      added_ncol <- ncol(full_data) - prev_ncol
      prev_ncol <- ncol(full_data)
      print(c(i, added_ncol))
    }
  }
  
  rownames(full_data) <- NULL
  unlink(folder, recursive = T)
  if(!is.null(relevant_cols)) {
    relevant_cols <- unlist(lapply(relevant_cols, function(x) {
      if(length(x) > 1) {
        if(!endsWith(x = x[1], "!!")) {
          x[1] <- paste0(x[1], "!!")
        }
        x <- paste0(x[1], x[2:length(x)])
      }
      return(tolower(x))
    }))
    print(setdiff(relevant_cols, colnames(full_data)))
    return(full_data[, relevant_cols])
  } else {
    return(full_data)
  }
}

relevant_cols <- list("data_type",
                      c("estimate!!sex and age!!total population!!", "female", "male"),
                      c("estimate!!sex and age!!total population!!", "under 5 years", "5 to 9 years", "10 to 14 years", "15 to 19 years", "20 to 24 years", "25 to 34 years", "35 to 44 years", "45 to 54 years", "55 to 59 years", "60 to 64 years", "65 to 74 years", "75 to 84 years", "85 years and over", "median age (years)"),
                      c("estimate!!race alone or in combination with one or more other races!!total population!!", "white", "black or african american", "american indian and alaska native", "asian", "Native Hawaiian and Other Pacific Islander", "Some Other Race"),
                      "estimate!!total housing units",
                      c("estimate!!race!!total population!!one race!!asian!!", "asian indian", "chinese"))

full_data <- read_zip(zip_file = "C:/Coding/House/Census Bureau/Pittsburgh/ACS Demographic and Housing Estimates.zip", relevant_cols = relevant_cols)
relevant_cols <- list("data_type",
                      c("total!!estimate!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!", "Less than high school graduate", "High school graduate (includes equivalency)", "Some college or associate's degree", "Bachelor's degree", "Graduate or professional degree"),
                      c("total!!estimate!!individual income in the past 12 months (in 2017 inflation-adjusted dollars)!!population 15 years and over!!", "$1 to $9,999 or loss", "$10,000 to $14,999", "$15,000 to $24,999", "$25,000 to $34,999", "$35,000 to $49,999", "$50,000 to $64,999", "$65,000 to $74,999", "$75,000 or more"),
                      "total!!estimate!!Median income (dollars)",
                      c("total!!estimate!!POVERTY STATUS IN THE PAST 12 MONTHS!!Population 1 year and over for whom poverty status is determined!!", "Below 100 percent of the poverty level", "100 to 149 percent of the poverty level", "At or above 150 percent of the poverty level"),
                      c("total!!estimate!!HOUSING TENURE!!Population 1 year and over in housing units!!", "Householder lived in owner-occupied housing units", "Householder lived in renter-occupied housing units"),
                      c("total!!estimate!!PERCENT ALLOCATED!!", "Residence 1 year ago"),
                      c("moved; within same county!!estimate!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!", "Less than high school graduate", "High school graduate (includes equivalency)", "Some college or associate's degree", "Bachelor's degree", "Graduate or professional degree"),
                      c("moved; within same county!!estimate!!individual income in the past 12 months (in 2017 inflation-adjusted dollars)!!population 15 years and over!!", "$1 to $9,999 or loss", "$10,000 to $14,999", "$15,000 to $24,999", "$25,000 to $34,999", "$35,000 to $49,999", "$50,000 to $64,999", "$65,000 to $74,999", "$75,000 or more"),
                      "moved; within same county!!estimate!!Median income (dollars)",
                      c("moved; within same county!!estimate!!POVERTY STATUS IN THE PAST 12 MONTHS!!Population 1 year and over for whom poverty status is determined!!", "Below 100 percent of the poverty level", "100 to 149 percent of the poverty level", "At or above 150 percent of the poverty level"),
                      c("moved; within same county!!estimate!!HOUSING TENURE!!Population 1 year and over in housing units!!", "Householder lived in owner-occupied housing units", "Householder lived in renter-occupied housing units"),
                      c("moved; within same county!!estimate!!PERCENT ALLOCATED!!", "Residence 1 year ago"),
                      c("moved; from different county, same state!!estimate!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!", "Less than high school graduate", "High school graduate (includes equivalency)", "Some college or associate's degree", "Bachelor's degree", "Graduate or professional degree"),
                      c("moved; from different county, same state!!estimate!!individual income in the past 12 months (in 2017 inflation-adjusted dollars)!!population 15 years and over!!", "$1 to $9,999 or loss", "$10,000 to $14,999", "$15,000 to $24,999", "$25,000 to $34,999", "$35,000 to $49,999", "$50,000 to $64,999", "$65,000 to $74,999", "$75,000 or more"),
                      "moved; from different county, same state!!estimate!!Median income (dollars)",
                      c("moved; from different county, same state!!estimate!!POVERTY STATUS IN THE PAST 12 MONTHS!!Population 1 year and over for whom poverty status is determined!!", "Below 100 percent of the poverty level", "100 to 149 percent of the poverty level", "At or above 150 percent of the poverty level"),
                      c("moved; from different county, same state!!estimate!!HOUSING TENURE!!Population 1 year and over in housing units!!", "Householder lived in owner-occupied housing units", "Householder lived in renter-occupied housing units"),
                      c("moved; from different county, same state!!estimate!!PERCENT ALLOCATED!!", "Residence 1 year ago"),
                      c("moved; from different state!!estimate!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!", "Less than high school graduate", "High school graduate (includes equivalency)", "Some college or associate's degree", "Bachelor's degree", "Graduate or professional degree"),
                      c("moved; from different state!!estimate!!individual income in the past 12 months (in 2017 inflation-adjusted dollars)!!population 15 years and over!!", "$1 to $9,999 or loss", "$10,000 to $14,999", "$15,000 to $24,999", "$25,000 to $34,999", "$35,000 to $49,999", "$50,000 to $64,999", "$65,000 to $74,999", "$75,000 or more"),
                      "moved; from different state!!estimate!!Median income (dollars)",
                      c("moved; from different state!!estimate!!POVERTY STATUS IN THE PAST 12 MONTHS!!Population 1 year and over for whom poverty status is determined!!", "Below 100 percent of the poverty level", "100 to 149 percent of the poverty level", "At or above 150 percent of the poverty level"),
                      c("moved; from different state!!estimate!!HOUSING TENURE!!Population 1 year and over in housing units!!", "Householder lived in owner-occupied housing units", "Householder lived in renter-occupied housing units"),
                      c("moved; from different state!!estimate!!PERCENT ALLOCATED!!", "Residence 1 year ago"),
                      c("moved; from abroad!!estimate!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!", "Less than high school graduate", "High school graduate (includes equivalency)", "Some college or associate's degree", "Bachelor's degree", "Graduate or professional degree"),
                      c("moved; from abroad!!estimate!!individual income in the past 12 months (in 2017 inflation-adjusted dollars)!!population 15 years and over!!", "$1 to $9,999 or loss", "$10,000 to $14,999", "$15,000 to $24,999", "$25,000 to $34,999", "$35,000 to $49,999", "$50,000 to $64,999", "$65,000 to $74,999", "$75,000 or more"),
                      "moved; from abroad!!estimate!!Median income (dollars)",
                      c("moved; from abroad!!estimate!!POVERTY STATUS IN THE PAST 12 MONTHS!!Population 1 year and over for whom poverty status is determined!!", "Below 100 percent of the poverty level", "100 to 149 percent of the poverty level", "At or above 150 percent of the poverty level"),
                      c("moved; from abroad!!estimate!!HOUSING TENURE!!Population 1 year and over in housing units!!", "Householder lived in owner-occupied housing units", "Householder lived in renter-occupied housing units"),
                      c("moved; from abroad!!estimate!!percent allocated!!", "residence 1 year ago"))
full_data <- read_zip(zip_file = "C:/Coding/House/Census Bureau/Pittsburgh/Geographic Mobility by Selected Characteristics in the United States.zip", relevant_cols = relevant_cols)
