library(openxlsx)

mortgage_rate <- openxlsx::read.xlsx("historicalweeklydata.xlsx", startRow = 4, fillMergedCells = F)
mortgage_rate$Week <- as.Date(as.numeric(mortgage_rate$Week), origin = "1970-01-01")
