table_to_popup_df <- function(df) {
  colnames(df) <- tolower(colnames(df))
  df <- data.frame(df[order(df$date, decreasing = T), ], stringsAsFactors = F)
  y <- apply(df, 1, function(z) {
    paste0("<tr><td>", as.character(z["date"]), "</td><td>",
           as.character(z["price"]), "</td><td>",
           z["status"], "</td></tr>")
  })
  return(y)
}

popup_df_to_table <- function(popup_str) {
  popup_str <- lapply(strsplit(popup_str, "<td>"), function(x) x[2:7])
  cols <- sapply(popup_str, function(x) gsub(pattern = "<b>|</b>|<td>|</td>|<table>|</table>|<tr>|</tr>|<tbody>|</tbody>", replacement = "", x = x[1:3]))
  vals <- sapply(popup_str, function(x) gsub(pattern = "<b>|</b>|<td>|</td>|<table>|</table>|<tr>|</tr>|<tbody>|</tbody>", replacement = "", x = x[4:6]))
  vec <- sapply(1:ncol(cols), function(i) {
    vec1 <- vals[, i]
    names(vec1) <- cols[, i]
    return(vec1)
  })
  vec <- data.frame(t(vec))
  vec$Date <- as.Date(vec$Date)
  vec$Price <- as.numeric(vec$Price)
  vec <- data.frame(vec %>% group_by(Price, Status) %>% summarize(Date = min(Date)))
  vec <- vec[order(vec$Date, decreasing = T), ]
  rownames(vec) <- NULL
  return(vec[, c("Date", "Price", "Status")])
}

process_price <- function(price, popup_df) {
  tbl <- popup_df_to_table(popup_df)
  print(head(tbl))
  return(min(c(tbl$Price[1], price), na.rm = T))
}

most_frequent <- function(type) {
  x <- table(type)
  x <- x[order(x, decreasing = T)]
  return(names(x)[1])
}

process_status <- function(status, popup_df) {
  status <- status[!is.na(status)]
  tbl <- popup_df_to_table(popup_df)
  return(tbl$Status[1])
}

process_popup <- function(popup_df) {
  tbl <- popup_df_to_table(popup_df)
  return(table_to_popup_df(tbl))
}

process_address <- function(address1) {
  address1 <- strsplit(x = address1, split = " - ")
  address_part1 <- sapply(address1, function(x) x[1])
  address_part2 <- sapply(address1, function(x) x[2])
  address_part3 <- sapply(address1, function(x) x[3])
  address_part4 <- sapply(address1, function(x) x[4])
  if(sum(address_part2 != "Pittsburgh") > 0) {
    address_part2 <- address_part2[address_part2 != "Pittsburgh"]
  }
  address_part1 <- most_frequent(tolower(address_part1))
  address_part2 <- most_frequent(tolower(address_part2))
  address_part3 <- most_frequent(tolower(address_part3))
  address_part4 <- most_frequent(tolower(address_part4))
  return(paste0(address_part1, " - ", address_part2, " - ", address_part3, " - ", address_part4))
}