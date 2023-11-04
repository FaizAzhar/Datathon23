ts_table <- function(start_date, end_date, ctry_list, item_list){
  dd <- readRDS("data/fpi.rds")
  dd <- dd[which(!(dd$Year < start_date) & !(dd$Year > end_date) & (dd$Country %in% ctry_list) & (dd$Production %in% item_list)),]
  return(dd)
}
