update_wdi_data <- function(){
  ctry <- c('USA', 'GBR', 'ARE',
            'CHN', 'IDN', 'JPN', 'MYS', 'MMR', 'SGP',
            'LKA', 'THA', 'VNM')
  df <- wb_data("NY.GDP.PCAP.CD", country = ctry, start_date = 1985, end_date = format(Sys.Date(),"%Y"))
  df <- df[,-c(1,6,7,8,9)]
  colnames(df) <- c('ISO3','Country', 'Year','GDP')
  saveRDS(df, "data/gdp.rds")
}
