update_fao_data <- function(){
  fpi <- get_faostat_bulk(code = "QI", data_folder = "data")
  saveRDS(fpi, "data/fpi.rds")
  file.remove("data/Production_Indices_E_All_Data_(Normalized).zip")
}
