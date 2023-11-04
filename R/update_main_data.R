update_main_data <- function(){
  fpi <- readRDS("data/fpi.rds")
  crops <- readRDS("data/crops.rds")
  food <- readRDS("data/food.rds")
  veg <- readRDS("data/veg.rds")
  gdp <- readRDS("data/gdp.rds")
  # coord <- readRDS("data/coord.rds")
  fpi <- merge(fpi, gdp, by = c('Country','Year'), all.x = TRUE)
  fpi <- fpi[which(!is.na(fpi$GDP)),]
  # fpi <- merge(fpi, coord[,-5], by = "ISO3", all.x=TRUE)
  crops <- merge(crops, gdp, by = c('Country','Year'), all.x = TRUE)
  crops <- crops[which(!is.na(crops$GDP)),]
  # crops <- merge(crops, coord[,-5], by = "ISO3", all.x=TRUE)
  food <- merge(food, gdp, by = c('Country','Year'), all.x = TRUE)
  food <- food[which(!is.na(food$GDP)),]
  # food <- merge(food, coord[,-5], by = "ISO3", all.x=TRUE)
  veg <- merge(veg, gdp, by = c('Country','Year'), all.x = TRUE)
  veg <- veg[which(!is.na(veg$GDP)),]
  # veg <- merge(veg, coord[,-5], by = "ISO3", all.x=TRUE)
  saveRDS(fpi, "data/fpi.rds")
  saveRDS(crops, "data/crops.rds")
  saveRDS(food, "data/food.rds")
  saveRDS(veg, "data/veg.rds")
}
