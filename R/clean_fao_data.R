clean_fao_data <- function(){
  df <- readRDS("data/fpi.rds")
  ctry <- c('United States of America', 'United Kingdom of Great Britain and Northern Ireland', 'United Arab Emirates',
            'China, mainland', 'Indonesia', 'Japan', 'Malaysia', 'Myanmar', 'Singapore',
            'Sri Lanka', 'Thailand', 'Viet Nam')
  food.item <- c('Crops','Food','Vegetables and Fruit Primary')
  indctrs <- 'gross_per_capita_production_index_number__2014_2016___100_'
  dat <- df[which((df$area %in% ctry) & (df$item %in% food.item) & (df$element == indctrs)),]
  dat <- dat[,-c(1,2,4,5,7,8,9,13)]
  dat[which(dat$area == 'United States of America'),]$area <- 'United States'
  dat[which(dat$area == 'United Kingdom of Great Britain and Northern Ireland'),]$area <- 'United Kingdom'
  dat[which(dat$area == 'China, mainland'),]$area <- 'China'
  colnames(dat) <- c("Country","Production","Year","Unit","FPI")
  crops <- dat[which(dat$Production == 'Crops'),]
  food <- dat[which(dat$Production == 'Food'),]
  veg <- dat[which(dat$Production == 'Vegetables and Fruit Primary'),]
  saveRDS(dat,"data/fpi.rds")
  saveRDS(crops,"data/crops.rds")
  saveRDS(food,"data/food.rds")
  saveRDS(veg,"data/veg.rds")
}
