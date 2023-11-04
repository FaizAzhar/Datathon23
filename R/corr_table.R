corr_table <- function(start_date, end_date){
  crops <- readRDS("data/crops.rds")
  food <- readRDS("data/food.rds")
  veg <- readRDS("data/veg.rds")
  coord <- readRDS("data/coord.rds")
  crops <- crops[which(!(crops$Year < start_date) & !(crops$Year > end_date)),]
  food <- food[which(!(food$Year < start_date) & !(food$Year > end_date)),]
  veg <- veg[which(!(veg$Year < start_date) & !(veg$Year > end_date)),]
  cor.crop <- crops %>% group_by(Country) %>% summarize(correlation = cor(FPI,GDP), avg_gdp = mean(GDP), ISO3 = unique(ISO3)) %>% data.frame()
  cor.food <- food %>% group_by(Country) %>% summarize(correlation = cor(FPI,GDP), avg_gdp = mean(GDP), ISO3 = unique(ISO3)) %>% data.frame()
  cor.veg <- veg %>% group_by(Country) %>% summarize(correlation = cor(FPI,GDP), avg_gdp = mean(GDP), ISO3 = unique(ISO3)) %>% data.frame()
  return(list(crop = cor.crop, food = cor.food, veg = cor.veg))
}
