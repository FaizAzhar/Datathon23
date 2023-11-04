update_coord <- function(){
  data("wrld_simpl")
  app_shapefile <- wrld_simpl %>% subset(.,ISO3 %in% c('ARE','CHN','GBR','IDN','JPN','LKA','MMR','MYS','SGP','THA','USA','VNM')) %>%
    st_as_sf(.,crs=4326) %>% select(ISO3,NAME, LON, LAT, geometry) %>% rename(lon = LON, lat=LAT)
  saveRDS(app_shapefile, "data/coord.rds")
}
