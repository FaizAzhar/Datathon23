# # processing png image for country's land use
# library(raster)
#
# # file path
# test <- raster("C:\\Users\\farea\\Documents\\RWorkspace\\datathon-discuss\\mys_landuse\\data\\Peninsular_Malaysia_landuse_map.tif")
# png("C:\\Users\\farea\\Documents\\RWorkspace\\datathon\\www\\MYS.png",width = 650, height = 450)
# plot(test, breaks = c(0:9), col = c("#f7b763","#dff02b","#5edbcf",
#                                     "#e359e3","#d63857","#630563",
#                                     "#392d3d","#0c5e0c","#4c28a1"),
#      main = "Land usage in Peninsular Malaysia (Kaelin et al, 2018)", legend=F)
# par(xpd = TRUE)
# legend(x=105.75,y=3.35,legend = c("Agriculture (non-paddy)","Agriculture (paddy)",
#                                 "Rural Residential", "Urban Residential", "Commercial and Institutional",
#                                 "Industrial and Infrastructure","Roads","Other (forest, lakes, beach)",
#                                 "Urban"),
#        fill = c("#f7b763","#dff02b","#5edbcf",
#                 "#e359e3","#d63857","#630563",
#                 "#392d3d","#0c5e0c","#4c28a1"),
#        cex = 0.7)
# dev.off()
