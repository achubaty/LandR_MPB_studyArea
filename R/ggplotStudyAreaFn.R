ggplotStudyAreaFn <- function(poly, cols, studyArea, lake) {                          
  ggplot(poly) +                                                                      
    geom_sf(fill = "white", colour = "black", alpha = 0.5) +                          
    geom_sf(data = studyArea, fill = "darkgreen", colour = "darkgreen", alpha = 0.5) +
    geom_sf(data = lake, fill = "blue", colour = "blue", alpha = 0.5) +               
    theme_bw() +                                                                      
    annotation_north_arrow(location = "bl", which_north = "true",                     
                           pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),        
                           style = north_arrow_fancy_orienteering) +                  
    xlab("Longitude") + ylab("Latitude") +                                            
    ggtitle("MPB study area")                                                         
}                                                                                     
                                                                                      
