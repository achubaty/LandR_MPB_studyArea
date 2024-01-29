#' @export
doEvent.LandR_MPB_studyArea = function(sim, eventTime, eventType) {                                        
  switch(                                                                                                  
    eventType,                                                                                             
    init = {                                                                                               
      # do stuff for this event                                                                            
      fullStudyAreaName <- P(sim)$studyAreaName                                                            
                                                                                                           
      chunks <- strsplit(fullStudyAreaName, "_")[[1]]                                                      
      mod$ecoprov <- chunks[which(!is.na(suppressWarnings(as.integer(chunks))))] ## keep as character      
      mod$ecoprov <- if (length(mod$ecoprov) > 0) mod$ecoprov else NULL                                    
                                                                                                           
      sim <- InitStudyAreaRTM(sim)                                                                         
      sim <- InitSpecies(sim) ## needs studyArea defined to identify species to use                        
      sim <- InitStudyAreaLCC(sim)                                                                         
      sim <- InitAge(sim)                                                                                  
                                                                                                           
      ## check that rasters all match                                                                      
      compareRaster(sim$rasterToMatchLarge, sim$LCC, sim$standAgeMap2001, sim$standAgeMap2011, orig = TRUE)
                                                                                                           
      # schedule future event(s)                                                                           
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "Ontario_preamble", "plot", .last())              
    },                                                                                                     
    plot = {                                                                                               
      # ! ----- EDIT BELOW ----- ! #                                                                       
      # do stuff for this event                                                                            
      ABSK <- as_Spatial(sim$absk)                                                                         
      Plot(ABSK, col = "Greens")                                                                           
      Plot(sim$studyArea, addTo = "ABSK", col = "Accent")                                                  
                                                                                                           
      Plot(sim$rasterToMatch)                                                                              
      Plot(sim$rasterToMatchLarge)                                                                         
                                                                                                           
      Plot(sim$standAgeMap2001)                                                                            
      Plot(sim$standAgeMap2011)                                                                            
                                                                                                           
      Plot(sim$LCC)                                                                                        
      # ! ----- STOP EDITING ----- ! #                                                                     
    },                                                                                                     
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],                 
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))      
  )                                                                                                        
  return(invisible(sim))                                                                                   
}                                                                                                          
