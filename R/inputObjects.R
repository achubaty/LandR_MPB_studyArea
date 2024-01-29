.inputObjects <- function(sim) {                                                         
  cacheTags <- c(currentModule(sim), "function:.inputObjects")                           
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)           
  message(currentModule(sim), ": using dataPath '", dPath, "'.")                         
                                                                                         
  # ! ----- EDIT BELOW ----- ! #                                                         
  if (!suppliedElsewhere(targetCRS)) {                                                   
    sim$targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",          
                           "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  }                                                                                      
                                                                                         
  # ! ----- STOP EDITING ----- ! #                                                       
  return(invisible(sim))                                                                 
}                                                                                        
                                                                                         
