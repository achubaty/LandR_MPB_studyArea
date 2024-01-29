#' New function
#' @importFrom LandR speciesInStudyArea
InitSpecies <- function(sim) {                                                                                      
  # # ! ----- EDIT BELOW ----- ! #                                                                                  
  cacheTags <- c(P(sim)$studyAreaName, currentModule(sim))                                                          
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)                                      
  message(currentModule(sim), ": using dataPath\n '", dPath, "'.")                                                 
                                                                                                                    
  ## SPECIES STUFF                                                                                                  
  spp <- Cache(LandR::speciesInStudyArea, sim$studyArea)                                                            
  spp2use <- grep("_Spp", spp$speciesList, invert = TRUE, value = TRUE) ## omit generic '*_Spp'                     
  spp2use <- grep("Lari_Lya", spp2use, invert = TRUE, value = TRUE) ## omit Lari_lya: no trait values; only 2 pixels
  spp2use <- sort(spp2use)                                                                                          
                                                                                                                    
  sim$sppEquiv <- LandR::sppEquivalencies_CA[KNN %in% spp2use]                                                      
  sim$sppEquivCol <- "LandR"                                                                                        
  sim$sppColorVect <- sppColors(sim$sppEquiv, sim$sppEquivCol, newVals = "Mixed", palette = "Paired")               
                                                                                                                    
  sim$speciesTable <- getSpeciesTable(dPath = dPath) ## uses default URL                                            
                                                                                                                    
  # ! ----- STOP EDITING ----- ! #                                                                                  
  return(invisible(sim))                                                                                            
}                                                                                                                   
                                                                                                                    
