InitStudyAreaLCC <- function(sim) {                                                                           
  cacheTags <- c(P(sim)$runName, currentModule(sim))                                                          
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)                                
  message(currentModule(sim), ": using dataPath\n '", dPath, "'.")                                           
                                                                                                              
  # ! ----- EDIT BELOW ----- ! #                                                                              
                                                                                                              
  allClasses <- 1:39                                                                                          
                                                                                                              
  ## LANDCOVER MAPS (LCC2005)                                                                                 
  LCC2005 <- prepInputsLCC(year = 2005, studyArea = sim$studyAreaLarge, destinationPath = dPath)              
                                                                                                              
  uniqueLCCclasses <- na.omit(unique(LCC2005[]))                                                              
                                                                                                              
  treeClassesLCC <- c(1:15, 20, 32, 34:35)                                                                    
  nontreeClassesLCC <- (1:39)[!(1:39 %in% treeClassesLCC)]                                                    
                                                                                                              
  sim$LCC <- LCC2005                                                                                          
                                                                                                              
  treePixelsLCC <- which(sim$LCC[] %in% treeClassesLCC) ## c(1:15, 20, 32, 34:35)                             
  nonTreePixels <- which(sim$LCC[] %in% nontreeClassesLCC)                                                    
                                                                                                              
  fireSenseForestedLCC <- LandRforestedLCC <- treeClassesLCC                                                  
  sim$nonForestClasses <- nontreeClassesLCC                                                                   
                                                                                                              
  nonflammableLCC  <- c(0, 25, 30, 33, 36:39)                                                                 
  nonForestLCCGroups <- list(                                                                                 
    nonForest_highFlam = c(16:19, 22),                                                                        
    nonForest_lowFlam = c(21, 23:24, 26:29, 31)                                                               
  )                                                                                                           
  sim$missingLCCGroup <- "nonForest_highFlam"                                                                 
                                                                                                              
  sim$LCC <- setValues(sim$LCC, asInteger(values(sim$LCC)))                                                   
  sim$LandRforestedLCC <- LandRforestedLCC                                                                    
  sim$fireSenseForestedLCC <- fireSenseForestedLCC                                                            
  sim$nonForestLCCGroups <- nonForestLCCGroups                                                                
  sim$nonflammableLCC <- nonflammableLCC                                                                      
                                                                                                              
  sim$nonTreePixels <- nonTreePixels                                                                          
  sim$treeClasses <- sim$LandRforestedLCC                                                                     
  sim$nontreeClasses <- nontreeClassesLCC                                                                     
  sim$flammableRTM <- defineFlammable(crop(sim$LCC, sim$rasterToMatch),                                       
                                      nonFlammClasses = sim$nonflammableLCC,                                  
                                      mask = sim$rasterToMatch)                                               
                                                                                                              
  # check that all LCC classes accounted for in forest, nonForest, and non flamm classes for fS               
  fS_classes <- sort(unique(c(sim$fireSenseForestedLCC, unlist(sim$nonForestLCCGroups), sim$nonflammableLCC)))
  if (!all(allClasses %in% fS_classes)) {                                                                     
    stop("Some LCCs not accounted for:\n",                                                                   
         "Expected: ", allClasses, "\n",                                                                     
         "Assigned to fireSense classes: ", fS_classes)                                                       
  }                                                                                                           
                                                                                                              
  # ! ----- STOP EDITING ----- ! #                                                                            
  return(invisible(sim))                                                                                      
}                                                                                                             
                                                                                                              
