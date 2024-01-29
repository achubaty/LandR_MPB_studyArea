InitAge <- function(sim) {                                                                                  
  # # ! ----- EDIT BELOW ----- ! #                                                                          
  cacheTags <- c(P(sim)$runName, currentModule(sim))                                                        
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)                              
  message(currentModule(sim), ": using dataPath\n '", dPath, "'.")                                         
                                                                                                            
  ## STAND AGE MAP (TIME SINCE DISTURBANCE)                                                                 
  standAgeMapURL <- paste0(                                                                                 
    "http://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",                                             
    "canada-forests-attributes_attributs-forests-canada/2001-attributes_attributs-2001/",                   
    "NFI_MODIS250m_2001_kNN_Structure_Stand_Age_v1.tif"                                                     
  )                                                                                                         
  standAgeMapFileName <- basename(standAgeMapURL)                                                           
                                                                                                            
  fireURL <- "https://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/NFDB_poly.zip"         
                                                                                                            
  fireYear <- Cache(prepInputsFireYear,                                                                     
                    earliestYear = 1950,                                                                    
                    url = fireURL,                                                                          
                    destinationPath = dPath,                                                                
                    rasterToMatch = sim$rasterToMatchLarge)                                                 
                                                                                                            
  fireYear <- postProcess(fireYear, rasterToMatch = sim$rasterToMatchLarge) ## needed cropping              
                                                                                                            
  standAgeMap <- Cache(                                                                                     
    LandR::prepInputsStandAgeMap,                                                                           
    ageURL = standAgeMapURL,                                                                                
    rasterToMatch = sim$rasterToMatchLarge,                                                                 
    studyArea = sim$studyAreaLarge,                                                                         
    destinationPath = dPath,                                                                                
    startTime = 2010,                                                                                       
    fireFun = "terra::vect",                                                                                
    fireURL = fireURL,                                                                                      
    filename2 = .suffix("standAgeMap.tif", paste0("_", P(sim)$studyAreaName)),                              
    userTags = c("stable", currentModule(sim), P(sim)$studyAreaName)                                        
  )                                                                                                         
                                                                                                            
  standAgeMap2001 <- Cache(                                                                                 
    LandR::prepInputsStandAgeMap,                                                                           
    ageURL = standAgeMapURL,                                                                                
    rasterToMatch = sim$rasterToMatchLarge,                                                                 
    studyArea = sim$studyAreaLarge,                                                                         
    destinationPath = dPath,                                                                                
    startTime = 2001,                                                                                       
    fireFun = "terra::vect",                                                                                
    fireURL = fireURL,                                                                                      
    filename2 = .suffix("standAgeMap_2001.tif", paste0("_", P(sim)$studyAreaName)),                         
    userTags = c("stable", currentModule(sim), P(sim)$studyAreaName)                                        
  )                                                                                                         
                                                                                                            
  standAgeMap2011 <- Cache(                                                                                 
    LandR::prepInputsStandAgeMap,                                                                           
    ageURL = paste0("https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",                            
                    "canada-forests-attributes_attributs-forests-canada/",                                  
                    "2011-attributes_attributs-2011/",                                                      
                    "NFI_MODIS250m_2011_kNN_Structure_Stand_Age_v1.tif"),                                   
    rasterToMatch = sim$rasterToMatchLarge,                                                                 
    studyArea = sim$studyAreaLarge,                                                                         
    destinationPath = dPath,                                                                                
    startTime = 2011,                                                                                       
    fireFun = "terra::vect",                                                                                
    fireURL = fireURL,                                                                                      
    filename2 = .suffix("standAgeMap_2011.tif", paste0("_", P(sim)$studyAreaName)),                         
    userTags = c("stable", currentModule(sim), P(sim)$studyAreaName)                                        
  )                                                                                                         
                                                                                                            
  ## stand age maps already adjusted within fire polygons using LandR::prepInputsStandAgeMap.               
  ## now, adjust pixels which are younger than oldest fires upward                                          
  earliestFireYear <- as.integer(reproducible::minFn(fireYear))                                             
                                                                                                            
  minNonDisturbedAge <- 2010L - earliestFireYear                                                            
  toChange <- is.na(fireYear[]) & standAgeMap[] <= minNonDisturbedAge                                       
  standAgeMap[toChange] <- minNonDisturbedAge + 2L ## make it an even 40 years old instead of 39            
  imputedPixID <- unique(attr(standAgeMap, "imputedPixID"), which(toChange))                                
                                                                                                            
  minNonDisturbedAge2001 <- 2001L - earliestFireYear                                                        
  toChange2001 <- is.na(fireYear[]) & standAgeMap2001[] <= minNonDisturbedAge2001                           
  standAgeMap2001[toChange2001] <- minNonDisturbedAge2001 + 2L ## make it an even 40 years old instead of 39
  imputedPixID2001 <- unique(attr(standAgeMap2001, "imputedPixID"), which(toChange2001))                    
                                                                                                            
  minNonDisturbedAge2011 <- 2011L - earliestFireYear                                                        
  toChange2011 <- is.na(fireYear[]) & standAgeMap2011[] <= minNonDisturbedAge2011                           
  standAgeMap2011[toChange2011] <- minNonDisturbedAge2011 + 2L ## make it an even 50 years old instead of 49
  imputedPixID2011 <- unique(attr(standAgeMap2011, "imputedPixID"), which(toChange2011))                    
                                                                                                            
  sim$standAgeMap <- asInteger(standAgeMap)                                                                 
  attr(sim$standAgeMap, "imputedPixID") <- imputedPixID                                                     
                                                                                                            
  sim$standAgeMap2001 <- asInteger(standAgeMap2001)                                                         
  attr(sim$standAgeMap2001, "imputedPixID") <- imputedPixID2001                                             
                                                                                                            
  sim$standAgeMap2011 <- asInteger(standAgeMap2011)                                                         
  attr(sim$standAgeMap2011, "imputedPixID") <- imputedPixID2011                                             
                                                                                                            
  # ! ----- STOP EDITING ----- ! #                                                                          
  return(invisible(sim))                                                                                    
}                                                                                                           
                                                                                                            
