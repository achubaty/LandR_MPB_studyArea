InitStudyAreaRTM <- function(sim) {                                                                                        
  # # ! ----- EDIT BELOW ----- ! #                                                                                         
  cacheTags <- c(P(sim)$runName, currentModule(sim))                                                                       
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)                                             
  message(currentModule(sim), ": using dataPath\n '", dPath, "'.")                                                        
                                                                                                                           
  provs <- c("Alberta", "Saskatchewan")                                                                                    
  ## provincial boundaries                                                                                                 
  sim$absk <- try(geodata::gadm(country = "CAN", level = 1, path = dPath) |>                                               
    sf::st_as_sf() |>                                                                                                      
    subset(x = _, NAME_1 %in% provs) |>                                                                                    
    sf::st_transform(sim$targetCRS))                                                                                       
  if (is(sim$absk, "try-error") || is.null(sim$absk)) {                                                                    
    # -- this has a stashed version on Google Drive if gadm server is down                                                 
    if (!requireNamespace("SpaDES.project"))  {                                                                            
      repo <- c("predictiveecology.r-universe.dev", getOption("repos"))                                                    
      install.packages(p, repos = repo)                                                                                    
      stop("The geodata server is down; please ",                                                                          
           "install.packages('SpaDES.project', repos = c(\"predictiveecology.r-universe.dev\", getOption(\"repos\")))")
    }                                                                                                                      
    sim$absk <- SpaDES.project::setupStudyArea(list(country = "CAN", NAME_1 = "Alberta|Saskatchewan", level = 1,           
                                    to = sim$targetCRS), paths = paths(sim))                                               
                                                                                                                           
  }                                                                                                                        
                                                                                                                           
  slaveLake <- prepInputs(url = "https://static.ags.aer.ca/files/document/DIG/DIG_2008_0793.zip",                          
                          archive = "DIG_2008_0793.zip",                                                                   
                          targetFile = "less_bdy_py_tm.shp",                                                               
                          alsoExtract = "similar",                                                                         
                          fun = "sf::st_read",                                                                             
                          destinationPath = dPath)                                                                         
                                                                                                                           
  ## ECOPROVINCES                                                                                                          
  ecoprov <- prepInputs(                                                                                                   
    url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/province/ecoprovince_shp.zip",                                       
    targetFile = "ecoprovinces.shp",                                                                                       
    alsoExtract = "similar",                                                                                               
    fun = "sf::st_read",                                                                                                   
    destinationPath = dPath                                                                                                
  ) |>                                                                                                                     
    subset(x = _, ECOPROVINC %in% mod$ecoprov) |>                                                                          
    st_transform(crs = sim$targetCRS)                                                                                      
                                                                                                                           
  ## ECOZONES                                                                                                              
  ez <- c("Boreal PLain", "Boreal Shield") ## NOTE: typo is in original shapefile                                          
                                                                                                                           
  ecozones <- prepInputs(                                                                                                  
    url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",                                               
    targetFile = "ecozones.shp",                                                                                           
    alsoExtract = "similar",                                                                                               
    fun = "sf::st_read",                                                                                                   
    destinationPath = dPath                                                                                                
  ) |>                                                                                                                     
    st_transform(crs = sim$targetCRS)                                                                                      
  ecozones[["ZONE_NAME"]] <- toupper(ecozones[["ZONE_NAME"]])                                                              
  ecozone <- ecozones[ecozones$ZONE_NAME %in% ez, ]                                                                        
  rm(ecozones)                                                                                                             
                                                                                                                           
                                                                                                                           
  ## STUDY AREAS                                                                                                           
                                                                                                                           
  ## study area ecoregions:                                                                                                
  ##   Wabasca Lowlands (112)                                                                                              
  ##   Mid-Boreal Uplands (122, 124, 126)                                                                                  
  ##   Western Alberta Uplands (120)                                                                                       
  studyAreaReporting <- mpbStudyArea(ecoregions = P(sim)$ecoregions4studyArea,                                             
                                     targetCRS = sim$targetCRS,                                                            
                                     cPath = cachePath(sim),                                                               
                                     dPath = dPath) %>%                                                                    
    st_intersection(., sim$absk) %>%                                                                                       
    st_union(.)                                                                                                            
  studyArea <- st_buffer(studyAreaReporting, P(sim)$bufferDist)                                                            
                                                                                                                           
  ## NOTE: studyArea and studyAreaLarge are the same [buffered] area                                                       
  ## convert to spdf for use with other modules                                                                            
  sim$studyAreaReporting <- as_Spatial(studyAreaReporting)                                                                 
  sim$studyArea <- as_Spatial(studyArea) ## TODO: st_convex_hull() ?                                                       
  sim$studyAreaLarge <- sim$studyArea                                                                                      
  sim$studyAreaPSP <- ecozone                                                                                              
                                                                                                                           
  ## TODO: move to plot event and update the other plots there to use `Plots()`                                            
  figPath <- checkPath(file.path(outputPath(sim), "figures"), create = TRUE)                                               
  Plots(                                                                                                                   
    data = sim$absk, cols = cols, studyArea = studyAreaReporting, lake = slaveLake,                                        
    .plotInitialTime = time(sim),                                                                                          
    fn = ggplotStudyAreaFn,                                                                                                
    types = P(sim)$.plots,                                                                                                 
    filename = file.path(figPath, "mpb_studyArea"),                                                                        
    ggsaveArgs = list(width = 7, height = 7)                                                                               
  )                                                                                                                        
                                                                                                                           
  ## RASTERS TO MATCH                                                                                                      
  sim$rasterToMatch <- Cache(LandR::prepInputsLCC,                                                                         
                             year = 2005,                                                                                  
                             studyArea = sim$studyArea,                                                                    
                             destinationPath = dPath,                                                                      
                             useCache = P(sim)$.useCache,                                                                  
                             filename2 = NULL)                                                                             
                                                                                                                           
  sim$rasterToMatchLarge <- Cache(LandR::prepInputsLCC,                                                                    
                                  year = 2005,                                                                             
                                  studyArea = sim$studyAreaLarge,                                                          
                                  destinationPath = dPath,                                                                 
                                  useCache = P(sim)$.useCache,                                                             
                                  filename2 = NULL)                                                                        
                                                                                                                           
  sim$rasterToMatchReporting <- Cache(LandR::prepInputsLCC,                                                                
                                      year = 2005,                                                                         
                                      studyArea = sim$studyAreaReporting,                                                  
                                      destinationPath = dPath,                                                             
                                      useCache = P(sim)$.useCache,                                                         
                                      filename2 = NULL)                                                                    
                                                                                                                           
  writeRaster(sim$rasterToMatch, file.path(dPath, paste0(P(sim)$studyAreaName, "_rtm.tif")),                               
              datatype = "INT1U", overwrite = TRUE)                                                                        
  writeRaster(sim$rasterToMatchLarge,  file.path(dPath, paste0(P(sim)$studyAreaName, "_rtml.tif")),                        
              datatype = "INT1U", overwrite = TRUE)                                                                        
  writeRaster(sim$rasterToMatchReporting,  file.path(dPath, paste0(P(sim)$studyAreaName, "_rtmr.tif")),                    
              datatype = "INT1U", overwrite = TRUE)                                                                        
                                                                                                                           
  # ! ----- STOP EDITING ----- ! #                                                                                         
  return(invisible(sim))                                                                                                   
}                                                                                                                          
                                                                                                                           
