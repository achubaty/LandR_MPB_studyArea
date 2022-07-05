defineModule(sim, list(
  name = "LandR_MPB_studyArea",
  description = paste("Prepares 2 sets of objects needed for MPB + LandR-fireSense simulations in western, Canada:",
                      "1. study areas and corresponding rasterToMatch (as well as large versions);",
                      "2. species equivalencies tables and the sppEquiv column;",
                      "Each is customized to the study area parameter passed as studyAreaName."),
  keywords = "",
  authors = c(
    person(c("Alex", "M"), "Chubaty", email = "achubaty@for-cast.ca", role = c("aut", "cre"))
  ),
  childModules = character(0),
  version = list(LandR_MPB_studyArea = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.md", "LandR_MPB_studyArea.Rmd")), ## same file
  reqdPkgs = list("ggplot2", "ggspatial", "raster", "sf", "sp",
                  "PredictiveEcology/mpbutils (>= 0.1.2)",
                  "PredictiveEcology/reproducible@development (>= 1.2.8.9033)",
                  "PredictiveEcology/fireSenseUtils@development (>= 0.0.4.9014)",
                  "PredictiveEcology/LandR@development"),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".plots", "character", c("screen", "png"), NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant")),
    defineParameter("bufferDist", "numeric", 20000, NA, NA,
                    "Distance (m) to buffer studyArea and rasterToMatch when creating 'Large' versions."),
    defineParameter("ecoregions4studyArea", "integer", c(112, 120, 122, 124, 126), NA, NA,
                    "National ecoregions in AB/SK to include in studyArea.")
  ),
  inputObjects = bindrows(
    #expectsInput(objectName = "targetCRS", objectClass = "character", desc = "", sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput("absk", "sf", "Alberta and Saskatchewan proviwcial boundaries"),
    createsOutput("rasterToMatch", "RasterLayer", "template raster"),
    createsOutput("rasterToMatchLarge", "RasterLayer", "template raster for larger area"),
    createsOutput("rasterToMatchReporting", "RasterLayer", "template raster for reporting area"),
    createsOutput("sppColorVect", "character", "species colours for plotting"),
    createsOutput("sppEquiv", "data.table", "table of LandR species names equivalencies"),
    createsOutput("sppEquivCol", "character", "name of column to use in sppEquiv"),
    createsOutput("studyAreaPSP", "SpatialPolygonsDataFrame",
                  paste("this area will be used to subset PSP plots before building the statistical model.",
                               "Currently PSP datasets with repeat measures exist only for Saskatchewan,",
                               "Alberta, and Boreal British Columbia")),
    createsOutput("studyArea", "SpatialPolygonsDataFrame",
                  "study area used for simulation (buffered to mitigate edge effects)"),
    createsOutput("studyAreaLarge", "SpatialPolygonsDataFrame",
                  "study area used for module parameterization (buffered)"),
    createsOutput("studyAreaReporting", "SpatialPolygonsDataFrame",
                  "study area used for reporting/post-processing")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.LandR_MPB_studyArea = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- InitStudyArea(sim)
      sim <- InitRTM(sim)
      sim <- InitSpecies(sim)
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
InitStudyArea <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)

  targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                     "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

  # The following is sloppy -- needs this first preProcess to getData
  #   second prepInputs will fail if the getData didn't already run, because of ...
  tmp_res <- Cache(preProcess,
                   "GADM",
                   country = "CAN", level = 1, path = dPath,
                   dlFun = "raster::getData",
                   targetFile = "gadm36_CAN_1_sp.rds") ## TODO: this will change as GADM data update

  sim$absk <- Cache(prepInputs,
                    "GADM",
                    fun = quote(loadABSK(targetFilePath, targetCRS)),
                    dlFun = "raster::getData", targetCRS = targetCRS,
                    loadABSK = loadABSK,
                    country = "CAN", level = 1, path = dPath,
                    targetFile = "gadm36_CAN_1_sp.rds", ## TODO: this will change as GADM data update
                    cacheRepo = cachePath(sim),
                    destinationPath = dPath)

  slaveLake <- prepInputs(url = "https://static.ags.aer.ca/files/document/DIG/DIG_2008_0793.zip",
                          archive = "DIG_2008_0793.zip",
                          targetFile = "less_bdy_py_tm.shp",
                          alsoExtract = "similar",
                          fun = "sf::st_read",
                          destinationPath = dPath)

  ## study area ecoregions:
  ##   Wabasca Lowlands (112)
  ##   Mid-Boreal Uplands (122, 124, 126)
  ##   Western Alberta Uplands (120)
  studyAreaReporting <- mpbStudyArea(ecoregions = P(sim)$ecoregions4studyArea,
                                     targetCRS = targetCRS,
                                     cPath = cachePath(sim),
                                     dPath = dPath) %>%
    st_intersection(., sim$absk) %>%
    st_union(.)
  studyArea <- st_buffer(studyAreaReporting, P(sim)$bufferDist)

  ## Turn this on or off with P(sim)$.plots
  figPath <- checkPath(file.path(outputPath(sim), "figures"), create = TRUE)
  Plots(
    data = sim$absk, cols = cols, studyArea = studyAreaReporting, lake = slaveLake,
    .plotInitialTime = time(sim),
    fn = ggplotStudyAreaFn,
    types = P(sim)$.plots,
    filename = file.path(figPath, "mpb_studyArea"),
    ggsaveArgs = list(width = 7, height = 7)
  )

  ## NOTE: studyArea and studyAreaLarge are the same [buffered] area
  ## convert to spdf for use with other modules
  sim$studyAreaReporting <- as_Spatial(studyAreaReporting)
  sim$studyArea <- as_Spatial(studyArea)
  sim$studyAreaLarge <- sim$studyArea

  ecozones2use <- c("Boreal PLain", "Boreal Shield")

  ecozones_absk <- Cache(prepInputs,
                         targetFile = "ecozones.shp",
                         archive = asPath("ecozone_shp.zip"),
                         url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
                         alsoExtract = "similar",
                         destinationPath = dPath,
                         filename2 = NULL,
                         studyArea = sim$absk,
                         overwrite = TRUE,
                         useSAcrs = TRUE,
                         fun = "sf::st_read",
                         userTags = c("prepInputsEcozones", currentModule(sim), cacheTags))
  ecozones_absk <- ecozones_absk[ecozones_absk$ZONE_NAME %in% ecozones2use, ] ## remove boundary artifacts

  ecozones_SA <- Cache(prepInputs,
                       targetFile = "ecozones.shp",
                       archive = asPath("ecozone_shp.zip"),
                       url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
                       alsoExtract = "similar",
                       destinationPath = dPath,
                       filename2 = NULL,
                       studyArea = sim$studyAreaReporting,
                       overwrite = TRUE,
                       useSAcrs = TRUE,
                       fun = "sf::st_read",
                       userTags = c("prepInputsEcozones", currentModule(sim), cacheTags))
  ecozones_SA <- ecozones_SA[ecozones_SA$ZONE_NAME %in% ecozones2use, ] ## remove boundary artifacts

  ## use ecozone boundaries within WBI study area for parameterizing PSP for current study area
  sim$studyAreaPSP <- ecozones_absk[ecozones_absk$ZONE_NAME %in% ecozones_SA$ZONE_NAME, ] %>%
    as_Spatial()

  return(invisible(sim))
}

InitRTM <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)

  sim$rasterToMatch <- Cache(LandR::prepInputsLCC,
                             year = 2005,
                             studyArea = sim$studyArea,
                             destinationPath = dPath,
                             useCache = P(sim)$.useCache,
                             overwrite = TRUE,
                             filename2 = paste0(P(sim)$studyAreaName, "_rtm.tif"))
  sim$rasterToMatchLarge <- sim$rasterToMatch
  sim$rasterToMatchReporting <- Cache(maskInputs, sim$rasterToMatch, sim$studyAreaReporting)

  return(invisible(sim))
}

InitSpecies <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)

  sppEquiv <- LandR::sppEquivalencies_CA
  spp2use <- c("Abie_Bal", "Abie_Las",
               "Betu_Pap",
               "Lari_Lar",
               "Pice_Gla", "Pice_Mar", ## "Pice_Eng" ?
               "Pinu_Ban", "Pinu_Con",
               "Popu_Tre")
  sim$sppEquiv <- sppEquiv[KNN %in% spp2use]
  sim$sppEquivCol <- "LandR"
  sim$sppColorVect <- LandR::sppColors(sppEquiv = sim$sppEquiv, sppEquivCol = sim$sppEquivCol,
                                       palette = "Paired", newVals = "Mixed")

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # cacheTags <- c(currentModule(sim), "function:.inputObjects")
  # dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  # message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

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

loadABSK <- function(x, targetCRS) {
  x <- base::readRDS(x)
  x <- x[x$NAME_1 %in% c("Alberta", "Saskatchewan"), ]
  x <- st_as_sf(x)
  x <- st_transform(x, targetCRS) ## keep as sf for plotting
}
