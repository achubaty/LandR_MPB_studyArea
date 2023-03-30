defineModule(sim, list(
  name = "LandR_MPB_studyArea",
  description = paste("Prepares 2 sets of objects needed for MPB + LandR-fireSense simulations in western Canada:",
                      "1. study areas and corresponding rasterToMatch (as well as large versions);",
                      "2. species equivalencies tables and the sppEquiv column;",
                      "Each is customized to the study area parameter passed as studyAreaName."),
  keywords = "",
  authors = c(
    person(c("Alex", "M"), "Chubaty", email = "achubaty@for-cast.ca", role = c("aut", "cre")),
    person("Ian", "Eddy", email = "ian.eddy@nrcan-rncan.gc.ca", role = "ctb")
  ),
  childModules = character(0),
  version = list(LandR_MPB_studyArea = "0.0.2"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.md", "LandR_MPB_studyArea.Rmd")), ## same file
  reqdPkgs = list("archive", "geodata", "ggplot2", "ggspatial", "httr", "raster", "rgeos", "sf", "sp",
                  "PredictiveEcology/mpbutils (>= 0.1.2)",
                  "PredictiveEcology/reproducible@development (>= 1.2.8.9033)",
                  "PredictiveEcology/LandR@development (>= 1.1.0.9020)"),
  parameters = rbind(
    defineParameter("bufferDist", "numeric", 20000, NA, NA,
                    "Distance (m) to buffer studyArea and rasterToMatch when creating 'Large' versions."),
    defineParameter("ecoregions4studyArea", "integer", c(112, 120, 122, 124, 126), NA, NA,
                    "National ecoregion ids to include in `studyArea`; should align with ecoprovinces used in `studyAreaName`."),
    defineParameter("studyAreaName", "character", "ABSK_9.2", NA, NA,
                    paste("Should include 'ABSK' to identify the provinces in the study area.",
                          "May also include the ecoprovince ID in which to run the model.")),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
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
                          "and time are not relevant"))
  ),
  inputObjects = bindrows(
    expectsInput("targetCRS", "character", desc = "Geospatial projection to use.", sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput("absk", "sf", "Alberta and Saskatchewan provincial boundaries"),
    createsOutput("fireSenseForestedLCC", "integer", desc = "vector of LCC classes considered to be forested by fireSense."),
    createsOutput("flammableRTM", "RasterLayer", desc = "RTM without ice/rocks/urban/water. Flammable map with 0 and 1."),
    createsOutput("LCC", "RasterLayer", desc = "Land cover classification map, derived from national LCC 2005 product."),
    createsOutput("missingLCCgroup", "character", "the group in `nonForestLCCGroups` that describes forested pixels omitted by LandR"),
    createsOutput("nonflammableLCC", "integer", desc = "vector of LCC classes considered to be non-flammable"),
    createsOutput("nonForestLCCGroups", "list",desc = "named list of non-forested landcover groups for fireSense"),
    createsOutput("nontreeClasses", "integer", desc = "vector of LCC classes considered to be non-forested/treed."), ## TODO what is this used for?
    createsOutput("nonTreePixels", "integer", desc = "pixel indices indicating non-treed pixels"), ## TODO: what is this used for?
    createsOutput("rasterToMatch", "RasterLayer", desc = "Raster to match, based on study area."),
    createsOutput("rasterToMatchLarge", "RasterLayer", desc = "Raster to match (large) based on `studyAreaLarge.`"),
    createsOutput("rasterToMatchReporting", "RasterLayer", desc = "Raster to match based on `studyAreaReporting.`"),
    createsOutput("speciesTable", "data.table", desc = "Species parameter table."),
    createsOutput("sppColorVect", "character", desc = "Species colour vector."),
    createsOutput("sppEquiv", "data.table", desc = "Species equivalency table."),
    createsOutput("sppEquivCol", "character", desc = "name of column to use in `sppEquiv`."),
    createsOutput("standAgeMap", "RasterLayer", desc = "Age (time since disturbance) map, derived from national kNN product."),
    createsOutput("standAgeMap2001", "RasterLayer", desc = "raster of time since disurbance for year 2001."),
    createsOutput("standAgeMap2011", "RasterLayer", desc = "raster of time since disurbance for year 2011."),
    createsOutput("studyArea", "SpatialPolygons", desc = "Buffered study area in which to run simulations."),
    createsOutput("studyAreaLarge", "SpatialPolygons", desc = "Buffered study area used for parameterization/calibration."),
    createsOutput("studyAreaPSP", "SpatialPolygonsDataFrame",
                  paste("this area will be used to subset PSP plots before building the statistical model.",
                        "Currently PSP datasets with repeat measures exist only for Saskatchewan,",
                        "Alberta, Boreal British Columbia, and Ontario.")),
    createsOutput("studyAreaReporting", "SpatialPolygons", desc = "Unbuffered study area used for reporting/post-processing.")
  )
))

## event types
#   - type `init` is required for initialization

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

InitStudyAreaRTM <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  cacheTags <- c(P(sim)$runName, currentModule(sim))
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath\n '", dPath, "'.")

  ## provincial boundaries
  sim$absk <- geodata::gadm(country = "CAN", level = 1, path = dPath) |>
    sf::st_as_sf() |>
    subset(x = _, NAME_1 %in% c("Alberta", "Saskatchewan")) |>
    sf::st_transform(sim$targetCRS)

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

  sim$LCC <- setValues(sim$LCC, asInteger(getValues(sim$LCC)))
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
  earliestFireYear <- as.integer(minValue(fireYear))

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

