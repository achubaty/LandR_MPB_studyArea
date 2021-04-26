defineModule(sim, list(
  name = "MPB_SK_studyArea",
  description = "",
  keywords = "",
  authors = c(
    person(c("Alex", "M"), "Chubaty", email = "achubaty@for-cast.ca", role = c("aut", "cre"))
  ),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.6", MPB_SK_studyArea = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.md", "MPB_SK_studyArea.Rmd")), ## same file
  reqdPkgs = list("ggplot2", "ggspatial", "raster", "sf"),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
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
    expectsInput(objectName = "targetCRS", objectClass = "character",
                 desc = "", sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "studyArea", objectClass = "SpatialPolygonsDataFrame",
                  desc = "buffered study area for simulation"),
    createsOutput(objectName = "studyAreaFit", objectClass = "SpatialPolygonsDataFrame",
                  desc = "unbuffered study area used for fitting spread data"),
    createsOutput(objectName = "studyAreaReporting", objectClass = "SpatialPolygonsDataFrame",
                  desc = "unbuffered study area for reporting/post-processing"),
  )
))

## event types
#   - type `init` is required for initialization

doEvent.MPB_SK_studyArea = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "MPB_SK_studyArea", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "MPB_SK_studyArea", "save")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      Plot(mod$gg_studyAreas)
      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      figPath <- checkPath(file.path(outputPath(sim), "figures"), create = TRUE)
      ggsave(mod$gg_studyAreas, filename = file.path(figPath, "mpb_studyArea.png"),
             width = 7, height = 7)
      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #

  provinces <- Cache(prepInputs,
                     "GADM",
                     fun = "base::readRDS",
                     dlFun = "raster::getData",
                     country = "CAN", level = 1, path = inputPath(sim),
                     targetFile = "gadm36_CAN_1_sp.rds", ## TODO: this will change as GADM data update
                     cacheRepo = cachePath(sim),
                     destinationPath = inputPath(sim)) %>%
    st_as_sf(.) %>%
    st_transform(., sim$targetCRS) ## keep as sf for plotting

  absk <- provinces[provinces$NAME_1 %in% c("Alberta", "Saskatchewan"), ]

  ecoregions <- prepInputs(url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip",
                           targetFile = "ecoregions.shp", alsoExtract = "similar",
                           fun = "sf::st_read",
                           cacheRepo = cachePath(sim),
                           destinationPath = inputPath(sim)) %>%
    st_transform(., sim$targetCRS) ## keep as sf for plotting

  ## study area ecoregions:
  ##   Wabasca Lowlands (112)
  ##   Mid-Boreal Uplands (122, 124, 126)
  ##   Western Alberta Uplands (120)
  studyAreaReporting <- ecoregions[ecoregions$REGION_ID %in% c(112, 122, 124, 126), ]
  studyAreaFit <- ecoregions[ecoregions$REGION_ID %in% c(120), ]
  studyArea <- st_buffer(studyAreaReporting, 10000) ## 10 km buffer
  studyAreasJoined <- ecoregions[ecoregions$REGION_ID %in% c(112, 122, 124, 126, 120), ] %>%
    st_intersection(., absk)

  cols <- c("darkgreen", "forestgreen", "darkred")
  mod$gg_studyAreas <- ggplot(absk) +
    geom_sf(fill = "white", colour = "black", alpha = 0.5) +
    geom_sf(data = studyAreasJoined, mapping = aes(fill = REGION_NAM, colour = REGION_NAM), alpha = 0.5) +
    theme_bw() +
    annotation_north_arrow(location = "bl", which_north = "true",
                           pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                           style = north_arrow_fancy_orienteering) +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("MPB study areas") +
    scale_colour_manual(values = cols) +
    scale_fill_manual(values = cols)

  ## convert to spdf for use with other modules
  sim$studyAreaReporting <- as_Spatial(studyAreaReporting)
  sim$studyAreaFit <- as_Spatial(studyAreaFit)
  sim$studyArea <- as_Spatial(studyArea)

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #
  if (!suppliedElsewhere("targetCRS")) {
    sim$targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                           "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
