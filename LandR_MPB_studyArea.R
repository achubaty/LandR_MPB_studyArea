defineModule(sim, list(
  name = "LandR_MPB_studyArea",
  description = "",
  keywords = "",
  authors = c(
    person(c("Alex", "M"), "Chubaty", email = "achubaty@for-cast.ca", role = c("aut", "cre"))
  ),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.6", LandR_MPB_studyArea = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.md", "LandR_MPB_studyArea.Rmd")), ## same file
  reqdPkgs = list("ggplot2", "ggspatial", "raster", "sf",
                  "PredictiveEcology/mpbutils (>= 0.1.2)"),
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
                          "and time are not relevant"))
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "targetCRS", objectClass = "character",
                 desc = "", sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "absk", objectClass = "SpatialPolygonsDataFrame",
                  desc = "Alberta and Saskatchewan political outlines"),
    createsOutput(objectName = "studyArea", objectClass = "SpatialPolygonsDataFrame",
                  desc = "buffered study area for simulation and fitting"),
    createsOutput(objectName = "studyAreaReporting", objectClass = "SpatialPolygonsDataFrame",
                  desc = "unbuffered study area for reporting/post-processing")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.LandR_MPB_studyArea = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      # sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "LandR_MPB_studyArea", "plot")
      # sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "LandR_MPB_studyArea", "save")
    },
    # plot = {
    #   # ! ----- EDIT BELOW ----- ! #
    #   studyArea <- mod$gg_studyAreas
    #   Plot(studyArea)
    #   # ! ----- STOP EDITING ----- ! #
    # },
    # save = {
    #   # ! ----- EDIT BELOW ----- ! #
    #   #figPath <- checkPath(file.path(outputPath(sim), "figures"), create = TRUE)
    #   #ggsave(mod$gg_studyAreas, filename = file.path(figPath, "mpb_studyArea.png"),
    #   #       width = 7, height = 7)
    #   # ! ----- STOP EDITING ----- ! #
    # },
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

  opts <- options(reproducible.useTerra = TRUE)
  on.exit(options(opts))

  targetCRS <- sim$targetCRS
  # The following is sloppy -- needs this first preProcess to getData
  #   second prepInputs will fail if the getData didn't already run, because of ...
  Cache(preProcess,
        dlFun = "raster::getData", targetCRS = sim$targetCRS,
        loadABSK = loadABSK,
        country = "CAN", level = 1, path = inputPath(sim),
        targetFile = "gadm36_CAN_1_sp.rds" ## TODO: this will change as GADM data update
  )
  sim$absk <- Cache(prepInputs,
                     "GADM",
                     fun = quote(loadABSK(targetFilePath, targetCRS)), #base::readRDS",
                     dlFun = "raster::getData", targetCRS = sim$targetCRS,
                     loadABSK = loadABSK,
                     country = "CAN", level = 1, path = inputPath(sim),
                     targetFile = "gadm36_CAN_1_sp.rds", ## TODO: this will change as GADM data update
                     cacheRepo = cachePath(sim),
                     destinationPath = inputPath(sim))# %>%

  # Put these all inside the load function (loadABSK) so it is faster next time!
  #   st_as_sf(.) %>%
  #   st_transform(., sim$targetCRS) ## keep as sf for plotting
  # sim$absk <- provinces[provinces$NAME_1 %in% c("Alberta", "Saskatchewan"), ]

  slaveLake <- prepInputs(url = "https://static.ags.aer.ca/files/document/DIG/DIG_2008_0793.zip",
                          archive = "DIG_2008_0793.zip",
                          targetFile = "less_bdy_py_tm.shp",
                          alsoExtract = "similar",
                          fun = "sf::st_read",
                          destinationPath = inputPath(sim))

  ## study area ecoregions:
  ##   Wabasca Lowlands (112)
  ##   Mid-Boreal Uplands (122, 124, 126)
  ##   Western Alberta Uplands (120)
  studyAreaReporting <- mpbStudyArea(ecoregions = c(112, 120, 122, 124, 126),
                                     targetCRS = sim$targetCRS,
                                     cPath = cachePath(sim),
                                     dPath = mod$dPath) %>%
    st_intersection(., sim$absk) %>%
    st_union(.)
  studyArea <- st_buffer(studyAreaReporting, 10000) ## 10 km buffer

  # Turn this on or off with P(sim)$.plots
  figPath <- checkPath(file.path(outputPath(sim), "figures"), create = TRUE)
  Plots(
    data = sim$absk, cols = cols, studyArea = studyAreaReporting, lake = slaveLake,
    .plotInitialTime = time(sim),
    fn = ggplotStudyAreaFn,
    types = P(sim)$.plots,
    filename = file.path(figPath, "mpb_studyArea"),
    ggsaveArgs = list(width = 7, height = 7)
  )

  ## convert to spdf for use with other modules
  sim$studyAreaReporting <- as_Spatial(studyAreaReporting)

  # Use larger study area to have all data
  sim$studyArea <- as_Spatial(studyArea)

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  mod$dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", mod$dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #
  if (!suppliedElsewhere("targetCRS")) {
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

loadABSK <- function(x, targetCRS) {
  x <- base::readRDS(x)
  x <- x[x$NAME_1 %in% c("Alberta", "Saskatchewan"), ]
  x <- st_as_sf(x)
  x <- st_transform(x, targetCRS) ## keep as sf for plotting
}
