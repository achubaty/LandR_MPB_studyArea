---
title: "MPB_SK_studyArea"
author: "achubaty"
date: "20 April 2021"
output: 
  html_document: 
    keep_md: yes
editor_options:
  chunk_output_type: console
---



# Overview

Provide an overview of what the module does / how to use the module.

Module documentation should be written so that others can use your module.
This is a template for module documentation, and should be changed to reflect your module.

## R Markdown

R Markdown syntax allows R code, outputs, and figures to be rendered in the documentation.

For help writing in R Markdown, see https://rmarkdown.rstudio.com/.

# Usage


```r
library(SpaDES.core)
```

```
## Loading required package: quickPlot
```

```
## Loading required package: reproducible
```

```
## 
## Attaching package: 'SpaDES.core'
```

```
## The following objects are masked from 'package:stats':
## 
##     end, start
```

```
## The following object is masked from 'package:utils':
## 
##     citation
```

```r
setPaths(modulePath = file.path("/home/achubaty/Documents/GitHub"))
```

```
## Setting:
##   options(
##     spades.modulePath = '/home/achubaty/Documents/GitHub'
##   )
```

```
## Paths set to:
##   options(
##     rasterTmpDir = '/tmp/RtmpGV5rUw/raster'
##     reproducible.cachePath = '/tmp/RtmpGV5rUw/Require/cache'
##     spades.inputPath = '/tmp/RtmptIFHCb/SpaDES/inputs'
##     spades.outputPath = '/tmp/RtmptIFHCb/SpaDES/outputs'
##     spades.modulePath = '/home/achubaty/Documents/GitHub'
##   )
```

```r
getPaths() # shows where the 4 relevant paths are

times <- list(start = 0, end = 10)

parameters <- list(
  #.progress = list(type = "text", interval = 1), # for a progress bar
  ## If there are further modules, each can have its own set of parameters:
  #module1 = list(param1 = value1, param2 = value2),
  #module2 = list(param1 = value1, param2 = value2)
)
modules <- list("MPB_SK_studyArea")
objects <- list()
inputs <- list()
outputs <- list()

mySim <- simInit(times = times, params = parameters, modules = modules,
                 objects = objects)
```

```
## Setting:
##   options(
##     rasterTmpDir = '/tmp/RtmpGV5rUw/raster'
##     reproducible.cachePath = '/tmp/RtmpGV5rUw/Require/cache'
##     spades.inputPath = '/tmp/RtmptIFHCb/SpaDES/inputs'
##     spades.outputPath = '/tmp/RtmptIFHCb/SpaDES/outputs'
##     spades.modulePath = '/home/achubaty/Documents/GitHub'
##   )
```

```
## MPB_SK_studyArea: module code appears clean
```

```
## Apr20 10:25:13 simInit: MPB_SK Running .inputObjects for MPB_SK_studyArea
```

```
## Apr20 10:25:13 simInit: MPB_SK MPB_SK_studyArea: using dataPath '/home/achubaty/Documents/GitHub/MPB_SK_studyArea/data'.
```

```r
mySimOut <- spades(mySim)
```

```
## Apr20 10:25:13 chckpn total elpsd: 0.0018 secs | 0 checkpoint init 0
```

```
## Apr20 10:25:13 save   total elpsd: 0.0038 secs | 0 save init 0
```

```
## Apr20 10:25:13 prgrss total elpsd: 0.0053 secs | 0 progress init 0
```

```
## Apr20 10:25:13 load   total elpsd: 0.0067 secs | 0 load init 0
```

```
## Apr20 10:25:13 MPB_SK total elpsd: 0.008 secs | 0 MPB_SK_studyArea init
```

```
## simList saved in
##  SpaDES.core:::.pkgEnv$.sim 
## It will be deleted at next spades() call.
```

```
## $cachePath
## [1] "/tmp/RtmpGV5rUw/Require/cache"
## 
## $inputPath
## [1] "/tmp/RtmptIFHCb/SpaDES/inputs"
## 
## $modulePath
## [1] "/home/achubaty/Documents/GitHub"
## 
## $outputPath
## [1] "/tmp/RtmptIFHCb/SpaDES/outputs"
## 
## $rasterPath
## [1] "/tmp/RtmpGV5rUw/raster//"
```

# Parameters

Provide a summary of user-visible parameters.


|paramName        |paramClass |default |min |max |paramDesc                                                                                                                                                |
|:----------------|:----------|:-------|:---|:---|:--------------------------------------------------------------------------------------------------------------------------------------------------------|
|.plotInitialTime |numeric    |NA      |NA  |NA  |Describes the simulation time at which the first plot event should occur.                                                                                |
|.plotInterval    |numeric    |NA      |NA  |NA  |Describes the simulation time interval between plot events.                                                                                              |
|.saveInitialTime |numeric    |NA      |NA  |NA  |Describes the simulation time at which the first save event should occur.                                                                                |
|.saveInterval    |numeric    |NA      |NA  |NA  |This describes the simulation time interval between save events.                                                                                         |
|.useCache        |logical    |FALSE   |NA  |NA  |Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant |

# Events

Describe what happens for each event type.

## Plotting

Write what is plotted.

## Saving

Write what is saved.

# Data dependencies

## Input data

How to obtain input data, and a description of the data required by the module.
If `sourceURL` is specified, `downloadData("MPB_SK_studyArea", "/home/achubaty/Documents/GitHub")` may be sufficient.


|objectName |objectClass |desc |sourceURL |
|:----------|:-----------|:----|:---------|
|NA         |NA          |NA   |NA        |

## Output data

Description of the module outputs.


|objectName |objectClass |desc |
|:----------|:-----------|:----|
|NA         |NA          |NA   |

# Links to other modules

Describe any anticipated linkages to other modules.
