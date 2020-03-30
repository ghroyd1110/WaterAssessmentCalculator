#!/bin/bash Rscript
library(rsconnect)
#rsconnect::applications()
setwd("/wcalc/work/app/")
rsconnect::deployApp(appDir = "./deploy/", appPrimaryDoc = "index.Rmd", appSourceDoc = "./deploy/index.Rmd",
                    account = "azdeq", server = "shinyapps.io", appName = "waterapp",
                    # account = "elawater", server = "shinyapps.io", appName = "waterapp",
                    appTitle = "waterapp", launch.browser = FALSE, forceUpdate = TRUE,
                     lint = FALSE, metadata = list(asMultiple = FALSE, asStatic = FALSE),  logLevel = "normal")
