#!/usr/bin/env Rscript
# /* Copyright (C) 2024 Athanasios Natsis <natsisthanasis@gmail.com> */

#### Make file logic for R project

rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()


PROJECT <- "~/MANUSCRIPTS/02_enhancement/"
MODES   <- c("pdf", "run")
source("~/CODE/FUNCTIONS/R/make_tools.R")



## Setup project
setwd(PROJECT)
renv::load(PROJECT)

## Collect arguments
args <- commandArgs(TRUE)

## Default option on empty
if (length(args) < 1) {
  args <- MODES[1]
}


cat("Mode:", paste(args), "\n")

cat("UKNOWN MODE:", paste(args[!args %in% MODES]), "\n")






if ("pdf" %in% args) {
  cat("\n~ ~ DOING PDF ~ ~\n\n")
  library(rmarkdown)
  library(knitr)


  glo_source <- c("GHI_enh_00_variables.R")
  glo_data   <- c()
  glo_target <- c()





  l_source   <- "GHI_enh_06_sza.R"
  l_target   <- c(sub(".R", ".pdf",   l_source),
                  sub(".R", "_files", l_source))
  l_data     <- "./data/GHI_enh_03_process.Rda"
  glo_source <- c(glo_source, l_source)
  glo_data   <- c(glo_data,   l_data)


  if (Rmk_check_dependencies(
    depend.source = glo_source,
    depend.data   = glo_data,
    targets       = l_target
  )) {
    render(l_source, output_dir = "./")
  }
  Rmk_store_dependencies()



}


if ("run" %in% args) {
  cat("\n~ ~ DOING RUN ~ ~\n\n")
}








cat(paste("\n\n IS YOU SEE THIS: \n", "make"," GOT TO THE END!! \n"))
cat(paste("\n EVERYTHING SHOULD BE FINE \n"))

## END ##
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],"make",difftime(tac,tic,units="mins")))
