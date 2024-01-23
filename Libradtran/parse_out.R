#!/usr/bin/env Rscript
# /* Copyright (C) 2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:  "......."
#' author: "Natsis Athanasios"
#' institute: "AUTH"
#' affiliation: "Laboratory of Atmospheric Physics"
#' abstract: "........."
#' output:
#'   html_document:
#'     toc: true
#'     fig_width:  9
#'     fig_height: 4
#'   pdf_document:
#' date: "`r format(Sys.time(), '%F')`"
#' ---


#+ echo=F, include=T
rm(list = (ls()[ls() != ""]))
Script.Name <- "~/MANUSCRIPTS/02_enhancement/Libradtran/parse_out.R"
dir.create("./runtime/", showWarnings = FALSE)
d <- filelock::lock(paste0("./runtime/", basename(sub("\\.R$",".lock", Script.Name))), timeout = 0)
Sys.setenv(TZ = "UTC")
## standard output
if (!interactive()) {
    pdf( file = paste0("./runtime/",  basename(sub("\\.R$",".pdf", Script.Name))))
    sink(file = paste0("./runtime/",  basename(sub("\\.R$",".out", Script.Name))), split = TRUE)
}
## error notification function
options(error = function() {
    if (interactive()) {
        system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
        system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'An error occurred!'"))
    }
})
tic <- Sys.time()

library(data.table)
source("~/FUNCTIONS/R/data.R")

## run files
repo_dir <- "~/MANUSCRIPTS/02_enhancement/Libradtran/io_repo/"

## empty runs
run_list_rds <- "~/MANUSCRIPTS/02_enhancement/Libradtran/run.Rds"

## all runs
model_cs     <- "~/MANUSCRIPTS/02_enhancement/Libradtran/Model_CS.Rds"


## Function to parse .err files ##
error_param <- function(errfile) {
    ## file should exist
    if (!file.exists(errfile)) stop(paste("Missing file: ", errfile))

    ## read error file
    fcon   = file(errfile)
    lines  = readLines(fcon)
    close(fcon)

    ## get start and end times
    minD = min(as.numeric(grep("^[0-9]+$", lines, value = TRUE)))
    maxD = max(as.numeric(grep("^[0-9]+$", lines, value = TRUE)))
    ## get host name
    hosts = grep("hostname=.*", lines, value = TRUE)
    hosts = unlist(strsplit(hosts,"="))[2]
    if (hosts != "") {
        host = hosts
    } else {
        host = "unknown"
    }

    return(data.frame( hostname = host,
                       ticTime  = minD,
                       tacTime  = maxD, stringsAsFactors = F ))
}


## parse libradtran output
todo <- readRDS(run_list_rds)

out_files <- list.files(path       = repo_dir,
                        pattern    = "out.gz",
                        full.names = T )

err_files <- list.files(path       = repo_dir,
                        pattern    = "err",
                        full.names = T )

## read new data
if (length(out_files) > 0 & length(out_files) == length(err_files)) {

    hist(file.size(err_files), breaks = 100)

    gather <- data.table()
    for (af in out_files) {

        file.size(af)

        ## get hash id
        hash <- gsub("LBT_|.out.gz","", basename(af))

        ## get data
        temp <- read.table(af)
        names(temp) <- c("lambda", "edir", "edn", "eup", "uavgdir", "uavgdn", "uavgup")

        ## get meta data
        meta <- error_param(sub(".out.gz", ".err", af))

        ## gather records
        gather <- rbind(gather, cbind(ID = hash, temp, meta))
    }

    data <- merge(todo, gather, all.x = T, by = "ID")
    data <- data[!is.na(edir)]
}

if (nrow(data) < 1) {
    ## No new data
    stop()
}

## append new data to storage

if (file.exists(model_cs)) {
    storage <- readRDS(model_cs)
} else {
    storage <- data.table()
}


trastorage <- rbind(storage, data)

stopifnot(any(!duplicated(storage$ID)))

saveRDS(storage, model_cs)

# file.remove(out_files)
# file.remove(err_files)



#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (difftime(tac, tic, units = "sec") > 30) {
    system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
    system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'FINISHED'"))
}
