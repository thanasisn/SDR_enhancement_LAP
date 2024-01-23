#!/usr/bin/env Rscript
# /* Copyright (C) 2019 Athanasios Natsis <natsisthanasis@gmail.com> */

####    Set environment    #####################################################
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic = Sys.time()
Script.Name = c("parse_out.R")

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

## append new data to storage

if (file.exists(model_cs)) {
    storage <- readRDS(model_cs)
} else {
    storage <- data.table()
}

storage <- rbind(storage, data)

stopifnot(any(!duplicated(storage$ID)))

saveRDS(storage, model_cs)

file.remove(out_files)
file.remove(err_files)

