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
library(RlibRadtran)
library(ggplot2)

source("~/FUNCTIONS/R/data.R")

## run files
repo_dir <- "~/MANUSCRIPTS/02_enhancement/Libradtran/io_repo/"

## empty runs
run_list_rds <- "~/MANUSCRIPTS/02_enhancement/Libradtran/run.Rds"

## all runs
model_cs     <- "~/MANUSCRIPTS/02_enhancement/Libradtran/Model_CS.Rds"


## Based on raw data
DATA <- data.table(readRDS("../data/CE_ID_Input.Rds"))

## Fill with model
CS <- data.table(readRDS("./Model_CS.Rds"))

LKUO <- DATA[, .(Date, SZA, sun_dist, wattGLB)]
rm(DATA)


# LKUO[, target_atm := date_to_standard_atmosphere_file(Date) ]


for (aday in unique(as.Date(LKUO$Date))) {

    LKUO[as.Date(Date) == aday]

    theday  <- as.POSIXct(as.Date(aday, "1970-01-01"))

    atmfile <- date_to_standard_atmosphere_file(as.POSIXct(as.Date(aday, "1970-01-01")))


    ## choose data to interpolate
    CS_exact <- CS[atmosphere_file == atmfile &
                       month == month(theday) &
                       type == "Exact B", .(sza, (edir + edn)/1000 )  ]

    CS_low   <- CS[atmosphere_file == atmfile &
                       month == month(theday) &
                       type == "Low B",   .(sza, (edir + edn)/1000 )  ]



    plot(  CS_low)
    points(CS_exact)

    ## Interpolate to SZA
    CS_low_fn   <- approxfun( CS_low$sza,   CS_low$V2)
    CS_exact_fn <- approxfun( CS_exact$sza, CS_exact$V2)

    LKUO[as.Date(Date) == aday,
         CS_low := CS_low_fn( LKUO[as.Date(Date) == aday, SZA] )]

    LKUO[as.Date(Date) == aday,
         CS_exact := CS_exact_fn( LKUO[as.Date(Date) == aday, SZA] )]

    ## Apply sun - earth distance

    LKUO[as.Date(Date) == aday, CS_exact := CS_exact / sun_dist^2 ]
    LKUO[as.Date(Date) == aday, CS_low   := CS_low   / sun_dist^2 ]

names(LKUO)

ggplot(LKUO[as.Date(Date) == aday]) +
           geom_line(aes(Date, CS_low)) +
           geom_line(aes(Date, CS_exact)) +
           geom_line(aes(Date, )) +



    stop()
}








#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (difftime(tac, tic, units = "sec") > 30) {
    system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
    system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'FINISHED'"))
}
