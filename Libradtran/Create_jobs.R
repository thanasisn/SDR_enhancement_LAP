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
Script.Name <- "~/MANUSCRIPTS/02_enhancement/Libradtran/Create_jobs.R"
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



## __ Document options ---------------------------------------------------------
knitr::opts_chunk$set(comment    = ""       )
# knitr::opts_chunk$set(dev        = c("pdf", "png"))
knitr::opts_chunk$set(dev        = "png"    )
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
knitr::opts_chunk$set(cache      =  FALSE   )  ## !! breaks calculations
knitr::opts_chunk$set(fig.pos    = '!h'     )

## __  Set environment ---------------------------------------------------------
require(data.table, quietly = TRUE, warn.conflicts = FALSE)

library("RlibRadtran")
library("janitor")

## read Climatology
AER <- fread("Thessaloniki_overall.txt", skip = 6, fill = TRUE)

AER <- clean_names(AER)


## create month index
AER$Month <- match(tolower(AER$Month), tolower(month.abb))

## keep only month data
AER <- AER[!is.na(Month)]


AER$pw_avg_mm <- AER$pw_avg_cm * 10

make.names(names(AER), unique = TRUE, allow_ = TRUE)


repo <- "~/MANUSCRIPTS/02_enhancement/Libradtran/todo.Rds"


if (file.exists(repo)) {
    repo_runs <- readRDS(repo)
    if (nrow(repo_runs) > 0) {
        cat(paste("some job to redo"))
        meas_data <- repo_runs
    }
} else {

    atmosphere_file   <- c("afglms.dat", "afglmw.dat")
    source_solar      <- "kurudz_0.1nm"
    albedo            <- 0.2
    pressure          <- 1013
    SZA               <- unique(seq(0,90,5))
    mol_modify_O3     <- 300                  # DU
    mol_modify_H2O    <- AER$pw_avg_mm        ## this is set with onother value
    number_of_streams <- 8

    ## outer join ?
    mol_modify_H2O    <- AER$pw_avg_mm        ## this is set with onother value



    expand.grid(
        atmosphere_file   = atmosphere_file,
        source_solar      = source_solar,
        albedo            = albedo,
        pressure          = pressure,
        sza               = SZA,
        mol_modify_O3     = mol_modify_O3,
        number_of_streams = number_of_streams,
        mol_modify_H2O    = mol_modify_H2O
    )




    meas_data[, source_solar      := "kurudz_0.1nm" ]
    meas_data[, number_of_streams := "32" ]
    meas_data[, rte_solver        := "sdisort" ]
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
