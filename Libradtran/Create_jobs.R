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
AER <- fread("Thessaloniki_overall.txt", skip = 6, fill = TRUE, na.strings = "-100")

## remove NA
AER <- remove_empty(AER, which = "cols")
AER <- data.frame(AER)
AER[AER == -100] <- NA
AER <- remove_empty(AER, which = "cols")

AER <- clean_names(AER)
AER <- data.table(AER)



## create month index
AER$month <- match(tolower(AER$month), tolower(month.abb))

## keep only month data
AER <- AER[!is.na(month), ]


AER$pw_avg_mm <- AER$pw_avg_cm * 10


## create a and b
#'
#' $α = \ln(τ_1/τ_2)/\ln(λ_2/λ_1)$
#'
#' $β = τ_1 λ_1 ^α = τ_2 λ_2 ^α$
#'

## select Aod

## use angstrom exponent
AER$alpha500   <- AER$ae_440nm_870nm_std

## choose an AOD values
AER$tau500     <- AER$aod_500nm_avg
## choose an best clear case AOD value
AER$tau500_cs  <- AER$aod_500nm_avg - 1 * AER$aod_500_std

## calculate beta
AER$beta500    <- AER$tau500    * ( 500 / 1000 )^AER$alpha500
AER$beta500_cs <- AER$tau500_cs * ( 500 / 1000 )^AER$alpha500


COMB <- rbind(
    AER[, .(month, pw_avg_mm, a = alpha500, b = beta500,    type = "Exact B")],
    AER[, .(month, pw_avg_mm, a = alpha500, b = beta500_cs, type = "Low B"  )]
)



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
    SZA               <- unique(seq(0,90,5))

    ## outer join ?


    ## create basic options
    BASE <- expand.grid(
        atmosphere_file        = atmosphere_file,
        source_solar           = source_solar,
        albedo                 = 0.2,
        pressure               = 1013,
        sza                    = SZA,
        mol_modify_O3          = 300,           # DU
        number_of_streams      = 8,
        aerosol_modify_ssa_set = 0.95,
        aerosol_modify_gg_set  = 0.70,
        mol_abs_param          = "LOWTRAN",
        rte_solver             = "disort",
        pseudospherical        = "pseudospherical",
        wavelength_min         = 280,
        wavelength_max         = 2500
    )

    ALLRUNS <- data.table(dplyr::cross_join(BASE, COMB))


    ## create hash id
    ALLRUNS$ID <- apply(ALLRUNS[, !c("type") ], 1, function(x) digest::digest(x, "md5"))


    # ## clean duplicate columns
    # # create a vector with the checksum for each column (and keep the column names as row names)
    # col.checksums <- sapply(meas_data_ANT, function(x) digest::digest(x, "md5"), USE.NAMES = T)
    # # make a data table with one row per column name and hash value
    # dup.cols      <- data.table(col.name = names(col.checksums), hash.value = col.checksums)
    # # self join using the hash values and filter out all column name pairs that were joined to themselves
    # dupscol <- dup.cols[dup.cols,, on = "hash.value", allow.cartesian=T][col.name != i.col.name,]
    # # remove them
    # remcol <- unique(grep("SSA_" , as.vector(dupscol$col.name), value = T))
    # meas_data_ANT[ , (remcol) := NULL]


    #TODO
    #
    # check if runs have been done
    #
    # list all run todo

    TODO <- ALLRUNS

}



## create list to run








#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (difftime(tac, tic, units = "sec") > 30) {
    system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
    system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'FINISHED'"))
}
