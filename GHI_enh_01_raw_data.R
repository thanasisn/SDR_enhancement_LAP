#!/usr/bin/env Rscript
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Enhancement of SDR in Thessaloniki "
#' author:
#'   - Natsis Athanasios^[Laboratory of Atmospheric Physics, AUTH, natsisphysicist@gmail.com]
#'   - Alkiviadis Bais^[Laboratory of Atmospheric Physics, AUTH]
#' abstract:
#'   "Study of GHI enchantment."
#'
#' documentclass:  article
#' classoption:    a4paper,oneside
#' fontsize:       10pt
#' geometry:       "left=0.5in,right=0.5in,top=0.5in,bottom=0.5in"
#' link-citations: yes
#' colorlinks:     yes
#'
#' header-includes:
#' - \usepackage{caption}
#' - \usepackage{placeins}
#' - \captionsetup{font=small}
#'
#' output:
#'   bookdown::pdf_document2:
#'     number_sections: no
#'     fig_caption:     no
#'     keep_tex:        yes
#'     latex_engine:    xelatex
#'     toc:             yes
#'     toc_depth:       4
#'     fig_width:       7
#'     fig_height:      4.5
#'   html_document:
#'     toc:             true
#'     keep_md:         yes
#'     fig_width:       7
#'     fig_height:      4.5
#'
#' date: "`r format(Sys.time(), '%F')`"
#'
#' ---


#'  ## Prepare raw data for use in the paper
#'
#'  This collects raw data and prepare it mainly SDR analysis.
#'  It uses the data output of one of the ClearSky algorithms.
#'  It caches the raw data in order to reuse it in next states of analyses.
#'  Creates:
#'    - Raw_Input.Rds
#'

#+ echo=F, include=T

## __ Document options ---------------------------------------------------------
knitr::opts_chunk$set(comment    = ""       )
knitr::opts_chunk$set(echo       = FALSE    )
knitr::opts_chunk$set(dev        = c("pdf", "png"))
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
knitr::opts_chunk$set(cache      =  FALSE   )  ## !! breaks calculations
knitr::opts_chunk$set(fig.pos    = '!h'     )

## __ Set environment ----------------------------------------------------------
Sys.setenv(TZ = "UTC")
Script.Name <- "./GHI_enh_01_raw_data.R"
tic <- Sys.time()

if (!interactive()) {
    pdf( file = paste0("./runtime/", basename(sub("\\.R$",".pdf", Script.Name))))
}

library(data.table, quietly = TRUE, warn.conflicts = FALSE)
require(zoo       , quietly = TRUE, warn.conflicts = FALSE)
library(pander    , quietly = TRUE, warn.conflicts = FALSE)
library(lubridate , quietly = TRUE, warn.conflicts = FALSE)

panderOptions("table.alignment.default", "right")
panderOptions("table.split.table",        120   )

source("~/CODE/FUNCTIONS/R/trig_deg.R")
source("~/CODE/FUNCTIONS/R/data.R")
source("~/CODE/R_myRtools/myRtools/R/write_.R")
source("./GHI_enh_00_variables.R")

#  Run data construction  ------------------------------------------------------

D_15   <- FALSE
D_14_2 <- FALSE
D_14   <- FALSE
D_13   <- FALSE

D_15   <- TRUE
# D_14_2 <- TRUE
# D_14   <- TRUE
# D_13   <- TRUE

TEST <- TRUE
TEST <- FALSE

## _ Select ClearSky algorithm  ------------------------------------------------

## new new implementation with corrected limits
if (D_14_2) {
    inpatern    <- "Clear_sky_id_Reno-Hansen_apply_v14_2_[0-9]{4}.Rds"
}

if (D_15) {
    inpatern    <- "Clear_sky_id_Reno-Hansen_apply_v15_[0-9]{4}.Rds"
}


## new implementation with corrected limits
if (D_14) {
    inpatern    <- "Clear_sky_id_Reno-Hansen_apply_v14_[0-9]{4}.Rds"
}

## old implementation with corrected limits
if (D_13) {
    inpatern    <- "Clear_Sky_[0-9]{4}.Rds"
}


## create local folders
dir.create(dirname(raw_input_data), showWarnings = FALSE)
dir.create("./figures",          showWarnings = FALSE)
dir.create("./images",           showWarnings = FALSE)
dir.create("./runtime",          showWarnings = FALSE)


## _ Check if we need to run data export  --------------------------------------
havetorun <- !file.exists(raw_input_data)                              |
    file.mtime(variables_fl)              > file.mtime(raw_input_data) |
    file.mtime("./GHI_enh_01_raw_data.R") > file.mtime(raw_input_data)

if (havetorun) {
    cat(paste("\n !! Create raw input data ->", raw_input_data), "\n")

    ## _ Get data from Clear sky id data  --------------------------------------
    input_files <- list.files(path       = CLEARdir,
                              pattern    = inpatern,
                              full.names = T )
    input_files <- grep("_stats_", input_files, value = TRUE, invert = TRUE)

    if (TEST == TRUE) {
        warning("\n\nTEST MODE IS ACTIVE!!\n")
        input_files    <- sample(input_files, 2)
        raw_input_data <- sub("\\.Rds", "_test.Rds", raw_input_data)
    }

    cat(paste("\n    Will read", length(input_files), "input files\n"))

    cat(paste("\n    Load data from Clear Sky process from original\n\n"))
    DATA <- data.table()
    for (af in input_files) {
        cat("READING:", af, "\n")
        temp <- readRDS(af)
        ## drop some data
        temp$CHP1temp           <- NULL
        temp$CHP1tempSD         <- NULL
        temp$CHP1tempUNC        <- NULL
        temp$CS_ref             <- NULL
        temp$CS_ref_HOR         <- NULL
        temp$Clearness_Kt       <- NULL
        temp$ClrSW              <- NULL
        temp$ClrSW_ref2         <- NULL
        temp$DIFF_strict        <- NULL
        temp$DIF_HOR            <- NULL
        temp$DIR_strict         <- NULL
        temp$DiffuseFraction_Kd <- NULL
        temp$DiffuseFraction_kd <- NULL
        temp$Direct_max         <- NULL
        temp$GLBINC_SD_wpsm     <- NULL
        temp$GLBINC_strict      <- NULL
        temp$GLBINC_wpsm        <- NULL
        temp$Glo_max_ref        <- NULL
        temp$Global_max         <- NULL
        temp$HOR_strict         <- NULL
        temp$Pressure           <- NULL
        temp$Pressure_source    <- NULL
        temp$RaylDIFF           <- NULL
        temp$chp1TempCF         <- NULL
        temp$pressure           <- NULL
        temp$wattDIR_tmp_cr     <- NULL
        temp$wattHOR_tmp_cr     <- NULL
        temp$ClearnessIndex_kt  <- NULL

        rm.cols.DT(temp, "VIL_*"    , quiet = TRUE)
        rm.cols.DT(temp, "*Clim_lim", quiet = TRUE)

        temp <- unique(temp)
        DATA <- rbind(temp, DATA, fill = TRUE)
        rm(temp)
    } ## FOR END: read input files
    DATA  <- unique(DATA)
    dummy <- gc()



    ## TODO warn duplicate dates
    if (sum(duplicated(DATA$Date)) > 0) {
        warning("There are duplicate dates in the data")
    }

    ## There are some duplicates introduced at some point!!
    test <- DATA[duplicated(DATA$Date) | duplicated(DATA$Date, fromLast = TRUE)]
    stopifnot( nrow(test) < 1000 )

    ## Workaround for duplicates
    test_vec <- DATA[is.na(wattGLB) &
                         (duplicated(DATA$Date) | duplicated(DATA$Date, fromLast = TRUE)),
                     which = TRUE]

    ## Drop some data
    DATA <- DATA[!test_vec]

    ## retest
    test <- DATA[duplicated(DATA$Date) | duplicated(DATA$Date, fromLast = TRUE)]
    cat("\n  There are ", nrow(test), " duplicate dates remaining!\n")

    ## FIXME do we still need this?
    ## this is used by old scripts
    setorder(DATA, Date)

    ## _ Skip data ranges for CM-21  -------------------------------------------
    cat("\n  Extra SKIP of CM-21 data!\n")
    for (as in nrow(SKIP_cm21)) {
        skip <- SKIP_cm21[as,]
        DATA[ Date >= skip$From & Date <= skip$Until, wattGLB    := NA ]
        DATA[ Date >= skip$From & Date <= skip$Until, wattGLB_SD := NA ]
    }


    #   Select data for this project  ------------------------------------------

    ## _ Set date range to use  ------------------------------------------------
    DATA <- DATA[Date < LAST_DAY ]
    DATA <- DATA[Date > FIRST_DAY]

    ## _ Keep near daylight only  ----------------------------------------------
    DATA <- DATA[Elevat >= -10, ]

    ## _ Keep data characterized as 'good' by Radiation Quality control v13 ----
    cat("\n  Select quality data only\n\n")
    if (D_13) {
        keepQF <- c("good",
                    "Possible Direct Obstruction (23)",
                    "Biology Building (22)")
        DATA[!QCF_DIR %in% keepQF, wattDIR := NA]
        DATA[!QCF_DIR %in% keepQF, wattHOR := NA]
        DATA[!QCF_GLB %in% keepQF, wattGLB := NA]
    }

    ## _ Keep data characterized as 'TRUE' by Radiation Quality control v14 ----
    if (D_14 | D_14_2 | D_15) {

        cat("\nDROP DIRECT DATA POINTS:", DATA[QCF_DIR == FALSE & !is.na(wattDIR), .N], "!!\n\n")
        cat("\nDROP GLOBAL DATA POINTS:", DATA[QCF_GLB == FALSE & !is.na(wattGLB), .N], "!!\n\n")

        DATA[QCF_DIR == FALSE, wattDIR           := NA]
        DATA[QCF_DIR == FALSE, wattHOR           := NA]
        DATA[QCF_GLB == FALSE, wattGLB           := NA]
        DATA[QCF_GLB == FALSE, ClearnessIndex_kt := NA]
    }

    ## _ Zero any negative radiation  ------------------------------------------
    DATA[wattDIR < 0, wattDIR           := 0]
    DATA[wattGLB < 0, wattGLB           := 0]
    DATA[wattGLB < 0, ClearnessIndex_kt := 0]
    DATA[wattHOR < 0, wattHOR           := 0]

    ## _ Drop some data  -------------------------------------------------------
    rm.cols.DT(DATA, "QCv9*", quiet = TRUE)
    rm.cols.DT(DATA, "QCF_*", quiet = TRUE)


    ## _ Data representation  --------------------------------------------------

    ## CastillejoCuberos2020

    ## !! valid data: 45min /h and 5h / day  !!

    ## For global
    DATA[, floor_date := floor_date(DATA$Date, "1 hour")]
    DATA[, BAD_h      := sum(!is.na(wattGLB)) < 45,
         by = floor_date]

    table(DATA[, sum(!is.na(wattGLB)),
               by = floor_date]$V1)

    hours <- DATA[, .(Good = sum(!is.na(wattGLB)) >= 45,
                      N    = sum(!is.na(wattGLB))),
                  by = .(Day, floor_date)]
    days  <- hours[, .(GoodH_N = sum(Good)),
                   by = Day]

    table(days$GoodH_N)
    table(hours$N)

    hist(days$GoodH_N, main = "Valid hour per day")
    hist(hours$N,      main = "Valid data per hour")

    ## _ Keep only days with good global representation  -----------------------
    ## Keep days with at least 5 acceptable hours
    DATA[!Day %in% days[GoodH_N > 5, Day], wattGLB    := NA]
    DATA[!Day %in% days[GoodH_N > 5, Day], wattGLB_SD := NA]
    rm(days, hours)


    ## add all the minutes
    test <- readRDS("~/DATA/Broad_Band/Date_SZA_Azimuth.Rds")
    add  <- test[!test$Date %in% DATA$Date]
    DATA <- rbind(DATA, add, fill = T)
    rm(test, add)

    ## check all days
    missing_days <- length(seq.Date(min(as.Date(DATA$Date)), max(as.Date(DATA$Date)), by = "day")) - DATA[, length(unique(as.Date(Date)))]
    stopifnot(missing_days == 0)


    ##_  Count daylight length  ------------------------------------------------
    ## Drop all night data
    DATA <- DATA[Elevat > 0]
    DATA[, Day := as.Date(Date)]
    DATA[, DayLength := .N, by = Day]

    ## _ Use only data above the biology building  -----------------------------
    DATA <- DATA[Elevat > BIO_ELEVA]


    ## _ DROP MISSING RECORDS!! ------------------------------------------------
    # DATA <- DATA[ !(is.na(wattDIR) & is.na(wattGLB)) ]

    ## _ Info for TIS time span source used  -----------------------------------
    TSI_info <- DATA[, .(Start = min(Date),
                         End   = max(Date)), by = TSI_Source]
    dir.create("./figures/", showWarnings = FALSE)
    write_dat(object = TSI_info,
              file   = "./figures/tbl_tsi_info.dat",
              clean  = TRUE)
    rm(TSI_info)


    #  Data preparation  -------------------------------------------------------

    ## _ Create Clearness Index (BB may not filled yet) ------------------------
    DATA[, ClearnessIndex_kt := NA]
    DATA[, ETH := cosde(SZA) * TSIextEARTH_comb ]  ## TSI at ground
    DATA[, ClearnessIndex_kt := wattGLB / ETH ]

    # ## _ Move measurements to mean earth distance  -----------------------------
    # cat("\n  Sun Earth distance correction\n")
    # DATA[, wattDIR_1au := wattDIR * (sun_dist ^ 2)]
    # DATA[, wattGLB_1au := wattGLB * (sun_dist ^ 2)]
    # DATA[, wattHOR_1au := wattHOR * (sun_dist ^ 2)]

    ## fix noon just in case
    DATA[Azimuth <= 180 , preNoon := TRUE ]
    DATA[Azimuth >  180 , preNoon := FALSE]

    ## _ DROP SOME DATA  -------------------------------------------------------
    # DATA[, CS_ref_HOR         := NULL]
    DATA[, Elevat             := NULL]
    # DATA[, Glo_max_ref        := NULL]


#    #  GLB Representation filtering  -------------------------------------------
#    #
#    #  Remove days with too few data, as they can not be representative of a
#    #  normal day.
#    #
#    temp <- DATA[!is.na(wattGLB),
#                 .(Day_N = .N,
#                   DayLim = max(DayLength) * All_daily_ratio_lim),
#                 by = Day]
#
#    Days_with_all_glb_data      <- temp[ , .N ]
#    Days_with_filtered_glb_data <- temp[ Day_N >= DayLim, .N ]
#    cat("\n  Excluded days with less than", All_daily_ratio_lim, "of daylight GLB points:", Days_with_all_glb_data - Days_with_filtered_glb_data, "\n\n")
#
#    all_days_to_keep <- temp[ Day_N >= DayLim, Day ]
#    rm(temp)
#
#    ## Keep only good enough days
#    all_glb_datapoints     <- DATA[Day %in% all_days_to_keep, .N]
#    filterd_glb_datapoints <- DATA[, .N]
#    cat("\n  Keeping:", 100 * all_glb_datapoints / filterd_glb_datapoints, "% of ALL data\n\n")
#
#    DATA <- DATA[Day %in% all_days_to_keep ]


    #  Mark data Clear / cloud sky  --------------------------------------------
    #
    #  Method based and adapted from: Identification of Periods of Clear Sky
    #  Irradiance in Time Series of GHI Measurements _Matthew J. Reno and
    #  Clifford W. Hansen.
    #
    #  For this paper we use only flags derived from CM-21.
    #

    ##_ Select only CM-21 flags for trends -------------------------------------
    wecare     <- grep("CSflag_", names(DATA), value = T)
    wecare     <- grep("_11", wecare, invert = T, value = T)

    ##_ Set flag for sky conditions --------------------------------------------
    DATA[rowSums(DATA[, ..wecare ], na.rm = T) == 0, TYPE := "Clear"]
    DATA[rowSums(DATA[, ..wecare ], na.rm = T) != 0, TYPE := "Cloud"]

    ## remove unused columns
    rm.cols.DT(DATA, "CSflag_*", quiet = TRUE)

    #  Save raw input data  ----------------------------------------------------
    saveRDS(DATA, file = raw_input_data, compress = "xz")
    cat("\n  Saved raw input data:", raw_input_data, "\n\n")
} else {
    cat(paste("\n Raw input data are ready in ", raw_input_data), "\n")
}




#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("\n%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (interactive()) {
    system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
    system(paste("notify-send -u normal -t 30000 ", basename(Script.Name), " 'R script ended'"))
}
