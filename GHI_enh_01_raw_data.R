
#  Prepare raw data for use in the paper ---------------------------------------
#
#  This collects raw data and prepare it mainly SDR analysis.
#  It uses the data output of one of ClearSky algorithms.
#  It caches the raw data in order to reuse it in next states of analyses.
#  Creates:
#    - Raw_Input.Rds
#

require(data.table)
require(zoo)
source("~/CODE/FUNCTIONS/R/trig_deg.R")
source("~/CODE/FUNCTIONS/R/data.R")
source("~/CODE/R_myRtools/myRtools/R/write_.R")
source("./GHI_enh_00_variables.R")

#  Run data construction  ------------------------------------------------------

D_14_2 <- FALSE
D_14   <- FALSE
D_13   <- FALSE

D_14_2 <- TRUE
# D_14   <- TRUE
# D_13   <- TRUE

TEST <- TRUE
TEST <- FALSE

## _ Select ClearSky algorithm  ------------------------------------------------

## new new implementation with corrected limits
if (D_14_2) {
    inpatern    <- "Clear_sky_id_Reno-Hansen_apply_v14_2_[0-9]{4}.Rds"
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
havetorun <- !file.exists(raw_input_data) |
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
        temp$Clearness_Kt       <- NULL
        temp$DIFF_strict        <- NULL
        temp$DIF_HOR            <- NULL
        temp$DIR_strict         <- NULL
        temp$DiffuseFraction_Kd <- NULL
        temp$DiffuseFraction_kd <- NULL
        temp$Direct_max         <- NULL
        temp$GLBINC_SD_wpsm     <- NULL
        temp$GLBINC_strict      <- NULL
        temp$GLBINC_wpsm        <- NULL
        temp$Global_max         <- NULL
        temp$HOR_strict         <- NULL
        temp$Pressure           <- NULL
        temp$Pressure_source    <- NULL
        temp$RaylDIFF           <- NULL
        temp$chp1TempCF         <- NULL
        temp$pressure           <- NULL
        temp$wattDIR_tmp_cr     <- NULL
        temp$wattHOR_tmp_cr     <- NULL

        rm.cols.DT(temp, "VIL_*"    , quiet = TRUE)
        rm.cols.DT(temp, "*Clim_lim", quiet = TRUE)

        temp <- unique(temp)
        DATA <- rbind(temp, DATA, fill = TRUE)
        rm(temp)
    } ## FOR END: read input files
    DATA <- unique(DATA)
    gc()

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

    ## _ Keep daylight only  ---------------------------------------------------
    DATA <- DATA[Elevat >= 0, ]

    ## _ Exclude low Sun elevation  --------------------------------------------
    DATA[Elevat < MIN_ELEVA, wattDIR     := NA ]
    DATA[Elevat < MIN_ELEVA, wattDIR_sds := NA ]
    DATA[Elevat < MIN_ELEVA, wattGLB     := NA ]
    DATA[Elevat < MIN_ELEVA, wattGLB_sds := NA ]
    DATA[Elevat < MIN_ELEVA, wattHOR     := NA ]
    DATA[Elevat < MIN_ELEVA, wattHOR_sds := NA ]

    ## show included data
    ## FIXME there is some error in Azimuth/Elevation angles see plot!!
    # plot(DATA[ !is.na(wattGLB) ,Elevat, Azimuth])

    ##_  Bais paper obstacle filter  -------------------------------------------
    cat("\n  Keep Azimuth and Elevation according to Bais paper!\n")
    DATA[Azimuth > 35 & Azimuth < 120 & Elevat < 10, wattDIR     := NA ]
    DATA[Azimuth > 35 & Azimuth < 120 & Elevat < 10, wattDIR_sds := NA ]
    DATA[Azimuth > 35 & Azimuth < 120 & Elevat < 10, wattGLB     := NA ]
    DATA[Azimuth > 35 & Azimuth < 120 & Elevat < 10, wattGLB_sds := NA ]
    DATA[Azimuth > 35 & Azimuth < 120 & Elevat < 10, wattHOR     := NA ]
    DATA[Azimuth > 35 & Azimuth < 120 & Elevat < 10, wattHOR_sds := NA ]

    ## show included data
    # plot(DATA[ !is.na(wattGLB) ,Elevat, Azimuth])

    ## Filter min elevation
    # DATA <- DATA[Elevat >= MIN_ELEVA, ]


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
    if (D_14 | D_14_2) {
        DATA[QCF_DIR == FALSE, wattDIR := NA]
        DATA[QCF_DIR == FALSE, wattHOR := NA]
        DATA[QCF_GLB == FALSE, wattGLB := NA]
    }

    ## _ Count daylight length  ------------------------------------------------
    DATA[, DayLength := .N, by = Day]

    ## _ DROP MISSING RECORDS!! ------------------------------------------------
    DATA <- DATA[ !(is.na(wattDIR) & is.na(wattGLB)) ]

    ## _ Info for TIS time span source used  -----------------------------------
    TSI_info <- DATA[, .(Start = min(Date),
                         End   = max(Date)), by = TSI_Source]
    dir.create("./figures/", showWarnings = FALSE)
    write_dat(object = TSI_info,
              file   = "./figures/tbl_tsi_info.dat",
              clean  = TRUE)
    rm(TSI_info)


    #  Data preparation  -------------------------------------------------------

    ## - Create Clearness Index (BB may not filled yet) ------------------------
    DATA[, ClearnessIndex_kt := wattGLB / (cosde(SZA) * TSIextEARTH_comb)]

    ## _ Move measurements to mean earth distance  -----------------------------
    cat("\n  Sun Earth distance correction\n")
    DATA[, wattDIR_1au := wattDIR * (sun_dist ^ 2)]
    DATA[, wattGLB_1au := wattGLB * (sun_dist ^ 2)]
    DATA[, wattHOR_1au := wattHOR * (sun_dist ^ 2)]

    ## fix noon just in case
    DATA[Azimuth <= 180 , preNoon := TRUE ]
    DATA[Azimuth >  180 , preNoon := FALSE]

    ## _ DROP SOME DATA  -------------------------------------------------------
    DATA[, CS_ref_HOR         := NULL]
    DATA[, Elevat             := NULL]
    DATA[, Glo_max_ref        := NULL]

    rm.cols.DT(DATA, "QCv9*", quiet = TRUE)
    rm.cols.DT(DATA, "QCF_*", quiet = TRUE)


    #  GLB Representation filtering  -------------------------------------------
    #
    #  Remove days with too few data, as they can not be representative of a
    #  normal day.
    #
    temp <- DATA[!is.na(wattGLB),
                 .(Day_N = .N,
                   DayLim = max(DayLength) * All_daily_ratio_lim),
                 by = Day]

    Days_with_all_glb_data      <- temp[ , .N ]
    Days_with_filtered_glb_data <- temp[ Day_N >= DayLim, .N ]
    cat("\n  Excluded days with less than", All_daily_ratio_lim, "of daylight GLB points:", Days_with_all_glb_data - Days_with_filtered_glb_data, "\n\n")

    all_days_to_keep <- temp[ Day_N >= DayLim, Day ]
    rm(temp)

    ## Keep only good enough days
    all_glb_datapoints     <- DATA[Day %in% all_days_to_keep, .N]
    filterd_glb_datapoints <- DATA[, .N]
    cat("\n  Keeping:", 100 * all_glb_datapoints / filterd_glb_datapoints, "% of ALL data\n\n")

    DATA <- DATA[Day %in% all_days_to_keep ]


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

    ## legacy flags usage
    # DATA_Clear <- DATA_all[ CSflag == 0 ]


    ## _ Enhancement ID  ------------------------------------------------------

    ## __ Variables  -----------------------------------------------------------
    GLB_ench_THRES     <- 0     ## enchantment % relative to HAU
    GLB_diff_THRES     <- 10    ## enchantment absolute diff to HAU
    Clearness_Kt_THRES <- 0.8   ## enchantment threshold
    wattGLB_THRES      <- 20    ## minimum value to consider
    wattDIR_THRES      <- 20    ## minimum value to consider
    min_elevation      <- 10    ## minimum sun elevation to use
    ampl               <- 1.05  ## adjusted HAU amplified threshold
    SZA_BIN            <- 1



    ## __ Create some metrics  -------------------------------------------------
    DATA[ , GLB_diff :=   wattGLB - CS_ref            ]  ## enhancement
    DATA[ , GLB_ench := ( wattGLB - CS_ref ) / CS_ref ]  ## relative enhancement
    DATA[ , GLB_rati :=   wattGLB / CS_ref            ]


    ## __ Enhancement criteria  ------------------------------------------------
    DATA[, Enhancement := FALSE]
    DATA[GLB_ench              > GLB_ench_THRES      &
             ClearnessIndex_kt > Clearness_Kt_THRES  &
             wattGLB           > wattGLB_THRES       &
             GLB_diff          > GLB_diff_THRES,
         Enhancement := TRUE]


    ## __ Group contimioys values  ---------------------------------------------
    DATA[, cnF := cumsum(Enhancement == FALSE)]
    DATA[, cnT := cumsum(Enhancement == TRUE) ]
    ## Init groups logical
    DATA[, G1  := Enhancement]
    DATA[, G0  := Enhancement]

    ## Find groups with one gap
    for (i in 1:nrow(DATA)) {
        p1 <- i - 1
        n1 <- i + 1
        if (p1 > 0 & n1 <= nrow(DATA)) {
            if (DATA$G1[p1] == TRUE  &
                DATA$G1[i]  == FALSE &
                DATA$G1[n1] == TRUE  ) {
                DATA$G1[i]  <- TRUE
            }
        }
    }

    DATA[, Grp1 := rleid(c(NA,diff(cumsum(G1))))]
    DATA[G1 == FALSE, Grp1 := NA]

    DATA[, Grp0 := rleid(c(NA,diff(cumsum(G0))))]
    DATA[G0 == FALSE, Grp0 := NA]





    #  Save raw input data  ----------------------------------------------------
    saveRDS(DATA, file = raw_input_data, compress = "xz")
    cat("\n  Saved raw input data:", raw_input_data, "\n\n")
} else {
    cat(paste("\n Raw input data are ready in ", raw_input_data), "\n")
}

