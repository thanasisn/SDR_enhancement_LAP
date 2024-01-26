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


#'
#' This creates only global and it is intended process to validate the process
#' of building the look up table with the other lookuptable script.
#'
#' - This is slow.
#' - This creates only glo.
#'
#+ echo=F, include=T
rm(list = (ls()[ls() != ""]))
Script.Name <- "./build_lookuptable_iteration.R"
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
# library(RlibRadtran)
library(ggplot2)

source("~/FUNCTIONS/R/data.R")
source("~/Aerosols/RlibRadtran/R/date_to_standard_atmosphere_file.R")

## empty runs from create_jobs
run_list_rds <- "./run.Rds"

## all runs are stored here
model_cs     <- "./Model_CS.Rds"


##  Get raw data we want to create reference for  ------------------------------
DATA <- data.table(readRDS("~/MANUSCRIPTS/02_enhancement/data/CE_ID_Input.Rds"))

## Fill with CS approximation model
CS <- data.table(readRDS("./Model_CS.Rds"))
CS[, SZA := sza]

## Create global irradiance W/m^2
CS[, glo := (edir + edn) / 1000 ]


CS <- janitor::remove_constant(CS)

## Keep only relevant input
LKUO <- DATA[, .(Date, SZA, sun_dist, wattGLB)]

rm(DATA); dummy <- gc()



table(CS$type)

##TODO check for low data!!
table(CS$type, CS$month)
table(CS$type, CS$month) == min(table(CS$type, CS$month))

##TODO check for low data!!
table(CS$type, CS$sza)
table(CS$type, CS$sza) == min(table(CS$type, CS$sza))


## Fill data with CS variables
LKUO[, atmosphere_file := date_to_standard_atmosphere_file(Date)]
LKUO[, month           := month(Date)]




##  Run iteratively an fill source data with reference  ------------------------
#'
#'  #  Fill data and plot
#'
#+ echo=F, include=T, results="asis"
cc <- 0

# ## tests
# for (aday in sample(unique(as.Date(LKUO[month(Date)==7, Date])))) {
# for (aday in sample(unique(as.Date(LKUO$Date)))) {

for (aday in (unique(as.Date(LKUO$Date)))) {
    cc     <- cc + 1
    theday <- as.POSIXct(as.Date(aday, "1970-01-01"))

    cat(paste(theday,"\n"))

    ## get atmosphere file name for this day
    atmfile <- date_to_standard_atmosphere_file(as.POSIXct(as.Date(aday, "1970-01-01")))


    ## choose data and interpolate global for different cases of runs
    CS_exact <- CS[atmosphere_file == atmfile &
                       month == month(theday) &
                       type == "Exact B", .(sza, (edir + edn) / 1000 )]
    CS_low   <- CS[atmosphere_file == atmfile &
                       month == month(theday) &
                       type == "Low B",   .(sza, (edir + edn) / 1000 )]
    CS_2_low <- CS[atmosphere_file == atmfile &
                       month == month(theday) &
                       type == "Low 2 B", .(sza, (edir + edn) / 1000 )]

    ## Interpolate to SZA
    CS_2_low_fn <- approxfun(CS_2_low$sza, CS_2_low$V2)
    CS_low_fn   <- approxfun(  CS_low$sza,   CS_low$V2)
    CS_exact_fn <- approxfun(CS_exact$sza, CS_exact$V2)

    ## Fill data to the lookup table
    LKUO[as.Date(Date) == aday,
         CS_2_low := CS_2_low_fn(LKUO[as.Date(Date) == aday, SZA])]
    LKUO[as.Date(Date) == aday,
         CS_low   := CS_low_fn(  LKUO[as.Date(Date) == aday, SZA])]
    LKUO[as.Date(Date) == aday,
         CS_exact := CS_exact_fn(LKUO[as.Date(Date) == aday, SZA])]


    # ## Apply sun - earth distance correction
    # LKUO[as.Date(Date) == aday, CS_exact := CS_exact / sun_dist^2]
    # LKUO[as.Date(Date) == aday, CS_low   := CS_low   / sun_dist^2]
    # LKUO[as.Date(Date) == aday, CS_2_low := CS_2_low / sun_dist^2]


    ## Plot every nth day just for check
    if ( cc %% 60 == 0 ) {
        suppressWarnings({

            p <- ggplot(LKUO[as.Date(Date) == aday], aes(x = Date)) +
                geom_point(aes(y = wattGLB ), col = "green", size = 0.3 ) +
                geom_line( aes(y = CS_low  ), col = "red")     +
                geom_line( aes(y = CS_2_low), col = "cyan")    +
                geom_line( aes(y = CS_exact), col = "magenta") +
                labs( title = theday) +
                theme_bw()
            print(p)
            # plotly::ggplotly(p)

            p <- ggplot(LKUO[as.Date(Date) == aday], aes(x = SZA)) +
                geom_point(aes(y = wattGLB ), col = "green", size = 0.3 ) +
                geom_line( aes(y = CS_low  ), col = "red")     +
                geom_line( aes(y = CS_2_low), col = "cyan")    +
                geom_line( aes(y = CS_exact), col = "magenta") +
                labs( title = theday) +
                theme_bw()
            print(p)
            # plotly::ggplotly(p)
        })
    }
}

LKUO[, wattGLB := NULL ]
saveRDS(LKUO, sub(".R", ".Rds", basename(Script.Name)))




#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (difftime(tac, tic, units = "sec") > 30) {
    system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
    system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'FINISHED'"))
}
