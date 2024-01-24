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
repo_dir     <- "~/MANUSCRIPTS/02_enhancement/Libradtran/io_repo/"

## empty runs
run_list_rds <- "~/MANUSCRIPTS/02_enhancement/Libradtran/run.Rds"

## all runs
model_cs     <- "~/MANUSCRIPTS/02_enhancement/Libradtran/Model_CS.Rds"


## Based on raw data
DATA <- data.table(readRDS("../data/CE_ID_Input.Rds"))

## Fill with CS approximation model
CS <- data.table(readRDS("./Model_CS.Rds"))

## Keep only relevant
LKUO <- DATA[, .(Date, SZA, sun_dist, wattGLB)]
rm(DATA)

table(CS$type)

warning("check for low data")
##TODO check for low data!!
table(CS$type, CS$month)

##TODO check for low data!!
table(CS$type, CS$sza)



LKUO[, atmosphere_file := date_to_standard_atmosphere_file(Date)]



stop()

##  Table for rendering document
#'
#'  # Plots
#'
#+ echo=F, include=T, results="asis"

cc <- 0


# test
# for (aday in sample(unique(as.Date(LKUO[month(Date)==7, Date])))) {
# for (aday in sample(unique(as.Date(LKUO$Date)))) {

for (aday in (unique(as.Date(LKUO$Date)))) {

    cc <- cc + 1
    # LKUO[as.Date(Date) == aday]

    theday  <- as.POSIXct(as.Date(aday, "1970-01-01"))

    cat(paste(theday,"\n"))

    atmfile <- date_to_standard_atmosphere_file(as.POSIXct(as.Date(aday, "1970-01-01")))


    ## choose data and interpolate global
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

    LKUO[as.Date(Date) == aday,
         CS_2_low := CS_2_low_fn(LKUO[as.Date(Date) == aday, SZA])]

    LKUO[as.Date(Date) == aday,
         CS_low   := CS_low_fn(  LKUO[as.Date(Date) == aday, SZA])]

    LKUO[as.Date(Date) == aday,
         CS_exact := CS_exact_fn(LKUO[as.Date(Date) == aday, SZA])]


    ## Apply sun - earth distance correction
    LKUO[as.Date(Date) == aday, CS_exact := CS_exact / sun_dist^2]
    LKUO[as.Date(Date) == aday, CS_low   := CS_low   / sun_dist^2]
    LKUO[as.Date(Date) == aday, CS_2_low := CS_2_low / sun_dist^2]


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
saveRDS(LKUO, "/CS_LoolUpTable.Rds")






#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (difftime(tac, tic, units = "sec") > 30) {
    system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
    system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'FINISHED'"))
}


3

data <- data.table(A = c("A","A","A","B","B","B","C","C","C"),
                   B = c(1,1,1,1,1,1,2,2,2),
                   C = rep(c(0.15, 0.22, 0.3),3))



look <- data.table(A = c("A","A","A","B","B","B","C","i","C"),
                   B = c(1,1,1,1,1,1,2,2,2),
                   C = rep(c(0.1, 0.2, 0.3),3),
                   D = c(10, 20, 30, 11,22,33,12,24,36))

rep("A","B","c",3)

f <- approxfun(c(0.1, 0.2, 0.3), c(10, 20, 30) )
f(c(0.15, 0.22, 0.3))



