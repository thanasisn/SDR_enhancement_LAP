# /* #!/usr/bin/env Rscript */
# /* Copyright (C) 2022 Athanasios Natsis <natsisphysicist@gmail.com> */
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
#' - \usepackage{float}
#' - \usepackage{placeins}
#' - \captionsetup{font=small}
#'
#' output:
#'   bookdown::pdf_document2:
#'     number_sections: no
#'     fig_caption:     yes
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

#+ echo=F, include=T

## __ Document options  --------------------------------------------------------


#+ echo=FALSE, include=TRUE

knitr::opts_chunk$set(comment    = ""       )
knitr::opts_chunk$set(dev        = c("pdf", "png")) ## expected option
# knitr::opts_chunk$set(dev        = "png"    )       ## for too much data
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
knitr::opts_chunk$set(fig.cap    = " - empty caption - " )
knitr::opts_chunk$set(cache      =  FALSE   )  ## !! breaks calculations
knitr::opts_chunk$set(fig.pos    = 'H'    )

#+ echo=FALSE, include=TRUE
## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
# Script.Name <- "./GHI_enh_07_Aerosols.R"
tic <- Sys.time()

## use worktree
setwd("~/MANUSCRIPTS/02_enhancement/")

# if (!interactive()) {
#   pdf( file = paste0("./runtime/", basename(sub("\\.R$",".pdf", Script.Name))))
# }

#+ echo=F, include=T
library(data.table    , quietly = TRUE, warn.conflicts = FALSE)
library(janitor       , quietly = TRUE, warn.conflicts = FALSE)
# library(ggpmisc       , quietly = TRUE, warn.conflicts = FALSE)
library(ggplot2       , quietly = TRUE, warn.conflicts = FALSE)
library(data.table    , quietly = TRUE, warn.conflicts = FALSE)
library(pander        , quietly = TRUE, warn.conflicts = FALSE)
library(lmtest        , quietly = TRUE, warn.conflicts = FALSE)
library(viridis       , quietly = TRUE, warn.conflicts = FALSE)
library(ggpointdensity, quietly = TRUE, warn.conflicts = FALSE)
library(patchwork     , quietly = TRUE, warn.conflicts = FALSE)
library(ggh4x         , quietly = TRUE, warn.conflicts = FALSE)
library(grid          , quietly = TRUE, warn.conflicts = FALSE)
library(latex2exp     , quietly = TRUE, warn.conflicts = FALSE)
library(cowplot       , quietly = TRUE, warn.conflicts = FALSE)

source("./GHI_enh_00_variables.R")
source("./GHI_enh_00_dictionary.R")

##  Load and prepare data  -----------------------------------------------------
AEin1 <- "~/DATA/Aeronet/Thessaloniki_Monthly/20030101_20241231_Thessaloniki.lev20"
AE1   <- fread(AEin1, skip = 6, fill = T, na.strings = "-999")

names(AE1)[names(AE1) == "Month"] <- "Date"

AE1 <- AE1[, lapply(.SD, function(x) replace(x, which(x < -998), NA))]
AE1 <- data.table(remove_constant(AE1))

# AEin2 <- "~/DATA/Aeronet/Thessaloniki_Monthly/20030101_20241231_Thessaloniki.tot_lev20"
# AE2 <- fread(AEin2, skip = 6, fill = T, na.strings = "-999")

AE1[, c("Year", "Month") := tstrsplit(Date, "-")]
AE1[, Year  := as.numeric(Year)]
AE1[, Month := match(Month, toupper(month.abb))]
AE1[, tsy := Year + (Month - 1) / 12]


## Find cross over point  ------------------------------------------------------
setorder(AE1, tsy)
AE1       <- AE1[!is.na(AOD_500nm)]

## mid point of available data
mid       <- ceiling(nrow(AE1) / 2)
# zeropoint <- AE1[mid, tsy]

## time middle of data
zeropoint <- min(AE1$tsy) + (max(AE1$tsy) - min(AE1$tsy)) / 2
zeropoint <- 2005

cat("Cross over point:", zeropoint, "\n")

## AOD trend  ------------------------------------------------------------------

lm_transp_trend <- lm(AE1$AOD_500nm ~ AE1$tsy)

# plot(AE1[, AOD_500nm])

plot(AE1[, AOD_500nm, tsy])
abline(lm_transp_trend)

# plot(AE1[, `NUM_DAYS[AOD_500nm]`, tsy])



## AOD transparency trend  -----------------------------------------------------

lm_transp_trend <- lm(exp(-AE1$AOD_500nm) ~ AE1$tsy)
lm_AOD_trend    <- lm(     AE1$AOD_500nm  ~ AE1$tsy)


# plot(AE1[, exp(-AOD_500nm), tsy])
# abline(lm_transp_trend)


## Calculate offset for zero point  --------------------------------------------
b <- -coef(lm_transp_trend)[2] * zeropoint
t <- -coef(lm_AOD_trend)[2]    * zeropoint



## create a closure of the function

trans_trend <- function(tsy = tsy, a. = coef(lm_transp_trend)[2], b. = b) {
  return(b. + a. * tsy)
}

trans_AOD   <- function(tsy = tsy, a. = coef(lm_AOD_trend)[2], b. = t) {
  return(b. + a. * tsy)
}




write.csv(
  data.frame(year  = 1993:2023,
             trans = trans_trend(1993:2023))
  ,"./figures/transparency_trend.csv",
  row.names = F)



plot(AE1[, exp(-AOD_500nm), tsy],
     ylab = "exp(-AOD_500nm)")
abline(lm_transp_trend)
plot(1993:2024, trans_trend(1993:2024), col = "red")



plot(min(AE1$tsy):max(AE1$tsy), trans_AOD(min(AE1$tsy):max(AE1$tsy)), col = "red")



CS <- readRDS("./data/Model_CS_2.Rds")
CS$hostname <- NULL
CS$ticTime  <- NULL
CS$tacTime  <- NULL
CS$ID       <- NULL

# CS[sza == 17 & type == "Low B.Low W",]

# min(unique(CS$sza))


# LT <- readRDS("./data/lookuptable_datatable.Rds")
#
# LT

# plot(LT$Date, LT$Low_B.Low_2_W.edn / 1000)




AEY <- AE1[, .( Mean500   = mean(AOD_500nm),
                Median500 = median(AOD_500nm),
                meantsy   = mean(tsy)         ,
                .N),
           by = Year]



plot(AE1[, AOD_500nm, tsy])
abline(lm_AOD_trend)
points(AEY$meantsy, AEY$Mean500,   col = "red", pch = 19 )
points(AEY$meantsy, AEY$Median500, col = "green", pch = 19 )
title("Yearly means and median")



## Libradtran for monthly AERONET ---------------

AEM          <- readRDS("./data/Model_CS_trend_yearly.Rds")
AEM$ID       <- NULL
AEM$hostname <- NULL
AEM$ticTime  <- NULL
AEM$tacTime  <- NULL
AEM          <- data.table(remove_constant(AEM))

AEM[!is.na(month), tsy := year + (month - 1)/12 ]
AEM[ is.na(month), tsy := year + 0.5 ]

min(AEM$year)

LKU <- readRDS("~/DATA/SUN/TSI_COMPOSITE.Rds")
LKU <- LKU[year(Date) >= min(AEM$year), ]

LKU_month <- LKU[, .(sun_dist     = mean(sun_dist),
                     tsi_1au_comb = mean(tsi_1au_comb)),
                 by = .(year(Date), month(Date))]

## Nine years of UV aerosol optical depth measurements at {T}hessaloniki, {G}reece
LKU_year <- LKU[, .(sun_dist     = mean(sun_dist),
                    tsi_1au_comb = mean(tsi_1au_comb),
                    month        = NA),
                 by = .(year(Date))]


## apply kuruds and distance

library(pracma)

Kurudz <- read.table("~/LibRadTranG/libRadtran-2.0.5/data/solar_flux/kurudz_0.1nm.dat")
Kurudz <- data.table(Kurudz)

# currently we only on set to run Libradtran
spectrum  <- Kurudz[ V1 >= 280 & V1 <= 2500]
Kurudz_SC <- trapz(x = spectrum$V1, y = spectrum$V2) / 1000

LKU_month[ , TSI_Kurudz_factor := tsi_1au_comb / Kurudz_SC ]
LKU_year[  , TSI_Kurudz_factor := tsi_1au_comb / Kurudz_SC ]


AEM <- rbind(
  merge(AEM[!is.na(month), ], LKU_month, all.x = T),
  merge(AEM[ is.na(month), ], LKU_year,  all.x = T)
)



# AEM <- merge(AEM, LKU_month, all.x = T)
rm(LKU, LKU_month, LKU_year)

# AEM[ is.na(month),]
# AEM[!is.na(month),]

## corrections

## create global
AEM[, glo := (edn + edir) / 1000]

## sun distance
AEM[, glo := glo / sun_dist^2 ]

## kurudz
AEM[, glo := glo * TSI_Kurudz_factor ]

## clean to no reuse them
AEM$TSI_Kurudz_factor <- NULL
AEM$tsi_1au_comb      <- NULL
AEM$sun_dist          <- NULL


#
# asza  <- 55
# aatm  <- "afglms"
# atype <- "Month Exact"
#
# # unique(AEM$typer)
#
#
# sel <- AEM[,  atmosphere_file == aatm & typer == atype]
#
# ylim <- range(AEM[sel, glo])
# ylim[2] <- ylim[2] * 1.05
#
# plot(AEM[sel, glo, tsy],
#      ylim = ylim)
#
# lm1   <- lm(  AEM[sel, tsy, glo])
# amean <- mean(AEM[sel, glo])
#
# title(paste("sza:", asza, "atm:", aatm, "type:", atype))
#
# abline(lm1, col = "red")
#
# ## display trend on graph
# fit <- lm1[[1]]
# units <- "W/m^2"
# legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
#        c(paste("Trend: ",
#              if (fit[2] > 0) "+" else "-",
#              signif(abs(fit[2]), 2) , bquote(.(units)), "/y"),
#          paste("Trend: ",
#                if (fit[2] > 0) "+" else "-",
#                signif(abs(100 * fit[2] / amean), 2) , "%/y")
#          )
# )





## SZA 55 ------------

aatm  <- "afglms"
atype <- "SZA 55"

sel2 <- AEM[, atmosphere_file == aatm & typer == atype]

ylim <- range(AEM[sel2, glo])
ylim[2] <- ylim[2] * 1.05

plot(AEM[sel2, glo, tsy],
     ylim = ylim)

lm_55   <- lm(  AEM[sel2, tsy, glo])
mean_55 <- mean(AEM[sel2, glo])

title(paste("Second part, atm:", aatm, "type:", atype))

abline(lm_55, col = "red")

## display trend on graph
fit <- lm_55[[1]]
units <- "W/m^2"
legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
       c(paste("Trend: ",
               if (fit[2] > 0) "+" else "-",
               signif(abs(fit[2]), 2) , bquote(.(units)), "/y"),
         paste("Trend: ",
               if (fit[2] > 0) "+" else "-",
               signif(abs(100 * fit[2] / mean_55), 2) , "%/y")
       )
)


## Yeary BR SZA 55 ------------
atype <- "Year BR SZA 55"

sel1 <- AEM[, is.na(month) & atmosphere_file == aatm & typer == atype, ]

plot(AEM[sel1, glo, tsy])

zeropointA <- min(AEM[sel1, tsy]) + (max(AEM[sel1, tsy]) - min(AEM[sel1, tsy])) / 2
zeropointA <- 2005

lm_BR_55   <- lm(  AEM[sel1, tsy, glo])
mean_BR_55 <- mean(AEM[sel1, glo])

title(paste("First part, atm:", aatm, "type:", atype))

abline(lm_BR_55, col = "red")

## display trend on graph
fit <- lm_BR_55[[1]]
units <- "W/m^2"
legend("bottomright", lty = 1, bty = "n", lwd = 2, cex = 1,
       c(paste("Trend: ",
               if (fit[2] > 0) "+" else "-",
               signif(abs(fit[2]), 2) , bquote(.(units)), "/y"),
         paste("Trend: ",
               if (fit[2] > 0) "+" else "-",
               signif(abs(100 * fit[2] / mean_BR_55), 2) , "%/y")
       )
)



plot(AEM[sel1 | sel2, glo, tsy])
abline(lm_BR_55, col = "red")
abline(lm_55, col = "red")
title("Both")

test <- AEM[sel1 | sel2,]

global_sza_55 <- function(tsy   = tsy,
                           a.    =  coef(lm_55)[2] ,
                           b.    = -coef(lm_55)[2] * zeropoint,
                           mean. = mean_55) {
  return( (b. + a. * tsy) / mean. )
}

tsy <- 1993:2024

global_sza_55 <- function(tsy) {
  tsyA <- tsy[tsy <  AEM[sel2, min(tsy)]]
  tsyB <- tsy[tsy >= AEM[sel2, min(tsy)]]

  res <- rbind(
    cbind(tsyA, (-coef(lm_BR_55)[2] * zeropointA + tsyA * coef(lm_BR_55)[2]) / mean_BR_55 )  ,
    cbind(tsyB, (-coef(lm_55)[2]    * zeropoint  + tsyB * coef(lm_55)[2])    / mean_55 )
  )

  ## return resulst with the same order
  res[match(tsy, res[,1]),][,2]
}





#
#
# ## SZA min ------------
#
# aatm  <- "afglms"
# atype <- "SZA min"
#
# sel2 <- AEM[, atmosphere_file == aatm & typer == atype]
#
# ylim <- range(AEM[sel2, glo])
# ylim[2] <- ylim[2] * 1.05
#
# plot(AEM[sel2, glo, tsy],
#      ylim = ylim)
#
# lm_min   <- lm(  AEM[sel2, tsy, glo])
# mean_min <- mean(AEM[sel2, glo])
#
# title(paste("Second part, atm:", aatm, "type:", atype))
#
# abline(lm_min, col = "red")
#
# ## display trend on graph
# fit <- lm_min[[1]]
# units <- "W/m^2"
# legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
#        c(paste("Trend: ",
#              if (fit[2] > 0) "+" else "-",
#              signif(abs(fit[2]), 2) , bquote(.(units)), "/y"),
#          paste("Trend: ",
#                if (fit[2] > 0) "+" else "-",
#                signif(abs(100 * fit[2] / mean_min), 2) , "%/y")
#          )
# )
#
#
# ## BR SZA min ------------
#
# sel1 <- AEM[, is.na(month) & atmosphere_file == aatm & typer == "BR SZA min", ]
#
# plot(AEM[sel1, glo, tsy],
#      ylim = ylim)
#
# zeropointA <- min(AEM[sel1, tsy]) + (max(AEM[sel1, tsy]) - min(AEM[sel1, tsy])) / 2
# zeropointA <- 2005
#
# lm_BR_min   <- lm(  AEM[sel1, tsy, glo])
# mean_BR_min <- mean(AEM[sel1, glo])
#
# title(paste("First part, atm:", aatm, "type:", atype))
#
# abline(lm_BR_min, col = "red")
#
# ## display trend on graph
# fit <- lm_BR_min[[1]]
# units <- "W/m^2"
# legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
#        c(paste("Trend: ",
#                if (fit[2] > 0) "+" else "-",
#                signif(abs(fit[2]), 2) , bquote(.(units)), "/y"),
#          paste("Trend: ",
#                if (fit[2] > 0) "+" else "-",
#                signif(abs(100 * fit[2] / mean_BR_min), 2) , "%/y")
#        )
# )
#
#
#
# plot(AEM[sel1 | sel2, glo, tsy])
# abline(lm_BR_min, col = "red")
# abline(lm_min, col = "red")
# title("Both")
#
# test <- AEM[sel1 | sel2,]
#
# global_sza_min <- function(tsy   = tsy,
#                            a.    =  coef(lm_min)[2] ,
#                            b.    = -coef(lm_min)[2] * zeropoint,
#                            mean. = mean_min) {
#   return( (b. + a. * tsy) / mean. )
# }
#
# tsy <- 1993:2024
#
# global_sza_min <- function(tsy) {
#     tsyA <- tsy[tsy <  AEM[sel2, min(tsy)]]
#     tsyB <- tsy[tsy >= AEM[sel2, min(tsy)]]
#
#     res <- rbind(
#       cbind(tsyA, (-coef(lm_BR_min)[2] * zeropointA + tsyA * coef(lm_BR_min)[2]) / mean_BR_min )  ,
#       cbind(tsyB, (-coef(lm_min)[2]    * zeropoint  + tsyB * coef(lm_min)[2])    / mean_min )
#     )
#
#   ## return resulst with the same order
#   res[match(tsy, res[,1]),][,2]
# }




#
#
# ## SZA mean ------------
#
# aatm  <- "afglms"
# atype <- "SZA mean"
#
# sel <- AEM[, atmosphere_file == aatm & typer == atype]
#
# ylim <- range(AEM[sel, glo])
# ylim[2] <- ylim[2] * 1.12
#
# plot(AEM[sel, glo, tsy],
#      ylim = ylim)
#
# lm_mean   <- lm(  AEM[sel, tsy, glo])
# mean_mean <- mean(AEM[sel, glo])
#
# title(paste("atm:", aatm, "type:", atype))
#
# abline(lm_mean, col = "red")
#
# ## display trend on graph
# fit   <- lm_mean[[1]]
# units <- "W/m^2"
# legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
#        c(paste("Trend: ",
#                if (fit[2] > 0) "+" else "-",
#                signif(abs(fit[2]), 2) , bquote(.(units)), "/y"),
#          paste("Trend: ",
#                if (fit[2] > 0) "+" else "-",
#                signif(abs(100 * fit[2] / mean_mean), 2) , "%/y")
#        )
# )
#
# global_sza_mean <- function(tsy   = tsy,
#                            a.    =  coef(lm_mean)[2] ,
#                            b.    = -coef(lm_mean)[2] * zeropoint,
#                            mean. = mean_mean) {
#   return( (b. + a. * tsy) / mean. )
# }
#
# plot(1993:2024, global_sza_mean(1993:2024), col = "red")
#
#
# ## BR SZA mean ------------
#
# sel1 <- AEM[, is.na(month) & atmosphere_file == aatm & typer == "BR SZA mean", ]
#
# zeropointA <- min(AEM[sel1, tsy]) + (max(AEM[sel1, tsy]) - min(AEM[sel1, tsy])) / 2
# zeropointA <- 2005
#
# lm_BR_mean   <- lm(  AEM[sel1, tsy, glo])
# mean_BR_mean <- mean(AEM[sel1, glo])
#
# title(paste("Secod part, atm:", aatm, "type:", atype))
#
# abline(lm_BR_mean, col = "red")
#
# ## display trend on graph
# fit <- lm_BR_mean[[1]]
# units <- "W/m^2"
# legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
#        c(paste("Trend: ",
#                if (fit[2] > 0) "+" else "-",
#                signif(abs(fit[2]), 2) , bquote(.(units)), "/y"),
#          paste("Trend: ",
#                if (fit[2] > 0) "+" else "-",
#                signif(abs(100 * fit[2] / mean_BR_mean), 2) , "%/y")
#        )
# )
#
#







#
#
#
# ## SZA median ------------
#
# aatm  <- "afglms"
# atype <- "SZA median"
#
# sel <- AEM[, atmosphere_file == aatm & typer == atype]
#
# ylim <- range(AEM[sel, glo])
# ylim[2] <- ylim[2] * 1.12
#
# plot(AEM[sel, glo, tsy],
#      ylim = ylim)
#
# lm_median   <- lm(  AEM[sel, tsy, glo])
# mean_median <- mean(AEM[sel, glo])
#
# title(paste("atm:", aatm, "type:", atype))
#
# abline(lm_median, col = "red")
#
# ## display trend on graph
# fit <- lm_median[[1]]
# units <- "W/m^2"
# legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
#        c(paste("Trend: ",
#                if (fit[2] > 0) "+" else "-",
#                signif(abs(fit[2]), 2) , bquote(.(units)), "/y"),
#          paste("Trend: ",
#                if (fit[2] > 0) "+" else "-",
#                signif(abs(100 * fit[2] / mean_median), 2) , "%/y")
#        )
# )
#
#
# global_sza_median <- function(tsy   = tsy,
#                             a.    =  coef(lm_median)[2] ,
#                             b.    = -coef(lm_median)[2] * zeropoint,
#                             mean. = mean_median) {
#   return( (b. + a. * tsy) / mean. )
# }
#
# plot(1993:2024, global_sza_median(1993:2024), col = "red")
#
#
# ## BR SZA median ------------
#
# sel1 <- AEM[, is.na(month) & atmosphere_file == aatm & typer == "BR SZA median", ]
#
# zeropointA <- min(AEM[sel1, tsy]) + (max(AEM[sel1, tsy]) - min(AEM[sel1, tsy])) / 2
# zeropointA <- 2005
#
# lm_BR_median   <- lm(  AEM[sel1, tsy, glo])
# mean_BR_median <- mean(AEM[sel1, glo])
# mean_BR_median <- mean(AEM[sel1 & year == 2005, glo])
#
#
# title(paste("Secod part, atm:", aatm, "type:", atype))
#
# abline(lm_BR_median, col = "red")
#
# ## display trend on graph
# fit <- lm_BR_median[[1]]
# units <- "W/m^2"
# legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
#        c(paste("Trend: ",
#                if (fit[2] > 0) "+" else "-",
#                signif(abs(fit[2]), 2) , bquote(.(units)), "/y"),
#          paste("Trend: ",
#                if (fit[2] > 0) "+" else "-",
#                signif(abs(100 * fit[2] / mean_BR_min), 2) , "%/y")
#        )
# )
#



# save(list = c("lm_median",
#               "lm_min",
#               "lm_mean",
#               "lm_AOD_trend",
#               "lm_transp_trend",
#               "zeropoint"),
#      file = "./figures/Aerosols_trends.Rda")



## MONTHLY TREND  -------------------------

#+ echo=F, include=T, results='asis'
unique(AEM$typer)

gather <- data.frame()
## iterate SZA
for (atype in c("SZA 55") ) {
  ## iterate months
  for (mm in 1:12) {

    if (mm %in% c(4,5,6,7,8,9)) {
      aatm <- "afglms"
    }

    if (mm %in% c(10,11,12,1,2,3)) {
      aatm <- "afglmw"
    }

    pp <- AEM[atmosphere_file == aatm & typer == atype & month == mm]

    lm1          <- lm(pp[, tsy, glo])
    # lm_mean      <- mean(pp[, glo])
    lm_mean      <- mean(pp[year == min(year), glo])
    lm_zeropoint <- min(pp[, tsy]) + (max(pp[, tsy]) - min(pp[, tsy])) / 2


    plot(pp[, glo, tsy])
    abline(lm1, col = "red")

    ## display trend on graph
    fit   <- lm1[[1]]
    units <- "W/m^2"
    legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
           c(paste("Trend: ",
                   if (fit[2] > 0) "+" else "-",
                   signif(abs(fit[2]), 2) , bquote(.(units)), "/y"),
             paste("Trend: ",
                   if (fit[2] > 0) "+" else "-",
                   signif(abs(100 * fit[2] / lm_mean), 2) , "%/y")
           )
    )
    title(paste(atype, month.name[mm]))




    tt <- data.frame(Intercept = lm1[[1]][1],
                     Slope     = lm1[[1]][2],
                     Month     = mm,
                     MeanGlo   = lm_mean,
                     Type      = atype,
                     Zeropoint = lm_zeropoint,
                     SlopeRela = lm1[[1]][2] / lm_mean)
    gather <- rbind(gather, tt)

    # pander::pander(tt)
  }

  row.names(gather) <- NULL
  gather <- data.table(gather)
  cat(paste(atype, ":", mean(gather[Type == atype,  SlopeRela]), "\n\n"))
}


pander::pander(gather)


cat("\n\\newpage\n\n")




# unique(gather$Type)

atype <- "SZA median"

slope_median <- mean(gather[Type == "SZA median", SlopeRela])
slope_mean   <- mean(gather[Type == "SZA mean",   SlopeRela])
slope_min    <- mean(gather[Type == "SZA min",    SlopeRela])
slope_55     <- mean(gather[Type == "SZA 55",    SlopeRela])



# gather[Type == "SZA min",]

month_trend_median <- function(tsy) {
  -slope_median * zeropoint  + tsy * slope_median
}

month_trend_mean <- function(tsy) {
  -slope_mean * zeropoint  + tsy * slope_mean
}

month_trend_min <- function(tsy) {
  -slope_min * zeropoint  + tsy * slope_min
}

month_trend_55 <- function(tsy) {
  -slope_55 * zeropoint  + tsy * slope_55
}




year_trend_median <- function(tsy) {
  (-coef(lm_BR_median)[2] * zeropointA + tsy * coef(lm_BR_median)[2]) / mean_BR_median
}

year_trend_min <- function(tsy) {
  (-coef(lm_BR_min)[2]    * zeropointA + tsy * coef(lm_BR_min)[2])    / mean_BR_min
}

year_trend_mean <- function(tsy) {
  (-coef(lm_BR_mean)[2]   * zeropointA + tsy * coef(lm_BR_mean)[2])   / mean_BR_mean
}

year_trend_55 <- function(tsy) {
  (-coef(lm_BR_55)[2]     * zeropointA + tsy * coef(lm_BR_55)[2])     / mean_BR_55
}

trend_mean <- function(tsy) {
  tsyA <- tsy[tsy <  2005]
  tsyB <- tsy[tsy >= 2005]
  ## calculate values
  res <- rbind(
    cbind(tsyA,  year_trend_mean(tsyA)),
    cbind(tsyB, month_trend_mean(tsyB))
  )
  ## return results with the same order
  res[match(tsy, res[,1]),][,2]
}


trend_median <- function(tsy) {
  tsyA <- tsy[tsy <  2005]
  tsyB <- tsy[tsy >= 2005]
  ## calculate values
  res <- rbind(
    cbind(tsyA,  year_trend_median(tsyA)),
    cbind(tsyB, month_trend_median(tsyB))
  )
  ## return results with order
  res[match(tsy, res[,1]),][,2]
}


trend_min <- function(tsy) {
  tsyA <- tsy[tsy <  2005]
  tsyB <- tsy[tsy >= 2005]
  ## calculate values
  res <- rbind(
    cbind(tsyA,  year_trend_min(tsyA)),
    cbind(tsyB, month_trend_min(tsyB))
  )
  ## return results with order
  res[match(tsy, res[,1]),][,2]
}


trend_55 <- function(tsy) {
  tsyA <- tsy[tsy <  2005]
  tsyB <- tsy[tsy >= 2005]
  ## calculate values
  res <- rbind(
    cbind(tsyA,  year_trend_55(tsyA)),
    cbind(tsyB, month_trend_55(tsyB))
  )
  ## return results with order
  res[match(tsy, res[,1]),][,2]
}




trend_median_adj <- function(tsy) {
  trend_median(tsy) - 0.01995656
}

Exdenat=trend_mean_adj <- function(tsy) {
  trend_mean(tsy) - 0.01995656
}


adjpoint55 <- trend_55(c(2005+(2024-2005)/2):2020)[1]
trend_55_adj <- function(tsy) {
  trend_55(tsy) - adjpoint55
}





# ### Median  --------------------
#
# aa <- AEM[typer %in% c("SZA median", "BR SZA median"), .(MeanGlo = mean(glo)), by = .(year, typer) ]
# pander::pander(setorder(aa, year))
#
#
# xlim <- range(1993:2024)
# ylim <- range(year_trend_median(xlim), month_trend_median(xlim), trend_median_adj(xlim))
#
# plot(  1993:2005, year_trend_median(1993:2005), col = "red",
#        xlim = xlim,
#        ylim = ylim,
#        xlab = "",
#        ylab = "")
# points(2005:2024, month_trend_median(2005:2024), col = "blue")
#
# points(1993:2024, trend_median_adj(1993:2024), col = "magenta")
#
# legend("top", pch = 1, lty = NA, bty = "n", lwd = 2, cex = 1,
#        col = c("red", "blue"),
#        c(paste(if (coef(lm_BR_median)[2]/ mean_BR_median > 0) "+" else "-",
#                signif(abs(100 * coef(lm_BR_median)[2]/ mean_BR_median), 2), "%/y"),
#          paste(if (slope_median > 0) "+" else "-",
#                signif(100 * slope_median, 2) , "%/y")
#        )
# )
#
# title("Median SZA")


#
# cat("\n\\newpage\n\n")
# #### Min ------------
#
# aa <- AEM[typer %in% c("SZA min", "BR SZA min"), .(MeanGlo = mean(glo)), by = .(year, typer) ]
# pander::pander(setorder(aa, year))
#
#
# xlim <- range(1993:2024)
# ylim <- range(year_trend_min(xlim), month_trend_min(xlim))
#
# plot(  1993:2005, year_trend_min(1993:2005), col = "red",
#        xlim = xlim,
#        ylim = ylim,
#        xlab = "",
#        ylab = "")
# points(2005:2024, month_trend_min(2005:2024), col = "blue")
#
# points(1993:2024, trend_min(1993:2024), col = "magenta")
#
# legend("top", pch = 1, lty = NA, bty = "n", lwd = 2, cex = 1,
#        col = c("red", "blue"),
#        c(paste(if (coef(lm_BR_min)[2]/ mean_BR_min > 0) "+" else "-",
#                signif(abs(100 * coef(lm_BR_min)[2] / mean_BR_min), 2), "%/y"),
#          paste(if (slope_min > 0) "+" else "-",
#                signif(100 * slope_min, 2) , "%/y")
#        )
# )
#
# title("Min SZA")
#


#
#
# cat("\n\\newpage\n\n")
# #### Mean  ------------------
#
# aa <- AEM[typer %in% c("SZA mean", "BR SZA mean"), .(MeanGlo = mean(glo)), by = .(year, typer) ]
# pander::pander(setorder(aa, year))
#
#
# xlim <- range(1993:2024)
# ylim <- range(c(year_trend_mean(xlim), month_trend_mean(xlim), trend_mean_adj(xlim)))
#
# plot(  1993:2005, year_trend_mean(1993:2005), col = "red",
#        xlim = xlim,
#        ylim = ylim,
#        xlab = "",
#        ylab = "")
# points(2005:2024, month_trend_mean(2005:2024), col = "blue")
#
# points(1993:2024, trend_mean_adj(1993:2024), col = "magenta")
#
# legend("top", pch = 1, lty = NA, bty = "n", lwd = 2, cex = 1,
#        col = c("red", "blue"),
#        c(paste(if (coef(lm_BR_mean)[2]/ mean_BR_mean > 0) "+" else "-",
#                signif(abs(100 * coef(lm_BR_mean)[2] / mean_BR_mean), 2), "%/y"),
#          paste(if (slope_mean > 0) "+" else "-",
#                signif(100 * slope_mean, 2) , "%/y")
#        )
# )
#
# title("Mean SZA")








cat("\n\\newpage\n\n")
#### 55  ------------------

aa <- AEM[typer %in% c("SZA 55", "Year BR SZA 55"), .(MeanGlo = mean(glo)), by = .(year, typer)]
pander::pander(setorder(aa, year))


xlim <- range(1993:2024)
ylim <- range(c(year_trend_55(xlim), month_trend_55(xlim), trend_55_adj(xlim)))

plot(  1993:2005, year_trend_55(1993:2005), col = "red",
       xlim = xlim,
       ylim = ylim,
       xlab = "",
       ylab = "")
points(2005:2024, month_trend_55(2005:2024), col = "blue")

points(1993:2024, trend_55_adj(1993:2024), col = "magenta")

legend("top", pch = 1, lty = NA, bty = "n", lwd = 2, cex = 1,
       col = c("red", "blue"),
       c(paste(if (coef(lm_BR_55)[2] / mean_BR_55 > 0) "+" else "-",
               signif(abs(100 * coef(lm_BR_55)[2] / mean_BR_55), 2), "%/y"),
         paste(if (slope_55 > 0) "+" else "-",
               signif(100 * slope_55, 2) , "%/y")
       )
)

title("55 SZA")

dataset <- rbind(
  data.table(year   = 1993:2005,
             change = 100 * trend_55_adj(1993:2005),
             Source = "Brewer"),
  data.table(year   = 2005:2024,
             change = 100 * trend_55_adj(2005:2024),
             Source = "AERONET")
)


#+ P-CS-change, echo=F, include=T, results="asis"
# p2 <-
ggplot(dataset,
       aes(x = year,
           y = change,
           colour = Source)) +
  geom_line(linewidth = 1.3) +
  ylab(bquote("Difference %" )) +
  xlab("Date") +
  annotate("text", x = 2000, y = dataset[year==2000, change],
           label = "paste(\"+0.022 %/y\")", parse = TRUE,
           fontface = 2,
           size     = 5,
           hjust    = 1.1,
           vjust    = 0) +
  annotate("text", x = 2015, y = dataset[year==2015, change],
           label = "paste(\"+0.14 %/y\")", parse = TRUE,
           fontface = 2,
           size     = 5,
           hjust    = 1.1,
           vjust    = 0) +
  theme(legend.justification = c(0, 1),
        legend.title         = element_text(size=10),
        legend.position      = c(0.01, .99),
        legend.key           = element_blank(),
        legend.background    = element_rect(fill = "transparent")) +
  scale_x_continuous(guide  = "axis_minor",
                     limits = c(1993, NA),
                     breaks = c(
                       1993,
                       pretty(dataset[, year], n = 4),
                       max(ceiling(dataset[, year]))),
                     minor_breaks = seq(1990, 2050, by = 1) )

# theme(plot.margin = margin(t = 0, r = 0.5 , b = 0.5, l = 0, "cm"))

#   scale_y_continuous(guide        = "axis_minor",
#                      minor_breaks = seq(0, 500, by = 25)) +
#   scale_x_continuous(guide        = "axis_minor",
#                      limits = c(1993, NA),
#                      breaks = c(
#                        1993,
#                        pretty(dataset[,year], n = 4),
#                        max(ceiling(dataset[,year]))),
#                      minor_breaks = seq(1990, 2050, by = 1) )
# p2
#



# From Stelios' paper 2007:
# AOD @340 from Brewer 086
# Period: 1997-2005
# The mean AOD for this period was calculated to 0.403 for the Brewer and 0.422 for the CIMEL.
# A linear regression on the Brewer deseasonalized data reveals a change of −3.8±0.93% per year.
# From the above data AOD @340 changes by 0.0153 per year (or 0.138 for the 9 years).
# Using Angstrom a= 1.6  this translates to a change in Angstrom β=0.00272 per year (or β=0.084 in 1997 and β=0.059 in 2005).
#
# Libradtran calculations suggest for the clear-sky GHI:
# SZA	1997	2005
# 60 	407	426	(Wm-2)
# 30	844	865
# 15	968	988
#
# Μπορούμε να διαμορφώσουμε τα thresholds με αυτό το trend για την περίοδο 1993-2005 και το αντίστοιχα από το cimel για την περίοδο 2004-2024.
# Αν οι διαφορές στην noon-GHI φαίνονται λογικές, μπορούμε να το σκεφτούμε πως θα αντιμετωπίσουμε και τις υπόλοιπες γωνίες στην κατασκευή του reference





if (!interactive()) {
  dev.off()
}



#' **END**
#+ include=T, echo=F
tac <- Sys.time()
# cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            # Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (difftime(tac,tic,units = "sec") > 30) {
  system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
  # system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'R script ended'"))
}
