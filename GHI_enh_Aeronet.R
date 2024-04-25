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

#+ echo=F, include=T

## __ Document options  --------------------------------------------------------


#+ echo=FALSE, include=TRUE

knitr::opts_chunk$set(comment    = ""       )
knitr::opts_chunk$set(dev        = c("pdf", "png")) ## expected option
# knitr::opts_chunk$set(dev        = "png"    )       ## for too much data
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
knitr::opts_chunk$set(cache      =  FALSE   )  ## !! breaks calculations
knitr::opts_chunk$set(fig.pos    = '!h'     )

#+ echo=FALSE, include=TRUE
## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
Script.Name <- "./GHI_enh_Aeronet.R"
tic <- Sys.time()

## use worktree
setwd("~/MANUSCRIPTS/02_enhancement/")

if (!interactive()) {
  pdf( file = paste0("./runtime/", basename(sub("\\.R$",".pdf", Script.Name))))
}

#+ echo=F, include=T
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(janitor   , quietly = TRUE, warn.conflicts = FALSE)


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

# rm(AE1)


# min(AE1$tsy):max(AE1$tsy)


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

AEM          <- readRDS("./data/Model_CS_trend.Rds")
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






asza  <- 17
aatm  <- "afglms"
atype <- "Month Exact"

# unique(AEM$typer)



sel <- AEM[, sza == asza & atmosphere_file == aatm & typer == atype]

ylim <- range(AEM[sel, glo])
ylim[2] <- ylim[2] * 1.05

plot(AEM[sel, glo, tsy],
     ylim = ylim)

lm1   <- lm(  AEM[sel, tsy, glo])
amean <- mean(AEM[sel, glo])

title(paste("sza:", asza, "atm:", aatm, "type:", atype))

abline(lm1, col = "red")

## display trend on graph
fit <- lm1[[1]]
units <- "Watt/m^2"
legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
       c(paste("Trend: ",
             if (fit[2] > 0) "+" else "-",
             signif(abs(fit[2]), 2) , bquote(.(units)), "/y"),
         paste("Trend: ",
               if (fit[2] > 0) "+" else "-",
               signif(abs(100 * fit[2] / amean), 2) , "%/y")
         )
)






aatm  <- "afglms"
atype <- "SZA min"

sel2 <- AEM[, atmosphere_file == aatm & typer == atype]

ylim <- range(AEM[sel2, glo])
ylim[2] <- ylim[2] * 1.05

plot(AEM[sel2, glo, tsy],
     ylim = ylim)

lm_min   <- lm(  AEM[sel2, tsy, glo])
mean_min <- mean(AEM[sel2, glo])

title(paste("Second part, atm:", aatm, "type:", atype))

abline(lm_min, col = "red")

## display trend on graph
fit <- lm_min[[1]]
units <- "Watt/m^2"
legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
       c(paste("Trend: ",
             if (fit[2] > 0) "+" else "-",
             signif(abs(fit[2]), 2) , bquote(.(units)), "/y"),
         paste("Trend: ",
               if (fit[2] > 0) "+" else "-",
               signif(abs(100 * fit[2] / mean_min), 2) , "%/y")
         )
)


sel1 <- AEM[, is.na(month) & atmosphere_file == aatm & typer == "BR SZA min", ]

plot(AEM[sel1, glo, tsy],
     ylim = ylim)

zeropointA <- min(AEM[sel1, tsy]) + (max(AEM[sel1, tsy]) - min(AEM[sel1, tsy])) / 2

lm_BR_min   <- lm(  AEM[sel1, tsy, glo])
mean_BR_min <- mean(AEM[sel1, glo])

title(paste("First part, atm:", aatm, "type:", atype))

abline(lm_BR_min, col = "red")

## display trend on graph
fit <- lm_BR_min[[1]]
units <- "Watt/m^2"
legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
       c(paste("Trend: ",
               if (fit[2] > 0) "+" else "-",
               signif(abs(fit[2]), 2) , bquote(.(units)), "/y"),
         paste("Trend: ",
               if (fit[2] > 0) "+" else "-",
               signif(abs(100 * fit[2] / mean_BR_min), 2) , "%/y")
       )
)



plot(AEM[sel1 | sel2, glo, tsy])
abline(lm_BR_min, col = "red")
abline(lm_min, col = "red")
title("Both")

test <- AEM[sel1 | sel2,]

AEM[sel2, min(tsy)]

global_sza_min <- function(tsy   = tsy,
                           a.    =  coef(lm_min)[2] ,
                           b.    = -coef(lm_min)[2] * zeropoint,
                           mean. = mean_min) {
  return( (b. + a. * tsy) / mean. )
}

tsy <-1993:2024
global_sza_min <- function(tsy) {
  tsyA <- tsy[tsy <  AEM[sel2, min(tsy)]]
  tsyB <- tsy[tsy >= AEM[sel2, min(tsy)]]

  rbind(
    cbind(tsyA, (-coef(lm_BR_min)[2] * zeropointA + tsyA * coef(lm_BR_min)[2]) / mean_BR_min )  ,
    cbind(tsyB, (-coef(lm_min)[2]    * zeropoint  + tsyB * coef(lm_min)[2])    / mean_min )
  )
}

# global_sza_min(1993:2024)


plot(1993:2024, global_sza_min(1993:2024)[,2], col = "red")
title("Combined trend")







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
# units <- "Watt/m^2"
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
#
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
# units <- "Watt/m^2"
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


# save(list = c("lm_median",
#               "lm_min",
#               "lm_mean",
#               "lm_AOD_trend",
#               "lm_transp_trend",
#               "zeropoint"),
#      file = "./figures/Aerosols_trends.Rda")




# 100 * fit[2] / amean

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
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (difftime(tac,tic,units = "sec") > 30) {
  system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
  system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'R script ended'"))
}
