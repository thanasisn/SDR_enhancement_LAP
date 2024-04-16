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
AE1 <- remove_constant(AE1)

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

# plot(AE1[, exp(-AOD_500nm), tsy])
# abline(lm_transp_trend)


## Calculate offset for zero point  --------------------------------------------
b <- -coef(lm_transp_trend)[2] * zeropoint



## create a closure of the function
trans_trend <- {
  function(tsy)
    function(tsy = tsy, a. = coef(lm_transp_trend)[2], b. = b) {
      return(b. + a. * tsy)
    }
}(tsy)

trans_trend <- function(tsy = tsy, a. = coef(lm_transp_trend)[2], b. = b) {
  return(b. + a. * tsy)
}



write.csv(
  data.frame(year  = 1993:2023,
             trans = trans_trend(1993:2023))
  ,"./figures/transparency_trend.csv",
  row.names = F)



plot(AE1[, exp(-AOD_500nm), tsy])
abline(lm_transp_trend)
plot(1993:2024, trans_trend(1993:2024), col = "red")

rm(AE1)

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
