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

source("~/Aerosols/RlibRadtran/R/date_to_standard_atmosphere_file.R")


CS <- readRDS("~/LibRadTranG/Clear_sky_model_AERONET_monthly/Model_CS_trend_fix_2.Rds")


CS <- CS[, .(year, month, sza, edir, edn, typer, atmosphere_file)]
CS[, glo := (edir + edn) / 1000 ]
CS[, tsy := year + (month - 1) / 12]

CS[, Date := as.POSIXct(as.Date(paste(year, month, 1), "%Y %m %d")) ]

## select proper atmosphere file
CS <- CS[date_to_standard_atmosphere_file(CS$Date) == CS$atmosphere_file, ]


ggplot(data = CS, aes(x = tsy, y = glo, colour = typer)) +
  geom_point()





xlim <- range(1993, CS$tsy)
ylim <- range(CS$glo)

CS[typer == "Cimel SZA 55", glo, tsy]

plot(CS[typer == "Cimel SZA 55", glo, tsy], xlim = xlim, ylim = ylim, col = "blue")

points(CS[typer == "BR SZA 55", glo, tsy], col = "green")


clm <- lm(CS[typer == "Cimel SZA 55", glo] ~ CS[typer == "Cimel SZA 55", tsy])
abline(clm, col = "blue")

Blm <- lm(CS[typer == "BR SZA 55", glo] ~ CS[typer == "BR SZA 55", tsy])
abline(Blm, col = "green")


legend("topleft", pch = 1, lty = NA, bty = "n", lwd = 2, cex = 1,
       col = c("blue", "green"),
       c(paste(if (coef(clm)[2] / mean(CS[typer == "Cimel SZA 55", glo], na.rm = T) > 0) "+" else "-",
               signif(abs(100 * coef(clm)[2] / mean(CS[typer == "Cimel SZA 55", glo], na.rm = T) ), 2), "%/y"),
         paste(if (coef(clm)[2] / mean(CS[typer == "BR SZA 55", glo], na.rm = T) > 0) "+" else "-",
               signif(abs(100 * coef(clm)[2] / mean(CS[typer == "BR SZA 55", glo], na.rm = T) ), 2), "%/y")
       )
)




## create mean of each month for continuity

COM <- CS[, .(glo = mean(glo)), by = Date]
COM[, tsy := year(Date) + (month(Date) - 1) / 12]




ggplot(data = COM, aes(x = tsy, y = glo)) +
  geom_point()


plot(COM[, glo, tsy])
lmm <- lm(COM[, glo] ~ COM[, tsy])

abline(lmm, col = "green")
legend("topleft", pch = NA, lty = 1, bty = "n", lwd = 2, cex = 1,
       col = c("green"),
       c(paste(if (coef(lmm)[2] / mean(COM[, glo], na.rm = T) > 0) "+" else "-",
               signif(abs(100 * coef(lmm)[2] / mean(COM[, glo], na.rm = T) ), 2), "%/y")
       )
)


## Todo exponetian and second order






#' **END**
#+ include=T, echo=F
tac <- Sys.time()
# cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            # Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (difftime(tac,tic,units = "sec") > 30) {
  system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
  # system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'R script ended'"))
}
