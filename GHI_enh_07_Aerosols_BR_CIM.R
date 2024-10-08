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
library(lubridate     , quietly = TRUE, warn.conflicts = FALSE)

source("./GHI_enh_00_variables.R")
source("./GHI_enh_00_dictionary.R")

source("~/Aerosols/RlibRadtran/R/date_to_standard_atmosphere_file.R")


CS <- readRDS("~/LibRadTranG/Clear_sky_model_AERONET_monthly/Model_CS_trend_fix_2.Rds")


CS <- CS[, .(year, month, sza, edir, edn, typer, atmosphere_file)]
CS[, glo  := (edir + edn) / 1000 ]
CS[, Date := as.POSIXct(as.Date(paste(year, month, 1), "%Y %m %d")) ]
CS[, tsy  := decimal_date(Date)]


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
# COM[, tsy := year(Date) + (month(Date) - 1) / 12]
COM[, tsy := decimal_date(Date)]




ggplot(data = COM, aes(x = tsy, y = glo)) +
  geom_point()


xlim <- range(1994, COM[, tsy])
plot(COM[, glo, tsy],
     xlim = xlim)

lmm  <- lm(    COM[, glo]  ~ COM[, tsy])
logm <- lm(log(COM[, glo]) ~ COM[, tsy])
polm <- lm(COM[, glo] ~ poly(COM[, tsy], 2, raw=TRUE))

logm_C  <- coef(logm)
polym_C <- coef(polm)

summary(polm)

# abline(lmm, col = "green")


## create range to plot
t <- (tsy = seq(1994, 2024, 0.1))


dataset       <- data.table(tsy = t)
dataset$expon <- exp(logm_C[1])*exp(logm_C[2]*t)
dataset$secon <- polym_C[1] + polym_C[2] * t + polym_C[3] * t^2


# lines(dataset[, expon, tsy], col = "dodgerblue", lwd = 2)

lines(dataset[, secon, tsy], col = "dodgerblue4", lwd = 2)


# legend("topleft", pch = NA, lty = 1, bty = "n", lwd = 2, cex = 1,
#        col = c("green"),
#        c(paste(if (coef(lmm)[2] / mean(COM[, glo], na.rm = T) > 0) "+" else "-",
#                signif(abs(100 * coef(lmm)[2] / mean(COM[, glo], na.rm = T) ), 2), "%/y")
#        )
# )


## select zero point
tzero      <- 2015
polyf_zero <- dataset[tsy == tzero, secon]

## function to apply to GHI
trend_polyf <- function(tsy) {
  -1 + (polym_C[1] / polyf_zero)         +
       (polym_C[2] / polyf_zero) * tsy   +
       (polym_C[3] / polyf_zero) * tsy^2
}



x    <- (seq(1994, 2024, 0.01))
test <- data.table(x = x, y = 100 * ( trend_polyf(x) )  )



first(test)
last(test)
test[which.max(y),]



cat("Range of delta(AOD):", signif(100 * range(trend_polyf(seq(1994, 2024, 0.01))), 2), "\n\n")

cat("Range of delta(AOD):", round(100 * range(trend_polyf(seq(1994, 2024, 0.01))), 2), "\n\n")


cat("GHI_model (abs) =", polym_C[1], "+",
                         polym_C[2], "* year_frac +",
                         polym_C[3], "* year_frac^2", "\n\n")

cat("GHI_model (rat) =",
     signif(-1 + (polym_C[1] / polyf_zero), digits = 4), "+",
     signif(     (polym_C[2] / polyf_zero), digits = 4), "* x +",
     signif(     (polym_C[3] / polyf_zero), digits = 4), "* x^2", "\n\n")

cat("GHI_model (%)   =",
    signif(100 * (-1 + (polym_C[1] / polyf_zero)), digits = 6), "+",
    signif(100 *        polym_C[2] / polyf_zero,   digits = 5), "* x +",
    signif(100 *        polym_C[3] / polyf_zero,   digits = 5), "* x^2", "\n\n")


cat("GHI_model (%)   =",
    round(100 * (-1 + (polym_C[1] / polyf_zero)), digits = 2), "+",
    round(100 *        polym_C[2] / polyf_zero,   digits = 4), "* x +",
    round(100 *        polym_C[3] / polyf_zero,   digits = 6), "* x^2", "\n\n")



relativ <- copy(dataset)

relativ[, secon := 100 *  trend_polyf(tsy)]


#+ P-CS-change-poly, echo=F, include=T, results="asis"
ggplot(relativ,
       aes(x = tsy,
           y = secon)) +
  geom_line(linewidth = 1.3) +
  ylab(bquote("Cloud-free GHI change %" )) +
  xlab("Date") +
  # annotate("text", x = 2000, y = relativ[tsy==1995, secon],
  #          label = "paste(\"+0.21 %/y\")", parse = TRUE,
  #          fontface = 2,
  #          size     = 5,
  #          hjust    = 0,
  #          vjust    = 0)
  # annotate("text", x = 2015, y = dataset[year==2015, change],
  #          label = "paste(\"+0.14 %/y\")", parse = TRUE,
  #          fontface = 2,
  #          size     = 5,
  #          hjust    = 1.1,
  #          vjust    = 0) +
  theme(legend.justification = c(0, 1),
        legend.key           = element_blank(),
        legend.background    = element_rect(fill = "transparent")) +
  scale_x_continuous(guide  = "axis_minor",
                     limits = c(1994, NA),
                     breaks = c(
                       1994,
                       pretty(relativ[, tsy], n = 4),
                       max(ceiling(relativ[, tsy]))),
                     minor_breaks = seq(1990, 2050, by = 1) )




#
# lmts <- lm(relativ[, secon] ~ poly(relativ[, tsy], 2, raw=TRUE))
# dd <- coefficients(lmts)
#
# tess  <- function(x) dd[1] + dd[2] * x + dd[3] *x^2
#
# trend_polyf(1994)
#
# tess(1994)
# tess(2024)
#
#
# cat("GHI_model (%)   =",
#     round(dd[1], digits = 4), "+",
#     round(dd[2], digits = 4), "* x +",
#     round(dd[3], digits = 4), "* x^2", "\n\n")
#
#
#
# -12153.97 + 12.0304 * 1994 - 0.003 * 1994*1994
#
# dd[1] + dd[2] * 1994 + dd[3] * 1994^2
#
#
# ( -1 + (polym_C[1] / polyf_zero)          +
#        (polym_C[2] / polyf_zero) * 1994   +
#        (polym_C[3] / polyf_zero) * 1994^2 ) * 100
#
# paste(
#   (-1 + (polym_C[1] / polyf_zero)) * 100,
#   (     (polym_C[2] / polyf_zero)) * 100,
#   (     (polym_C[3] / polyf_zero)) * 100
# )
#
# -12153.9716401981 + 12.0304174067744 * 1994  -0.0029770072925644 * 1994^2
#


#' **END**
#+ include=T, echo=F
tac <- Sys.time()
# cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            # Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (difftime(tac,tic,units = "sec") > 30) {
  system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
  # system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'R script ended'"))
}
