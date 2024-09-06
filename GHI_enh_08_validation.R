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
Script.Name <- "./GHI_enh_08_validation.R"
tic <- Sys.time()

if (!interactive()) {
  pdf( file = paste0("./runtime/", basename(sub("\\.R$",".pdf", Script.Name))))
}

#+ echo=F, include=T
library(data.table  , quietly = TRUE, warn.conflicts = FALSE)
require(zoo         , quietly = TRUE, warn.conflicts = FALSE)
library(pander      , quietly = TRUE, warn.conflicts = FALSE)
library(ggplot2     , quietly = TRUE, warn.conflicts = FALSE)
library(RColorBrewer, quietly = TRUE, warn.conflicts = FALSE)
library(pracma      , quietly = TRUE, warn.conflicts = FALSE)
library(lubridate   , quietly = TRUE, warn.conflicts = FALSE)


panderOptions("table.alignment.default", "right")
panderOptions("table.split.table",        120   )

## __ Load external functions --------------------------------------------------
## Functions from `https://github.com/thanasisn/IStillBreakStuff/tree/main/FUNCTIONS/R`
source("~/CODE/FUNCTIONS/R/data.R")
source("~/CODE/FUNCTIONS/R/trig_deg.R")


## __ Source initial scripts  --------------------------------------------------
source("./GHI_enh_00_variables.R")
source("./GHI_enh_00_dictionary.R")


## Override notification function
options(error = function() {
    if (interactive()) {
        system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
        system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'An error occurred!'"))
    }
})


##  Prepare raw data if needed  ------------------------------------------------
load("./data/GHI_enh_02_ID_CE.Rda")



##  Reset Randomness  ----------------------------------------------------------
RANDOM_SEED <- 333
set.seed(RANDOM_SEED)
## may need to reset seed in each randomness generation below




cat("Drop all enhacement cases")
DATA <- DATA[Enhanc_C_4 == F]

cat("Drop all clouds")
DATA <- DATA[TYPE == "Clear"]


DATA <- DATA[!is.na(wattGLB)]
DATA <- janitor::remove_empty(DATA, which = "cols")

COMPLETE <- DATA[, .(Completeness = sum(!is.na(wattGLB)) / DayLength) , by = Day ]


day_fill <- 0.8

KEEP <- DATA[Day %in% COMPLETE[Completeness > day_fill, Day], ]

hist(COMPLETE$Completeness)






sunny_days <- KEEP[, .(Sunshine = sum(TYPE == "Clear") / max(DayLength, na.rm = TRUE),
                       Energy   = sum(ClearnessIndex_kt, na.rm = TRUE)/sum(TYPE == "Clear"),
                       EC       = sum(get(SelEnhanc)),
                       Cloud    = sum(TYPE == "Cloud")),
                   by = Day]



## days with maximum values
# setorder(enh_days, -Enh_diff_max)
# maxenhd <- enh_days[1:400]

## strong total enhancement days
# setorder(enh_days, -Enh_sum)
# daylist <- enh_days[1:400]
# daylist <- daylist[!Day %in% maxenhd$Day]
# # enhsnd  <- daylist[sample(1:nrow(daylist), 300)]
# enhsnd  <- daylist

## select some sunny days
sunnyd  <- sunny_days[Sunshine > 0.79 & Energy > 0.74]
# sunnyd  <- sunnyd[!Day %in% maxenhd$Day & !Day %in% enhsnd$Day]
sunnyd  <- sunnyd[sample(1:nrow(sunnyd), 400, replace = T)]

## sunny with enhancements
sunnyenh <- sunny_days[Sunshine > 0.77 & Energy > 0.73 & EC > 0]
# sunnyenh <- sunnyenh[!Day %in% maxenhd$Day & !Day %in% enhsnd$Day & !Day %in% sunnyd$Day]

## cloudy days
clouds <- sunny_days[Sunshine > 0.6 & Energy > 0.6 & EC > 2 & Cloud > 5]
# clouds <- clouds[!Day %in% sunnyenh$Day & !Day %in% maxenhd$Day & !Day %in% enhsnd$Day & !Day %in% sunnyd$Day]
clouds <- clouds[sample(1:nrow(clouds), 400, replace = T)]

## some random days
all_days <- data.table(Day = unique(KEEP[, Day]))
# all_days <- all_days[!Day %in% sunnyenh$Day & !Day %in% maxenhd$Day & !Day %in% enhsnd$Day & !Day %in% sunnyd$Day & !Day %in% clouds]
all_days <- all_days[sample(1:nrow(all_days), 300)]

## manual selection
testdays <- data.table(Day = c(
    "2000-07-14",
    "2007-07-06",
    "2013-05-27",
    "2016-08-29"
))


## __  Days with strong enhancement cases  -------------------------------------

#' \FloatBarrier
#'
#' # Plot some days with strong enhancement cases
#'
#+ example-days, echo=F, include=T, results="asis"

vec_days <- matrix(
    ##   Data      Description
    c(
      # "maxenhd",  "extreme cases day",
      # "enhsnd",   "strong enhancement day",
      "sunnyd",   "sunny day",
      "sunnyenh", "sunny enhancement day",
      "clouds",   "cloudy day",
      "all_days", "random day",
      "testdays", "manual test days",
      NULL),
    byrow = TRUE,
    ncol  = 2)


## Format to data frame
vec_days <- data.frame(Data        = vec_days[,1],
                       Descriprion = vec_days[,2])

gather_days <- data.frame()
for (ii in 1:nrow(vec_days)) {
  cat("\n\\FloatBarrier\n\n")
  cat("\n## Days with", vec_days$Descriprion[ii], "\n\n")
  temp    <- get(vec_days$Data[ii])

  if (nrow(temp)>0) {
    gather_days <- rbind(gather_days,
                         data.frame(Day  = temp$Day,
                                    Type = vec_days$Descriprion[ii])
    )
  }
}



gather_days <- gather_days[!duplicated(gather_days$Day), ]
setorder(gather_days, -Day)

gather_days <- gather_days[sample(1:nrow(gather_days), 5), ]


##test
# daylist <- "2019-07-11"

for (ii in 1:nrow(gather_days)) {

  aday <- gather_days[ii, "Day"]
  type <- gather_days[ii, "Type"]
  doy  <- yday(aday)

  temp <- KEEP[Day == aday]
  par(mar = c(4, 4, 1, 1))
  # ylim <- range(0, temp$ETH, temp$wattGLB, na.rm = TRUE)
  ylim <- range(0, temp$ETH, temp$wattGLB, solar_constant, na.rm = TRUE)

  plot(temp$Date, temp$wattGLB, col = "green",
       pch  = ".", cex = 2,
       ylim = ylim,
       ylab = bquote("GHI" ~ group("[", W/m^2,"]")),
       xlab = "Time (UTC)")

  abline(h = solar_constant, col = "orange2", lty = 1, lwd = 2)

  ## Global
  lines(temp$Date, temp$wattGLB, col = "green")

  ## Direct
  # lines(temp$Date, temp$wattHOR, col = "blue")

  ## TSI on ground
  lines(temp$Date, temp$ETH)

  ## Active model reference
  lines(temp[, get(paste0(SelEnhanc, "_ref")), Date], col = "red" )

  ## Cloud-free ref
  lines(temp[, get(paste0(csmodel,".glo")), Date], col = "darkorchid" )

  # ## add sza axis
  # aaa <- temp[Date %in% c(min(Date), (pretty(Date, 10) + 30), max(Date))  , ]
  # axis(1, at = aaa$Date, labels = round(aaa$SZA,1),
  #      line = 1.2, lwd = 0, lwd.ticks = 0, cex.axis = 0.8)

  ## Enchantment cases
  points(temp[get(SelEnhanc) == TRUE & wattGLB <  ETH, wattGLB, Date], col = "burlywood4")
  points(temp[get(SelEnhanc) == TRUE & wattGLB >= ETH, wattGLB, Date], col = "red")


  ## Cloud cases
  points(temp[TYPE == "Cloud", wattGLB, Date], col = "blue", pch = 3, cex = 0.3)

  ## Decorations
  # title(main = paste(as.Date(aday, origin = "1970-01-01"), temp[get(SelEnhanc) == TRUE, .N], temp[TYPE == "Cloud", .N], vec_days$Descriprion[ii]))
  title(main = paste(as.Date(aday, origin = "1970-01-01"), doy , type))

  legend("bottomright", ncol = 2,
         c(  "GHI","CE threshold","TSI at TOA on horizontal plane","Solar Constant", "CE events","ECE events","Identified clouds",  "Cloud-free"),
         col = c("green",         "red",                  "black",       "orange2","burlywood4",       "red",             "blue","darkorchid"),
         pch = c(     NA,            NA,                       NA,              NA,          1 ,          1 ,                  3,           NA),
         lty = c(      1,             1,                        1,               1,          NA,          NA,                 NA,            1),
         lwd = c(      1,             1,                        1,               2,          NA,          NA,                 NA,            1),
         bty = "n",
         cex = 0.8
  )

  cat(' \n \n')
}

#+ echo=F, include=T






#' \newpage
#+ echo=F, include=T


plot(KEEP[, Low_B.Low_W.glo, wattGLB], xlab = "Low_B.Low_W.glo Cloud-free")
title(paste("Days:", length(unique(KEEP[, Day])), "Day fill:", day_fill, "Points:", KEEP[, sum(!is.na(wattGLB))]))
abline(a = 0, b = 1, col = "green")

print(cor.test(KEEP$wattGLB, KEEP$Low_B.Low_W.glo, method = c("pearson")))

print(lm(KEEP[, wattGLB, Low_B.Low_W.glo]))








#' \newpage
#+ echo=F, include=T


plot(KEEP[, Enhanc_C_4_ref, wattGLB], xlab = "CE threshold")
title(paste("Days:", length(unique(KEEP[, Day])), "Day fill:", day_fill, "Points:", KEEP[, sum(!is.na(wattGLB))]))
abline(a = 0, b = 1, col = "green")


print(cor.test(KEEP$wattGLB, KEEP$Enhanc_C_4_ref, method = c("pearson")))

print(lm(KEEP[, wattGLB, Enhanc_C_4_ref]))


library(ggplot2)
library(ggpmisc)


lm_eqn <- function(x, y){
  m <- lm(y ~ x);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}



ggplot(KEEP, aes(wattGLB, Enhanc_C_4_ref)) +
  geom_point(colour = "black",
             alpha  = .1,
             na.rm  = TRUE,
             size   = 0.3) +
  geom_abline(aes(intercept = 0, slope = 1), colour = "green") +
  ylab(bquote("CE threshold" ~ group("[", W/m^2,"]"))) +
  xlab(bquote("GHI" ~ group("[", W/m^2,"]"))) +
  labs(color = bquote("OI" ~ group("[", W/m^2,"]"))) +
  # stat_poly_line() +
  # stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_text(x = 300, y = 300, label = lm_eqn(KEEP$wattGLB, KEEP$Enhanc_C_4_ref), parse = TRUE)
  # theme(
  #   legend.title         = element_text(size = 10),
  #   legend.position      = c(.03, .97),
  #   legend.justification = c("left", "top"),
  #   legend.box.just      = "right",
  #   legend.key           = element_blank(),
  #   legend.background    = element_rect(fill = "transparent"),
  #   legend.margin        = margin(6, 6, 6, 6) ) +
#   scale_x_continuous(expand = expansion(mult = c(0.03, 0.03))) +
#   scale_y_continuous(breaks = scales::breaks_extended(n = 6),
#                      expand = expansion(mult = c(0.03, 0.03)))











#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (difftime(tac,tic,units = "sec") > 30) {
    system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
    system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'R script ended'"))
}
