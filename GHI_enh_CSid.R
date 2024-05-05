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
Script.Name <- "./GHI_enh_CSid.R"
tic <- Sys.time()

## use worktree
setwd("~/MANUSCRIPTS/02_enhancement/")

# if (!interactive()) {
#   pdf( file = paste0("./runtime/", basename(sub("\\.R$",".pdf", Script.Name))))
# }

#+ echo=F, include=T
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr,      quietly = TRUE, warn.conflicts = FALSE)
library(janitor   , quietly = TRUE, warn.conflicts = FALSE)



## Load raw from main
# load("./data/GHI_enh_02_ID_CE.Rda")
load("./threshold_change/data/GHI_enh_02_ID_CE.Rda")


DATA$wattDIR           <- NULL
DATA$wattDIR_sds       <- NULL
DATA$wattHOR           <- NULL
DATA$wattHOR_sds       <- NULL
DATA$ClearnessIndex_kt <- NULL


DATA <- DATA |>
  select(!ends_with(  "W.edir")) |>
  select(!ends_with(  "W.edn"))  |>
  select(!ends_with(  "W.eup"))  |>
  select(!starts_with("C1Grp"))



#'
#' `Low_B.Low_W.glo` has compensation for sun distance, TSI variation and Kurudz difference only
#'


## keep only clear
DATA <- DATA[TYPE == "Clear"]

table(DATA$TYPE)


DATA <- DATA[!is.na(wattGLB)]

hist(DATA$ClearnessIndex_C_4)

## Hard limits
DATA <- DATA[ClearnessIndex_C_4 > 0.8 ]
DATA <- DATA[ClearnessIndex_C_4 < 1.3 ]

source("./GHI_enh_Aeronet.R")


DATA[, tsy := year(Date) + (month(Date) - 1)/12 ]


## apply AOD trend
trendsf <- c(trend_median, trend_mean, trend_min)


trend_apply  <- trend_median
trend_factor <- 0.8


DATA[, Thresh_test := Enhanc_C_4_ref * (1 + trend_apply(tsy) * trend_factor)]
rmserr(DATA$Enhanc_C_4_ref, DATA$wattGLB)

f <- function(x) {
  rmserr(DATA[, Enhanc_C_4_ref * (1 + trend_apply(tsy) * x)],
         DATA$wattGLB)$rmse
}

optimise(f, interval = c(0.2, 1.5), maximum = FALSE)

optim(0.8, f, method = "Brent", lower = 0.2, upper = 1.5, tol = 0.01)

plot(DATA[, .(tsy, (1 + trend_apply(tsy) * trend_factor))])




DTdaily <- DATA[,
                .(
                  glo_Sum    = sum(Low_B.Low_W.glo),
                  GLB_Sum    = sum(wattGLB),
                  GLB_N      = sum(!is.na(wattGLB)),
                  DayLength  = max(DayLength),
                  En_Ref_sum = sum(Enhanc_C_4_ref),
                  Thresh_sum = sum(Thresh_test),
                  .N
                ),
                by = Day]


Day_points <- 30

DATA <- DATA[ Day %in% DTdaily[GLB_N > Day_points, Day]]


hist(DTdaily$GLB_N)
hist(DTdaily[, GLB_N/DayLength])
# plot(DTdaily[, GLB_N/DayLength, Day])
summary(DTdaily[, GLB_N/DayLength])

plot(DTdaily[, En_Ref_sum, Day])
plot(DTdaily[, glo_Sum, Day])
plot(DTdaily[, GLB_Sum, Day])


## remove days with few relative data
Daytime_Ratio <- 0.3

DTdaily[GLB_N/DayLength < Daytime_Ratio, .N]

DATA <- DATA[Day %in% DTdaily[GLB_N/DayLength > Daytime_Ratio, Day]]



DTdaily <- DATA[,
                .(
                  glo_Sum    = sum(Low_B.Low_W.glo),
                  GLB_Sum    = sum(wattGLB),
                  GLB_N      = sum(!is.na(wattGLB)),
                  DayLength  = max(DayLength),
                  En_Ref_sum = sum(Enhanc_C_4_ref),
                  Thresh_sum = sum(Thresh_test),
                  .N
                ),
                by = Day]



# plot(DATA[, ClearnessIndex_C_4, Date ])

# table(DATA$BAD_h)


DTyear <- DATA[, .(glo = mean(Low_B.Low_W.glo),
                   GLB = mean(wattGLB),
                   Ref = mean(Enhanc_C_4_ref),
                   Thr = mean(Thresh_test),
                   .N),
               by = .(year(Date))]

ylim <- range(DTyear[, glo, GLB])



# plot(  DTyear[, glo, year],
#        ylim = ylim)
# points(DTyear[, GLB, year], col = "green")

plot(DTyear[, N, year], col = "blue")

plot(DTyear[, GLB/glo, year], col = "red")
title("Clear GLB / CS libratran yearly means")

plot(DTyear[, Ref/GLB, year], col = "red")
title("Threshold ref / Clear Global yearly means")


plot(DTyear[, Ref/glo, year], col = "red")
title("Threshold ref / CS libratran yearly means")

plot(DTyear[, Thr/GLB, year], col = "red")
title(paste("Threshold / global yearly means", trend_factor))


plot(DTyear[, GLB, year], col = "red")
plot(DTyear[, Thr, year], col = "red")
plot(DTyear[, glo, year], col = "red")

rmserr(DTyear$GLB, DTyear$Thr)


stop()

DTmonth <- DATA[, .(glo = mean(Low_B.Low_W.glo),
                    GLB = mean(wattGLB),
                    Thr = mean(Thresh_test),
                    .N),
                by = .(year(Date), month(Date))]
DTmonth[, tsy := year + (month - 1)/12 ]


ylim <- range(DTmonth[, glo, GLB])

# plot(  DTmonth[, glo, tsy],
#        ylim = ylim)
# points(DTmonth[, GLB, tsy], col = "green")

# plot(  DTmonth[, .N, tsy], col = "blue")
plot(  DTmonth[, GLB/glo, tsy], col = "red")
title("Clear GLB / CS libratran by month")



knitr::opts_chunk$set(out.height = "32%"   )


Ratio_lim <- 0.85

DTdaily[GLB_Sum/glo_Sum < Ratio_lim, .N]

days <- unique(c(DTdaily[GLB_Sum/glo_Sum < Ratio_lim, Day]))

days <- sample(DTdaily[, Day], 40)

# for (ad in sort(days)) {
#   temp <- DATA[Day == ad,]
#
#   ylim <- range(temp[, Low_B.Low_W.glo, wattGLB])
#
#   plot(  temp[, Low_B.Low_W.glo, Date],
#          col = "blue",
#          ylim = ylim)
#   points(temp[, wattGLB, Date], col = "green")
#   title(as.Date(ad, origin = "1970-01-01"))
# }








#' **END**
#+ include=T, echo=F
tac <- Sys.time()
# cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            # Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (difftime(tac,tic,units = "sec") > 30) {
  system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
  # system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'R script ended'"))
}
