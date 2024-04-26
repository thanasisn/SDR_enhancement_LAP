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



## Load raw
load("./data/GHI_enh_01_raw_data.Rda")

DATA$wattDIR     <- NULL
DATA$wattDIR_sds <- NULL
DATA$wattHOR     <- NULL
DATA$wattHOR_sds <- NULL



## keep only clear
DATA <- DATA[TYPE == "Clear"]

DATA <- DATA |>
  select(!ends_with(  "W.edir")) |>
  select(!ends_with(  "W.edn"))  |>
  select(!ends_with(  "W.eup"))  |>
  select(!starts_with("C1Grp"))

DATA <- DATA[!is.na(wattGLB)]

DATA[, .N, by = Day]

DTdaily <- DATA[,
                .(
                  glo_Sum   = sum(Low_B.Low_W.glo),
                  GLB_Sum   = sum(wattGLB),
                  GLB_N     = sum(!is.na(wattGLB)),
                  DayLength = max(DayLength)
                ),
                by = Day]


hist(DTdaily$GLB_N)
hist(DTdaily[, GLB_N/DayLength])

Daytime_Ratio <- 0.006

DTdaily[GLB_N/DayLength < Daytime_Ratio, .N]

DATA <- DATA[ Day %in% DTdaily[GLB_N/DayLength > Daytime_Ratio, Day]]



DTdaily <- DATA[,
                .(
                  glo_Sum   = sum(Low_B.Low_W.glo),
                  GLB_Sum   = sum(wattGLB),
                  GLB_N     = sum(!is.na(wattGLB)),
                  DayLength = max(DayLength)
                ),
                by = Day]



plot(DATA[, wattGLB / Low_B.Low_W.glo ])

table(DATA$BAD_h)




DTyear <- DATA[, .(glo = mean(Low_B.Low_W.glo),
                   GLB = mean(wattGLB)),
               by = .(year(Date))]

ylim <- range(DTyear[, glo, GLB])

# plot(  DTyear[, glo, year],
#        ylim = ylim)
# points(DTyear[, GLB, year], col = "green")

plot(  DTyear[, GLB/glo, year], col = "red")
title("Clear GLB / CSlibratran by year")

DTmonth <- DATA[, .(glo = mean(Low_B.Low_W.glo),
                   GLB = mean(wattGLB)),
               by = .(year(Date), month(Date))]
DTmonth[, tsy := year + (month - 1)/12 ]


ylim <- range(DTmonth[, glo, GLB])

# plot(  DTmonth[, glo, tsy],
#        ylim = ylim)
# points(DTmonth[, GLB, tsy], col = "green")

plot(  DTmonth[, GLB/glo, tsy], col = "red")
title("Clear GLB / CSlibratran by month")





days <- unique(c(DTdaily[GLB_Sum/glo_Sum < 0.6, Day]))

for (ad in days) {
  temp <- DATA[Day == ad,]

  ylim <- range(temp[, Low_B.Low_W.glo, wattGLB])

  plot(  temp[, Low_B.Low_W.glo, Date],
         col = "blue",
         ylim = ylim)
  points(temp[, wattGLB, Date], col = "green")
  title(as.Date(ad))

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
