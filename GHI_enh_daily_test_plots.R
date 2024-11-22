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
# knitr::opts_chunk$set(dev        = c("pdf", "png"))
knitr::opts_chunk$set(dev        = "pdf"    )
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
knitr::opts_chunk$set(fig.cap    = " - empty caption - " )
knitr::opts_chunk$set(cache      =  FALSE   )  ## !! breaks calculations
knitr::opts_chunk$set(fig.pos    = 'H'    )

#+ echo=FALSE, include=TRUE
## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
Script.Name <- "./GHI_enh_daily_test_plots.R"
tic <- Sys.time()

if (!interactive()) {
  pdf( file = paste0("./runtime/", basename(sub("\\.R$",".pdf", Script.Name))))
}


#+ echo=F, include=T
suppressPackageStartupMessages({
  library(Matrix)
  library(ggplot2       , quietly = TRUE, warn.conflicts = FALSE)
  # library(ggpmisc       , quietly = TRUE, warn.conflicts = FALSE)
  library(cowplot       , quietly = TRUE, warn.conflicts = FALSE)
  library(data.table  , quietly = TRUE, warn.conflicts = FALSE)
  require(zoo         , quietly = TRUE, warn.conflicts = FALSE)
  library(pander      , quietly = TRUE, warn.conflicts = FALSE)
  library(ggplot2     , quietly = TRUE, warn.conflicts = FALSE)
  library(RColorBrewer, quietly = TRUE, warn.conflicts = FALSE)
  library(pracma      , quietly = TRUE, warn.conflicts = FALSE)
  library(lubridate   , quietly = TRUE, warn.conflicts = FALSE)
})

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



## __ Execution control  -------------------------------------------------------
TEST <- FALSE
# TEST <- TRUE

if (TEST) {
  warning("\n\n ** Test is active!! ** \n\n")
  knitr::opts_chunk$set(dev        = "png"    )
}




##  Reset Randomness  ----------------------------------------------------------
RANDOM_SEED <- 333
set.seed(RANDOM_SEED)
## may need to reset seed in each randomness generation below








##  Estimate enhancement daily magnitude  --------------------------------------
enh_days <- DATA[get(SelEnhanc) == TRUE,
                 .(Enh_sum      = sum(GLB_ench, na.rm = TRUE),
                   Enh_max      = max(GLB_ench, na.rm = TRUE),
                   Enh_diff_sum = sum(GLB_diff, na.rm = TRUE),
                   Enh_diff_max = max(GLB_diff, na.rm = TRUE),
                   Enh_N        = sum(get(SelEnhanc))),
                 Day]


sunny_days <- DATA[, .(Sunshine = sum(TYPE == "Clear") / max(DayLength, na.rm = TRUE),
                       Energy   = sum(ClearnessIndex_kt, na.rm = TRUE)/sum(TYPE == "Clear"),
                       EC       = sum(get(SelEnhanc)),
                       Cloud    = sum(TYPE == "Cloud")),
                   by = Day]



## days with maximum values
setorder(enh_days, -Enh_diff_max)
maxenhd <- enh_days[1:400]

## strong total enhancement days
setorder(enh_days, -Enh_sum)
daylist <- enh_days[1:400]
daylist <- daylist[!Day %in% maxenhd$Day]
# enhsnd  <- daylist[sample(1:nrow(daylist), 300)]
enhsnd  <- daylist

## select some sunny days
sunnyd  <- sunny_days[Sunshine > 0.79 & Energy > 0.74]
sunnyd  <- sunnyd[!Day %in% maxenhd$Day & !Day %in% enhsnd$Day]
sunnyd  <- sunnyd[sample(1:nrow(sunnyd), 400, replace = T)]

## sunny with enhancements
sunnyenh <- sunny_days[Sunshine > 0.77 & Energy > 0.73 & EC > 0]
sunnyenh <- sunnyenh[!Day %in% maxenhd$Day & !Day %in% enhsnd$Day & !Day %in% sunnyd$Day]

## cloudy days
clouds <- sunny_days[Sunshine > 0.6 & Energy > 0.6 & EC > 2 & Cloud > 5]
clouds <- clouds[!Day %in% sunnyenh$Day & !Day %in% maxenhd$Day & !Day %in% enhsnd$Day & !Day %in% sunnyd$Day]
clouds <- clouds[sample(1:nrow(clouds), 400, replace = T)]

## some random days
# all_days <- data.table(Day = unique(DATA[, Day]))
# all_days <- all_days[!Day %in% sunnyenh$Day & !Day %in% maxenhd$Day & !Day %in% enhsnd$Day & !Day %in% sunnyd$Day & !Day %in% clouds]
# all_days <- all_days[sample(1:nrow(all_days), 300)]

## manual selection
testdays <- data.table(Day = c(
    "2000-07-14",
    "2007-07-06",
    "2013-05-27",
    "2016-08-29",
    "1999-06-24"
))


## __  Days with strong enhancement cases  -------------------------------------

#' \FloatBarrier
#'
#' # Plot some days with strong enhancement cases
#'
#+ example-days, echo=F, include=T, results="asis"

vec_days <- matrix(
    ##   Data      Description
    c("maxenhd",  "extreme cases day",
      "enhsnd",   "strong enhancement day",
      "sunnyd",   "sunny day",
      "sunnyenh", "sunny enhancement day",
      "clouds",   "cloudy day",
      # "all_days", "random day",
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
  daylist <- sort(temp$Day)

  gather_days <- rbind(gather_days,
                       data.frame(Day  = temp$Day,
                                  Type = vec_days$Descriprion[ii])
  )
}


gather_days <- gather_days[!duplicated(gather_days$Day), ]
setorder(gather_days, -Day)




##test
# daylist <- "2019-07-11"
if (FALSE) {
for (ii in 1:nrow(gather_days)) {

  aday <- gather_days[ii, "Day"]
  type <- gather_days[ii, "Type"]
  doy  <- yday(aday)

  temp <- DATA[Day == aday]
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
         c(        "GHI","CE threshold","TSI at TOA on horizontal plane","Solar Constant", "CE events","ECE events","Identified clouds", "Cloud-free"),
         col = c("green",         "red",                         "black",       "orange2","burlywood4",       "red",             "blue", "darkorchid"),
         pch = c(     NA,            NA,                              NA,              NA,          1 ,          1 ,                  3,           NA),
         lty = c(      1,             1,                               1,               1,          NA,          NA,                 NA,            1),
         lwd = c(      1,             1,                               1,               2,          NA,          NA,                 NA,            1),
         bty = "n",
         cex = 0.8
  )

  cat(' \n \n')
}
}
#+ echo=F, include=T


## presentation


library(ggplot2)
library(plotly)

## Print example day -----------------------------------------------------------
#'
#' ## Example day
#'
#+ P-example-day-present,  echo=F, include=T, fig.width=7, fig.height=6

## select day
example_day <- "1999-06-24"
# example_day <- "2007-05-24"
# example_day <- "2016-10-28"


DT_example <- DATA[Day == example_day, .(Date, wattGLB, TYPE, Enhanc_C_4, GLB_diff, TSI_OI = wattGLB - ETH)]

DT_example[  TSI_OI < 0,   TSI_OI := NA]
DT_example[GLB_diff < 0, GLB_diff := NA]

cat("Doy", unique(yday(DT_example$Date)), "\n\n")


## display sky cam images ---------


temp <- DATA[Day == example_day]

## add fave value for scale
temp <- rbind(temp, data.table(ETH = solar_constant), fill = T)

ylim <- range(0, temp$wattGLB, solar_constant, temp$ETH)

pp1 <- ggplot(data = temp, aes(x = Date)) +
  ## DATA lines
  geom_line(aes(y = wattGLB                        , colour = "GHI"                           ), linewidth = .9) +

  ## legend
  scale_colour_manual("",
                      guide = guide_legend(ncol = 1),
                      breaks = c("GHI",
                                 "Cloud-free",
                                 "CE Threshold",
                                 "TOA TSI on horiz. plane",
                                 "Solar Constant",
                                 "CE events",
                                 "ECE events",
                                 # "Identified clouds",
                                 "Sky camera photos"),
                      values = c("GHI"                     = "#317529",
                                 "CE Threshold"            = "#b00821" ,
                                 "TOA TSI on horiz. plane" = "black",
                                 # "Identified clouds"       = "#0000cd",
                                 "CE events"               = "#b00821",
                                 "ECE events"              = "#ff00ff",
                                 "Solar Constant"          = "orange2",
                                 "Cloud-free"              = "darkorchid",
                                 "Sky camera photos"       = "#ff652d")) +
  # guides(fill = guide_legend(ncol = 2)) +

  # labs(title = paste(as.Date(example_day, origin = "1970-01-01"))) +
  theme(
    #   legend.title         = element_text(size = 10),
    legend.position       = c(.200, .005),
    legend.justification  = c("left", "bottom"),
    # legend.box.just       = "right",
    legend.background     = element_blank(),
    # legend.spacing.y = unit(0, 'cm'),
    # legend.spacing.x = unit(.005, 'cm'),
    legend.box.background = element_rect(color = NA, fill = NA),
    legend.key            = element_blank(),
    legend.margin         = margin(1, 1, 1, 1),
    plot.title            = element_text(size = gg_text_size - 4,
                                         hjust = 0.5,
                                         face = "bold",
                                         margin = margin(0,0,0,0))
  ) +

  ## AXIS ##
  # scale_x_continuous(expand = expansion(mult = c(0.03, 0.03))) +
  ylim(ylim) +
  scale_y_continuous(breaks = seq(0, 1600, 200)) +
  ylab(bquote("GHI" ~ group("[", W/m^2,"]"))) +
  xlab(element_blank()) +
  theme(aspect.ratio = 0.8) +
  theme(
    panel.background = element_rect(fill   = "white",
                                    colour = "white")
  )


pp1

pp2 <- pp1 + geom_line(aes(y = get(paste0(csmodel,".glo")),
                           colour = "Cloud-free"),
                       linewidth = .8)
pp2

pp3 <- pp1 + geom_line(aes(y = get(paste0(SelEnhanc, "_ref")),
                           colour = "CE Threshold"),
                       linewidth = .8)
pp3

pp4 <- pp3 + geom_point(data = temp[get(SelEnhanc) == TRUE, ],
                        aes(y =  wattGLB, colour = "CE events"),
                        shape  = 21,
                        size   = 1.8,
                        stroke = 0.8)
pp4

pp5 <- pp4 + geom_line(aes(y = ETH,
                           colour = "TOA TSI on horiz. plane"),
                       linewidth = .8)
pp5

pp6 <- pp5 + geom_point(data = temp[get(SelEnhanc) == TRUE & wattGLB >= ETH, ],
                        aes(y =  wattGLB,
                            colour = "ECE events"),
                        shape  = 21,
                        size   = 1.8,
                        stroke = 0.8    )
pp6

pp7 <- pp6 + geom_hline(aes(yintercept = solar_constant,
                            colour     = "Solar Constant"),
                        linewidth      = 1.0)
pp7

# ggplotly(pp1)



#
# pp2 <- pp1 + theme(legend.position       = c(1, .5),
#                    legend.title          = element_blank(),
#                    legend.text           = element_text(size = 14),
#                    legend.justification  = c("right", "center"),
#                    legend.key.size       = unit(.4, "lines"))
#



#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (difftime(tac,tic,units = "sec") > 30) {
    system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
    system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'R script ended'"))
}
