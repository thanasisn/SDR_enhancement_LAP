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
Script.Name <- "./GHI_enh_02_ID_CE.R"
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



## __ Execution control  -------------------------------------------------------
TEST <- TRUE
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
         c(  "GHI","CE threshold","TSI at TOA on horizontal plane","Solar Constant", "CE events","ECE events","Identified clouds",  "Cloud-free")
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


## presentastion
## Print example day -----------------------------------------------------------
#'
#' ## Example day
#'
#+ P-example-day-pre,  echo=F, include=T, fig.width=7, fig.height=6
## select day
example_day <- "2019-07-11"

DT_example <- DATA[Day == example_day, .(Date, wattGLB, TYPE, Enhanc_C_4, GLB_diff, TSI_OI = wattGLB - ETH)]

DT_example[  TSI_OI < 0,   TSI_OI := NA]
DT_example[GLB_diff < 0, GLB_diff := NA]

cat("Doy", unique(yday(DT_example$Date)), "\n\n")

pander::pander(DT_example)


## display sky cam images ---------


library(magick)

if (Sys.info()["nodename"] == "tyler") {
  skycambase <- "/home/folder/LAP_skycam/skycam/"
} else {
  skycambase <- "/home/single/LAP_skycam/skycam/"
}


# left  <- image_read("/home/folder/LAP_skycam/skycam/2019/1922019/ 1922019100002.JPEG")
left   <- image_read(paste0(skycambase, "/2019/1922019/ 1922019101501.JPEG"))
right  <- image_read(paste0(skycambase, "/2019/1922019/ 1922019103001.JPEG"))
left1  <- image_read(paste0(skycambase, "/2019/1922019/ 1922019093002.JPEG"))


# print(left)


# image_info(left)

ll1 <- image_scale(
  image_crop(left1,
             geometry = "960x1200+180+0"),
  "600")
ll <- image_scale(
  image_crop(left,
             geometry = "960x1200+180+0"),
  "600")
rr <- image_scale(
  image_crop(right,
             geometry = "960x1200+180+0"),
  "600")


A <- image_ggplot(ll1) +
  annotate(geom = "text", x = 10, y = 15,  label = "(a)",       hjust = 0, vjust = 0, size =  4, colour = "#ff652d", fontface = "bold") +
  annotate(geom = "text", x = 15, y = 585, label = "09:30 UTC", hjust = 0, vjust = 1, size =  4, colour = "#ff652d", fontface = "bold")
date_A <- as.POSIXct(strptime(paste(example_day,   "09:30"), "%F %H:%M"))


B <- image_ggplot(ll) +
  annotate(geom = "text", x = 10, y = 15,  label = "(b)",       hjust = 0, vjust = 0, size =  4, colour = "#ff652d", fontface = "bold") +
  annotate(geom = "text", x = 15, y = 585, label = "10:15 UTC", hjust = 0, vjust = 1, size =  4, colour = "#ff652d", fontface = "bold")
date_B <- as.POSIXct(strptime(paste(example_day,   "10:15"), "%F %H:%M"))


C <- image_ggplot(rr) +
  annotate(geom = "text", x = 10, y = 15,  label = "(c)",       hjust = 0, vjust = 0, size =  4, colour = "#ff652d", fontface = "bold") +
  annotate(geom = "text", x = 15, y = 585, label = "10:30 UTC", hjust = 0, vjust = 1, size =  4, colour = "#ff652d", fontface = "bold")
date_C <- as.POSIXct(strptime(paste(example_day,   "10:30"), "%F %H:%M"))

grid.arrange(A, B, C, nrow = 1)






temp <- DATA[Day == example_day]

# par(mar = c(4, 4, 1, 1))
# par(cex       = 0.7
#     # cex.main = 0.8, #change font size of title
#     # cex.sub  = 0.8,  #change font size of subtitle
#     # cex.lab  = 0.8, #change font size of axis labels
#     # cex.axis = 0.8,
#     ) #change font size of axis text
#
# ylim <- range(0, temp$ETH, temp$wattGLB, solar_constant, na.rm = TRUE)
#
# plot(temp$Date, temp$wattGLB, col = "green",
#      pch  = ".", cex = 2,
#      ylim = ylim,
#      ylab = bquote("GHI" ~ group("[", W/m^2,"]")),
#      xlab = "Time (UTC)")
#
# ## mark photos
# abline(v = date_A, col = "grey", lwd = 2, lty = 2)
# abline(v = date_B, col = "grey", lwd = 2, lty = 2)
# abline(v = date_C, col = "grey", lwd = 2, lty = 2)
#
# text(x = date_A, y = 250, "(a)", pos = 2, offset = 0.2, col = "gray", cex = 0.8)
# text(x = date_B, y = 250, "(b)", pos = 2, offset = 0.2, col = "gray", cex = 0.8)
# text(x = date_C, y = 250, "(c)", pos = 4, offset = 0.2, col = "gray", cex = 0.8)
#
# abline(h = solar_constant, col = "orange2", lty = 1, lwd = 2)
#
# ## Global
# lines(temp$Date, temp$wattGLB, col = "green")
#
# ## TSI on ground
# lines(temp$Date, temp$ETH)
#
# ## Active model reference
# lines(temp[, get(paste0(SelEnhanc, "_ref")), Date], col = "red" )
#
# ## Cloud-free ref
# lines(temp[, get(paste0(csmodel,".glo")), Date], col = "darkorchid" )
#
#
# ## Enchantment cases
# points(temp[get(SelEnhanc) == TRUE & wattGLB <  ETH, wattGLB, Date], col = "burlywood4")
# points(temp[get(SelEnhanc) == TRUE & wattGLB >= ETH, wattGLB, Date], col = "red")
#
#
# ## Cloud cases
# points(temp[TYPE == "Cloud", wattGLB, Date], col = "blue", pch = 3, cex = 0.3)
#
# ## Decorations
# # title(main = paste(as.Date(aday, origin = "1970-01-01"), temp[get(SelEnhanc) == TRUE, .N], temp[TYPE == "Cloud", .N], vec_days$Descriprion[ii]))
# title(main = paste(as.Date(example_day, origin = "1970-01-01")))
#
# legend("bottom", ncol = 2,
#        c(  "GHI","CE threshold","TOA TSI on horiz. plane             ","Solar Constant", "CE events   ","ECE events     ","Identified clouds    ",  "Cloud-free    "),
#        col = c("green",         "red",                  "black",       "orange2","burlywood4",       "red",             "blue","darkorchid"),
#        pch = c(     NA,            NA,                       NA,              NA,          1 ,          1 ,                  3,           NA),
#        lty = c(      1,             1,                        1,               1,          NA,          NA,                 NA,            1),
#        lwd = c(      1,             1,                        1,               2,          NA,          NA,                 NA,            1),
#        bty = "n",
#        cex = 0.8
# )
#
#
# ## store base plot
# p1 <- recordPlot()
#
#
# bt <- grid.arrange(A, B, C, nrow = 1)
#
# merg <- plot_grid(
#   p1, bt,
#   nrow = 2,
#   rel_heights = c(3,1)
# )
#
# print(merg)
#
# merg + theme(aspect.ratio = 1)



## redo example in ggplot !!!---------


pp1 <- ggplot(data = temp, aes(x = Date)) +
  ## DATA lines
  geom_line(aes(y = wattGLB                        , colour = "GHI"                           )) +
  geom_line(aes(y = get(paste0(SelEnhanc, "_ref")) , colour = "CE Threshold"                  ), linewidth = .8) +
  geom_line(aes(y = get(paste0(csmodel,".glo"))    , colour = "Cloud-free"                    ), linewidth = .8) +
  geom_line(aes(y = ETH                            , colour = "TOA TSI on horiz. plane"       ), linewidth = .8) +
  ## constant liens
  geom_hline(aes(yintercept = solar_constant       , colour = "Solar Constant"), linewidth = 1.0) +
  geom_vline(aes(xintercept = date_A), linetype = "longdash", linewidth = .6, color = "#ff652d") +
  annotate(geom = "text", x = date_A - 300, y = 100, label = "(a)", hjust = 1, color = "#ff652d") +
  geom_vline(aes(xintercept = date_B), linetype = "longdash", linewidth = .6, color = "#ff652d") +
  annotate(geom = "text", x = date_B - 300, y = 100, label = "(b)", hjust = 1, color = "#ff652d") +
  geom_vline(aes(xintercept = date_C, color = "Sky camera photos"), linetype = "longdash", linewidth = .6) +
  annotate(geom = "text", x = date_C + 300, y = 100, label = "(c)", hjust = 0, color = "#ff652d") +
  ## data points
  geom_point(data = temp[TYPE == "Cloud"],
             aes(y =  wattGLB, colour = "Identified clouds"), shape = 3, size = 0.9                   ) +
  geom_point(data = temp[get(SelEnhanc) == TRUE & wattGLB <  ETH, ],
             aes(y =  wattGLB, colour = "CE events"),         shape = 21, size = 1.8, stroke = 0.8    ) +
  geom_point(data = temp[get(SelEnhanc) == TRUE & wattGLB >= ETH, ],
             aes(y =  wattGLB, colour = "ECE events"),        shape = 21, size = 1.8, stroke = 0.8    ) +
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
                                 "Identified clouds",
                                 "Sky camera photos"),
                      values = c("GHI"                     = "#317529",
                                 "CE Threshold"            = "#b00821" ,
                                 "TOA TSI on horiz. plane" = "black",
                                 "Identified clouds"       = "#0000cd",
                                 "CE events"               = "#b00821",
                                 "ECE events"              = "#ff00ff",
                                 "Solar Constant"          = "orange2",
                                 "Cloud-free"              = "darkorchid",
                                 "Sky camera photos"       = "#ff652d")) +
  # guides(fill = guide_legend(ncol = 2)) +

  labs(title = paste(as.Date(example_day, origin = "1970-01-01"))) +
  theme(
    #   legend.title         = element_text(size = 10),
    legend.position       = c(.995, .005),
    legend.justification  = c("right", "bottom"),
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
  scale_y_continuous(breaks = seq(0, 1600, 200)) +
  ylab(bquote("GHI" ~ group("[", W/m^2,"]"))) +
  xlab(bquote("Time (UTC)"))
pp1


# pp1 + theme(legend.title = element_text(size = 10),
#             legend.text  = element_text(size = 10),
#             legend.key.size = unit(.9, "lines"))
#
#
# pp2 <- pp1 + theme(legend.position = "bottom",
#                    legend.key.size = unit(.5, "lines"))

pp2 <- pp1 + theme(legend.position       = c(1, .5),
                   legend.title          = element_blank(),
                   legend.text           = element_text(size = 14),
                   legend.justification  = c("right", "center"),
                   legend.key.size       = unit(.4, "lines"))


# merg <- plot_grid(
#   pp2, bt,
#   nrow = 2,
#   rel_heights = c(3,1)
# )
#
# print(merg)



## seperate legend
# theme(legend.box.margin = margin(0, 0, 0, 12))

legend <- get_legend(pp2)
# legend <- legend + theme(guide = guide_legend(ncol = 1))


## prepare second row
prow <- plot_grid(
  A,
  B,
  C,
  legend,
  nrow = 1,
  rel_widths = c(1,1,1, 1.5)
)
# prow

## create complete figure
mergln <- plot_grid(
  pp2 + theme(legend.position = "none"),
  prow,
  nrow = 2,
  rel_heights = c(3, 1.2)
)
show(mergln)


# mergln + theme(aspect.ratio = 1)

mergln + theme(aspect.ratio = 0.8)




#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (difftime(tac,tic,units = "sec") > 30) {
    system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
    system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'R script ended'"))
}
