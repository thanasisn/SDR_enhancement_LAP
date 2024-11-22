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
knitr::opts_chunk$set(dev        = c("pdf", "png"))
# knitr::opts_chunk$set(dev        = "png"    )
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
knitr::opts_chunk$set(fig.cap    = " - empty caption - " )
knitr::opts_chunk$set(cache      =  FALSE   )  ## !! breaks calculations
knitr::opts_chunk$set(fig.pos    = 'H'    )


#+ echo=FALSE, include=TRUE
## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
Script.Name <- "./GHI_enh_05_distributions.R"

if (!interactive()) {
  pdf( file = paste0("./runtime/", basename(sub("\\.R$", ".pdf", Script.Name))))
}



#+ echo=F, include=T
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(pander    , quietly = TRUE, warn.conflicts = FALSE)
library(purrr     , quietly = TRUE, warn.conflicts = FALSE)
library(grid      , quietly = TRUE, warn.conflicts = FALSE)
library(gridExtra , quietly = TRUE, warn.conflicts = FALSE)
library(ggplot2   , quietly = TRUE, warn.conflicts = FALSE)
library(patchwork , quietly = TRUE, warn.conflicts = FALSE)
library(cowplot)
library(gridGraphics)

panderOptions("table.alignment.default", "right")
panderOptions("table.split.table",        120   )

## __ Load external functions --------------------------------------------------
## Functions from `https://github.com/thanasisn/IStillBreakStuff/tree/main/FUNCTIONS/R`
source("~/CODE/FUNCTIONS/R/data.R")
source("~/CODE/FUNCTIONS/R/trig_deg.R")


## __ Source initial scripts ---------------------------------------------------
source("./GHI_enh_00_variables.R")
source("./GHI_enh_00_dictionary.R")
source("~/CODE/FUNCTIONS/R/linear_fit_stats.R")
source("~/CODE/FUNCTIONS/R/cor_test_stats.R")


## Override notification function
options(error = function() {
  if (interactive()) {
    system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
    system("notify-send -u normal -t 30000 'R session' 'An error occurred!'")
  }
})



##  Load Enhancement data  -----------------------------------------------------

## load statistics
load("./data/GHI_enh_03_process.Rda")
load("./data/GHI_enh_02_ID_CE.Rda")
tic  <- Sys.time()

DRAFT <- TRUE
DRAFT <- FALSE

cat("Reference mode:      ", csmodel,   "\n\n")
cat("Enhancemnet criteria:", SelEnhanc, "\n\n")






## Print example day -----------------------------------------------------------
#'
#' ## Example day
#'
#+ P-example-day,  echo=F, include=T, fig.width=7, fig.height=6

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


example_day

paste0(skycambase,"/",strftime(example_day, format = "%Y"))
strftime(example_day, format = "%j%Y.*.JPEG")

imglist <- list.files(path       = paste0(skycambase,"/",strftime(example_day, format = "%Y")),
                      pattern    = strftime(example_day, format = "%j%Y.*.JPEG"),
                      recursive  = T,
                      full.names = T)


basename(imglist) |> sub(pattern = " ", replacement = "", .)
basename(imglist) |> { gsub(pattern = "\\.", replacement="") }


# left  <- image_read("/home/folder/LAP_skycam/skycam/2019/1922019/ 1922019100002.JPEG")
left   <- image_read(paste0(skycambase, "/2019/1922019/ 1922019101501.JPEG"))
right  <- image_read(paste0(skycambase, "/2019/1922019/ 1922019103001.JPEG"))
left1  <- image_read(paste0(skycambase, "/2019/1922019/ 1922019093002.JPEG"))


    k

stop()



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



# calc_element(
#   "axis.text.x.bottom",
#   ggplot2:::plot_theme(ggplot_build(pp1)$plot)
# )$size
#
# calc_element(
#   "text",
#   ggplot2:::plot_theme(ggplot_build(pp1)$plot)
# )$size


#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (difftime(tac,tic,units = "sec") > 30) {
  system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
  system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'R script ended'"))
}

