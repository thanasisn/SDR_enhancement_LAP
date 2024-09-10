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
#'     keep_tex:        no
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
Script.Name <- "./GHI_enh_06_sza.R"
tic <- Sys.time()

if (!interactive()) {
  pdf( file = paste0("./runtime/",  basename(sub("\\.R$", ".pdf", Script.Name))))
}

#+ echo=F, include=T
library(data.table    , quietly = TRUE, warn.conflicts = FALSE)
library(pander        , quietly = TRUE, warn.conflicts = FALSE)
library(ggplot2       , quietly = TRUE, warn.conflicts = FALSE)
library(lmtest        , quietly = TRUE, warn.conflicts = FALSE)
library(viridis       , quietly = TRUE, warn.conflicts = FALSE)
library(ggpointdensity, quietly = TRUE, warn.conflicts = FALSE)
library(patchwork     , quietly = TRUE, warn.conflicts = FALSE)
library(ggh4x         , quietly = TRUE, warn.conflicts = FALSE)


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


##  Prepare raw data if needed  ------------------------------------------------
if (
  file.exists("./data/GHI_enh_03_process.Rda") == FALSE |
  file.mtime("./data/GHI_enh_03_process.Rda") < file.mtime("./GHI_enh_00_variables.R") |
  file.mtime("./data/GHI_enh_03_process.Rda") < file.mtime("./GHI_enh_03_process.R")
) {
  torun <- "./GHI_enh_03_process.R"
  cat(paste("Run previous step:", torun))
  source(torun)
  dummy <- gc()
}


##  Load Enhancement data  -----------------------------------------------------

## load statistics
load("./data/GHI_enh_03_process.Rda")
tic  <- Sys.time()

DRAFT <- TRUE



##  SZA enhancements  ---------------------------------------

#'
#' \newpage
#' \FloatBarrier
#'
#' ### SZA enhancements
#'
#+ sza, echo=F, include=T, results="asis"


avar <- "GLB_ench.sum"
plot(ST_E_sza[, get(avar), SZA],
     ylab = paste(varname(avar), staname(avar)),
     main = paste(varname(avar), staname(avar)))

avar <- "GLB_diff.sum"
plot(ST_E_sza[, get(avar), SZA],
     ylab = paste(varname(avar), staname(avar)),
     main = paste(varname(avar), staname(avar)))

avar <- "GLB_ench.N"
plot(ST_E_sza[, get(avar), SZA],
     ylab = paste(varname(avar), staname(avar)),
     main = paste(varname(avar), staname(avar)))


#'
#' \newpage
#' \FloatBarrier
#'
#' ### SZA enhancements by month
#'
#+ sza-month, echo=F, include=T, results="asis"

for (am in 1:12) {
  temp <- ST_E_sza_monthly[Month == am, ]
  xlim <- range(ST_E_sza_monthly[,SZA])

  avar <- "GLB_ench.sum"
  plot(temp[, get(avar), SZA],
       xlim = xlim,
       ylab = paste(varname(avar), staname(avar)),
       main = paste(month.name[am], varname(avar), staname(avar)))
}



for (am in 1:12) {
  temp <- ST_E_sza_monthly[Month == am, ]
  xlim <- range(ST_E_sza_monthly[,SZA])

  avar <- "GLB_diff.sum"
  plot(temp[, get(avar), SZA],
       xlim = xlim,
       ylab = paste(varname(avar), staname(avar)),
       main = paste(month.name[am], varname(avar), staname(avar)))
}



for (am in 1:12) {
  temp <- ST_E_sza_monthly[Month == am, ]
  xlim <- range(ST_E_sza_monthly[,SZA])

  avar <- "GLB_ench.N"
  plot(temp[, get(avar), SZA],
       xlim = xlim,
       ylab = paste(varname(avar), staname(avar)),
       main = paste(month.name[am], varname(avar), staname(avar)))
}



#'
#' \newpage
#' \FloatBarrier
#'
#' ### SZA heatmaps of DOY and SZA
#'
#+ sza-doy, echo=F, include=T, results="asis"

lim <- 10
ST_E_sza_doy[ GLB_diff.N > lim] |>
  ggplot(aes(Doy, SZA, fill = GLB_diff.N)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "black",
                      # trans = "log"
  ) +
  theme(legend.position      = c(0.99, 0.01),
        legend.justification = c(1, 0)) +
  theme(legend.background    = element_rect(fill = "white", colour = NA)) +
  labs(title = "Number of CE") +
  if(lim > 0) labs(caption = paste("Values under", lim, "are excluded for clarity"))




lim <- 100
ST_E_sza_doy[ GLB_diff.max > lim] |>
  ggplot(aes(Doy, SZA, fill = GLB_diff.max)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "black") +
  theme(legend.position      = c(0.99, 0.01),
        legend.justification = c(1, 0)) +
  theme(legend.background    = element_rect(fill = "white", colour = NA)) +
  labs(title = "Maximum OI") +
  if(lim > 0) labs(caption = paste("Values under", lim, "are excluded for clarity"))




lim <- 50
ST_E_sza_doy[ GLB_diff.sum > lim] |>
  ggplot(aes(Doy, SZA, fill = GLB_diff.sum)) +
  geom_tile() +
  # scale_fill_viridis_c()
  scale_fill_gradient(low = "yellow", high = "red", na.value = "black") +
  theme(legend.position      = c(0.99, 0.01),
        legend.justification = c(1, 0)) +
  theme(legend.background    = element_rect(fill = "white", colour = NA)) +
  labs(title = "Total OI energy") +
  if(lim > 0) labs(caption = paste("Values under", lim, "are excluded for clarity"))




lim <- 80
ST_E_sza_doy[ GLB_diff.mean > lim] |>
  ggplot( aes(Doy, SZA, fill = GLB_diff.mean)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "black") +
  theme(legend.position      = c(0.99, 0.01),
        legend.justification = c(1, 0)) +
  theme(legend.background    = element_rect(fill = "white", colour = NA)) +
  labs(title = "Mean OI") +
  if(lim > 0) labs(caption = paste("Values under", lim, "are excluded for clarity"))



#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (difftime(tac,tic,units = "sec") > 31) {
  system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
  system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'R script ended'"))
}
