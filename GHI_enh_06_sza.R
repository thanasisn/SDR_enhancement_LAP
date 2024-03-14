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


## __ Document options ---------------------------------------------------------

#+ echo=F, include=F
knitr::opts_chunk$set(comment    = ""       )
knitr::opts_chunk$set(dev        = c("pdf", "png"))
# knitr::opts_chunk$set(dev        = "png"    )
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
knitr::opts_chunk$set(cache      =  FALSE   )  ## !! breaks calculations
# knitr::opts_chunk$set(fig.pos    = '!h'    )


#+ include=F, echo=F
## __ Set environment ----------------------------------------------------------
Sys.setenv(TZ = "UTC")
Script.Name <- "./GHI_enh_06_sza.R"

if (!interactive()) {
  pdf( file = paste0("./runtime/",  basename(sub("\\.R$", ".pdf", Script.Name))))
  sink(file = paste0("./runtime/",  basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
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
#+ sza_month, echo=F, include=T, results="asis"

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



# Heatmap
ggplot(ST_E_sza_doy, aes(Doy, SZA, fill = GLB_diff.N)) +
  geom_tile() +
  # scale_fill_viridis_c()
  scale_fill_gradient(low = "yellow", high = "red", na.value = "black",
                      trans = "log") +
  theme(legend.position      = c(0.99, 0.01),
        legend.justification = c(1, 0)) +
  theme(legend.background    = element_rect(fill = "white", colour = NA)) +
  labs(title = "Number of CE")



ggplot(ST_E_sza_doy, aes(Doy, SZA, fill = GLB_diff.max)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "black") +
  theme(legend.position      = c(0.99, 0.01),
        legend.justification = c(1, 0)) +
  theme(legend.background    = element_rect(fill = "white", colour = NA)) +
  labs(title = "Maximum over irradiance")


ggplot(ST_E_sza_doy, aes(Doy, SZA, fill = GLB_diff.sum)) +
  geom_tile() +
  # scale_fill_viridis_c()
  scale_fill_gradient(low = "yellow", high = "red", na.value = "black") +
  theme(legend.position      = c(0.99, 0.01),
        legend.justification = c(1, 0)) +
  theme(legend.background    = element_rect(fill = "white", colour = NA)) +
  labs(title = "Total over irradiance energy")


ggplot(ST_E_sza_doy, aes(Doy, SZA, fill = GLB_diff.mean)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "black") +
  theme(legend.position      = c(0.99, 0.01),
        legend.justification = c(1, 0)) +
  theme(legend.background    = element_rect(fill = "white", colour = NA)) +
  labs(title = "Mean over irradiance")



##TODO check groups for low sun characteristics

#
# gr_N_min   <- 8
# gr_SZA_min <- 60
#
# test <- ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min]
#
# plot( ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min, GLB_diff.sum/GLB_ench.N, SZA.mean ])
#
# plot( ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min, GLB_diff.mean, SZA.mean ])
#
# plot( ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min, GLB_diff.median, SZA.mean ])
#
# plot( ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min, GLB_diff.sum/GLB_ench.N, SZA.max ])
#
# plot( ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min, GLB_diff.sum/GLB_ench.N, SZA.min ])
#
# plot( ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min, GLB_diff.sum/GLB_ench.N, SZA.mean])
#
# plot( ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min
#              & SZA.min > 72 & GLB_diff.sum/GLB_ench.N < 10 , GLB_diff.sum/GLB_ench.N, SZA.mean])
# ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min
# & SZA.min > 72 & GLB_diff.sum/GLB_ench.N < 10  ]
#
# hist( ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min, GLB_diff.sum/GLB_ench.N])
#
# plot( ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min, GLB_ench.max, SZA.mean ])
#
#
# plot( ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min, GLB_ench.mean, SZA.mean ])
# plot( ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min, GLB_diff.mean, SZA.mean ])
# plot( ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min, GLB_diff.max,  SZA.max  ])
#
# ST_G0[as.Date(Date) == "2004-05-25"]
#
# ST_G0[as.Date(Date) == "2003-09-05"]
# ST_G0[as.Date(Date) == "2003-09-05", GLB_diff.sum/GLB_ench.N]


# test <- DATA[as.Date(Date) == "2004-05-25"& GLB_diff>0]
#
# plot(DATA[as.Date(Date) == "2004-05-25", GLB_diff, Date])


## group fix
# test <- DATA[as.Date(Date) == "2004-05-25"]



#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (interactive() & difftime(tac,tic,units = "sec") > 30) {
  system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
  system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'R script ended'"))
}
