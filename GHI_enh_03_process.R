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
Script.Name <- "./GHI_enh_03_process.R"

if (!interactive()) {
    pdf( file = paste0("./runtime/",  basename(sub("\\.R$",".pdf", Script.Name))))
    sink(file = paste0("./runtime/",  basename(sub("\\.R$",".out", Script.Name))), split = TRUE)
}


#+ echo=F, include=T
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
require(zoo       , quietly = TRUE, warn.conflicts = FALSE)
library(pander    , quietly = TRUE, warn.conflicts = FALSE)
library(ggplot2   , quietly = TRUE, warn.conflicts = FALSE)

panderOptions("table.alignment.default", "right")
panderOptions("table.split.table",        120   )

## __ Load external functions --------------------------------------------------
## Functions from `https://github.com/thanasisn/IStillBreakStuff/tree/main/FUNCTIONS/R`
source("~/CODE/FUNCTIONS/R/data.R")
source("~/CODE/FUNCTIONS/R/trig_deg.R")


## __ Source initial scripts ---------------------------------------------------
# source("./DHI_GHI_0_data_input.R")
source("./GHI_enh_00_variables.R")
source("./GHI_enh_00_dictionary.R")


## Overide notification function
options(error = function() {
    if (interactive()) {
        system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
        system("notify-send -u normal -t 30000 'R session' 'An error occurred!'")
    }
})


##  Prepare raw data if needed  ------------------------------------------------
if (
    file.exists("data/GHI_enh_02_ID_CE.Rda") == FALSE |
    file.mtime("data/GHI_enh_02_ID_CE.Rda") < file.mtime("./GHI_enh_00_variables.R") |
    file.mtime("data/GHI_enh_02_ID_CE.Rda") < file.mtime("./GHI_enh_02_ID_CE.R")
) {
    source("./GHI_enh_02_ID_CE.R")
    dummy <- gc()
}


##  Load Enhancement data  -----------------------------------------------------
load("data/GHI_enh_02_ID_CE.Rda")
DATA[, DOY := yday(Date)]
tic  <- Sys.time()


# TODO -------------------------------------------------------------------------
# k clastering Vamvakas2020
# Stats on groups
# Stats on enhancement cases
# R-R analysis
# Seasonal occurrence
# Tapakis2014 plots and stats
# Different criteria

#'
#'  Alpha * HAU is CS_ref
#'
#+ include=FALSE, echo=FALSE
alpha <- 0.9653023236718680788471



## __ Enhancement criteria  ------------------------------------------------
SelEnhanc <- "Enhanc_C_1"
# SelEnhanc <- "Enhanc_C_2"
# SelEnhanc <- "Enhanc_C_3"



##;  #
##;  # Note from editor
##;  #
##;  # > In the future, it is interesting to analyze time series of downward
##;  # > shortwave irradiance anomalies derived from the data set. You need to
##;  # > compute monthly mean irradiances and climatological mean for January,
##;  # > February, March etc. Then the corresponding climatological mean is
##;  # > subtracted form each monthly mean to derive monthly anomaly. It is
##;  # > interesting to see how these anomalies change with time. In this way,
##;  # > you are not just focusing on enhancement of surface irradiance, but
##;  # > also including reduction of surface irradiance by clouds and
##;  # > aerosols.
##;  # >
##;  #
##;
##;
##;  #'
##;  #'
##;  #' ## Data and Methodology
##;  #'
##;  #' Measurements of solar shortwave global horizontal irradiance (GHI) and direct normal irradiance (DNI) are performed simultaneously since 2016 in Thessaloniki, Greece, respectively with a CM-21 pyranometer and a CHP-1 pyrheliometer both by Kipp & Zonen. A data quality assurance procedure was applied on these data based on methods proposed by
##;  #' Long and Shi\ [-@long_automated_2008; -@Long2006],
##;  #' which were adjusted for the specific site. Only data characterized with acceptable quality was used. We are using, for the global radiation reference, the Haurwitz’s model (reference). The selection was done with data of GHI and DNI for the period 2016 – 2021 using the iterative method of optimizing the ‘Clear sky’ identification method, as proposed by
##;  #' @Long2000 and @Reno2016.
##;  #' We have calibrated the above method for our site. Among the eight simple models (Daneshyar–Paltridge–Proctor, Kasten–Czeplak, Haurwitz, Berger–Duffie, Adnot–Bourges–Campana–Gicquel, Robledo-Soler, Kasten and Ineichen-Perez), as described in
##;  #' @Reno2012 and tested by
##;  #' @Reno2016,
##;  #' we found the best result with an adjusted Haurwitz model (A-HAU)
##;  #' (Eq. \@ref(eq:ahau)),
##;  #' using as the main selection criterion the root mean squared error (RMSE).
##;  #'
##;  #' The enhancement cases for the 1-minute measurements of GHI were identified when the following conditions were met:
##;  #' a) Sun elevation angle above $`r min_elevation`^\circ$,
##;  #' b) GHI values above $`r ampl` \times \text{A-HAU} + `r GLB_diff_THRES`$ ($\text{GHI}_\text{Threshold}$), and
##;  #' c) Clearness index $k_t > `r Clearness_Kt_THRES`$.
##;  #' These criteria have been used in previous studies
##;  #' (e.g., @Vamvakas2020).
##;  #' An example of this procedure is given for the 2017&#8209;04&#8209;08 in Fig.\ (\@ref(fig:dayexample)),
##;  #' where the enhancement cases and the role of the other physical quantities are visualized.
##;  #'
##;  #' \begin{equation}
##;  #' \text{GHI}_\text{Clear Sky} = `r signif(gather_results$alpha[gather_results$CS_models=="HAU"],digits = 3 )` \times 1098 \times \cos( \text{SZA} ) \times \exp \left( \frac{ - 0.057}{\cos(\text{SZA})} \right)  (\#eq:ahau)
##;  #' \end{equation}
##;  #'
##;

##  Enhancement cases statistics  ----------------------------------------------




##  Stats on enhancement cases  ------------------------------------------------
# DATA_Enh <- DATA[get(SelEnhanc) == TRUE ]



## _ Stats functions  ----------------------------------------------------------

## stats for all data
data.summary <- function(x, na.rm = FALSE)
    list(
        mean   = mean  (x, na.rm = na.rm),
        SD     = sd    (x, na.rm = na.rm),
        max    = max   (x, na.rm = na.rm),
        min    = min   (x, na.rm = na.rm),
        median = median(x, na.rm = na.rm),
        sum    = sum   (x, na.rm = na.rm),
        sumPOS = sum(x[which(x>0)], na.rm = na.rm),  ## not meaningful for a subset
        sumNEG = sum(x[which(x<0)], na.rm = na.rm),  ## not meaningful for a subset
        N      = sum(!is.na(x)),
        TotalN = length(x)                           ## not meaningful for a subset
            )

## stats for a subset of data
enhanc.summary <- function(x, na.rm = FALSE)
    list(
        mean   = mean  (x, na.rm = na.rm),
        SD     = sd    (x, na.rm = na.rm),
        max    = max   (x, na.rm = na.rm),
        min    = min   (x, na.rm = na.rm),
        median = median(x, na.rm = na.rm),
        sum    = sum   (x, na.rm = na.rm),
        N      = sum(!is.na(x))
    )

## Stats for columns
my.cols <- c("wattGLB",
             "GLB_ench",
             "GLB_diff")

ST_total <- DATA[, unlist(lapply(.SD, data.summary, na.rm = TRUE),
                        recursive = FALSE),
               .SDcols = my.cols]


# _ Daily stats  ---------------------------------------------------------------

## stats on all data
ST_daily <- DATA[, unlist(lapply(.SD, data.summary, na.rm = FALSE),
                          recursive = FALSE),
                 .SDcols = my.cols,
                 by = .(Date = Day)]
ST_daily[, yts := (year(Date) - min(year(Date))) + ( yday(Date) - 1 ) / Hmisc::yearDays(Date)]


## stats on extreme enhancement cases
ST_extreme_daily <- DATA[wattGLB > ETH,
                   unlist(lapply(.SD, data.summary, na.rm = TRUE),
                          recursive = FALSE),
                   .SDcols = my.cols,
                   by = .(Date = Day)]
ST_extreme_daily[, yts := (year(Date) - min(year(Date))) + ( yday(Date) - 1 ) / Hmisc::yearDays(Date)]


## stats on enhancement cases
ST_E_daily <- DATA[get(SelEnhanc) == TRUE,
                   unlist(lapply(.SD, enhanc.summary, na.rm = FALSE),
                          recursive = FALSE),
                   .SDcols = my.cols,
                   by = .(Date = Day)]
ST_E_daily[, yts := (year(Date) - min(year(Date))) + ( yday(Date) - 1 ) / Hmisc::yearDays(Date)]


## climatology daily
ST_E_daily_seas <- DATA[get(SelEnhanc) == TRUE,
                   unlist(lapply(.SD, enhanc.summary, na.rm = FALSE),
                          recursive = FALSE),
                   .SDcols = my.cols,
                   by = DOY]
ST_E_daily_seas[, yts := DOY ] ## just for convience of programming


for (avar in grep("^DOY$", names(ST_E_daily_seas), value = T, invert = T) ) {
    plot(ST_E_daily_seas[, get(avar), DOY],
         ylab = avar)
    title(paste("ST_E_daily_seas", avar))
}


# _ Monthly stats  -------------------------------------------------------------


## stats on all data
ST_monthly          <- DATA[, unlist(lapply(.SD, data.summary, na.rm = FALSE),
                                     recursive = FALSE),
                            .SDcols = my.cols,
                            by = .(year(Date), month(Date))]
ST_monthly$Date     <- as.POSIXct(strptime(paste(ST_monthly$year, ST_monthly$month, "1"),"%Y %m %d"))
ST_monthly[, yts := (year(Date) - min(year(Date))) + ( yday(Date) - 1 ) / Hmisc::yearDays(Date)]


## stats on extreme enhancement cases
ST_extreme_monthly <- DATA[wattGLB > ETH,
                           unlist(lapply(.SD, data.summary, na.rm = TRUE),
                                  recursive = FALSE),
                           .SDcols = my.cols,
                           by = .(year(Date), month(Date))]
ST_extreme_monthly$Date <- as.POSIXct(strptime(paste(ST_extreme_monthly$year, ST_extreme_monthly$month, "1"),"%Y %m %d"))
ST_extreme_monthly[, yts := (year(Date) - min(year(Date))) + ( yday(Date) - 1 ) / Hmisc::yearDays(Date)]



## stats on enhancement cases
ST_E_monthly <- DATA[get(SelEnhanc) == TRUE,
                     unlist(lapply(.SD, enhanc.summary, na.rm = FALSE),
                            recursive = FALSE),
                     .SDcols = my.cols,
                     by = .(year(Date), month(Date))]
ST_E_monthly$Date <- as.POSIXct(strptime(paste(ST_E_monthly$year, ST_E_monthly$month, "1"),"%Y %m %d"))
ST_E_monthly[, yts := (year(Date) - min(year(Date))) + ( yday(Date) - 1 ) / Hmisc::yearDays(Date)]


## monthly climatology
ST_E_monthly_seas <- DATA[get(SelEnhanc) == TRUE,
                     unlist(lapply(.SD, enhanc.summary, na.rm = FALSE),
                            recursive = FALSE),
                     .SDcols = my.cols,
                     by = .(month(Date))]


for (avar in grep("^month$", names(ST_E_monthly_seas), value = T, invert = T) ) {
    plot(ST_E_monthly_seas[, get(avar), month],
         ylab = avar)
    title(paste("ST_E_monthly_seas", avar))
}




# _ Yearly stats  --------------------------------------------------------------

## stats on all data
ST_yearly <- DATA[, unlist(lapply(.SD, data.summary, na.rm = TRUE),
                            recursive = FALSE),
                   .SDcols = my.cols,
                   by = .(year(Date))]
ST_yearly$Date <- as.POSIXct(strptime(paste(ST_yearly$year, "01", "1"),"%Y %m %d"))


## stats on extreme enhancement cases
ST_extreme_yearly <- DATA[wattGLB > ETH,
                           unlist(lapply(.SD, data.summary, na.rm = TRUE),
                                  recursive = FALSE),
                           .SDcols = my.cols,
                           by = .(year(Date))]
ST_extreme_yearly$Date <- as.POSIXct(strptime(paste(ST_extreme_yearly$year, "01", "1"),"%Y %m %d"))


## stats on enhancement cases
ST_E_yearly <- DATA[get(SelEnhanc) == TRUE,
                     unlist(lapply(.SD, enhanc.summary, na.rm = TRUE),
                            recursive = FALSE),
                     .SDcols = my.cols,
                     by = .(year(Date), month(Date))]
ST_E_yearly <- as.POSIXct(strptime(paste(ST_E_yearly$year, "01", "1"),"%Y %m %d"))








# _ SZA stats  -----------------------------------------------------------------


## stats on all data
ST_sza <- DATA[, unlist(lapply(.SD, data.summary, na.rm = TRUE),
                        recursive = FALSE),
               .SDcols = my.cols,
               by = .(SZA = (SZA - SZA_BIN / 2 ) %/% SZA_BIN)]

for (avar in grep("^SZA$", names(ST_sza), value = T, invert = T) ) {
    plot(ST_sza[, get(avar), SZA],
         ylab = avar)
    title(paste("ST_sza", avar))
}

## stats on extreme enhancement cases
ST_extreme_SZA <- DATA[wattGLB > ETH,
                           unlist(lapply(.SD, enhanc.summary, na.rm = TRUE),
                                  recursive = FALSE),
                           .SDcols = my.cols,
                           by = .(SZA = (SZA - SZA_BIN / 2 ) %/% SZA_BIN)]


## stats on enhancement cases
ST_E_sza <- DATA[get(SelEnhanc) == TRUE,
                 unlist(lapply(.SD, enhanc.summary, na.rm = FALSE),
                        recursive = FALSE),
                 .SDcols = my.cols,
                 by = .(SZA = (SZA - SZA_BIN / 2 ) %/% SZA_BIN)]





## Quarter of year with one month shift to include December in the next years winter
DATA[,       season_Yqrt := as.yearqtr(as.yearmon(paste(year(Date), month(Date), sep = "-")) + 1/12)]
ST_daily[,   season_Yqrt := as.yearqtr(as.yearmon(paste(year(Date), month(Date), sep = "-")) + 1/12)]
ST_monthly[, season_Yqrt := as.yearqtr(as.yearmon(paste(year(Date), month(Date), sep = "-")) + 1/12)]

## Flag seasons using quarters
DATA[season_Yqrt %% 1 == 0   , Season := "Winter"]
DATA[season_Yqrt %% 1 == 0.25, Season := "Spring"]
DATA[season_Yqrt %% 1 == 0.50, Season := "Summer"]
DATA[season_Yqrt %% 1 == 0.75, Season := "Autumn"]
ST_daily[season_Yqrt %% 1 == 0   , Season := "Winter"]
ST_daily[season_Yqrt %% 1 == 0.25, Season := "Spring"]
ST_daily[season_Yqrt %% 1 == 0.50, Season := "Summer"]
ST_daily[season_Yqrt %% 1 == 0.75, Season := "Autumn"]
ST_monthly[season_Yqrt %% 1 == 0   , Season := "Winter"]
ST_monthly[season_Yqrt %% 1 == 0.25, Season := "Spring"]
ST_monthly[season_Yqrt %% 1 == 0.50, Season := "Summer"]
ST_monthly[season_Yqrt %% 1 == 0.75, Season := "Autumn"]


# - by season
# - by season_yqrt

##TODO create deseasonal data

#
#
# CONF_INTERV <- .95
# conf_param  <- 1 - (1 - CONF_INTERV) / 2
# suppressWarnings({
#     Enh_sza[,   Ench_EM := qt(conf_param, df = N - 1) * sd_Ench / sqrt(N)]
#     Enh_daily[, Ench_EM := qt(conf_param, df = N - 1) * sd_Ench / sqrt(N)]
#     Enh_yearly[,Ench_EM := qt(conf_param, df = N - 1) * sd_Ench / sqrt(N)]
#     Enh_total[, Ench_EM := qt(conf_param, df = N - 1) * sd_Ench / sqrt(N)]
# })
#
#
# ## Make values relative ####
# Enh_yearly[, N_att        := 100 * (N - mean(N))/mean(N)]
# Enh_yearly[, sum_Ench_att := 100 * (sum_Ench - mean(sum_Ench))/mean(sum_Ench)]
# Enh_yearly[, Ench_intesit :=        sum_Ench / N ]
#
# Enh_daily[, N_att        := 100 * (N - mean(N))/mean(N)]
# Enh_daily[, sum_Ench_att := 100 * (sum_Ench - mean(sum_Ench))/mean(sum_Ench)]
# Enh_daily[, Ench_intesit :=        sum_Ench / N ]
#
# Enh_monthly[, N_att        := 100 * (N - mean(N))/mean(N)]
# Enh_monthly[, sum_Ench_att := 100 * (sum_Ench - mean(sum_Ench))/mean(sum_Ench)]
# Enh_monthly[, Ench_intesit :=        sum_Ench / N ]
#
#
#
# #+ include=F, echo=F
# plot(Enh_daily$Day, Enh_daily$N)
# plot(Enh_daily$Day, Enh_daily$N_ex)
# plot(Enh_daily$Day, Enh_daily$sum_Ench)
# plot(Enh_daily$Day, Enh_daily$avg_Ench)
#
#
# fit1 <- lm(Enh_yearly$N_att ~ Enh_yearly$year)[[1]]
# fit2 <- lm(Enh_yearly$Ench_intesit ~ Enh_yearly$year)[[1]]


##  PLOTS  ---------------------------------------------------------------------

##;
##;  #'
##;  #' ## Results
##;  #'
##;  #' The enhancement events occur in
##;  #' $`r signif( 100*(sum(!is.na(Enh$GLB_ench)) / sum(!is.na(CSdt$wattGLB))), 3 )`\%$
##;  #' of the total GHI measurements, and for
##;  #' $`r signif( 100* length(unique(Enh$Day)) / length(unique(CSdt$Day)), 3 )`\%$
##;  #' of the days in the data set.
##;  #' The total number of cases we identified, is increasing steadily the last decades,
##;  #' with a rate of $`r signif(abs(fit1[2]*1),3)`\%$ per year (Fig. \@ref(fig:enchtrend)).
##;  #' However, the yearly mean excess radiation (radiation above the threshold) per enhancement event seems to be almost
##;  #' constant with a mean value of $`r round(mean(Enh_yearly$Ench_intesit),1)`\,Wm^{-2}$, with a marginal trend of $`r signif((fit2[2]*1),2)`Wm^{-2}$ per year.
##;  #'
##;  #' Another aspect of the occurrence of enchantment events is in relation with the SZA, where we observe a correlation of the Sun elevation angle with the number of events (Fig. \@ref(fig:szacases)).
##;  #'
##;
##;


## _ Cases per year  -----------------------------------------------------------


# #+ enchtrendyear, include=T, echo=F, fig.cap="Trend of the total of enhancement cases per year."
# plot(Enh_yearly$year, Enh_yearly$N_att ,
#      xlab = "Year",
#      ylab = bquote("Difference from mean [%]" )
# )
# # title("Number of enchantments incidences", cex = 0.7)
# lm1        <- lm( Enh_yearly$N_att ~ Enh_yearly$year )
# abline(lm1)
# fit <- lm1[[1]]
# legend('topleft', lty = 1, bty = "n",
#        paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*1),3),'* year'))
# #'


## _ Cases per month  ----------------------------------------------------------



# plot(Enh_monthly$Date, Enh_monthly$N_att ,
#      ylab = bquote("Difference from mean [%]" )
# )
# # title("Number of enchantments incidences", cex = 0.7)
# lm1        <- lm( Enh_monthly$N_att ~ Enh_monthly$Date )
# abline(lm1, col = "red")
# fit <- lm1[[1]]
# legend('topleft', lty = 1, bty = "n",
#        paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*Days_of_year),3),'* year'))
#


## _ Cases per day  ------------------------------------------------------------

# #+ enchtrendday, include=T, echo=F, fig.cap="Trend of the total of enhancement cases per year."
# plot(Enh_daily$Day, Enh_daily$N_att ,
#      ylab = bquote("Difference from mean [%]" )
#      )
# # title("Number of enchantments incidences", cex = 0.7)
# lm1        <- lm( Enh_daily$N_att ~ Enh_daily$Day )
# abline(lm1, col = "red")
# fit <- lm1[[1]]
# legend('topleft', lty = 1, bty = "n",
#        paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*1),3),'* year'))
# #'




# #+ enchNtrendday, include=T, echo=F, fig.cap="Trend of the total of enhancement cases per year."
# plot(Enh_daily$Day, Enh_daily$N ,
#       # xlab = "Year",
#       ylab = bquote("" )
# )
# # title("Number of enchantments incidences", cex = 0.7)
# lm1        <- lm( Enh_daily$N ~ Enh_daily$Day )
# abline(lm1)
# fit <- lm1[[1]]
# legend('topleft', lty = 1, bty = "n",
#        paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*Days_of_year),3),'* year'))
# #'









# #+ enchtrendN, include=F, echo=F, fig.cap="Trend of yearly number of enhancement cases."
# plot( Enh_yearly$year, Enh_yearly$N ,
#       xlab = "",
#       ylab = bquote("Number of yearly cases" )
# )
# # title("Number of enchantments incidences", cex = 0.7)
# lm1        <- lm( Enh_yearly$N ~ Enh_yearly$year )
# abline(lm1)
# fit <- lm1[[1]]
# legend('topleft', lty = 1, bty = "n",
#        paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-', signif(abs(fit[2]*1),3),'* year'))
# #'


# #+ excessenergy, include=F, echo=F, fig.cap="The sum of the energy (in 1 minute resolution), above the reference model."
# plot( Enh_yearly$year, Enh_yearly$sum_Ench_att,
#       xlab = "Year",
#       ylab = bquote("Difference from mean [%]")
# )
# title("Sum of radiation above enhancement threshold", cex = 0.7)
# lm1        <- lm( Enh_yearly$sum_Ench_att ~ Enh_yearly$year )
# abline(lm1)
# fit <- lm1[[1]]
# legend('topleft', lty = 1, bty = "n",
#        paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*1),3),'* year'))
# #'


# #+ excess, include=F, echo=F, fig.cap="Trend and mean radiation enhancement radiation, above threshold, per case."
# plot( Enh_yearly$year, Enh_yearly$Ench_intesit,
#       xlab = "Year",
#       ylab = bquote("Mean enhancement intensity ["~ Watt~m^-2~N^-1~"]")
# )
# abline( h = mean(Enh_yearly$Ench_intesit, na.rm = TRUE), lty = 2 )
# lm1        <- lm( Enh_yearly$Ench_intesit ~ Enh_yearly$year )
# abline(lm1)
# fit <- lm1[[1]]
# legend('topleft', lty = 1, bty = "n",
#        paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*1),3),'* year'))
#

# #+ include=F, echo=F
# plot( Enh_yearly$year, Enh_yearly$avg_Ench,
#       xlab = "Year",
#       ylab = bquote("Average enchantment intensity ["~ Watt~m^-2~"]")
# )
#
# plot(Enh_daily$Day, Enh_daily$sum_Diff)


##;
##;  #### Conclusions ####
##;  #'
##;  #' ## Conclusions
##;  #'
##;  #' We can not attribute the steady increase in enhancement cases we observe to a single factor.
##;  #'
##;  #' clouds
##;  #' Aerosols
##;  #' brightening
##;  #'
##;  #' Although increase in energy at ground
##;  #'
##;  #'
##;
##;  #'
##;  #' The number of case characterization is skewed by the SZA angle of the case, although this connection is indicative to the complexity of factor we have to take into account.
##;  #'

##;
##;  #'
##;  #' there is a dependency of the magnitude of the enhancement with the
##;  #' SZA.
##;  #'




## consecutive data id
# test <- c(F,T,F, rep(T,5),F,F,T,F,F,F, rep(T,5))
#
# DT <- data.table(test)
# DT[, cnF := cumsum(test == FALSE)]
# DT[, cnT := cumsum(test == TRUE) ]
# # the realation of diff may be a general solution
# DT[, ddF := c(0, diff(cnF))]
# DT[, ddT := c(0, diff(cnT))]
#
# DT[, G1  := test]
# DT[, G0  := test]
#
#
# allow <- 1
# for (i in 1:nrow(DT)) {
#     p1 <- i - 1
#     n1 <- i + 1
#     if (p1 > 0 & n1 <= nrow(DT)) {
#         if (DT$G1[p1] == TRUE  &
#             DT$G1[i]  == FALSE &
#             DT$G1[n1] == TRUE  ) {
#             DT$G1[i]  <- TRUE
#         }
#     }
# }
#
# DT[, Grp1 := rleid(c(NA,diff(cumsum(G1))))]
# DT[G1 == FALSE, Grp1 := NA]
#
# DT[, Grp0 := rleid(c(NA,diff(cumsum(G0))))]
# DT[G0 == FALSE, Grp0 := NA]
#
# DT



library(timetk)

isinter <- (isTRUE(getOption('knitr.in.progress')) == F & interactive())

#'
#' ## Time series decomposition tests
#'
#' interactive `r as.character(isinter)`
#'



plot_time_series(ST_daily, Date, wattGLB.mean, .interactive = isinter)
plot_time_series(ST_daily, Date, wattGLB.sum , .interactive = isinter)
plot_time_series(ST_daily, Date, wattGLB.max , .interactive = isinter)
plot_time_series(ST_daily, Date, wattGLB.N   , .interactive = isinter)


plot_time_series(ST_E_daily, Date, wattGLB.mean, .interactive = isinter)
plot_time_series(ST_E_daily, Date, wattGLB.sum , .interactive = isinter)
plot_time_series(ST_E_daily, Date, wattGLB.max , .interactive = isinter)
plot_time_series(ST_E_daily, Date, wattGLB.N   , .interactive = isinter)

plot_time_series(ST_E_monthly, Date, wattGLB.mean, .interactive = isinter)
plot_time_series(ST_E_monthly, Date, wattGLB.sum , .interactive = isinter)
plot_time_series(ST_E_monthly, Date, wattGLB.max , .interactive = isinter)
plot_time_series(ST_E_monthly, Date, wattGLB.N   , .interactive = isinter)
plot_time_series(ST_E_monthly, Date, wattGLB.N   , .interactive = isinter, .smooth = T, .smooth_span = 0.3 )



plot_acf_diagnostics(ST_E_daily, Date, wattGLB.N, .lags = 1:60, .interactive = isinter)



plot_stl_diagnostics(ST_E_daily, Date, wattGLB.N,
                     .feature_set = c("observed","season", "trend", "remainder"),
                     .trend = 180,
                     .frequency = 30,
                     .interactive = isinter
                     )

plot_stl_diagnostics(ST_E_monthly, Date, wattGLB.N,
                     .feature_set = c("observed","season", "trend", "remainder"),
                     .trend = 180,
                     .frequency = 30,
                     .interactive = isinter
                     )


plot_seasonal_diagnostics(ST_E_daily, Date, wattGLB.N, .interactive = isinter,
                          .feature_set = c("week", "month.lbl", "quarter", "year"))

plot_seasonal_diagnostics(ST_E_monthly, Date, wattGLB.N, .interactive = isinter)

# https://business-science.github.io/timetk/articles/TK08_Automatic_Anomaly_Detection.html

plot_time_series_regression(
    .data         = ST_E_monthly,
    .date_var     = Date,
    .formula      = wattGLB.N ~ as.numeric(Date)  ,
    .facet_ncol   = 2,
    .interactive  = isinter,
    .show_summary = TRUE
)
lm1 <- lm(ST_E_monthly$wattGLB.N ~ ST_E_monthly$Date)
summary(lm1)

plot_time_series_regression(
    .data         = ST_E_daily,
    .date_var     = Date,
    .formula      = wattGLB.N ~ as.numeric(Date)  ,
    .facet_ncol   = 2,
    .interactive  = isinter,
    .show_summary = FALSE
)



#  Save from environment  ------------------------------------------------------
## Variables
objects <- grep("^tic$|^tac$|^Script.Name$|^tag$", ls(), value = T, invert = T)
objects <- objects[sapply(objects, function(x)
    is.numeric(get(x)) |
        is.character(get(x)) &
        object.size(get(x)) < 1009 &
        (!is.vector(get(x)) |
             !is.function(get(x))), simplify = T)]
## Data
objects <- c(
    objects,
    grep("^ST_", ls(), value = TRUE)
)


save(file = paste0("./data/", basename(sub("\\.R", ".Rda", Script.Name))),
     list = objects,
     compress = "xz")




#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (interactive() & difftime(tac,tic,units = "sec") > 30) {
    system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
    system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'R script ended'"))
}

