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
Script.Name <- "./GHI_enh_02_process.R"

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


## Overide notification function
options(error = function() {
    if (interactive()) {
        system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
        system("notify-send -u normal -t 30000 'R session' 'An error occurred!'")
    }
    traceback()
})


##  Prepare raw data if needed  ------------------------------------------------
## check previous steps
if (
    file.exists(raw_input_data) == FALSE |
    file.mtime(raw_input_data) < file.mtime("./GHI_enh_00_variables.R") |
    file.mtime(raw_input_data) < file.mtime("./GHI_enh_01_raw_data.R")
) {
    source("./GHI_enh_01_raw_data.R")
    dummy <- gc()
}


##  Prepare Enhancement data  ----------------------------------------------------------
DATA <- readRDS(raw_input_data)
tic  <- Sys.time()


# TODO -------------------------------------------------------------------------
# k clastering Vamvakas2020
# Stats on groups
# Stats on enhancement cases
# R-R analysis
# Seasonal occurance
# Tapakis2014 plots and stats

#'
#'  Alpha * HAU is CS_ref
#'
#+ include=FALSE, echo=FALSE


theme_ben <- function(base_size = 14) {
    theme_bw(base_size = base_size) %+replace%
        theme(
            # L'ensemble de la figure
            plot.title = element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
            # Zone où se situe le graphique
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            # Les axes
            axis.title = element_text(size = rel(0.85), face = "bold"),
            axis.text = element_text(size = rel(0.70), face = "bold"),
            axis.line = element_line(color = "black", arrow = arrow(length = unit(0.3, "lines"), type = "closed")),
            # La légende
            legend.title = element_text(size = rel(0.85), face = "bold"),
            legend.text = element_text(size = rel(0.70), face = "bold"),
            legend.key = element_rect(fill = "transparent", colour = NA),
            legend.key.size = unit(1.5, "lines"),
            legend.background = element_rect(fill = "transparent", colour = NA),
            # Les étiquettes dans le cas d'un facetting
            strip.background = element_rect(fill = "#17252D", color = "#17252D"),
            strip.text = element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
        )
}

## TODO plot only enhancement cases
## DO it whith baseplot
p <-
    ggplot(DATA[year(Date) == 2018], aes(CS_ref, wattGLB)) +
    geom_point(data = DATA[year(Date) == 2018 & GLB_diff < 0], colour = "black", size = 0.5) +
    geom_point(data = DATA[year(Date) == 2018 & GLB_diff > 0], size = 0.5, aes(color = GLB_diff)) +
    scale_colour_gradient(low = "blue", high = "red", na.value = NA) +
    theme(
        panel.background      = element_rect(fill='transparent'), #transparent panel bg
        plot.background       = element_rect(fill='transparent', color=NA), #transparent plot bg
        # panel.grid.major      = element_blank(), #remove major gridlines
        # panel.grid.minor      = element_blank(), #remove minor gridlines
        legend.background     = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent') #transparent legend panel
    ) +
    theme_ben()

#+ include=T, echo=FALSE
print(p)
#+ include=F, echo=FALSE

# ggplot(DATA, aes(CS_ref, wattGLB)) +
#     geom_point(data = DATA[GLB_diff < 0], colour = "black", size = 0.5) +
#     geom_point(data = DATA[GLB_diff > 0], size = 0.5, aes(color = GLB_diff)) +
#     scale_colour_gradient(low = "blue", high = "red", na.value = NA)


# ggplot(DATA[year(Date) == 2018], aes(CS_ref, wattGLB)) +
#     geom_point(data = DATA[year(Date) == 2018 & GLB_diff < 0], colour = "black", size = 0.5) +
#     geom_point(data = DATA[year(Date) == 2018 & GLB_diff > 0], size = 0.5, aes(color = GLB_diff)) +
#     scale_colour_gradient2(low = "black", mid = "yellow", high = "red", na.value = NA)



## __ Estimate enhancement daily magnitude  ------------------------------------
enh_days <- DATA[Enhancement == TRUE,
                 .(Enh_sum      = sum(GLB_ench, na.rm = TRUE),
                   Enh_max      = max(GLB_ench, na.rm = TRUE),
                   Enh_diff_sum = sum(GLB_diff, na.rm = TRUE),
                   Enh_diff_max = sum(GLB_diff, na.rm = TRUE)),
                 Day]


## interesting days first
setorder(enh_days, -Enh_sum     )
setorder(enh_days, -Enh_max     )
setorder(enh_days, -Enh_diff_sum)

## plot some interesting days
daylist <- enh_days$Day
daylist <- sort(daylist[1:30])




##  Days with strong enhancement cases  ----------------------------------------

#'
#' ## Plot some days with strong enhancement cases
#'
#+ strong_days, echo=F, include=T
for (aday in daylist) {
    temp <- DATA[ Day == aday ]
    par(mar = c(4,4,1,1))
    ylim = range(0, temp$TSIextEARTH_comb * cosde(temp$SZA), temp$wattGLB, na.rm = TRUE)

    plot(temp$Date, temp$wattGLB, "l", col = "green",
         ylim = ylim,
         ylab = expression(Watt/m^2), xlab = "Time (UTC)")

    lines(temp$Date, temp$wattHOR, col = "blue")

    lines(temp$Date, temp$TSIextEARTH_comb * cosde(temp$SZA))

    lines(temp$Date, temp$CS_ref + GLB_diff_THRES, col = "red" )

    # lines(temp$Date, temp$HAU + wattGLB_THRES , col = "red" )
    # lines(temp$Date, temp$CS_ref, col = "red" ,lty=3)
    # points(temp[ GLB_ench > GLB_ench_THRES, Date ], temp[ GLB_ench > GLB_ench_THRES, wattGLB ], col = "cyan")
    # points(temp[ Clearness_Kt > Clearness_Kt_THRES, Date ], temp[ Clearness_Kt > Clearness_Kt_THRES , wattGLB ], col = "yellow")


    points(temp[Enhancement == TRUE, wattGLB, Date], col = "red")

    title(main = as.Date(aday, origin = "1970-01-01"))
    # legend("topleft", c("GHI","DNI",  "A-HAU", "TSI on horizontal level","GHI Enhancement event"),
    #        col = c("green",   "blue", "red", "black", "red"),
    #        pch = c(     NA,       NA,    NA,      NA,    1 ),
    #        lty = c(      1,        1,     1,       1,   NA ),
    #        bty = "n"
    # )

    legend("topleft", c("GHI","DNI",  "GHI threshold", "TSI on horizontal level","GHI Enhancement event"),
           col = c("green",   "blue", "red", "black",  "red"),
           pch = c(     NA,       NA,    NA,      NA,     1 ),
           lty = c(      1,        1,     1,       1,    NA ),
           bty = "n"
    )

    # plot(temp$Date, temp$Clearness_Kt)
    # abline(h=.8,col="red")
    # plot(temp$Date, temp$DiffuseFraction_Kd)
    # plot(temp$Date, temp$GLB_ench)
    # plot(temp$Date, temp$GLB_diff)
}








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
##;
##;
##;
##;  ## plot one selected day ####
##;  #+ dayexample, include=T, echo=F, fig.cap="Diurnal variability of GHI (green) and DNI (blue) for 08-4-2017. Red cycles denote the enhancement cases that were identified during the day. The red line represents the the GHI threshold ($\\text{GHI}_\\text{Threshold}$) we use, and the black line is the TSI at the TOA for reference."
##;  daylist <- as.Date(c("2017-04-08"))
##;  for (aday in daylist) {
##;      temp <- DATA[ Day == aday ]
##;      par(mar = c(4,4,1,1))
##;      ylim = range(0, temp$TSIextEARTH_comb * cosde(temp$SZA), temp$wattGLB)
##;
##;      plot(temp$Date, temp$wattGLB, "l", col = "green",
##;           ylim = ylim,
##;           ylab = expression(Watt/m^2), xlab = "Time (UTC)")
##;
##;      lines(temp$Date, temp$wattHOR, col = "blue")
##;
##;      lines(temp$Date, temp$TSIextEARTH_comb * cosde(temp$SZA))
##;
##;      lines(temp$Date, temp$CS_ref + GLB_diff_THRES, col = "red" )
##;
##;      # lines(temp$Date, temp$HAU + wattGLB_THRES , col = "red" )
##;      # lines(temp$Date, temp$CS_ref, col = "red" ,lty=3)
##;      # points(temp[ GLB_ench > GLB_ench_THRES, Date ], temp[ GLB_ench > GLB_ench_THRES, wattGLB ], col = "cyan")
##;      # points(temp[ Clearness_Kt > Clearness_Kt_THRES, Date ], temp[ Clearness_Kt > Clearness_Kt_THRES , wattGLB ], col = "yellow")
##;
##;      temp[Enhancement == TRUE, wattGLB, Date]
##;
##;      points(temp[Enhancement == TRUE, wattGLB, Date], col = "red")
##;
##;      title(main = as.Date(aday, origin = "1970-01-01"))
##;      # legend("topleft", c("GHI","DNI",  "A-HAU", "TSI on horizontal level","GHI Enhancement event"),
##;      #        col = c("green",   "blue", "red", "black", "red"),
##;      #        pch = c(     NA,       NA,    NA,      NA,    1 ),
##;      #        lty = c(      1,        1,     1,       1,   NA ),
##;      #        bty = "n"
##;      # )
##;
##;      legend("topleft", c("GHI","DNI",  "GHI threshold", "TSI on horizontal level","GHI Enhancement event"),
##;             col = c("green",   "blue", "red", "black",  "red"),
##;             pch = c(     NA,       NA,    NA,      NA,     1 ),
##;             lty = c(      1,        1,     1,       1,    NA ),
##;             bty = "n"
##;      )
##;
##;      # plot(temp$Date, temp$Clearness_Kt)
##;      # abline(h=.8,col="red")
##;      # plot(temp$Date, temp$DiffuseFraction_Kd)
##;      # plot(temp$Date, temp$GLB_ench)
##;      # plot(temp$Date, temp$GLB_diff)
##;  }
##;  #'
##;
##;


##  Enhancement cases statistics  ----------------------------------------------

















##  Stats on enhancement cases  ------------------------------------------------
DATA_Enh <- DATA[Enhancement == TRUE ]





Enh_daily <- DATA_Enh[, .(N        = sum( Enhancement, na.rm = T),
                          N_ex     = sum( wattGLB > TSIextEARTH_comb * cosde(SZA)),
                          sum_Ench = sum( GLB_diff),
                          avg_Ench = mean(GLB_ench),
                          sd_Ench  = sd(  GLB_ench),
                          sum_Diff = sum( GLB_diff)),
                      by = "Day"]


Enh_yearly <- DATA_Enh[, .(N        = sum(!is.na(GLB_ench)),
                           N_ex     = sum( wattGLB > TSIextEARTH_comb * cosde(SZA)),
                           sum_Ench = sum( GLB_diff),
                           avg_Ench = mean(GLB_ench),
                           sd_Ench  = sd(  GLB_ench),
                           sum_Diff = sum( GLB_diff)),
                       by = year(Date)]

Enh_monthly <- DATA_Enh[, .(N        = sum(!is.na(GLB_ench)),
                           N_ex     = sum( wattGLB > TSIextEARTH_comb * cosde(SZA)),
                           sum_Ench = sum( GLB_diff),
                           avg_Ench = mean(GLB_ench),
                           sd_Ench  = sd(  GLB_ench),
                           sum_Diff = sum( GLB_diff)),
                       by = .(year(Date), month(Date))]

Enh_monthly$Date <- as.POSIXct(strptime(paste(Enh_monthly$year, Enh_monthly$month, "1"),"%Y %m %d"))





Enh_total <- DATA_Enh[, .(N        = sum(!is.na(GLB_ench)),
                          N_ex     = sum( wattGLB > TSIextEARTH_comb * cosde(SZA)),
                          sum_Ench = sum( GLB_diff),
                          avg_Ench = mean(GLB_ench),
                          sd_Ench  = sd(  GLB_ench),
                          sum_Diff = sum( GLB_diff))]


Enh_sza    <- DATA_Enh[, .(N        = sum(!is.na(GLB_ench)),
                           N_ex     = sum( wattGLB > TSIextEARTH_comb * cosde(SZA)),
                           sum_Ench = sum( GLB_diff),
                           avg_Ench = mean(GLB_ench),
                           sd_Ench  = sd(  GLB_ench),
                           sum_Diff = sum( GLB_diff)),
                       by = .(SZA = (SZA - SZA_BIN / 2 ) %/% SZA_BIN)]


Data_sza    <- DATA[, .(N_enha  = sum(Enhancement, na.rm = TRUE),
                        N_total = sum(!is.na(wattGLB))),
                    by = .(SZA = (SZA - SZA_BIN / 2 ) %/% SZA_BIN) ]



CONF_INTERV <- .95
conf_param  <- 1 - (1 - CONF_INTERV) / 2
suppressWarnings({
    Enh_sza[,   Ench_EM := qt(conf_param, df = N - 1) * sd_Ench / sqrt(N)]
    Enh_daily[, Ench_EM := qt(conf_param, df = N - 1) * sd_Ench / sqrt(N)]
    Enh_yearly[,Ench_EM := qt(conf_param, df = N - 1) * sd_Ench / sqrt(N)]
    Enh_total[, Ench_EM := qt(conf_param, df = N - 1) * sd_Ench / sqrt(N)]
})


## Make values relative ####
Enh_yearly[, N_att        := 100 * (N - mean(N))/mean(N)]
Enh_yearly[, sum_Ench_att := 100 * (sum_Ench - mean(sum_Ench))/mean(sum_Ench)]
Enh_yearly[, Ench_intesit :=        sum_Ench / N ]

Enh_daily[, N_att        := 100 * (N - mean(N))/mean(N)]
Enh_daily[, sum_Ench_att := 100 * (sum_Ench - mean(sum_Ench))/mean(sum_Ench)]
Enh_daily[, Ench_intesit :=        sum_Ench / N ]

Enh_monthly[, N_att        := 100 * (N - mean(N))/mean(N)]
Enh_monthly[, sum_Ench_att := 100 * (sum_Ench - mean(sum_Ench))/mean(sum_Ench)]
Enh_monthly[, Ench_intesit :=        sum_Ench / N ]



#+ include=F, echo=F
plot(Enh_daily$Day, Enh_daily$N)
plot(Enh_daily$Day, Enh_daily$N_ex)
plot(Enh_daily$Day, Enh_daily$sum_Ench)
plot(Enh_daily$Day, Enh_daily$avg_Ench)


fit1 <- lm(Enh_yearly$N_att ~ Enh_yearly$year)[[1]]
fit2 <- lm(Enh_yearly$Ench_intesit ~ Enh_yearly$year)[[1]]

##;  ## results ####
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

#+ enchtrendyear, include=T, echo=F, fig.cap="Trend of the total of enhancement cases per year."
plot(Enh_yearly$year, Enh_yearly$N_att ,
     xlab = "Year",
     ylab = bquote("Difference from mean [%]" )
)
# title("Number of enchantments incidences", cex = 0.7)
lm1        <- lm( Enh_yearly$N_att ~ Enh_yearly$year )
abline(lm1)
fit <- lm1[[1]]
legend('topleft', lty = 1, bty = "n",
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*1),3),'* year'))
#'


## _ Cases per month  ----------------------------------------------------------



plot(Enh_monthly$Date, Enh_monthly$N_att ,
     ylab = bquote("Difference from mean [%]" )
)
# title("Number of enchantments incidences", cex = 0.7)
lm1        <- lm( Enh_monthly$N_att ~ Enh_monthly$Date )
abline(lm1, col = "red")
fit <- lm1[[1]]
legend('topleft', lty = 1, bty = "n",
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*Days_of_year),3),'* year'))



## _ Cases per day  ------------------------------------------------------------

#+ enchtrendday, include=T, echo=F, fig.cap="Trend of the total of enhancement cases per year."
plot(Enh_daily$Day, Enh_daily$N_att ,
     ylab = bquote("Difference from mean [%]" )
     )
# title("Number of enchantments incidences", cex = 0.7)
lm1        <- lm( Enh_daily$N_att ~ Enh_daily$Day )
abline(lm1, col = "red")
fit <- lm1[[1]]
legend('topleft', lty = 1, bty = "n",
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*1),3),'* year'))
#'




#+ enchNtrendday, include=T, echo=F, fig.cap="Trend of the total of enhancement cases per year."
plot(Enh_daily$Day, Enh_daily$N ,
      # xlab = "Year",
      ylab = bquote("" )
)
# title("Number of enchantments incidences", cex = 0.7)
lm1        <- lm( Enh_daily$N ~ Enh_daily$Day )
abline(lm1)
fit <- lm1[[1]]
legend('topleft', lty = 1, bty = "n",
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*Days_of_year),3),'* year'))
#'









#+ enchtrendN, include=F, echo=F, fig.cap="Trend of yearly number of enhancement cases."
plot( Enh_yearly$year, Enh_yearly$N ,
      xlab = "",
      ylab = bquote("Number of yearly cases" )
)
# title("Number of enchantments incidences", cex = 0.7)
lm1        <- lm( Enh_yearly$N ~ Enh_yearly$year )
abline(lm1)
fit <- lm1[[1]]
legend('topleft', lty = 1, bty = "n",
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-', signif(abs(fit[2]*1),3),'* year'))
#'


#+ excessenergy, include=F, echo=F, fig.cap="The sum of the energy (in 1 minute resolution), above the reference model."
plot( Enh_yearly$year, Enh_yearly$sum_Ench_att,
      xlab = "Year",
      ylab = bquote("Difference from mean [%]")
)
title("Sum of radiation above enhancement threshold", cex = 0.7)
lm1        <- lm( Enh_yearly$sum_Ench_att ~ Enh_yearly$year )
abline(lm1)
fit <- lm1[[1]]
legend('topleft', lty = 1, bty = "n",
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*1),3),'* year'))
#'


#+ excess, include=F, echo=F, fig.cap="Trend and mean radiation enhancement radiation, above threshold, per case."
plot( Enh_yearly$year, Enh_yearly$Ench_intesit,
      xlab = "Year",
      ylab = bquote("Mean enhancement intensity ["~ Watt~m^-2~N^-1~"]")
)
abline( h = mean(Enh_yearly$Ench_intesit, na.rm = TRUE), lty = 2 )
lm1        <- lm( Enh_yearly$Ench_intesit ~ Enh_yearly$year )
abline(lm1)
fit <- lm1[[1]]
legend('topleft', lty = 1, bty = "n",
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*1),3),'* year'))


#+ include=F, echo=F
plot( Enh_yearly$year, Enh_yearly$avg_Ench,
      xlab = "Year",
      ylab = bquote("Average enchantment intensity ["~ Watt~m^-2~"]")
)

plot(Enh_daily$Day, Enh_daily$sum_Diff)


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

#+ include=F, echo=F, fig.cap="Number of cases by SZA."
plot(Enh_sza$SZA, Enh_sza$N)
#'

## ignore extreme cases for now
# plot(Enh_sza$SZA, Enh_sza$N_ex)


#+ include=F, echo=F
plot(Enh_sza$SZA, Enh_sza$sum_Ench, main = "Total exess energy")


##;
##;  #'
##;  #' there is a dependency of the magnitude of the enhancement with the
##;  #' SZA.
##;  #'

#+ include=F, echo=F, fig.cap="Mean enhancement intensity relative to reference (A-HAU) by SZA."
ylim <- range(Enh_sza$avg_Ench - Enh_sza$Ench_EM, Enh_sza$avg_Ench + Enh_sza$Ench_EM, na.rm = T)
plot(  Enh_sza$SZA, Enh_sza$avg_Ench, pch = 19, cex = 0.7, ylim = ylim)
arrows(Enh_sza$SZA, Enh_sza$avg_Ench - Enh_sza$Ench_EM, Enh_sza$SZA, Enh_sza$avg_Ench + Enh_sza$Ench_EM, length=0.03, angle=90, code=3)
#'


#+ szacases, include=T, echo=F, fig.cap="Enhancement cases percentage in total GHI data per SZA ($1^\\circ$ bin)."
plot( Data_sza$SZA, Data_sza[, 100 * N_enha / N_total ],
      xlab = "Solar zenith angle",
      ylab = bquote("Enhancement cases [%] of total data")
)
#'





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






#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
