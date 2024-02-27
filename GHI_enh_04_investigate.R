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
Script.Name <- "./GHI_enh_04_investigate.R"

if (!interactive()) {
    pdf( file = paste0("./runtime/",  basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("./runtime/",  basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}



#+ echo=F, include=T
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(pander    , quietly = TRUE, warn.conflicts = FALSE)
library(ggplot2   , quietly = TRUE, warn.conflicts = FALSE)
library(lmtest    , quietly = TRUE, warn.conflicts = FALSE)

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






##  Daily  ---------------------------------------------------------------------
cat(ls(pattern = "^ST.*daily"))


#'
#' \newpage
#' \FloatBarrier
#'
#' ### Daily Trends
#'
#+ daily, echo=F, include=T, results="asis"

## variables to plot
prefix <- c("GLB_ench", "GLB_diff")
sufix  <- c("max", "median", "sum", "N" )
vars   <- sort(levels(interaction(prefix,sufix, sep = ".")))

## data set to plot
dbs         <- c("ST_daily", "ST_E_daily", "ST_E_daily_seas", "ST_extreme_daily")
dbs         <- c("ST_daily", "ST_E_daily", "ST_extreme_daily")


## gather trends
dailytrends  <- data.frame()
dailytrendsY <- data.frame()


for (DBn in dbs) {
    DB <- get(DBn)
    cat("\n\\newpage\n")
    cat("\n#### Trends on", tr_var(DBn), "data\n\n" )

    for (avar in vars) {
        dataset <- DB

        if (all(is.na(dataset[[avar]]))) next()

        ## linear model by day step
        lmD <- lm(dataset[[avar]] ~ dataset$Date)
        d   <- summary(lmD)$coefficients
        cat("lmD:     ", lmD$coefficients[2] * Days_of_year, "+/-", d[2,2] * Days_of_year,"\n\n")

        ## correlation test by day step
        corD <- cor.test(x = dataset[[avar]], y = as.numeric(dataset$Date), method = 'pearson')


        ## linear model by year step
        lmY <- lm(dataset[[avar]] ~ dataset$yts)
        d2   <- summary(lmY)$coefficients
        cat("lmY:     ", lmY$coefficients[2] , "+/-", d2[2,2] ,"\n\n")

        ## correlation test by day step
        corY <- cor.test(x = dataset[[avar]], y = as.numeric(dataset$yts), method = 'pearson')

        #         lmY[[1]] / Days_of_year
        #         coef(lmY) / Days_of_year
        #         summary(lmY)
        #         summary(lmD)
        #         coef(lmD)
        #         coef(lmY) /Days_of_year

        ## _ auto reggression arima Tourpali -------------------------------
        ## create a time variable (with lag of 1 day ?)
        tmodelo <- arima(x = dataset[[avar]], order = c(1,0,0), xreg = dataset$yts, method = "ML")

        ## trend per year with auto correlation
        Tres <- data.frame(t(lmtest::coeftest(tmodelo)[3,]))
        Tint <- data.frame(t(lmtest::coeftest(tmodelo)[2,]))
        names(Tres) <- paste0("Tmod_", names(Tres))
        cat("Arima:", paste(round(Tres, 4)), "\n\n")


        # capture lm for table
        dailytrends <- rbind(dailytrends,
                             data.frame(
                                 linear_fit_stats(lmD, confidence_interval = 0.99),
                                 cor_test_stats(corD),
                                 DATA       = DBn,
                                 var        = avar,
                                 N          = sum(!is.na(dataset[[avar]])),
                                 # N_eff      = N_eff,
                                 # t_eff      = t_eff,
                                 # t_eff_cri  = t_eff_cri,
                                 # conf_2.5   = conf_2.5,
                                 # conf_97.5  = conf_97.5,
                                 # mean_clima = mean(dclima$V1, na.rm = T),
                                 Tres
                             )
        )

        dailytrendsY <- rbind(dailytrendsY,
                              data.frame(
                                  linear_fit_stats(lmY, confidence_interval = 0.99),
                                  cor_test_stats(corY),
                                  DATA       = DBn,
                                  var        = avar,
                                  N          = sum(!is.na(dataset[[avar]])),
                                  # N_eff      = N_eff,
                                  # t_eff      = t_eff,
                                  # t_eff_cri  = t_eff_cri,
                                  # conf_2.5   = conf_2.5,
                                  # conf_97.5  = conf_97.5,
                                  # mean_clima = mean(dclima$V1, na.rm = T),
                                  Tres
                              )
        )

        # if (grepl("near_tcc", avar)) {
        #     acol <- "cyan"
        # } else {
        #     acol <- get(paste0(c("col", unlist(strsplit(avar, split = "_"))[1:2]),
        #                        collapse = "_"))
        # }


        ## plot data
        plot(dataset$Date, dataset[[avar]],
             pch      = 16,
             col      = varcol(avar),
             cex      = 0.5,
             # main     = paste(tr_var(DBn), tr_var(avar)),
             cex.main = 0.8,
             yaxt     = "n",
             xlab     = "",
             ylab     = varname(avar)
        )

        ## plot fit line lm
        abline(lmD, lwd = 2, col = "red")


        # y axis
        axis(2, pretty(dataset[[avar]]), las = 2 )

        # x axis
        axis.Date(1,
                  at = seq(as.Date("1993-01-01"), max(dataset$Date), by = "year"),
                  format = "%Y",
                  labels = NA,
                  tcl = -0.25)


        if (DRAFT == TRUE) {
            title(main = paste(tr_var(DBn), varname(avar), staname(avar), avar),
                  cex.main = 0.8 )
        }


        ## display trend on graph
        fit <- lmD[[1]]
        legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
               paste("Trend: ",
                     if (fit[2] > 0) "+" else "-",
                     signif(abs(fit[2]) * Days_of_year, 2) ,"/y" )
        )

        # fit <- lmD[[1]]
        # legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
        #        paste("Trend: ",
        #              if (fit[2] > 0) "+" else "-",
        #              signif(abs(fit[2]) * Days_of_year, 2),
        #              "±", signif(2 * Tres[2], 2) ,"%/y" )
        # )
        cat(" \n \n")
    }
}
#+ echo=F, include=F

row.names(dailytrends ) <- NULL
row.names(dailytrendsY) <- NULL
write.csv(x = dailytrends,
          file = "./figures/Daily_trends_byDay.csv")

write.csv(x = dailytrendsY,
          file = "./figures/Daily_trends_byYear.csv")


##  Group stats  ---------------------------------------------------------------

#'
#' \newpage
#' \FloatBarrier
#'
#' ### Group stats
#'
#+ groups, echo=F, include=T, results="asis"





hist(ST_G0$GLB_ench.N,
     breaks = 50,
     xlab = "Minutes of enhacement",
     main = "Duration of enhancement cases")



plot(ST_G0$GLB_ench.N, ST_G0$GLB_diff.sum/ST_G0$GLB_ench.N,
     xlab = "Duration of enhancemnt",
     ylab = "Extra Irradiance per mimute")


cat( "## @Zhang2018 \n" )

plot(ST_G0[, GLB_diff.max, GLB_diff.N ],
     xlab = "Duration of enhancemnt",
     ylab = "Maximun Over Irradiance")


plot(ST_E_daily[, sum(GLB_ench.N), by = yday(Date)],
     ylab = "Enhancement cases",
     xlab = "DOY",
     main = "Enhancement cases Climatology")







#+ climCEmonth, echo=F, include=T, results="asis"
boxplot(ST_E_monthly$GLB_ench.N ~ ST_E_monthly$month )
title("Climatology of CE cases per month")

#+ climECEmonth, echo=F, include=T, results="asis"
boxplot(ST_extreme_monthly$GLB_ench.N ~ ST_extreme_monthly$month )
title("Climatology of ECE cases per month")



## Trends on groups ---------

# ST_G0
















##  Energy contribution of enhancements  ---------------------------------------

#'
#' \newpage
#' \FloatBarrier
#'
#' ### Energy contribution of enhancements
#'
#+ energy, echo=F, include=T, results="asis"


{
  plot(ST_yearly[, GLB_diff.sumPOS, year],
       ylab = "W/m^2")

  lmD <- lm( ST_yearly[, year, GLB_diff.sumPOS])
  abline(lmD)

  title("Energy excess each year due to CE")

  ## display trend on graph
  fit <- lmD[[1]]
  legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
         paste("Trend: ",
               if (fit[2] > 0) "+" else "-",
               signif(abs(fit[2]), 2) ,"W/m^2/y" )
  )
}



plot(ST_yearly[, GLB_diff.N_pos, year],
     ylab = "Total of enhancement minutes")
title("Number of CE each year")



{
  plot(ST_yearly[, GLB_diff.sumPOS/GLB_diff.N_pos, year],
       ylab = "W/m^2")
  lmD <- lm( ST_yearly[, year, GLB_diff.sumPOS/GLB_diff.N_pos])
  abline(lmD)

  title("Energy excess each year due to CE")

  ## display trend on graph
  fit <- lmD[[1]]
  legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
         paste("Trend: ",
               if (fit[2] > 0) "+" else "-",
               signif(abs(fit[2]), 2) ,"W/m^2/y" )
  )
}




plot(ST_yearly[, GLB_diff.TotalN, year],
     ylab = "Total of enhancement minutes")



plot(ST_yearly[, GLB_ench.N, year],
     ylab = "Total of enhancement minutes")


plot(ST_yearly[, GLB_ench.N_pos/GLB_ench.TotalN, year],
     ylab = "Total of enhancement minutes ratio")


plot(ST_yearly[, GLB_ench.sumPOS/GLB_ench.N_pos, year],
     ylab = "Enhancement energy per minute")


cat( "@Martins2022" )

plot(ST_yearly[, GLB_diff.sumPOS/wattGLB.sumPOS, year],
     ylab = "Fraction of the aqumulated enchancements over total energy",
     main = paste(varname("GLB_diff.sumPOS"),
                  staname("GLB_diff.sumPOS"),
                  "/",
                  varname("wattGLB.sumPOS"),
                  staname("wattGLB.sumPOS")
     )
)







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


##TODO by month






##TODO check groups for low sun characteristics

#
# gr_N_min   <- 8
# gr_SZA_min <- 60
#
# test <- ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min]
#
#
# plot( ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min, GLB_diff.sum/GLB_ench.N, SZA.mean ])
#
# plot( ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min, GLB_diff.mean, SZA.mean ])
#
# plot( ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min, GLB_diff.median, SZA.mean ])
#
#
# plot( ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min, GLB_diff.sum/GLB_ench.N, SZA.max ])
#
# plot( ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min, GLB_diff.sum/GLB_ench.N, SZA.min ])
#
# plot( ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min, GLB_diff.sum/GLB_ench.N, SZA.mean])
#
# plot( ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min
#              & SZA.min > 72 & GLB_diff.sum/GLB_ench.N < 10 , GLB_diff.sum/GLB_ench.N, SZA.mean])
#
# ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min
# & SZA.min > 72 & GLB_diff.sum/GLB_ench.N < 10  ]
#
#
# hist( ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min, GLB_diff.sum/GLB_ench.N])
#
# plot( ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min, GLB_ench.max, SZA.mean ])
#
#
# plot( ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min, GLB_ench.mean, SZA.mean ])
# plot( ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min, GLB_diff.mean, SZA.mean ])
# plot( ST_G0[ GLB_ench.N > gr_N_min & SZA.min > gr_SZA_min, GLB_diff.max,  SZA.max ])
#
#
# ST_G0[as.Date(Date) == "2004-05-25"]
#
# ST_G0[as.Date(Date) == "2003-09-05"]
# ST_G0[as.Date(Date) == "2003-09-05", GLB_diff.sum/GLB_ench.N]
#
#

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

