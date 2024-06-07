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
Script.Name <- "./GHI_enh_04_investigate.R"

if (!interactive()) {
  pdf( file = paste0("./runtime/", basename(sub("\\.R$", ".pdf", Script.Name))))
}


#+ echo=F, include=T
suppressPackageStartupMessages({
  library(data.table    , quietly = TRUE, warn.conflicts = FALSE)
  library(pander        , quietly = TRUE, warn.conflicts = FALSE)
  library(ggplot2       , quietly = TRUE, warn.conflicts = FALSE)
  library(lmtest        , quietly = TRUE, warn.conflicts = FALSE)
  library(viridis       , quietly = TRUE, warn.conflicts = FALSE)
  library(ggpointdensity, quietly = TRUE, warn.conflicts = FALSE)
  library(patchwork     , quietly = TRUE, warn.conflicts = FALSE)
  library(ggh4x         , quietly = TRUE, warn.conflicts = FALSE)
  library(grid          , quietly = TRUE, warn.conflicts = FALSE)
  library(latex2exp     , quietly = TRUE, warn.conflicts = FALSE)
  library(ggpmisc       , quietly = TRUE, warn.conflicts = FALSE)
  library(cowplot       , quietly = TRUE, warn.conflicts = FALSE)
  # library(patchwork     , quietly = TRUE, warn.conflicts = FALSE)
  # library(ggpubr        , quietly = TRUE, warn.conflicts = FALSE)
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
source("~/CODE/FUNCTIONS/R/linear_fit_stats.R")
source("~/CODE/FUNCTIONS/R/cor_test_stats.R")


## Override notification function
options(error = function() {
    if (interactive()) {
        system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
        system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'An error occurred!'"))
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
#+ daily, echo=F, include=F, results="asis"

## variables to plot
prefix <- c("GLB_ench", "GLB_diff")
sufix  <- c("max", "median", "sum", "N", "mean", "p_5", "p_10", "p_90", "p_95")
vars   <- sort(levels(interaction(prefix, sufix, sep = ".")))


## data set to plot
# dbs         <- c("ST_daily", "ST_E_daily", "ST_E_daily_seas", "ST_extreme_daily")
dbs         <- c("ST_daily", "ST_E_daily", "ST_extreme_daily")


## gather trends
dailytrends  <- data.frame()
dailytrendsY <- data.frame()

for (DBn in dbs) {
  DB <- get(DBn)
  cat("\n\\newpage\n")
  cat("\n#### Trends on", tr_var(DBn), "data\n\n")

  for (avar in vars) {
    dataset <- DB


    if (all(is.na(dataset[[avar]]))) next()

    ## Set units
    if (grepl("_diff\\.sum$", avar)) {
      units <- bquote(J/m^2)
      units <- "kJ/m^2"
    } else if (grepl("\\.N$", avar)) {
      units <- ""
    } else {
      units <- bquote(Watt/m^2)
      units <- "Watt/m^2"
      ## set units
      # ylab = bquote("Deseas." ~ .(translate(avar)) ~ "[" ~ Watt/m^2 ~ "]" )
    }

    cat("Units for", avar, ": ", paste(units), "\n\n")

    ## linear model by day step
    lmD <- lm(dataset[[avar]] ~ dataset$Date)
    d   <- summary(lmD)$coefficients
    cat("lmD:     ", round(lmD$coefficients[2] * Days_of_year, 6), "+/-", round(d[2,2] * Days_of_year, 6), "\n\n")

    ## correlation test by day step
    corD <- cor.test(x = dataset[[avar]], y = as.numeric(dataset$Date), method = 'pearson')

    ## linear model by year step
    lmY <- lm(dataset[[avar]] ~ dataset$yts)
    d2   <- summary(lmY)$coefficients
    cat("lmY:     ", round(lmY$coefficients[2], 6) , "+/-", round(d2[2,2], 6) ,"\n\n")

    ## correlation test by day step
    corY <- cor.test(x = dataset[[avar]], y = as.numeric(dataset$yts), method = 'pearson')

    #         lmY[[1]] / Days_of_year
    #         coef(lmY) / Days_of_year
    #         summary(lmY)
    #         summary(lmD)
    #         coef(lmD)
    #         coef(lmY) /Days_of_year

    ## _ auto regression arima Tourpali -------------------------------
    ## create a time variable (with lag of 1 day ?)
    tmodelo <- arima(x = dataset[[avar]], order = c(1,0,0), xreg = dataset$yts, method = "ML")

    ## trend per year with auto correlation
    Tres <- data.frame(t(lmtest::coeftest(tmodelo)[3,]))
    Tint <- data.frame(t(lmtest::coeftest(tmodelo)[2,]))
    names(Tres) <- paste0("Tmod_", names(Tres))
    # cat("Arima:  ", paste(round(Tres, 4)), "\n\n")

    cat("Arima:   ", paste(round(Tres[1], 6), "+/-", round(Tres[2], 6)), "\n\n")


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
                 signif(abs(fit[2]) * Days_of_year, 2) , bquote(.(units)), "/y" )
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
rm(DBn, avar)
#+ echo=F, include=F


row.names(dailytrends ) <- NULL
row.names(dailytrendsY) <- NULL
write.csv(x = dailytrends,
          file = "./figures/Daily_trends_byDay.csv")

write.csv(x = dailytrendsY,
          file = "./figures/Daily_trends_byYear.csv")





## P_daily_trend ---------------------------------------------------------------
#+ P_daily_trend, echo=F, include=F, results="asis"

pvar    <- "GLB_diff.mean"
dataset <- copy(ST_E_daily)
dataset[, yts := yts + min(year(Date))]

## auto regression arima Tourpali
## create a time variable (with lag of 1 day ?)
tmodelo <- arima(x = dataset[[pvar]], order = c(1,0,0), xreg = dataset$yts, method = "ML")

## trend per year with auto correlation
Tres <- data.frame(t(lmtest::coeftest(tmodelo)[3,]))
Tint <- data.frame(t(lmtest::coeftest(tmodelo)[2,]))
names(Tres) <- paste0("Tmod_", names(Tres))
cat("Arima:   ", paste(round(Tres[1], 6), "+/-", round(Tres[2], 6)), "\n\n")

grob <- grobTree(
  textGrob(
    label = TeX(
      paste("Trend: $", round(Tres[1], 2),
            "\\pm",     round(2 * Tres[2], 2),   ## showing 2 sigma!
            "\\,W/m^2/year$")),
    x = 0.5,  y = 0.95, hjust = 0.5,
    gp = gpar(col = "black", fontsize = 13, fontface = "bold")
  ))

dataset |>
  ggplot(aes(x = yts,
             y = get(pvar))) +
  geom_point(color = varcol(pvar),
             size  = 1) +
  geom_abline(intercept = unlist(Tint[1]), slope = unlist(Tres[1])) +
  ylab(bquote("CE" ~ .(varname(pvar)) ~ group("[", W/m^2,"]"))) +
  xlab("Date") +
  annotation_custom(grob) +
  scale_y_continuous(guide        = "axis_minor",
                     minor_breaks = seq(0, 500, by = 25)) +
  scale_x_continuous(guide        = "axis_minor",
                     breaks = c(
                       min(floor(dataset[,yts])),
                       pretty(dataset[,yts], n = 4),
                       max(ceiling(dataset[,yts]))),
                     minor_breaks = seq(1990, 2050, by = 1)) +
  theme(plot.margin = margin(t = 0, r = 0.5 , b = 0.5, l = 0, "cm"))













##  Group stats  ---------------------------------------------------------------

#'
#' \newpage
#' \FloatBarrier
#'
#' ### Group stats
#'
#+ groups, echo=F, include=T, results="asis"


# hist(ST_G0$GLB_ench.N,
#      breaks = 50,
#      xlab = "Duration of enhancement [min]",
#      main = "Duration of enhancement of CE groups cases groups")

binwidth <- 2.5
split <- 23.5

p1 <- ggplot(data = ST_G0, aes(x = GLB_ench.N)) +
  geom_histogram(aes(y = (after_stat(count))/sum(after_stat(count)) * 100),
                 binwidth = binwidth,
                 boundary = 0,
                 color    = "black") +
  xlab("[min]") +
  ylab("[%]") +
  coord_cartesian(xlim = c(split, max(ST_G0$GLB_ench.N)),
                  ylim = c(0, .8)) +
  theme(
    axis.title = element_text(size = 9),
    axis.text  = element_text(size = 9),
    panel.grid = element_line(linetype = 2)
  )



ggplot(data = ST_G0, aes(x = GLB_ench.N)) +
  geom_histogram(aes(y = (after_stat(count))/sum(after_stat(count)) * 100),
                 binwidth = binwidth,
                 boundary = 0,
                 color    = "black") +
  # xlab(bquote(.(varname("GLB_diff")) ~ group("[", W/m^2,"]"))) +
  xlab("Duration of enhancement group [min]") +
  ylab("Relative frequency [%]") +
  labs(caption = paste("Bin width:", binwidth, "min"))
#   inset_element(p1, left = 0.3, bottom = 0.3, right = 1, top = 1,
#                 align_to = "plot")

ST_G0[GLB_ench.N <= 5, .N] / ST_G0[, .N]


ggplot(data = ST_G0, aes(x = GLB_ench.N)) +
  geom_histogram(binwidth = binwidth,
                 color    = "black") +
  scale_y_log10() +
  # xlab(bquote(.(varname("GLB_diff")) ~ group("[", W/m^2,"]"))) +
  xlab("Duration of enhancement group [min]") +
  ylab("Frequency") +
  labs(caption = paste("Bin width:", binwidth, "min"))







## _ Use point density  --------------------------

ggplot(data    = ST_G0,
       mapping = aes(x = GLB_ench.N, y = ST_G0$GLB_diff.sum/ST_G0$GLB_ench.N)) +
  xlab("Duration of enhancement [min]") +
  ylab(bquote("Mean Over Irradiance per minute" ~ group("[", W/m^2,"]"))) +
  geom_pointdensity(aes(color = after_stat(log(n_neighbors))),
                    adjust = 1,
                    size   = 0.7) +
  scale_color_viridis()  +
  theme(legend.position      = c(0.99, 0.99),
        legend.justification = c(1, 1)) +
  theme(legend.background    = element_rect(fill = "white", colour = NA)) +
  labs(color = 'Log(Number) of\nneighbors') +
  scale_y_continuous(guide        = "axis_minor",
                     minor_breaks = seq(0, 500, by = 25)) +
  scale_x_continuous(guide        = "axis_minor",
                     minor_breaks = seq(0, 500, by = 10))




ggplot(data    = ST_G0,
       mapping = aes(x = GLB_ench.N, y = ST_G0$GLB_diff.sum/ST_G0$GLB_ench.N)) +
  xlab("Duration of enhancement [min]") +
  ylab(bquote("Mean Over Irradiance per minute" ~ group("[", W/m^2,"]"))) +
  geom_pointdensity(adjust = 10,
                    size   = 0.7) +
  scale_color_viridis() +
  theme(legend.position      = c(0.99, 0.99),
        legend.justification = c(1, 1)) +
  theme(legend.background    = element_rect(fill = "white", colour = NA)) +
  labs(color = 'Number of\nneighbors') +
  scale_y_continuous(guide        = "axis_minor",
                     minor_breaks = seq(0, 500, by = 25)) +
  scale_x_continuous(guide        = "axis_minor",
                     minor_breaks = seq(0, 500, by = 10))



plot(ST_G0$GLB_ench.N, ST_G0$GLB_diff.mean,
     xlab = "Duration of enhancement [min]",
     ylab = "Mean over Irradiance per minute")


ggplot(data    = ST_G0,
       mapping = aes(x = GLB_diff.N, y = GLB_diff.mean)) +
  xlab("Duration of enhancement [min]") +
  ylab(bquote("Mean Over Irradiance" ~ group("[", W/m^2,"]"))) +
  geom_pointdensity(adjust = 10,
                    size   = 0.7) +
  scale_color_viridis()  +
  theme(legend.position      = c(0.99, 0.99),
        legend.justification = c(1, 1)) +
  theme(legend.background    = element_rect(fill = "white", colour = NA)) +
  labs(color = 'Number of\nneighbors') +
  scale_y_continuous(guide        = "axis_minor",
                     minor_breaks = seq(0, 500, by = 25)) +
  scale_x_continuous(guide        = "axis_minor",
                     minor_breaks = seq(0, 500, by = 10))

# guides(x = "axis_minor", y = "axis_minor")



cat( "## @Zhang2018 \n" )

plot(ST_G0[, GLB_diff.max, GLB_diff.N ],
     xlab = "Duration of enhancement [min]",
     ylab = "Maximum Over Irradiance")


ggplot(data    = ST_G0,
       mapping = aes(x = GLB_diff.N, y = GLB_diff.max)) +
  xlab("Duration of enhancement [min]") +
  ylab(bquote("Maximum Over Irradiance" ~ group("[", W/m^2,"]"))) +
  geom_pointdensity(adjust = 10,
                    size   = 0.7) +
  scale_color_viridis()  +
  theme(legend.position      = c(0.99, 0.99),
        legend.justification = c(1, 1)) +
  theme(legend.background    = element_rect(fill = "white", colour = NA)) +
  labs(color = 'Number of\nneighbors') +
  scale_y_continuous(guide        = "axis_minor",
                     minor_breaks = seq(0, 500, by = 25)) +
  scale_x_continuous(guide        = "axis_minor",
                     minor_breaks = seq(0, 500, by = 10))


## _ Use bin2d -------------

#+ P-groups-bin2d, echo=F, include=T, results="asis"
my_breaks <- c(2, 10, 50, 250, 1250, 6000)
my_breaks <- 1 * 2^seq(0, 20, by = 2)
lim_dur   <- 60
bins      <- 60

ggplot(data    = ST_G0,
       mapping = aes(x = GLB_ench.N, y = GLB_diff.sum/GLB_ench.N)) +
  xlab("Duration of enhancement group [min]") +
  ylab(bquote("Mean group over irradiance" ~ group("[", kJ/m^2,"]"))) +
  geom_bin_2d(bins = bins) +
  scale_fill_continuous(type = "viridis", transform = "log",
                        breaks = my_breaks, labels = my_breaks) +
  theme(legend.position      = c(0.99, 0.99),
        legend.justification = c(1, 1)) +
  theme(legend.background    = element_rect(fill = "white", colour = NA)) +
  labs(fill = 'Count\n(log scale)') +
  scale_y_continuous(guide        = "axis_minor",
                     minor_breaks = seq(0, 500, by = 25)) +
  scale_x_continuous(guide        = "axis_minor",
                     minor_breaks = seq(0, 500, by = 10)) +
  labs(caption = paste("Removed", ST_G0[GLB_ench.N > lim_dur, .N],
                       "points with duration >", lim_dur, "minutes.")) +
  xlim(-1, lim_dur)


ggplot(data    = ST_G0,
       mapping = aes(x = GLB_ench.N, y = GLB_diff.sum/GLB_ench.N)) +
  xlab("Duration of enhancement group [min]") +
  ylab(bquote("Mean group over irradiance" ~ group("[", kJ/m^2,"]"))) +
  geom_bin_2d(bins = 30) +
  scale_fill_viridis()  +
  # scale_fill_continuous(type = "viridis", transform = "log",
  #                       breaks = my_breaks, labels = my_breaks) +
  theme(legend.position      = c(0.99, 0.99),
        legend.justification = c(1, 1)) +
  theme(legend.background    = element_rect(fill = "white", colour = NA)) +
  labs(color = 'Count') +
  scale_y_continuous(guide        = "axis_minor",
                     minor_breaks = seq(0, 500, by = 25)) +
  scale_x_continuous(guide        = "axis_minor",
                     minor_breaks = seq(0, 500, by = 10)) +
  labs(caption = paste("Removed", ST_G0[GLB_ench.N > lim_dur, .N],
                       "points with duration >", lim_dur, "minutes.")) +
  xlim(-1, lim_dur)




ggplot(data    = ST_G0,
       mapping = aes(x = GLB_ench.N, y = GLB_diff.sum)) +
  xlab("Duration of enhancement group [min]") +
  ylab(bquote("Total group over irradiance" ~ group("[", kJ/m^2,"]"))) +
  geom_bin_2d(bins = 60) +
  # scale_fill_viridis()  +
  scale_fill_continuous(type = "viridis", transform = "log",
                        breaks = my_breaks, labels = my_breaks) +
  theme(legend.position      = c(0.01, 0.99),
        legend.justification = c(0, 1)) +
  theme(legend.background    = element_rect(fill = "white", colour = NA)) +
  labs(color = 'Count') +
  scale_y_continuous(guide        = "axis_minor",
                     minor_breaks = seq(0, 500, by = 25)) +
  scale_x_continuous(guide        = "axis_minor",
                     minor_breaks = seq(0, 500, by = 10)) +
  labs(caption = paste("Removed", ST_G0[GLB_ench.N > lim_dur, .N],
                       "points with duration >", lim_dur, "minutes.")) +
  xlim(-1, lim_dur)



ggplot(data    = ST_G0[GLB_ench.N > 1,],
       mapping = aes(x = GLB_ench.N, y = GLB_diff.sum/GLB_ench.N)) +
  xlab("Duration of enhancement group [min]") +
  ylab(bquote("Mean group over irradiance" ~ group("[", kJ/m^2,"]"))) +
  geom_bin_2d(bins = 60) +
  scale_fill_continuous(type = "viridis",
                        transform = "log",
                        breaks = my_breaks,
                        labels = my_breaks) +
  theme(legend.position      = c(0.99, 0.99),
        legend.justification = c(1, 1)) +
  theme(legend.background    = element_rect(fill = "white", colour = NA)) +
  labs(fill = 'Count\n(log scale)') +
  scale_y_continuous(guide        = "axis_minor",
                     minor_breaks = seq(0, 500, by = 25)) +
  scale_x_continuous(guide        = "axis_minor",
                     minor_breaks = seq(0, 500, by = 10)) +
  labs(caption = paste("Removed", ST_G0[GLB_ench.N > lim_dur, .N],
                       "points with duration >", lim_dur, "minutes.")) +
  xlim(-1, lim_dur)


##  Climatology  ----------------------------------------------


#'
#' # Climatoogy
#'
#+ clim-CE-N-doy, echo=F, include=T
plot(ST_E_daily[, sum(GLB_ench.N), by = yday(Date)],
     ylab = "Enhancement cases",
     xlab = "DOY",
     main = "Enhancement cases Climatology")



#  monthly weight for all months
ST_E_monthly[      , GLB_ench.N_MW := GLB_ench.N * (GLB_ench.N / All_N) ]
ST_extreme_monthly[, GLB_ench.N_MW := GLB_ench.N * (GLB_ench.N / All_N) ]




#+ climCEmonth, echo=F, include=T, results="asis"
boxplot(ST_E_monthly$GLB_ench.N ~ ST_E_monthly$month )
title("Climatology of CE cases per month")

#+ climCEmonth-MW, echo=F, include=T, results="asis"
boxplot(ST_E_monthly$GLB_ench.N_MW ~ ST_E_monthly$month )
title("Climatology of monthly weighted CE cases per month")


# normalize with max value
#+ clim-CE-month-norm-MAX-N, echo=F, include=T, results="asis"
boxplot(ST_E_monthly[, GLB_ench.N/max(GLB_ench.N, na.rm = T) ~ month ])
title("Climatology of CE cases per month Norm by max N")


#+ clim-CE-month-norm-MAX-N-MW, echo=F, include=T, results="asis"
boxplot(ST_E_monthly[, GLB_ench.N_MW/max(GLB_ench.N_MW, na.rm = T) ~ month ])
title("Climatology of monthly weighted CE cases per month Norm by max N")




#'
#' ## Monthly  CE
#'
#+ clim-CE-month-norm-MAX-median-N, echo=F, include=T, results="asis"
# normalize with max monthly median
temp       <- ST_E_monthly[, median(GLB_ench.N, na.rm = T), by = month]
max_median <- max(temp$V1)
max_month  <- month.name[temp[which.max(temp$V1), month]]

# boxplot(ST_E_monthly[, GLB_ench.N/max_median ~ month ])
# title(paste("Climatology of CE cases per month Norm the median of", max_month))

ggplot(ST_E_monthly, aes(y = GLB_ench.N/max_median,
                         x = factor(month,
                                    levels = 1:12,
                                    labels = month.abb[1:12]))) +
  geom_boxplot(fill = varcol("GLB_diff"), outliers = FALSE) +
  xlab("") +
  ylab("Relative monthly occurrences") +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 3) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
  # geom_dotplot(binaxis='y', stackdir='center', dotsize=.3) +
  # geom_jitter(shape=16, position=position_jitter(0.2))


#'
#' ## Monthly weighted CE
#'
#+ clim-CE-month-norm-MAX-median-N-MW, echo=F, include=T, results="asis"

# normalize with max monthly median
temp       <- ST_E_monthly[, median(GLB_ench.N_MW, na.rm = T), by = month]
max_median <- max(temp$V1)
max_month  <- month.name[temp[which.max(temp$V1), month]]


# boxplot(ST_E_monthly[, GLB_ench.N_MW/max_median ~ month ])
# title(paste("Monthly weighed Climatology of CE cases per month Norm the median of", max_month))


ggplot(ST_E_monthly, aes(y = GLB_ench.N_MW/max_median,
                         x = factor(month,
                                    levels = 1:12,
                                    labels = month.abb[1:12]))) +
  geom_boxplot(fill = varcol("GLB_diff"), outliers = FALSE) +
  xlab("") +
  ylab("Relative monthly weighted occurrences") +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 3) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
# geom_dotplot(binaxis='y', stackdir='center', dotsize=.3) +
# geom_jitter(shape=16, position=position_jitter(0.2))



#'
#' ## Monthly extreme CE raw
#'

#+ climECEmonth, echo=F, include=T, results="asis"
boxplot(ST_extreme_monthly$GLB_ench.N ~ ST_extreme_monthly$month )
title("Climatology of ECE cases per month")

#+ climECEmonth-MW, echo=F, include=T, results="asis"
boxplot(ST_extreme_monthly$GLB_ench.N_MW ~ ST_extreme_monthly$month )
title("Climatology of ECE cases per month")


#'
#' ## Monthly extreme CE
#'
#+ clim-ECE-month-norm-MAX-median-N, echo=F, include=T, results="asis"
# normalize with max monthly median
temp       <- ST_extreme_monthly[, median(GLB_ench.N, na.rm = T), by = month]
max_median <- max(temp$V1)
max_month  <- month.name[temp[which.max(temp$V1), month]]

boxplot(ST_extreme_monthly[, GLB_ench.N/max_median ~ month ])
title(paste("Climatology of ECE cases per month Norm the median of", max_month))

ggplot(ST_extreme_monthly, aes(y = GLB_ench.N/max_median,
                         x = factor(month,
                                    levels = 1:12,
                                    labels = month.abb[1:12]))) +
  geom_boxplot(fill = "lightblue", outliers = FALSE) +
  xlab("") +
  ylab("Relative monthly occurrences") +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 3) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )


#'
#' ## Monthly weighted extreme CE
#'
#+ clim-ECE-month-norm-MAX-median-N-MW, echo=F, include=T, results="asis"

# normalize with max monthly median
temp       <- ST_extreme_monthly[, median(GLB_ench.N_MW, na.rm = T), by = month]
max_median <- max(temp$V1)
max_month  <- month.name[temp[which.max(temp$V1), month]]

# boxplot(ST_extreme_monthly[, GLB_ench.N_MW/max_median ~ month ])
# title(paste("Climatology of monthly weighted ECE cases per month Norm the median of", max_month))

ggplot(ST_extreme_monthly, aes(y = GLB_ench.N_MW/max_median,
                               x = factor(month,
                                          levels = 1:12,
                                          labels = month.abb[1:12]))) +
  geom_boxplot(fill = "lightblue", outliers = FALSE) +
  xlab("") +
  ylab("Relative monthly weighted occurrences") +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 3) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )






#+ climCE-daily, echo=F, include=FALSE, results="asis"
boxplot(ST_E_daily$GLB_ench.N ~ yday(ST_E_daily$Date) )
title("Climatology of CE cases per DOY")


#+ climCE-week, echo=F, include=FALSE, results="asis"
boxplot(ST_E_daily$GLB_ench.N ~ week(ST_E_daily$Date) )
title("Climatology of CE cases per weak")



## Trends on groups ---------

# ST_G0





## Yearly Energy contribution of enhancements  ---------------------------------------

#'
#' \newpage
#' \FloatBarrier
#'
#' ### Energy contribution of enhancements
#'
#+ energy, echo=F, include=T, results="asis"

{
  plot(ST_E_yearly[, GLB_diff.sum, year],
       col = varcol("GLB_diff.sum"),
       ylab = "kJ/m^2")

  lmD <- lm( ST_E_yearly[, year, GLB_diff.sum])
  abline(lmD)

  title("Over Irradiance SUM")

  ## display trend on graph
  fit <- lmD[[1]]
  legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
         paste("Trend: ",
               if (fit[2] > 0) "+" else "-",
               signif(abs(fit[2]), 2) ,"kJ/m^2/y" )
  )
}


{
  plot(ST_E_yearly[, GLB_diff.N, year],
       col = varcol("GLB_diff.N"),
       ylab = "Number of enhancements")
  title("Number of CE each year")

  lmD <- lm( ST_E_yearly[, year, GLB_diff.N])
  abline(lmD)

  ## display trend on graph
  fit <- lmD[[1]]
  legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
         paste("Trend: ",
               if (fit[2] > 0) "+" else "-",
               signif(abs(fit[2]), 2) ,"/y" )
  )
}


{
  plot(ST_E_yearly[, GLB_diff.mean, year],
       col = varcol("GLB_diff.mean"),
       ylab = "W/m^2")
  lmD <- lm( ST_E_yearly[, year, GLB_diff.mean])
  abline(lmD)

  title("Mean  of over irradiance per CE")

  ## display trend on graph
  fit <- lmD[[1]]
  legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
         paste("Trend: ",
               if (fit[2] > 0) "+" else "-",
               signif(abs(fit[2]), 2) ,"W/m^2/y" )
  )
}



{
  plot(ST_E_yearly[, GLB_diff.median, year],
       col = varcol("GLB_diff.median"),
       ylab = "W/m^2")
  lmD <- lm( ST_E_yearly[, year, GLB_diff.median])
  abline(lmD)

  title("Median  of over irradiance per CE")

  ## display trend on graph
  fit <- lmD[[1]]
  legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
         paste("Trend: ",
               if (fit[2] > 0) "+" else "-",
               signif(abs(fit[2]), 2) ,"W/m^2/y" )
  )
}


# {
#   plot(ST_E_yearly[, GLB_diff.p_5, year],
#        col = varcol("GLB_diff.p_5"),
#        ylab = "W/m^2")
#   lmD <- lm( ST_E_yearly[, year, GLB_diff.p_5])
#   abline(lmD)
#
#   title("p_5 of over irradiance per CE")
#
#   ## display trend on graph
#   fit <- lmD[[1]]
#   legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
#          paste("Trend: ",
#                if (fit[2] > 0) "+" else "-",
#                signif(abs(fit[2]), 2) ,"W/m^2/y" )
#   )
# }
#
#
# {
#   plot(ST_E_yearly[, GLB_diff.p_10, year],
#        col = varcol("GLB_diff.p_10"),
#        ylab = "W/m^2")
#   lmD <- lm( ST_E_yearly[, year, GLB_diff.p_10])
#   abline(lmD)
#
#   title("p_10 of over irradiance per CE")
#
#   ## display trend on graph
#   fit <- lmD[[1]]
#   legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
#          paste("Trend: ",
#                if (fit[2] > 0) "+" else "-",
#                signif(abs(fit[2]), 2) ,"W/m^2/y" )
#   )
# }
#
#
#
# {
#   plot(ST_E_yearly[, GLB_diff.p_90, year],
#        col = varcol("GLB_diff.p_90"),
#        ylab = "W/m^2")
#   lmD <- lm( ST_E_yearly[, year, GLB_diff.p_90])
#   abline(lmD)
#
#   title("p_90 of over irradiance per CE")
#
#   ## display trend on graph
#   fit <- lmD[[1]]
#   legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
#          paste("Trend: ",
#                if (fit[2] > 0) "+" else "-",
#                signif(abs(fit[2]), 2) ,"W/m^2/y" )
#   )
# }
#
#
#
# {
#   plot(ST_E_yearly[, GLB_diff.p_95, year],
#        col = varcol("GLB_diff.p_95"),
#        ylab = "W/m^2")
#   lmD <- lm( ST_E_yearly[, year, GLB_diff.p_95])
#   abline(lmD)
#
#   title("p_95 of over irradiance per CE")
#
#   ## display trend on graph
#   fit <- lmD[[1]]
#   legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
#          paste("Trend: ",
#                if (fit[2] > 0) "+" else "-",
#                signif(abs(fit[2]), 2) ,"W/m^2/y" )
#   )
# }




## relative to all points ------------------------------


{
  plot(ST_E_yearly[, GLB_diff.sum/All_N, year],
       col = varcol("GLB_diff.sum"),
       ylab = "kJ/m^2")

  lmD <- lm( ST_E_yearly[, year, GLB_diff.sum/All_N])
  abline(lmD)

  title("Over Irradiance SUM / total minutes")

  ## display trend on graph
  fit <- lmD[[1]]
  legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
         paste("Trend: ",
               if (fit[2] > 0) "+" else "-",
               signif(abs(fit[2]), 2) ,"kJ/m^2/y" )
  )
}

{
  plot(ST_E_yearly[, GLB_diff.sum/All_N, year],
       col = varcol("GLB_diff.sum"),
       ylab = "kJ/m^2")

  lmD <- lm( ST_E_yearly[, year, GLB_diff.sum/All_N])
  abline(lmD)

  title("Over Irradiance SUM / total minutes")

  ## display trend on graph
  fit <- lmD[[1]]
  legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
         paste("Trend: ",
               if (fit[2] > 0) "+" else "-",
               signif(abs(fit[2]), 2) ,"kJ/m^2/y" )
  )
}





# plot(ST_yearly[, GLB_diff.mean, year],
#      ylab = "Total of enhancement minutes")
#
#
#
# plot(ST_yearly[, GLB_ench.N, year],
#      ylab = "Total of enhancement minutes")
#
#
# plot(ST_yearly[, GLB_ench.N_pos/GLB_ench.TotalN, year],
#      ylab = "Total of enhancement minutes ratio")
#
#
# plot(ST_yearly[, GLB_ench.sumPOS/GLB_ench.N_pos, year],
#      ylab = "Enhancement energy per minute")


cat( "**@Martins2022**\n" )

plot(ST_yearly[, GLB_diff.sumPOS/wattGLB.sumPOS, year],
     ylab = "Fraction of the accumulated enhancements over total energy",
     main = paste(varname("GLB_diff.sumPOS"),
                  staname("GLB_diff.sumPOS"),
                  "/",
                  varname("wattGLB.sumPOS"),
                  staname("wattGLB.sumPOS")
     )
)






## P_energy ---------------------------------------------------------------

yeartrends <- data.table()

#' \newpage
#' ## Yearly trends
#'
#+ P-energy, echo=F, include=T, results="asis"


### Total energy per year  -------------------


pvar1   <- "GLB_diff.sum"
dataset <- copy(ST_E_yearly)
# partial year sum is not valid
dataset <- dataset[year > 1993]

## linear model by year step
lmY1 <- lm(dataset[[pvar1]] ~ dataset$year)
d2   <- summary(lmY1)$coefficients
cat("lmY:     ", round(lmY1$coefficients[2], 6) , "+/-", round(d2[2,2], 6) ,"\n\n")
## correlation test by day step
corY <- cor.test(x = dataset[[pvar1]], y = as.numeric(dataset$year), method = 'pearson')
# capture lm for table
yeartrends <- rbind(yeartrends,
                    data.frame(
                      linear_fit_stats(lmY1, confidence_interval = 0.99),
                      cor_test_stats(corY),
                      DATA       = "ST_E_yearly",
                      var        = pvar1,
                      N          = sum(!is.na(dataset[[pvar1]]))
                    )
)

grob <- grobTree(
  textGrob(
    label = TeX(
      paste("Trend:  $", round(lmY1$coefficients[2], 1),
            "\\pm",      round(2 * d2[2,2],          1),       ## show 2 sigma
            "\\,kJ/m^2/year$")),
    x = 0.95,  y = 0.05, hjust = 1,
    gp = gpar(col = "black", fontsize = 13, fontface = "bold")
  ))

p1 <- ggplot(dataset,
             aes(x = year,
                 y = get(pvar1))) +
  geom_point(color = varcol(pvar1),
             shape = 15,
             size  = 3) +
  geom_abline(intercept = lmY1$coefficients[1], slope = lmY1$coefficients[2]) +
  ylab(bquote("CE" ~ .(varname(pvar1)) ~ .(staname(pvar1)) ~ group("[", MJ/m^2,"]"))) +
  xlab("Date") +
  annotation_custom(grob) +
  scale_y_continuous(guide        = "axis_minor",
                     labels = function(x) x / 1000,
                     minor_breaks = seq(0, 500, by = 25)) +
  scale_x_continuous(guide        = "axis_minor",
                     limits = c(1993, NA),
                     breaks = c(
                       1993,
                       pretty(dataset[, year], n = 4),
                       max(ceiling(dataset[, year]))),
                     minor_breaks = seq(1990, 2050, by = 1) )
p1



### Number of cases per year  -----------------------

pvar2   <- "GLB_diff.N"
dataset <- copy(ST_E_yearly)
# partial year N is not valid
dataset <- dataset[year > 1993]

## linear model by year step
lmY2 <- lm(dataset[[pvar2]] ~ dataset$year)
d2   <- summary(lmY2)$coefficients
cat("lmY:     ", round(lmY2$coefficients[2], 6) , "+/-", round(d2[2,2], 6) ,"\n\n")
## correlation test by day step
corY <- cor.test(x = dataset[[pvar2]], y = as.numeric(dataset$year), method = 'pearson')
# capture lm for table
yeartrends <- rbind(yeartrends,
                    data.frame(
                      linear_fit_stats(lmY2, confidence_interval = 0.99),
                      cor_test_stats(corY),
                      DATA       = "ST_E_yearly",
                      var        = pvar2,
                      N          = sum(!is.na(dataset[[pvar2]]))
                    )
)

grob <- grobTree(
  textGrob(
    label = TeX(
      paste("Trend:  $", round(lmY2$coefficients[2], 1),
            "\\pm",      round(2 * d2[2,2],          1),   ## show 2 sigma
            "\\,cases/year$")),
    x = 0.95,  y = 0.05, hjust = 1,
    gp = gpar(col = "black", fontsize = 13, fontface= "bold")
  ))

p2 <- ggplot(dataset,
             aes(x = year,
                 y = get(pvar2))) +
  geom_point(color = varcol(pvar2),
             shape = 17,
             size  = 3) +
  geom_abline(intercept = lmY2$coefficients[1], slope = lmY2$coefficients[2]) +
  ylab(bquote("CE" ~ .(varname(pvar2)) ~ .(staname(pvar2)))) +
  xlab("Date") +
  annotation_custom(grob) +
  scale_y_continuous(guide        = "axis_minor",
                     minor_breaks = seq(0, 500, by = 25)) +
  scale_x_continuous(guide        = "axis_minor",
                     limits = c(1993, NA),
                     breaks = c(
                       1993,
                       pretty(dataset[,year], n = 4),
                       max(ceiling(dataset[,year]))),
                     minor_breaks = seq(1990, 2050, by = 1) )
p2


###  Mean OIR energy per year -------------

pvar3   <- "GLB_diff.mean"
dataset <- copy(ST_E_yearly)

## linear model by year step
lmY3 <- lm(dataset[[pvar3]] ~ dataset$year)
d2   <- summary(lmY3)$coefficients
cat("lmY:     ", round(lmY3$coefficients[2], 6) , "+/-", round(d2[2,2], 6) ,"\n\n")
## correlation test by day step
corY <- cor.test(x = dataset[[pvar3]], y = as.numeric(dataset$year), method = 'pearson')
# capture lm for table
yeartrends <- rbind(yeartrends,
                    data.frame(
                      linear_fit_stats(lmY3, confidence_interval = 0.99),
                      cor_test_stats(corY),
                      DATA       = "ST_E_yearly",
                      var        = pvar3,
                      N          = sum(!is.na(dataset[[pvar3]]))
                    )
)

grob <- grobTree(
  textGrob(
    label = TeX(
      paste("Trend:  $", round(lmY3$coefficients[2], 1),
            "\\pm",      round(2 * d2[2,2],          2),      ## show 2 sigma
            "\\,W/m^2/year$")),
    x = 0.95,  y = 0.05, hjust = 1,
    gp = gpar(col = "black", fontsize = 13, fontface= "bold")
  ))

p3 <- ggplot(dataset,
             aes(x = year,
                 y = get(pvar3))) +
  geom_point(color = varcol(pvar3),
             shape = 16,
             size  = 3) +
  geom_abline(intercept = lmY3$coefficients[1], slope = lmY3$coefficients[2]) +
  ylab(bquote("CE" ~ .(varname(pvar3)) ~ .(staname(pvar3)) ~ group("[", W/m^2,"]"))) +
  xlab("Date") +
  annotation_custom(grob) +
  scale_y_continuous(guide        = "axis_minor",
                     minor_breaks = seq(0, 500, by = 25)) +
  scale_x_continuous(guide        = "axis_minor",
                     limits = c(1993, NA),
                     breaks = c(
                       1993,
                       pretty(dataset[,year], n = 4),
                       max(ceiling(dataset[,year]))),
                     minor_breaks = seq(1990, 2050, by = 1) )
p3



###  Median energy per year  ----------------------------------

pvar4   <- "GLB_diff.median"
dataset <- copy(ST_E_yearly)

## linear model by year step
lmY4 <- lm(dataset[[pvar4]] ~ dataset$year)
d2   <- summary(lmY4)$coefficients
cat("lmY:     ", round(lmY4$coefficients[2], 6) , "+/-", round(d2[2,2], 6) ,"\n\n")
## correlation test by day step
corY <- cor.test(x = dataset[[pvar4]], y = as.numeric(dataset$year), method = 'pearson')
# capture lm for table
yeartrends <- rbind(yeartrends,
                    data.frame(
                      linear_fit_stats(lmY4, confidence_interval = 0.99),
                      cor_test_stats(corY),
                      DATA       = "ST_E_yearly",
                      var        = pvar4,
                      N          = sum(!is.na(dataset[[pvar4]]))
                    )
)

grob <- grobTree(
  textGrob(
    label = TeX(
      paste("Trend:  $", round(lmY4$coefficients[2], 1),
            "\\pm",      round(2 * d2[2,2], 1),          ## show 2 sigma
            "\\,W/m^2/year$")),
    x = 0.95,  y = 0.05, hjust = 1,
    gp = gpar(col = "black", fontsize = 13, fontface = "bold")
  ))

p4 <- ggplot(dataset,
             aes(x = year,
                 y = get(pvar4))) +
  # geom_errorbar(aes(ymin = get(pvar4) - GLB_diff.SD,     ## error bars too big
  #                   ymax = get(pvar4) + GLB_diff.SD)) +
  geom_point(color = varcol(pvar4),
             shape = 16,
             size  = 3) +
  geom_abline(intercept = lmY4$coefficients[1], slope = lmY4$coefficients[2]) +
  ylab(bquote("CE" ~ .(varname(pvar4)) ~ .(staname(pvar4)) ~ group("[", W/m^2,"]"))) +
  xlab("Date") +
  annotation_custom(grob) +
  scale_y_continuous(guide        = "axis_minor",
                     minor_breaks = seq(0, 500, by = 25)) +
  scale_x_continuous(guide        = "axis_minor",
                     limits = c(1993, NA),
                     breaks = c(
                       1993,
                       pretty(dataset[,year], n = 4),
                       max(ceiling(dataset[,year]))),
                     minor_breaks = seq(1990, 2050, by = 1) )
p4






#' \newpage
#' ## Yearly trends month weighted
#'
#+ P-MW-energy, echo=F, include=T, results="asis"

### MW Total energy per year monthly weight -------------------


##  create yearly from monthly data presence weight

ST_monthly[ , MW := wattGLB.N/All_N]


# ST_monthly[ , .(MW, GLB_diff.N, GLB_diff.N_pos, GLB_diff.sumPOS) ]

ST_MW_yearly <- ST_monthly[, .(
  GLB_diff.sum = sum(GLB_diff.sumPOS * MW, na.rm = T),
  GLB_diff.N   = sum(GLB_diff.N_pos  * MW, na.rm = T)
), by = year]



pvar1   <- "GLB_diff.sum"
dataset <- copy(ST_MW_yearly)
# partial year sum is not valid
dataset <- dataset[year > 1993]

## linear model by year step
lmY1 <- lm(dataset[[pvar1]] ~ dataset$year)
d2   <- summary(lmY1)$coefficients
cat("lmY:     ", round(lmY1$coefficients[2], 6) , "+/-", round(d2[2,2], 6) ,"\n\n")
## correlation test by day step
corY <- cor.test(x = dataset[[pvar1]], y = as.numeric(dataset$year), method = 'pearson')
# capture lm for table
yeartrends <- rbind(yeartrends,
                    data.frame(
                      linear_fit_stats(lmY1, confidence_interval = 0.99),
                      cor_test_stats(corY),
                      DATA       = "ST_yearly_MW",
                      var        = pvar1,
                      N          = sum(!is.na(dataset[[pvar1]]))
                    )
)

grob <- grobTree(
  textGrob(
    label = TeX(
      paste("Trend:  $", round(lmY1$coefficients[2], 1),
            "\\pm",      round(2 * d2[2,2],          1),       ## show 2 sigma
            "\\,kJ/m^2/year$")),
    x = 0.95,  y = 0.05, hjust = 1,
    gp = gpar(col = "black", fontsize = 13, fontface = "bold")
  ))

p1 <- ggplot(dataset,
             aes(x = year,
                 y = get(pvar1))) +
  geom_point(color = varcol(pvar1),
             shape = 15,
             size  = 3) +
  geom_abline(intercept = lmY1$coefficients[1], slope = lmY1$coefficients[2]) +
  ylab(bquote("CE" ~ .(varname(pvar1)) ~ .(staname(pvar1)) ~ group("[", MJ/m^2,"]"))) +
  xlab("Date") +
  annotation_custom(grob) +
  scale_y_continuous(guide        = "axis_minor",
                     labels = function(x) x / 1000,
                     minor_breaks = seq(0, 500, by = 25)) +
  scale_x_continuous(guide        = "axis_minor",
                     limits = c(1993, NA),
                     breaks = c(
                       1993,
                       pretty(dataset[, year], n = 4),
                       max(ceiling(dataset[, year]))),
                     minor_breaks = seq(1990, 2050, by = 1) )
p1



### Number of cases per year  -----------------------

pvar2   <- "GLB_diff.N"
dataset <- copy(ST_MW_yearly)
# partial year N is not valid
dataset <- dataset[year > 1993]

## linear model by year step
lmY2 <- lm(dataset[[pvar2]] ~ dataset$year)
d2   <- summary(lmY2)$coefficients
cat("lmY:     ", round(lmY2$coefficients[2], 6) , "+/-", round(d2[2,2], 6) ,"\n\n")
## correlation test by day step
corY <- cor.test(x = dataset[[pvar2]], y = as.numeric(dataset$year), method = 'pearson')
# capture lm for table
yeartrends <- rbind(yeartrends,
                    data.frame(
                      linear_fit_stats(lmY2, confidence_interval = 0.99),
                      cor_test_stats(corY),
                      DATA       = "ST_yearly_MW",
                      var        = pvar2,
                      N          = sum(!is.na(dataset[[pvar2]]))
                    )
)

grob <- grobTree(
  textGrob(
    label = TeX(
      paste("Trend:  $", round(lmY2$coefficients[2], 1),
            "\\pm",      round(2 * d2[2,2],          1),   ## show 2 sigma
            "\\,cases/year$")),
    x = 0.95,  y = 0.05, hjust = 1,
    gp = gpar(col = "black", fontsize = 13, fontface= "bold")
  ))

p2 <- ggplot(dataset,
             aes(x = year,
                 y = get(pvar2))) +
  geom_point(color = varcol(pvar2),
             shape = 17,
             size  = 3) +
  geom_abline(intercept = lmY2$coefficients[1], slope = lmY2$coefficients[2]) +
  ylab(bquote("CE" ~ .(varname(pvar2)) ~ .(staname(pvar2)))) +
  xlab("Date") +
  annotation_custom(grob) +
  scale_y_continuous(guide        = "axis_minor",
                     minor_breaks = seq(0, 500, by = 25)) +
  scale_x_continuous(guide        = "axis_minor",
                     limits = c(1993, NA),
                     breaks = c(
                       1993,
                       pretty(dataset[,year], n = 4),
                       max(ceiling(dataset[,year]))),
                     minor_breaks = seq(1990, 2050, by = 1) )
p2













### Multiple plots -----

# aligned <- align_plots(p1, p2, p3, p4, align = "v")
#
# ## plot one by one
# ggdraw(aligned[[1]])
# ggdraw(aligned[[2]])
# ggdraw(aligned[[3]])
# ggdraw(aligned[[4]])
#
#
#
# #+ energy-multi, echo=F, include=T, out.heigth="100%"
# plot_grid(p1, p2, p3, labels = c('A', 'B', "(C)"), ncol = 1, align = "v")
# #+ echo=F, include=T

write.csv(yeartrends, "./figures/Daily_trends_byYear_Proper.csv")









#+ rel-energy, echo=F, include=T, results="asis"

{
  plot(ST_yearly[, 100 * (GLB_diff.sumPOS/wattGLB.sum), year],
       col = varcol("GLB_diff.sumPOS"),
       ylab = "Over Irradiance fraction %")
  lmD <- lm( ST_yearly[, year, 100 * (GLB_diff.sumPOS/wattGLB.sum)])
  abline(lmD)

  title("CE Over Irradiance % of total energy")

  ## display trend on graph
  fit <- lmD[[1]]
  legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
         paste("Trend: ",
               if (fit[2] > 0) "+" else "-",
               signif(abs(fit[2]), 2) ,"%/y" )
  )
}


# ST_yearly[,  .(GLB_diff.sumPOS, wattGLB.N, All_N) ]

{
  plot(ST_yearly[,  GLB_diff.sumPOS * (wattGLB.N/All_N), year],
       col = varcol("GLB_diff.sumPOS"),
       ylab = "CE cases %")
  title("CE cases % in all measurements GLB_diff.N_pos/wattGLB.N")

  lmD <- lm( ST_yearly[, year, GLB_diff.sumPOS * (wattGLB.N/All_N)])
  abline(lmD)

  ## display trend on graph
  fit <- lmD[[1]]
  legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
         paste("Trend: ",
               if (fit[2] > 0) "+" else "-",
               signif(abs(fit[2]), 2) ,"%/y" )
  )
}

{
  plot(ST_yearly[, 100 * GLB_diff.N_pos/All_N, year],
       col = varcol("GLB_diff.sumPOS"),
       ylab = "CE cases %")
  title("CE cases % in all minutes GLB_diff.N_pos/All_N")

  lmD <- lm( ST_yearly[, year, 100 * GLB_diff.N_pos/All_N])
  abline(lmD)

  ## display trend on graph
  fit <- lmD[[1]]
  legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
         paste("Trend: ",
               if (fit[2] > 0) "+" else "-",
               signif(abs(fit[2]), 2) ,"%/y" )
  )
}






{
  plot(ST_yearly[, 100 * (GLB_diff.sumPOS/GLB_diff.N_pos) / (wattGLB.sumPOS/wattGLB.N_pos), year],
       col = varcol("GLB_diff.sumPOS"),
       ylab = "Mean CE / Mean GHI [%]")

  lmD <- lm( ST_yearly[, year, 100 * (GLB_diff.sumPOS/GLB_diff.N_pos) / (wattGLB.sumPOS/wattGLB.N_pos)])
  abline(lmD)

  title("Mean Over irradiance % on mean Global Radiation")

  ## display trend on graph
  fit <- lmD[[1]]
  legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
         paste("Trend: ",
               if (fit[2] > 0) "+" else "-",
               signif(abs(fit[2]), 2) ,"%/y" )
  )
}








##;
##;
##; ## Monthly Energy contribution of enhancements  ##; ---------------------------------------
##;
##; #'
##; #' \newpage
##; #' \FloatBarrier
##; #'
##; #' ### Monthly Energy contribution of enhancements
##; #'
##; #+ energy-monthly, echo=F, include=T, results="asis"
##;
##; mcol <- viridis(n = 12)
##;
##;
##; {
##;   plot(ST_E_monthly[, GLB_diff.sum, Date],
##;        # col = varcol("GLB_diff.sum"),
##;        col = mcol[ST_E_monthly$month],
##;        ylab = "W/m^2")
##;   title("Over Irradiance Energy to CE")
##;
##;   lmD <- lm( ST_E_monthly[, Date, GLB_diff.sum])
##;   abline(lmD)
##;
##;   ## display trend on graph
##;   fit <- lmD[[1]]
##;   legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
##;          paste("Trend: ",
##;                if (fit[2] > 0) "+" else "-",
##;                signif(abs(fit[2]), 2) ,"kJ/m^2/y" )
##;   )
##; }
##;
##;
##;
##;
##; {
##;   plot(ST_E_monthly[, GLB_diff.N, Date],
##;        # col = varcol("GLB_diff.sum"),
##;        col = mcol[ST_E_monthly$month],
##;        ylab = "Number of enhancements")
##;   title("Number of CE each year")
##;
##;   lmD <- lm( ST_E_monthly[, Date, GLB_diff.N])
##;   abline(lmD)
##;
##;   ## display trend on graph
##;   fit <- lmD[[1]]
##;   legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
##;          paste("Trend: ",
##;                if (fit[2] > 0) "+" else "-",
##;                signif(abs(fit[2]), 2) ,"/y" )
##;   )
##; }
##;
##;
##; {
##;   plot(ST_E_monthly[, GLB_diff.sum/GLB_diff.N, Date],
##;        col = mcol[ST_E_monthly$month],
##;        ylab = "W/m^2")
##;   lmD <- lm( ST_yearly[, year, GLB_diff.sumPOS/GLB_diff.N_pos])
##;   abline(lmD)
##;
##;   title("Mean Energy of over irradiance per CE")
##;
##;   ## display trend on graph
##;   fit <- lmD[[1]]
##;   legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
##;          paste("Trend: ",
##;                if (fit[2] > 0) "+" else "-",
##;                signif(abs(fit[2]), 2) ,"W/m^2/y" )
##;   )
##; }
##;
##;
##;
##;
##; plot(ST_E_monthly[, GLB_diff.mean, year],
##;      ylab = "Total of enhancement minutes")
##;
##;
##;
##; plot(ST_E_monthly[, GLB_ench.N, year],
##;      ylab = "Total of enhancement minutes")
##;
##;
##; # plot(ST_E_monthly[, GLB_ench.N/GLB_ench., year],
##; #      ylab = "Total of enhancement minutes ratio")
##;
##;
##; plot(ST_E_monthly[, GLB_ench.sum/GLB_ench.N, year],
##;      ylab = "Enhancement energy per minute")


cat( "@Martins2022" )

plot(ST_E_monthly[, GLB_diff.sum/wattGLB.sum, year],
     ylab = "Fraction of the accumulated enhancements over total energy",
     main = paste(varname("GLB_diff.sumPOS"),
                  staname("GLB_diff.sumPOS"),
                  "/",
                  varname("wattGLB.sumPOS"),
                  staname("wattGLB.sumPOS")
     )
)





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

test <- ST_monthly[, mean( MW ), by = month]
plot(test, main = "Mean monthly weight", ylab = "MW")


plot(ST_monthly[, MW,             month], main = "Monthly weight")

plot(ST_monthly[, All_N,          month], main = "All data")

plot(ST_monthly[, GLB_ench.N_pos, month], main = "CE N")




#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (difftime(tac,tic,units = "sec") > 30) {
  system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
  system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'R script ended'"))
}
