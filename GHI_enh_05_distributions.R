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


##  Prepare raw data if needed  ------------------------------------------------
if (
  file.exists("./data/GHI_enh_02_ID_CE.Rda") == FALSE |
  file.mtime("./data/GHI_enh_02_ID_CE.Rda") < file.mtime("./GHI_enh_00_variables.R") |
  file.mtime("./data/GHI_enh_02_ID_CE.Rda") < file.mtime("./GHI_enh_02_ID_CE.R")
) {
  torun <- "./GHI_enh_02_ID_CE.R"
  cat(paste("Run previous step:", torun))
  source(torun)
  dummy <- gc()
}


##  Load Enhancement data  -----------------------------------------------------

## load statistics
load("./data/GHI_enh_03_process.Rda")
load("./data/GHI_enh_02_ID_CE.Rda")
tic  <- Sys.time()

DRAFT <- TRUE
DRAFT <- FALSE

cat("Reference mode:      ", csmodel,   "\n\n")
cat("Enhancemnet criteria:", SelEnhanc, "\n\n")





##  Daily  ---------------------------------------------------------------------

#'
#' \FloatBarrier
#'
#' ### Frequency Distributions
#'
#+ freqdistributions, echo=F, include=T, results="asis"


breaks <- 50
hist(DATA[GLB_diff > 0, GLB_ench],
     breaks = breaks,
     col  = varcol( "GLB_ench"),
     xlab = varname("GLB_ench"),
     main = varname("GLB_ench"))

hist(DATA[GLB_diff > 0, GLB_diff],
     breaks = breaks,
     col  = varcol("GLB_diff"),
     xlab = bquote(.(varname("GLB_diff")) ~ group("[", W/m^2,"]")),
     main = bquote(.(varname("GLB_diff")) ~ group("[", W/m^2,"]")))

hist(DATA[GLB_diff > 0, GLB_rati],
     breaks = breaks,
     col  = varcol( "GLB_rati"),
     xlab = varname("GLB_rati"),
     main = varname("GLB_rati"))




##  Distributions  -------------------------------------------------------------

#'
#' \FloatBarrier
#'
#' ### Distributions
#'
#+ relative-distributions, echo=F, include=T, results="asis"

# hist(DATA[get(unique(CEC)) == TRUE, GLB_ench],
#      breaks = breaks,
#      freq   = FALSE,
#      col    = varcol( "GLB_ench"),
#      xlab   = varname("GLB_ench"),
#      main   = varname("GLB_ench"))

h <- hist(DATA[GLB_diff > 0, GLB_ench],
     breaks = breaks,
     freq   = FALSE,
     col    = varcol( "GLB_ench"),
     xlab   = varname("GLB_ench"),
     main   = varname("GLB_ench"),
     plot = FALSE)

## concert to per cent
h$density = h$counts/sum(h$counts)*100
plot(h,
     freq   = FALSE,
     col    = varcol( "GLB_ench"),
     xlab   = varname("GLB_ench"),
     ylab   = "Relative frequency [%]",
     main   = varname("GLB_ench")
)






h <- hist(DATA[GLB_diff > 0, GLB_diff],
          breaks = breaks,
          freq   = FALSE,
          col    = varcol("GLB_diff"),
          xlab   = bquote(.(varname("GLB_diff")) ~ group("[", W/m^2,"]")),
          main   = bquote(.(varname("GLB_diff")) ~ group("[", W/m^2,"]")),
          plot = FALSE)

h$density = h$counts/sum(h$counts)*100
plot(h,
     freq   = FALSE,
     col    = varcol("GLB_diff"),
     xlab   = bquote(.(varname("GLB_diff")) ~ group("[", W/m^2,"]")),
     ylab   = "Relative frequency [%]",
     main   = bquote(.(varname("GLB_diff")) ~ group("[", W/m^2,"]")),
)





h <- hist(DATA[GLB_diff > 0, GLB_rati],
          breaks = breaks,
          freq   = FALSE,
          col    = varcol( "GLB_rati"),
          xlab   = varname("GLB_rati"),
          main   = varname("GLB_rati"),
          plot = FALSE)

h$density = h$counts/sum(h$counts)*100

plot(h,
     freq   = FALSE,
     col    = varcol( "GLB_rati"),
     xlab   = varname("GLB_rati"),
     ylab   = "Relative frequency [%]",
     main   = varname("GLB_rati"),
)





ggplot(data = DATA[GLB_diff > 0,], aes(x = GLB_diff)) +
  geom_histogram(aes(y = (..count..)/sum(..count..) * 100),
                 binwidth = 20,
                 color    = "black",
                 fill     = varcol("GLB_diff")) +
  xlab(bquote(.(varname("GLB_diff")) ~ group("[", W/m^2,"]"))) +
  ylab( "Relative frequency [%]") +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(face="bold"))

# scale_y_continuous(labels = function(x) paste0(x, "%"))

# print(p, vp = grid::viewport(gp=grid::gpar(cex=1.5)))
# theme_gray(base_size = 18)



##  Extreme cases Distributions  -----------------------------------------------

#'
#' \FloatBarrier
#'
#' ### Extreme cases Distributions
#'
#+ extremedistributions, echo=F, include=T, results="asis"

DATA[, TSI_OI := wattGLB - ETH]

cat("Maximum TSI OI:", round(max(DATA[TSI_OI > 0, TSI_OI]), 1), "W/m^2\n\n")

cat("Quantiles of TSI OI:\n")
pander::pander(quantile(DATA[TSI_OI > 0, TSI_OI]))



hist(DATA[wattGLB > ETH, GLB_ench],
     breaks = breaks,
     freq   = FALSE,
     col    = "lightblue",
     xlab   = varname("GLB_ench"),
     main   = paste("Extreme cases of", varname("GLB_ench")))

hist(DATA[wattGLB > ETH, GLB_diff],
     breaks = breaks,
     col    = "lightblue",
     xlab   = bquote(.(varname("GLB_diff")) ~ group("[", W/m^2,"]")),
     main   = paste("Extreme cases of", varname("GLB_diff")) )


hist(DATA[wattGLB > ETH, GLB_rati],
     breaks = breaks,
     col    = "lightblue",
     xlab   = varname("GLB_rati"),
     main   = paste("Extreme cases of", varname("GLB_rati")))




hist(DATA[wattGLB > ETH, wattGLB - ETH],
     breaks = breaks,
     col    = "lightblue",
     xlab   = bquote(.(varname("GLB_diff")) ~ group("[", W/m^2,"]")),
     main   = paste("Extreme cases of CE above TSI") )




table(DATA$CEC, useNA = "ifany")





## test distribution for one year
ayear <- 2020

hist(DATA[get(unique(CEC)) == TRUE & year(Date) == ayear, GLB_ench],
     breaks = breaks,
     col  = varcol( "GLB_ench"),
     xlab = varname("GLB_ench"),
     main = paste(varname("GLB_ench"), ayear))

hist(DATA[get(unique(CEC)) == TRUE & year(Date) == ayear, GLB_diff],
     breaks = breaks,
     col  = varcol("GLB_diff"),
     xlab = bquote(.(varname("GLB_diff")) ~ group("[", W/m^2,"]")),
     main = bquote(.(varname("GLB_diff")) ~ group("[", W/m^2,"]")))

hist(DATA[get(unique(CEC)) == TRUE & year(Date) == ayear, GLB_rati],
     breaks = breaks,
     col  = varcol( "GLB_rati"),
     xlab = varname("GLB_rati"),
     main = varname("GLB_rati"))



## P-relative-distribution-diff replot for paper  -------

#+ P-relative-distribution-diff, echo=F, include=T, results="asis"
binwidth <- 20
split    <- 170
split    <- binwidth/2 + (split %/% binwidth) * binwidth

p1 <- ggplot(data = DATA[GLB_diff > 0], aes(x = GLB_diff)) +
  geom_histogram(aes(y = (after_stat(count))/sum(after_stat(count)) * 100),
                 binwidth = binwidth,
                 fill = varcol( "GLB_diff"),
                 boundary = 0,
                 color    = "black") +
  xlab(bquote(group("[", W/m^2,"]"))) +
  ylab("[%]") +
  coord_cartesian(xlim = c(split, max(DATA$GLB_diff)),
                  ylim = c(0, .8)) +
  theme(
    axis.title = element_text(size = 9),
    axis.text  = element_text(size = 9),
    panel.grid = element_line(linetype = 2)
  )



ggplot(data = DATA[GLB_diff > 0], aes(x = GLB_diff)) +
  geom_histogram(aes(y = (after_stat(count))/sum(after_stat(count)) * 100),
                 binwidth = binwidth,
                 boundary = 0,
                 fill = varcol( "GLB_diff"),
                 color    = "black") +
  xlab(bquote("OI" ~ group("[", W/m^2,"]"))) +
  ylab("Relative frequency [%]") +
  labs(caption = bquote(paste("Bin width: ", .(binwidth)) ~ group("[", W/m^2,"]"))) #  +
  # inset_element(p1, left = 0.4, bottom = 0.4, right = 1, top = 1,
  #               align_to = "plot")


cat(pander(summary(DATA[GLB_diff > 0, GLB_diff])),"\n")

cat(pander(quantile(DATA[GLB_diff > 0, GLB_diff])),"\n")

above100 <- 100
per100   <- 100 * DATA[GLB_diff > 0 & GLB_diff > above100, .N] / DATA[GLB_diff > 0, .N]

cat(paste("\n", per100, "% of the values are above", above100, "W/m^2\n\n" ))


belowAVG <- mean(ST_E_yearly$GLB_diff.mean)
perAVG   <- 100 * DATA[GLB_diff > 0 & GLB_diff < belowAVG, .N] / DATA[GLB_diff > 0, .N]

cat(paste("\n", round(perAVG,1), "% of the values are below mean", round(belowAVG,2), "W/m^2\n\n" ))


some_values <-  list(
    Enh_pc_below_mean = perAVG,
    Enh_OI_AVG        = belowAVG,
    Enh_pc_below_100  = per100,
    Enh_OI_100        = above100,
    Max_OI            = DATA[GLB_diff > 0, max(GLB_diff)]
  )
saveRDS(some_values, "./data/some_values_01.Rds")


## split histogram
binwidth <- 30
split    <- binwidth * 7

sa        <- hist(DATA[GLB_diff > 0 & GLB_diff <  split, GLB_diff], breaks = seq(0, split, binwidth),    plot = FALSE)
sa_counts <- sum(sa$counts)

sb        <- hist(DATA[GLB_diff > 0 & GLB_diff >= split, GLB_diff], breaks = seq(split, 1500, binwidth), plot = FALSE)
sb_counts <- sum(sb$counts)

sa_ratio <- sa_counts / (sa_counts + sb_counts)
sb_ratio <- sb_counts / (sa_counts + sb_counts)

## left
pa <- ggplot(data = DATA[GLB_diff > 0], aes(x = GLB_diff)) +
  geom_histogram(aes(y = (sa_ratio * after_stat(count))/sum(after_stat(count)) * 100),
                 binwidth = binwidth,
                 boundary = 0,
                 fill = varcol( "GLB_diff"),
                 color    = "black") +
  xlab(bquote("OI" ~ group("[", W/m^2,"]"))) +
  ylab("Relative frequency [%]") +
  scale_x_continuous(
    breaks = sort(unique(c(seq(0, split, binwidth * 2), split))),
    limits = c(0, split))

## right
pb <- ggplot(data = DATA[GLB_diff > 0], aes(x = GLB_diff)) +
  geom_histogram(aes(y = (sb_ratio * after_stat(count))/sum(after_stat(count)) * 100),
                 binwidth = binwidth,
                 boundary = 0,
                 fill = varcol( "GLB_diff"),
                 color    = "black") +
  xlab(bquote("OI" ~ group("[", W/m^2,"]"))) +
  ylab("Relative frequency [%]") +
  scale_x_continuous(
    breaks = c(seq(split, max(DATA[GLB_diff > 0, GLB_diff]), binwidth*2), ceiling(max(DATA[GLB_diff > 0, GLB_diff]))),
    limits = c(split, ceiling(max(DATA[GLB_diff > 0, GLB_diff]))))

## merge plots

library(ggpubr)

# Remove axis titles from all plots
p      <- list(pa, pb) |> map(~.x + labs(x=NULL, y=NULL, caption = NULL))
yleft  <- textGrob("Relative frequency [%]", rot = 90)
bottom <- textGrob(bquote("OI" ~ group("[", W/m^2,"]")))
margin <- theme(plot.margin = unit(c(.1,.1,.1,.1), "cm"))

## no labels
grid.arrange(grobs = lapply(p, "+", margin), ncol = 2, nrow = 1,
             left = yleft, bottom = bottom)


pa1 <- pa + labs(x=NULL, y=NULL, caption = NULL)
pa1 <- annotate_figure(pa1, fig.lab = "(a)")

pb1 <- pb + labs(x=NULL, y=NULL, caption = NULL)
pb1 <- annotate_figure(pb1, fig.lab = "(b)")

p1 <- list(pa1, pb1)

## with labels
grid.arrange(grobs = lapply(p1, "+", margin), ncol = 2, nrow = 1,
             left = yleft, bottom = bottom)
















## Extreme CE distribution  -------

#+ P-extreme-distribution, echo=F, include=T, results="asis"



binwidth = 10
ggplot(data = DATA[wattGLB > ETH], aes(x = GLB_diff)) +
  geom_histogram(aes(y = (after_stat(count))/sum(after_stat(count)) * 100),
                 binwidth = binwidth,
                 boundary = 0,
                 fill = "lightblue",
                 color    = "black") +
  xlab(bquote("OI" ~ group("[", W/m^2,"]"))) +
  ylab("Relative frequency [%]") +
  labs(caption = bquote(paste("Bin width: ", .(binwidth)) ~ group("[", W/m^2,"]"))) #  +
# inset_element(p1, left = 0.4, bottom = 0.4, right = 1, top = 1,
#               align_to = "plot")


pander(quantile(DATA[wattGLB > ETH, GLB_diff]),
       caption = "OI of ECE")

pander(quantile(DATA[wattGLB > ETH, TSI_OI]),
       caption = "TSI OI of ECE")




binwidth = 10
ggplot(data = DATA[wattGLB > ETH], aes(x = wattGLB - ETH)) +
  geom_histogram(aes(y = (after_stat(count))/sum(after_stat(count)) * 100),
                 binwidth = binwidth,
                 boundary = 0,
                 fill = "lightblue",
                 color    = "black") +
  xlab(bquote("Irradiance above TSI for the same SZA" ~ group("[", W/m^2,"]"))) +
  ylab("Relative frequency [%]") # +
  # labs(caption = bquote(paste("Bin width: ", .(binwidth)) ~ group("[", W/m^2,"]"))) #  +
# inset_element(p1, left = 0.4, bottom = 0.4, right = 1, top = 1,
#               align_to = "plot")















##  Store some date for max cases  ------------------------


MC <-   rbind(
  DATA[which.max(wattGLB),  .(Typ = "Max Global",  wattGLB, SZA, GLB_ench, GLB_diff, GLB_rati, Date, TSI_ref = ETH, TSI_OI)],
  DATA[which.max(GLB_ench), .(Typ = "Max Enh abs", wattGLB, SZA, GLB_ench, GLB_diff, GLB_rati, Date, TSI_ref = ETH, TSI_OI)],
  DATA[which.max(GLB_ench), .(Typ = "Max Enh rat", wattGLB, SZA, GLB_ench, GLB_diff, GLB_rati, Date, TSI_ref = ETH, TSI_OI)],
  DATA[which.max(TSI_OI),   .(Typ = "Max ECE",     wattGLB, SZA, GLB_ench, GLB_diff, GLB_rati, Date, TSI_ref = ETH, TSI_OI)]
)
MC[, DOY := yday(Date)]
MC[, Localtime := paste(as.POSIXlt(Date, tz = "Europe/Athens"))]




saveRDS(MC, "data/Max_cases.Rds")


pander::pander(MC, caption = "Max enhancements")


MC2 <- ST_G0[which.max(GLB_diff.N), .(Date, GLB_diff.N, SZA.first, SZA.last, SZA.min, SZA.max)]
MC2[ , DOY := yday(Date)]
MC2[ , Localtime := paste(as.POSIXlt(Date, tz = "Europe/Athens"))]

pander::pander(
  MC2,
  caption = "Max duration"
)


## Test missing data  -------



## test missing days
# DATA <- readRDS("data/CE_ID_Input.Rds")

missing_days <- length(seq.Date(min(as.Date(DATA$Date)), max(as.Date(DATA$Date)), by = "day")) - DATA[, length(unique(as.Date(DATA$Date)))]
total_days   <- length(seq.Date(min(as.Date(DATA$Date)), max(as.Date(DATA$Date)), by = "day"))

cat("Missing days", 100 * missing_days/total_days, "%", missing_days, "from", total_days,"\n\n")

## missing days by month

DAYS <- data.table(Day = seq.Date(min(DATA$Day), max(DATA$Day), by = "day"))

DAYS[, Missing_Day := !Day %in% DATA[, unique(Day)] ]



COMPLETE_monthly      <- DATA[, .(Complete = sum(!is.na(wattGLB))/.N) , by = .(year(Date), month(Date))]
COMPLETE_monthly$Date <- as.POSIXct(strptime(paste(COMPLETE_monthly$year, COMPLETE_monthly$month, "1"),"%Y %m %d"))

summary(COMPLETE_monthly$Complete)

quantile(COMPLETE_monthly$Complete, 0.04)

plot(COMPLETE_monthly[, Complete, Date])

hist(COMPLETE_monthly[, Complete])

COMPLETE_yearly      <- DATA[, .(Complete = sum(!is.na(wattGLB))/.N) , by = .(year(Date))]
# COMPLETE_monthly$Date <- as.POSIXct(strptime(paste(COMPLETE_monthly$year, COMPLETE_monthly$month, "1"),"%Y %m %d"))






##  Climatology of CE   ----------------------------------------------

monthly <- DATA[, .(Relative_enha_GLB = sum(Enhanc_C_4, na.rm = T)/sum(!is.na(wattGLB), na.rm = T),
                    Relative_enha_N   = sum(Enhanc_C_4, na.rm = T)/.N),
                by = .(year(Date), month(Date)) ]




boxplot(monthly$Relative_enha_GLB ~ monthly$month)
boxplot(monthly$Relative_enha_N ~ monthly$month)



#+ P-CE-climatology-normlz,  echo=F, include=T, results="asis"

ggplot(monthly, aes(y = 100 * Relative_enha_GLB,
                         x = factor(month,
                                    levels = 1:12,
                                    labels = month.abb[1:12]))) +
  geom_boxplot(fill = varcol("GLB_diff"), outliers = FALSE) +
  xlab("") +
  ylab("Relative frequency [%]") +
  # stat_summary(fun.y = mean, geom = "point", shape = 23, size = 3) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )



ggplot(monthly, aes(y = 100 * Relative_enha_N,
                    x = factor(month,
                               levels = 1:12,
                               labels = month.abb[1:12]))) +
  geom_boxplot(fill = varcol("GLB_diff"), outliers = FALSE) +
  xlab("") +
  ylab("CE occurence relative to all minutes [%]") +
  # stat_summary(fun.y = mean, geom = "point", shape = 23, size = 3) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
#+ echo=F, include=T







##  Climatology of ECE   ----------------------------------------------

monthlyE <- DATA[, .(Relative_enha_GLB = sum(wattGLB > ETH, na.rm = T)/sum(!is.na(wattGLB), na.rm = T),
                     Relative_enha_N   = sum(wattGLB > ETH, na.rm = T)/.N),
                by = .(year(Date), month(Date)) ]


boxplot(monthlyE$Relative_enha_GLB ~ monthlyE$month)
boxplot(monthlyE$Relative_enha_N ~ monthlyE$month)



#+ P-ECE-climatology-normlz, echo=F, include=T, results="asis"

ggplot(monthlyE, aes(y = 100 * Relative_enha_GLB,
                         x = factor(month,
                                    levels = 1:12,
                                    labels = month.abb[1:12]))) +
  geom_boxplot(fill = "lightblue", outliers = FALSE) +
  xlab("") +
  ylab("ECE occurrence frequency relative to GHI [%]") +
  # stat_summary(fun.y = mean, geom = "point", shape = 23, size = 3) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )



ggplot(monthlyE, aes(y = Relative_enha_N,
                    x = factor(month,
                               levels = 1:12,
                               labels = month.abb[1:12]))) +
  geom_boxplot(fill = "lightblue", outliers = FALSE) +
  xlab("") +
  ylab("ECE relative to all minutes") +
  # stat_summary(fun.y = mean, geom = "point", shape = 23, size = 3) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

#+ echo=F, include=T


mont <- DATA[Enhanc_C_4 == T,
             .(CE_OI = sum(GLB_diff, na.rm = F),
               OI_N  = length(GLB_diff>0)),
             by = .(year(Date), month(Date)) ]

boxplot(DATA[Enhanc_C_4 == T, GLB_diff] ~  DATA[Enhanc_C_4 == T, month])
title("Mean OI")


boxplot(mont[, CE_OI/OI_N] ~ mont$month)
title("Mean monthly OI")


## Check days for reviews



keep_ratio <- 0.6

Keep <- DATA[, sum(!is.na(wattGLB))/.N > keep_ratio, by = .(Day = as.Date(Date))]

cat("Keeping", keep_ratio, "of each day, removes", Keep[V1 == F, .N], "days of", Keep[V1 == T, .N], "total days or", 100*Keep[V1 == F, .N]/Keep[V1 == T, .N],"%\n")


## Doys with data
Keep[, doy := yday(Day)]

cnt_doy <- Keep[, sum(V1 == T), by = doy]

cnt_missing_doy <- Keep[, sum(V1 == F), by = doy]

plot(cnt_missing_doy$doy, cnt_missing_doy$V1)

pander(cnt_missing_doy[V1 == 0, ], caption = paste("There are", nrow(cnt_missing_doy[V1 == 0, ]), "doys with all days no missing more than", 100*keep_ratio, "% data"))






cat("Solar constant at 1au from NOAA TSI for the same data:",
    mean(DATA$tsi_1au_comb, na.rm = T), "\n\n")

mean(DATA$tsi_1au_comb, na.rm = T)
mean(DATA$TSIextEARTH_comb, na.rm = T)



## Solstice --------------------
LUK <- readRDS("./data/lookuptable_datatable.Rds")

solstis <- data.table()
for (ay in unique(year(LUK$Date))) {
  temp <- LUK[year(LUK$Date) == ay ]
  aday <- temp[as.Date(Date) == temp[which.min(SZA), as.Date(Date)]]

  energy <- sum(aday$Exact_B.Exact_W.edir + aday$Exact_B.Exact_W.edn) * 60 / 1000 /1000

  solstis <- rbind(solstis,
                   data.table(Date      = temp[which.min(SZA), as.Date(Date)],
                              SZA       = temp[which.min(SZA), SZA],
                              Enerhy_Kj = energy)
  )

}

pander(solstis, caption = "Solstices from Libradtran")


cat("Solstice max for all period", max(solstis$Enerhy_Kj/1000, na.rm = T), "MJ", "\n\n")
cat("Solstice mean for all period", mean(solstis$Enerhy_Kj/1000, na.rm = T), "MJ", "\n\n")



## Print example day -----------------------------------------------------------
#'
#' ## Example day
#'
#+ P-example-day,  echo=F, include=T

## select day
example_day <- "2019-07-11"

DT_example <- DATA[Day == example_day, .(Date, wattGLB, TYPE, Enhanc_C_4, GLB_diff, TSI_OI = wattGLB - ETH)]

DT_example[  TSI_OI < 0,   TSI_OI := NA]
DT_example[GLB_diff < 0, GLB_diff := NA]

cat("Doy", unique(yday(DT_example$Date)), "\n\n")

pander::pander(DT_example)


## display sky cam images ---------


library(magick)


# left  <- image_read("/home/folder/LAP_skycam/skycam/2019/1922019/ 1922019100002.JPEG")
left   <- image_read("/home/folder/LAP_skycam/skycam/2019/1922019/ 1922019101501.JPEG")
right  <- image_read("/home/folder/LAP_skycam/skycam/2019/1922019/ 1922019103001.JPEG")
left1  <- image_read("/home/folder/LAP_skycam/skycam/2019/1922019/ 1922019093002.JPEG")


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
  annotate(geom = "text", x = 10, y = 15,  label = "(a)",       hjust = 0, vjust = 0, size =  4, colour = "orange") +
  annotate(geom = "text", x = 15, y = 585, label = "09:30 UTC", hjust = 0, vjust = 1, size =  4, colour = "orange")
date_A <- as.POSIXct(strptime(paste(example_day,   "09:30"), "%F %H:%M"))


B <- image_ggplot(ll) +
  annotate(geom = "text", x = 10, y = 15,  label = "(b)",       hjust = 0, vjust = 0, size =  4, colour = "orange") +
  annotate(geom = "text", x = 15, y = 585, label = "10:15 UTC", hjust = 0, vjust = 1, size =  4, colour = "orange")
date_B <- as.POSIXct(strptime(paste(example_day,   "10:15"), "%F %H:%M"))


C <- image_ggplot(rr) +
  annotate(geom = "text", x = 10, y = 15,  label = "(c)",       hjust = 0, vjust = 0, size =  4, colour = "orange") +
  annotate(geom = "text", x = 15, y = 585, label = "10:30 UTC", hjust = 0, vjust = 1, size =  4, colour = "orange")
date_C <- as.POSIXct(strptime(paste(example_day,   "10:30"), "%F %H:%M"))

grid.arrange(A, B, C, nrow = 1)


# library(grid)
# library(gridExtra)
#
# image_info(ll)
#
# a <- rasterGrob(ll)
# b <- rasterGrob(rr)
#
# grid.arrange(a, b, nrow = 1, labels = c("a", "b"))
#
# library(ggpubr)
# ggarrange(a, b, ncol = 2, labels = c("a)","b)"))





temp <- DATA[Day == example_day]

par(mar = c(4, 4, 1, 1))
par(cex       = 0.7
    # cex.main = 0.8, #change font size of title
    # cex.sub  = 0.8,  #change font size of subtitle
    # cex.lab  = 0.8, #change font size of axis labels
    # cex.axis = 0.8,
    ) #change font size of axis text

ylim <- range(0, temp$ETH, temp$wattGLB, solar_constant, na.rm = TRUE)

plot(temp$Date, temp$wattGLB, col = "green",
     pch  = ".", cex = 2,
     ylim = ylim,
     ylab = bquote("GHI" ~ group("[", W/m^2,"]")),
     xlab = "Time (UTC)")

## mark photos
abline(v = date_A, col = "grey", lwd = 2, lty = 2)
abline(v = date_B, col = "grey", lwd = 2, lty = 2)
abline(v = date_C, col = "grey", lwd = 2, lty = 2)

text(x = date_A, y = 250, "(a)", pos = 2, offset = 0.2, col = "gray", cex = 0.8)
text(x = date_B, y = 250, "(b)", pos = 2, offset = 0.2, col = "gray", cex = 0.8)
text(x = date_C, y = 250, "(c)", pos = 4, offset = 0.2, col = "gray", cex = 0.8)

abline(h = solar_constant, col = "orange2", lty = 1, lwd = 2)

## Global
lines(temp$Date, temp$wattGLB, col = "green")

## TSI on ground
lines(temp$Date, temp$ETH)

## Active model reference
lines(temp[, get(paste0(SelEnhanc, "_ref")), Date], col = "red" )

## Cloud-free ref
lines(temp[, get(paste0(csmodel,".glo")), Date], col = "darkorchid" )


## Enchantment cases
points(temp[get(SelEnhanc) == TRUE & wattGLB <  ETH, wattGLB, Date], col = "burlywood4")
points(temp[get(SelEnhanc) == TRUE & wattGLB >= ETH, wattGLB, Date], col = "red")


## Cloud cases
points(temp[TYPE == "Cloud", wattGLB, Date], col = "blue", pch = 3, cex = 0.3)

## Decorations
# title(main = paste(as.Date(aday, origin = "1970-01-01"), temp[get(SelEnhanc) == TRUE, .N], temp[TYPE == "Cloud", .N], vec_days$Descriprion[ii]))
title(main = paste(as.Date(example_day, origin = "1970-01-01")))

legend("bottom", ncol = 2,
       c(  "GHI","CE threshold","TOA TSI on horiz. plane             ","Solar Constant", "CE events   ","ECE events     ","Identified clouds    ",  "Cloud-free    "),
       col = c("green",         "red",                  "black",       "orange2","burlywood4",       "red",             "blue","darkorchid"),
       pch = c(     NA,            NA,                       NA,              NA,          1 ,          1 ,                  3,           NA),
       lty = c(      1,             1,                        1,               1,          NA,          NA,                 NA,            1),
       lwd = c(      1,             1,                        1,               2,          NA,          NA,                 NA,            1),
       bty = "n",
       cex = 0.8
)


## store base plot
p1 <- recordPlot()


bt <- grid.arrange(A, B, C, nrow = 1)

merg <- plot_grid(
  p1, bt,
  nrow = 2,
  rel_heights = c(3,1)
)

print(merg)

merg + theme(aspect.ratio = 1)






## redo example in ggplot !!!---------

pp1 <- ggplot(data = temp, aes(x = Date)) +
  ## DATA lines
  geom_line(aes(y = wattGLB                        , colour = "GHI"                           )) +
  geom_line(aes(y = get(paste0(SelEnhanc, "_ref")) , colour = "CE Threshold"                  )) +
  geom_line(aes(y = get(paste0(csmodel,".glo"))    , colour = "Cloud-free"                    )) +
  geom_line(aes(y = ETH                            , colour = "TOA TSI on horiz. plane"       )) +
  ## constant liens
  geom_hline(aes(yintercept = solar_constant       , colour = "Solar Constant") , linewidth = 1.0 ) +
  geom_vline(aes(xintercept = date_A), linetype = 1, linewidth = .5) +
  geom_vline(aes(xintercept = date_B), linetype = 1, linewidth = .5) +
  geom_vline(aes(xintercept = date_C), linetype = 1, linewidth = .5) +
  ## data points
  geom_point(data = temp[TYPE == "Cloud"],
             aes(y =  wattGLB, colour = "Identified clouds"), shape = 3, size = 0.7          ) +
  geom_point(data = temp[get(SelEnhanc) == TRUE & wattGLB <  ETH, ],
             aes(y =  wattGLB, colour = "CE events"),         shape = 1, size = 1.6          ) +
  geom_point(data = temp[get(SelEnhanc) == TRUE & wattGLB >= ETH, ],
             aes(y =  wattGLB, colour = "ECE events"),        shape = 1, size = 1.6          ) +
  ## legend
  scale_colour_manual("",
                      guide = guide_legend(ncol = 2),
                      breaks = c("GHI",
                                 "Cloud-free",
                                 "CE Threshold",
                                 "TOA TSI on horiz. plane",
                                 "Solar Constant",
                                 "CE events",
                                 "ECE events",
                                 "Identified clouds"),
                      values = c("GHI"                     = "green",
                                 "CE Threshold"            = "red" ,
                                 "TOA TSI on horiz. plane" = "black",
                                 "Identified clouds"       = "blue",
                                 "CE events"               = "burlywood4",
                                 "ECE events"              = "red",
                                 "Solar Constant"          = "orange2",
                                 "Cloud-free"              = "darkorchid")) +
  # guides(fill = guide_legend(ncol = 2)) +

  theme(
    #   legend.title         = element_text(size = 10),
    legend.position       = c(.995, .005),
    legend.justification  = c("right", "bottom"),
    # legend.box.just       = "right",
    legend.background     = element_blank(),
    legend.spacing.y = unit(0, 'cm'),
    legend.spacing.x = unit(.005, 'cm'),
    legend.box.background = element_rect(color = NA, fill = NA),
    legend.key            = element_blank(),
    legend.margin         = margin(1, 1, 1, 1) ) +

  ## AXIS ##
  # scale_x_continuous(expand = expansion(mult = c(0.03, 0.03))) +
  scale_y_continuous(breaks = seq(0, 1600, 200)) +
  ylab(bquote("GHI" ~ group("[", W/m^2,"]"))) +
  xlab(bquote("Time (UTC)"))
pp1



merg <- plot_grid(
  pp1, bt,
  nrow = 2,
  rel_heights = c(3,1)
)

print(merg)

merg + theme(aspect.ratio = 1)














#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (difftime(tac,tic,units = "sec") > 30) {
  system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
  system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'R script ended'"))
}

