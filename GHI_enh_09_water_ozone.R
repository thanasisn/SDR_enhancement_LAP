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
knitr::opts_chunk$set(fig.pos    = 'h!'    )

#+ echo=FALSE, include=TRUE
## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
Script.Name <- "./GHI_enh_09_watter_ozone.R"
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
library(ggpmisc     , quietly = TRUE, warn.conflicts = FALSE)
library(latex2exp   , quietly = TRUE, warn.conflicts = FALSE)
library(janitor     , quietly = TRUE, warn.conflicts = FALSE)

panderOptions("table.alignment.default", "right")
panderOptions("table.split.table",        120   )

def.chunk.hook  <- knitr::knit_hooks$get("chunk")
knitr::knit_hooks$set(chunk = function(x, options) {
  x <- def.chunk.hook(x, options)
  ifelse(options$size != "normalsize", paste0("\n \\", options$size,"\n\n", x, "\n\n \\normalsize"), x)
})


## __ Load external functions --------------------------------------------------
## Functions from `https://github.com/thanasisn/IStillBreakStuff/tree/main/FUNCTIONS/R`
source("~/CODE/FUNCTIONS/R/data.R")
source("~/CODE/FUNCTIONS/R/trig_deg.R")
source("~/CODE/FUNCTIONS/R/linear_fit_stats.R")

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




##  Load and prepare data CIMEL ------------------------------------------------
AEin1 <- "~/DATA/Aeronet/Thessaloniki_Monthly/20030101_20241231_Thessaloniki.lev20"
AE1   <- fread(AEin1, skip = 6, fill = T, na.strings = "-999")

names(AE1)[names(AE1) == "Month"] <- "Date"

AE1 <- AE1[, lapply(.SD, function(x) replace(x, which(x < -998), NA))]
AE1 <- remove_constant(AE1)
AE1 <- remove_empty(AE1, which = "cols")
AE1 <- clean_names(AE1)

# AEin2 <- "~/DATA/Aeronet/Thessaloniki_Monthly/20030101_20241231_Thessaloniki.tot_lev20"
# AE2 <- fread(AEin2, skip = 6, fill = T, na.strings = "-999")

AE1[, c("Year", "Month") := tstrsplit(date, "-")]
AE1[, Year  := as.numeric(Year)]
AE1[, Month := match(Month, toupper(month.abb))]
AE1[, tsy := Year + (Month - 1) / 12]

## Change units
AE1$pw_mm                 <- AE1$precipitable_water_cm * 10
AE1$precipitable_water_cm <- NULL

## Use whole years
AE1 <- AE1[tsy >= 2004 ]
AE1 <- AE1[tsy <  2024 ]






#+ echo=T
plot(AE1$tsy, AE1$pw_mm)

ll <- lm(AE1$pw_mm ~ AE1$tsy)
abline(ll, col = "red")
summary(ll)

AE1$lm_mod <- predict(ll, AE1)


## Water column range of change
AE1[tsy %in% range(tsy), lm_mod ]


plot(AE1$tsy, 100 * (AE1$pw_mm - (mean(AE1$pw_mm,na.rm = T))) / mean(AE1$pw_mm,na.rm = T),
     ylab = "Delta(water) %")
ll <- lm(100 * (AE1$pw_mm - (mean(AE1$pw_mm,na.rm = T))) / mean(AE1$pw_mm,na.rm = T) ~ AE1$tsy)
abline(ll, col = "red")
summary(ll)

range(AE1$tsy)
range(AE1$date)
mean(AE1$pw_mm,na.rm = T)



## Yearly
AEY <- AE1[, .(pw_mm = mean(pw_mm, na.rm = T)), by = Year ]
plot(AEY$Year, 100 * (AEY$pw_mm - (mean(AEY$pw_mm,na.rm = T))) / mean(AEY$pw_mm,na.rm = T),
     ylab = "Delta(water) %")
ll <- lm(100 * (AEY$pw_mm - (mean(AEY$pw_mm,na.rm = T))) / mean(AEY$pw_mm,na.rm = T) ~ AEY$Year)
abline(ll, col = "red")
summary(ll)


## by month
gather <- data.table()
for (am in sort(unique(AE1$Month))) {
  AEM <- AE1[Month == am]

  plot(AEM$Year, 100 * (AEM$pw_mm - (mean(AEM$pw_mm,na.rm = T))) / mean(AEM$pw_mm,na.rm = T),
       ylab = "Delta(water) %")
  title(month.name[am])
  ll <- lm(100 * (AEM$pw_mm - (mean(AEM$pw_mm,na.rm = T))) / mean(AEM$pw_mm,na.rm = T) ~ AEY$Year)
  abline(ll, col = "red")
  summary(ll)

  temp <- data.table(
    linear_fit_stats(ll, confidence_interval = 0.99),
    Month =  am)
  gather <- rbind(gather, temp)

}

gather[, slope.sd := NULL]
gather[, intercept.ConfInt_0.95 := NULL]
gather[, intercept.ConfInt_0.99 := NULL]
gather[, intercept.sd := NULL]
gather[, Rsqrd   := NULL]
gather[, RsqrdAdj := NULL]
gather[, slope.ConfInt_0.95 := NULL]
gather[, slope.ConfInt_0.99 := NULL]


pander(gather)

pander(summary(gather))


#+ include=T, echo=F

libtest <- readRDS("./data/Model_CS_trend_fix_manual.Rds")
libtest <- remove_constant(libtest)
libtest[, ticTime  := NULL]
libtest[, tacTime  := NULL]
libtest[, hostname := NULL]
libtest[, ID       := NULL]
libtest[, uavgdir  := NULL]
libtest[, uavgdn   := NULL]
libtest[, uavgup   := NULL]

libtest <- libtest[atmosphere_file == "afglms"]
libtest[, glo := (edir + edn) / 1000]

setorder(libtest, mol_modify_O3)
pander(libtest)

setorder(libtest, pw_avg_mm)
pander(libtest)



#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (difftime(tac,tic,units = "sec") > 30) {
    system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
    system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'R script ended'"))
}
