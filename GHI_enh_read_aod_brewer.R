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
Script.Name <- "./GHI_enh_read_aod_brewer.R"
tic <- Sys.time()

## use worktree
setwd("~/MANUSCRIPTS/02_enhancement/")

## __  Set environment ---------------------------------------------------------
require(data.table, quietly = TRUE, warn.conflicts = FALSE)
require(dplyr,      quietly = TRUE, warn.conflicts = FALSE)
require(rmatio,     quietly = TRUE, warn.conflicts = FALSE)

#'
#' dinfo
#' στήλες: Year, DOY, cloudiness flag (0: no clouds), total ozone flag (1: measured, 3: climatological)
#'
#' daod
#' AOD at wavelengths 290 - 363 nm with a step of 0.5 nm (each row corresponds to a row from dinfo)
#'
#' dmin
#' Minute corresponding to each AOD values
#'
#' dsza
#' SZA corresponding to each AOD values
#'
#' dqflg
#' Εδώ έχω ένα quality flag που έβγαλα από το schic και τελικά δε χρησιμοποίησα
#'
#+ echo=F, include=T


filelist <- list.files("~/DATA_RAW/aod-from-brewer_2024-08-30_1135",
                       "*.mat",
                       full.names = T)

BAOD <- data.table()
for (af in filelist) {
  temp <- read.mat(af)
  names(temp)

  dinfo <- data.frame(temp$dinfo)
  names(dinfo) <- c("Year", "DOY", "Cloudiness", "OZ_flag")

  daod <- data.frame(temp$daod)
  names(daod) <- paste0("AOD_", seq(290, 363, 0.5))

  # dinfo$H <- (temp$dmin) %/% 60
  # dinfo$M <- temp$dmin - (dinfo$H * 60)
  # dinfo$S <- round((dinfo$M %% 1) * 60)


  BAOD <- rbind(BAOD, cbind(dinfo, daod))
}

cat("Ignore time of day\n")
cat("Ignore SZA\n")



cat("Drop empty columns\n")
BAOD      <- janitor::remove_empty(BAOD, which = "cols")
BAOD$Date <- as.POSIXct(strptime(paste(BAOD$Year, BAOD$DOY), "%Y %j"))


cat("Ignore negative AOD\n")
BAOD <- BAOD |> mutate(
  across(
    starts_with("AOD_"),
    ~ replace(., .<0, NA)
  )
)

cat("Ignore infinite AOD\n")
BAOD <- BAOD |> mutate(
  across(
    starts_with("AOD_"),
    ~ replace(., is.infinite(.), NA)
  )
)

cat("Drop year 1997\n")
BAOD <- BAOD[Year != 1997]



count <- BAOD |> summarise(
  across(
    starts_with("AOD_"),
    ~ sum(is.na(.))
  )
)

cat("Max N data for", which.max(count), "\n")



## select some AODs to plot
vars <- grep("AOD_3[56]", names(BAOD), value = T)



for (var in vars) {

  plot(BAOD[, get(var), Date],
       ylab = "",
       main = paste("", var))

}


## use below 361

for (var in vars) {

  monthly <- BAOD[, mean(get(var), na.rm = T), by = .(year(Date), month(Date)) ]

  monthly[, Date := as.Date(paste(year, month,1), "%Y %m %d")]


  llm <- lm(monthly[, Date, V1])

  plot(monthly[, V1, Date],
       ylab = "",
       main = paste("Mean monthly", var) )
  abline(llm, col = "red")

  ## display trend on graph
  legend("top", pch = NA, lty = 1, bty = "n", lwd = 2, cex = 1,
         col = c("red"),
         c(paste(if (coef(llm)[2] / mean(monthly$V1, na.rm = T) > 0) "+" else "-",
                 signif(12 * abs(100 * coef(llm)[2] / mean(monthly$V1, na.rm = T)), 2), "%/y")
         )
  )
}


## get all trenda
vars <- grep("AOD_", names(BAOD), value = T)
gather <- data.table()
for (var in vars) {

  monthly <- BAOD[, mean(get(var), na.rm = T), by = .(year(Date), month(Date)) ]

  monthly[, Date := as.Date(paste(year, month,1), "%Y %m %d")]


  llm <- lm(monthly[, Date, V1])

  gather <- rbind(gather,
                  data.frame(slope_pCpY = 12 * abs(100 * coef(llm)[2] / mean(monthly$V1, na.rm = T)),
                             AOD = var)
  )

}

pander::pander(gather, caption = "AOD change %/y")

cat("range", range(gather$slope_pCpY),"\n\n")



#' **END**
#+ include=T, echo=F
tac <- Sys.time()
# cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            # Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (difftime(tac,tic,units = "sec") > 30) {
  system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
  # system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'R script ended'"))
}
