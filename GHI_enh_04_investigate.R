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
    pdf( file = paste0("./runtime/",  basename(sub("\\.R$",".pdf", Script.Name))))
    sink(file = paste0("./runtime/",  basename(sub("\\.R$",".out", Script.Name))), split = TRUE)
}



#+ echo=F, include=T
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
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
dialytrends <- data.frame()

for (DBn in dbs) {
    DB <- get(DBn)
    cat("\n\\newpage\n")
    cat("\n#### Trends on", tr_var(DBn), "data\n\n" )

    for (avar in vars) {
        dataset <- DB

        if (all(is.na(dataset[[avar]]))) next()

        ## linear model by day step
        lm1 <- lm(dataset[[avar]] ~ dataset$Date)
        lm2 <- lm(dataset[[avar]] ~ dataset$yts)

        # d   <- summary(lm1)$coefficients
        # cat("lm:      ", lm1$coefficients[2] * Days_of_year, "+/-", d[2,2] * Days_of_year,"\n\n")
        # ## correlation test
        # cor1 <- cor.test(x = dataset[[avar]], y = as.numeric(dataset$Date), method = 'pearson')
        #
        #
        # ## get daily climatology
        # dclima <- dataset[, max(get(gsub("_des", "_seas", avar))), by = doy]
        #
        ## capture lm for table
        # dialytrends <- rbind(dialytrends,
        #                 data.frame(
        #                     linear_fit_stats(lm1, confidence_interval = Daily_confidence_limit),
        #                     cor_test_stats(cor1),
        #                     DATA       = DBn,
        #                     var        = avar,
        #                     N          = sum(!is.na(dataset[[avar]])),
        #                     N_eff      = N_eff,
        #                     t_eff      = t_eff,
        #                     t_eff_cri  = t_eff_cri,
        #                     conf_2.5   = conf_2.5,
        #                     conf_97.5  = conf_97.5,
        #                     mean_clima = mean(dclima$V1, na.rm = T),
        #                     Tres
        #                 )
        # )

        #
        # if (grepl("near_tcc", avar)) {
        #     acol <- "cyan"
        # } else {
        #     acol <- get(paste0(c("col", unlist(strsplit(avar, split = "_"))[1:2]),
        #                        collapse = "_"))
        # }



        ylab <- switch(gsub("\\..*$", "", gsub(".*_", "", avar)),
                       diff = "Difference from reference",
                       ench = "Relative to reference",
                       avar
        )

        vcol <- switch(gsub("\\..*$", "", gsub(".*_", "", avar)),
                       diff = "green",
                       ench = "blue",
                       "black"
        )


        vnma <- switch(gsub("\\..*$", "", gsub(".*_", "", avar)),
                       diff = "Above reference",
                       ench = "Relative Enchancement",
                       avar
        )

        vnma <- switch(gsub("\\..*$", "", gsub(".*_", "", avar)),
                       diff = "Above reference",
                       ench = "Relative Enchancement",
                       avar
        )

        snma <- switch(gsub(".*\\.", "", avar),
                       sum    = "totals",
                       min    = "minimum",
                       max    = "maximun",
                       median = "median",
                       sumPOS = "total of positives",
                       sumNEG = "total of negatives",
                       N      = "number of cases",
                       TotalN = "number of observations",
                       avar
        )



        ## plot data
        plot(dataset$Date, dataset[[avar]],
             pch      = ".",
             col      = vcol,
             cex      = 2,
             # main     = paste(tr_var(DBn), tr_var(avar)),
             cex.main = 0.8,
             yaxt     = "n",
             xlab     = "",
             ylab     = ylab
        )

        ## plot fit line lm
        abline(lm1, lwd = 2, col = "red")


        # y axis
        axis(2, pretty(dataset[[avar]]), las = 2 )

        # x axis
        axis.Date(1,
                  at = seq(as.Date("1993-01-01"), max(dataset$Date), by = "year"),
                  format = "%Y",
                  labels = NA,
                  tcl = -0.25)


        if (DRAFT == TRUE) {
            title(main = paste(tr_var(DBn), vnma, snma, avar),
                  cex.main = 0.8 )
        }


        ## display trend on graph
        fit <- lm1[[1]]
        legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
               paste("Trend: ",
                     if (fit[2] > 0) "+" else "-",
                     signif(abs(fit[2]) * Days_of_year, 2) ,"/y" )
        )
        lm2[[1]]


        # fit <- lm1[[1]]
        # legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
        #        paste("Trend: ",
        #              if (fit[2] > 0) "+" else "-",
        #              signif(abs(fit[2]) * Days_of_year, 2),
        #              "±", signif(2 * Tres[2], 2) ,"%/y" )
        # )
        cat(" \n \n \n")
    }
}
#+ echo=F, include=F














#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (interactive() & difftime(tac,tic,units = "sec") > 30) {
    system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
    system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'R script ended'"))
}

