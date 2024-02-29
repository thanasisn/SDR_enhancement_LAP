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
Script.Name <- "./GHI_enh_05_distributions.R"

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
load("./data/GHI_enh_02_ID_CE.Rda")
tic  <- Sys.time()

DRAFT <- TRUE
DRAFT <- FALSE






##  Daily  ---------------------------------------------------------------------


#'
#' \FloatBarrier
#'
#' ### Frequency Distributions
#'
#+ freqdistributions, echo=F, include=T, results="asis"


breaks <- 50
hist(DATA[get(unique(CEC)) == TRUE, GLB_ench],
     breaks = breaks,
     col  = varcol( "GLB_ench"),
     xlab = varname("GLB_ench"),
     main = varname("GLB_ench"))

hist(DATA[get(unique(CEC)) == TRUE, GLB_diff],
     breaks = breaks,
     col  = varcol("GLB_diff"),
     xlab = bquote(.(varname("GLB_diff")) ~ group("[", W/m^2,"]")),
     main = bquote(.(varname("GLB_diff")) ~ group("[", W/m^2,"]")))

hist(DATA[get(unique(CEC)) == TRUE, GLB_rati],
     breaks = breaks,
     col  = varcol( "GLB_rati"),
     xlab = varname("GLB_rati"),
     main = varname("GLB_rati"))




## test for one year
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





##  Distributions  -------------------------------------------------------------

#'
#' \FloatBarrier
#'
#' ### Distributions
#'
#+ relativedistributions, echo=F, include=T, results="asis"

hist(DATA[get(unique(CEC)) == TRUE, GLB_ench],
     breaks = breaks,
     freq   = FALSE,
     col    = varcol( "GLB_ench"),
     xlab   = varname("GLB_ench"),
     main   = varname("GLB_ench"))


hist(DATA[get(unique(CEC)) == TRUE, GLB_diff],
     breaks = breaks,
     freq   = FALSE,
     col    = varcol("GLB_diff"),
     xlab   = bquote(.(varname("GLB_diff")) ~ group("[", W/m^2,"]")),
     main   = bquote(.(varname("GLB_diff")) ~ group("[", W/m^2,"]")))


stop()

hist(DATA[get(unique(CEC)) == TRUE, GLB_rati],
     breaks = breaks,
     freq   = FALSE,
     col    = varcol( "GLB_rati"),
     xlab   = varname("GLB_rati"),
     main   = varname("GLB_rati"))






##  Extreme cases Distributions  -----------------------------------------------

#'
#' \FloatBarrier
#'
#' ### Extreme cases Distributions
#'
#+ extremedistributions, echo=F, include=T, results="asis"


hist(DATA[wattGLB > ETH, GLB_diff],
     breaks = breaks,
     col  = varcol("GLB_diff"),
     xlab = bquote(.(varname("GLB_diff")) ~ group("[", W/m^2,"]")),
     main = paste("Extreme cases of", varname("GLB_diff")) )


hist(DATA[wattGLB > ETH, GLB_rati],
     breaks = breaks,
     col  = varcol("GLB_rati"),
     xlab = varname("GLB_rati"),
     main = paste("Extreme cases of", varname("GLB_rati")))


hist(DATA[wattGLB > ETH, GLB_ench],
     breaks = breaks,
     freq   = FALSE,
     col    = varcol("GLB_ench"),
     xlab   = varname("GLB_ench"),
     main   = paste("Extreme cases of", varname("GLB_ench")))




##  Energy calculations  -------------------------------------------------------

#'
#' \FloatBarrier
#'
#' ### Energy calculations
#'
#+ energy, echo=F, include=T, results="asis"













#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (interactive() & difftime(tac,tic,units = "sec") > 30) {
    system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
    system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'R script ended'"))
}

