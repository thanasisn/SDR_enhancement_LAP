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

#+
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

above <- 100
per   <- 100 * DATA[GLB_diff > 0 & GLB_diff > above, .N] / DATA[GLB_diff > 0, .N]

cat(paste("\n", per, "% of the values are above", above, "W/m^2\n\n" ))


below <- mean(ST_E_yearly$GLB_diff.mean)
per   <- 100 * DATA[GLB_diff > 0 & GLB_diff < below, .N] / DATA[GLB_diff > 0, .N]

cat(paste("\n", per, "% of the values are below", below, "W/m^2\n\n" ))



## slipt histogram
binwidth <- 30
split    <- binwidth * 7

sa        <- hist(DATA[GLB_diff > 0 & GLB_diff <  split, GLB_diff], breaks = seq(0, split, binwidth),    plot = FALSE)
sa_counts <- sum(sa$counts)

sb        <- hist(DATA[GLB_diff > 0 & GLB_diff >= split, GLB_diff], breaks = seq(split, 1500, binwidth), plot = FALSE)
sb_counts <- sum(sb$counts)

sa_ratio <- sa_counts / (sa_counts + sb_counts)
sb_ratio <- sb_counts / (sa_counts + sb_counts)


pa <- ggplot(data = DATA[GLB_diff > 0], aes(x = GLB_diff)) +
  geom_histogram(aes(y = (sa_ratio * after_stat(count))/sum(after_stat(count)) * 100),
                 binwidth = binwidth,
                 boundary = 0,
                 fill = varcol( "GLB_diff"),
                 color    = "black") +
  xlab(bquote("OI" ~ group("[", W/m^2,"]"))) +
  ylab("Relative frequency [%]") +
  scale_x_continuous(
    breaks = seq(0, split + binwidth, binwidth),
    limits = c(0, split))
# print(pa)


pb <- ggplot(data = DATA[GLB_diff > 0], aes(x = GLB_diff)) +
  geom_histogram(aes(y = (sb_ratio * after_stat(count))/sum(after_stat(count)) * 100),
                 binwidth = binwidth,
                 boundary = 0,
                 fill = varcol( "GLB_diff"),
                 color    = "black") +
  xlab(bquote("OI" ~ group("[", W/m^2,"]"))) +
  ylab("Relative frequency [%]") +
  scale_x_continuous(
    breaks = c(seq(split, max(DATA[GLB_diff > 0, GLB_diff]), binwidth), ceiling(max(DATA[GLB_diff > 0, GLB_diff]))),
    limits = c(split, ceiling(max(DATA[GLB_diff > 0, GLB_diff]))))
# print(pb)



## merge plots

library(ggpubr)

# Remove axis titles from all plots
p      <- list(pa, pb) |> map(~.x + labs(x=NULL, y=NULL, caption = NULL))
yleft  <- textGrob("Relative frequency [%]", rot = 90)
bottom <- textGrob(bquote("OI" ~ group("[", W/m^2,"]")))


margin = theme(plot.margin = unit(c(.1,.1,.1,.1), "cm"))

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


quantile(DATA[wattGLB > ETH, GLB_diff])



## stopre some date for max cases

saveRDS(
  rbind(
    DATA[which.max(wattGLB),  .(Typ = "Max Glob", wattGLB, SZA, GLB_ench, GLB_diff, Date, ETH, CE = Enhanc_C_4, ECE = wattGLB > ETH)],
    DATA[which.max(GLB_ench), .(Typ = "Max Enha", wattGLB, SZA, GLB_ench, GLB_diff, Date, ETH, CE = Enhanc_C_4, ECE = wattGLB > ETH)]
  ),
  "data/Max_cases.Rds"
)





## Test missing data  -------


#+  echo=F, include=T, results="asis"





missing_days <- length(seq.Date(min(DATA$Day), max(DATA$Day), by = "day")) - DATA[, length(unique(Day))]
total_days   <- length(seq.Date(min(DATA$Day), max(DATA$Day), by = "day"))

cat("Missing days", 100 * missing_days/total_days, "%", missing_days, "from", total_days)

## missing days by month

DAYS <- data.table(Day = seq.Date(min(DATA$Day), max(DATA$Day), by = "day"))

DAYS[, Missing_Day := !Day %in% DATA[, unique(Day)] ]

DAYS <- merge(
  DATA[, .(Missing_GLB = sum(is.na(wattGLB))), by = Day],
  DAYS
)









#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (difftime(tac,tic,units = "sec") > 30) {
  system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
  system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'R script ended'"))
}

