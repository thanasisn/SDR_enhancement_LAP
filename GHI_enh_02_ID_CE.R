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

#+ echo=FALSE, include=TRUE
knitr::opts_chunk$set(comment    = ""       )
# knitr::opts_chunk$set(dev        = c("pdf", "png")) ## expected option
knitr::opts_chunk$set(dev        = "png"    )       ## for too much data
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
knitr::opts_chunk$set(cache      =  FALSE   )  ## !! breaks calculations


#+ echo=FALSE, include=TRUE
## __ Set environment ----------------------------------------------------------
Sys.setenv(TZ = "UTC")
Script.Name <- "./GHI_enh_02_ID_CE.R"

if (!interactive()) {
    pdf( file = paste0("./runtime/",  basename(sub("\\.R$",".pdf", Script.Name))))
    sink(file = paste0("./runtime/",  basename(sub("\\.R$",".out", Script.Name))), split = TRUE)
}


#+ echo=FALSE, include=TRUE
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
if (
    file.exists(raw_input_data) == FALSE |
    file.mtime(raw_input_data) < file.mtime("./GHI_enh_00_variables.R") |
    file.mtime(raw_input_data) < file.mtime("./GHI_enh_01_raw_data.R")
) {
    source("./GHI_enh_01_raw_data.R")
    dummy <- gc()
}



## __ Set plot options  --------------------------------------------------------

theme_paper <- function(){
    # font <- "Georgia"   #assign font family up front

    theme_bw() %+replace%    #replace elements we want to change
        theme(
            # panel.grid.major = element_blank(),    #strip major gridlines
            # panel.grid.minor = element_blank(),    #strip minor gridlines
            panel.background   = element_rect(fill = 'transparent'), #transparent panel bg

            # axis.ticks = element_blank(),          #strip axis ticks

            #text elements
            # plot.title = element_text(             #title
            #     family = font,            #set font family
            #     size = 20,                #set font size
            #     face = 'bold',            #bold typeface
            #     hjust = 0,                #left align
            #     vjust = 2),               #raise slightly
            #
            # plot.subtitle = element_text(          #subtitle
            #     family = font,            #font family
            #     size = 14),               #font size
            #
            # plot.caption = element_text(           #caption
            #     family = font,            #font family
            #     size = 9,                 #font size
            #     hjust = 1),               #right align
            #
            # axis.title = element_text(             #axis titles
            #     family = font,            #font family
            #     size = 10),               #font size
            #
            # axis.text = element_text(              #axis text
            #     family = font,            #axis famuly
            #     size = 9),                #font size
            #
            # axis.text.x = element_text(            #margin for axis text
            #     margin=margin(5, b = 10)),

            plot.background       = element_rect(fill = 'transparent', color = NA), #transparent plot bg
            # panel.grid.major      = element_blank(), #remove major gridlines
            # panel.grid.minor      = element_blank(), #remove minor gridlines
            legend.background     = element_rect(fill = 'transparent',
                                                 linewidth = 0.5,
                                                 color = "black"), #transparent legend bg
            legend.box.background = element_rect(fill = 'transparent'), #transparent legend panel
            # axis.line             = element_line(linewidth = .5, colour = "black", linetype = 1),

            NULL
        )
}

theme_set(theme_paper())



alpha <- 0.9653023236718680788471



##  Load Enhancement data  -----------------------------------------------------
DATA <- readRDS(raw_input_data)
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
#+ echo=TRUE, include=TRUE

## __ Enhancement criteria  ----------------------------------------------------
SelEnhanc <- "Enhanc_C_1"
# SelEnhanc <- "Enhanc_C_2"
# SelEnhanc <- "Enhanc_C_3"

## __ My criteria  -------------------------------------------------------------
GLB_ench_THRES     <-  1.10 ## enchantment relative to HAU
GLB_diff_THRES     <- 15    ## enchantment absolute diff to HAU
Clearness_Kt_THRES <-  0.8  ## enchantment threshold
wattGLB_THRES      <- 20    ## minimum value to consider

DATA[, Enhanc_C_1 := FALSE]
# DATA[wattGLB           > CS_ref * GLB_ench_THRES + GLB_diff_THRES &
#      ClearnessIndex_kt > Clearness_Kt_THRES,
#      Enhanc_C_1 := TRUE]

DATA[, Enhanc_C_1_ref := ETH * Clearness_Kt_THRES + GLB_diff_THRES]
DATA[wattGLB > Enhanc_C_1_ref,
     Enhanc_C_1 := TRUE]

if (SelEnhanc == "Enhanc_C_1") {
    DATA[ , GLB_diff :=   wattGLB - Enhanc_C_1_ref                    ] ## enhancement
    DATA[ , GLB_ench := ( wattGLB - Enhanc_C_1_ref ) / Enhanc_C_1_ref ] ## relative enhancement
    DATA[ , GLB_rati :=   wattGLB / Enhanc_C_1_ref                    ]
}


## __ Gueymard2017 Criteria  ---------------------------------------------------
## Clearness index > 0.8 / 1

DATA[, Enhanc_C_2 := FALSE]
DATA[, Enhanc_C_2_ref := ETH * Clearness_Kt_THRES]
# DATA[ClearnessIndex_kt > Clearness_Kt_THRES,
#      Enhanc_C_2 := TRUE]
DATA[wattGLB > Enhanc_C_2_ref,
     Enhanc_C_2 := TRUE]

if (SelEnhanc == "Enhanc_C_2") {
    DATA[ , GLB_diff :=   wattGLB - Enhanc_C_2_ref                    ] ## enhancement
    DATA[ , GLB_ench := ( wattGLB - Enhanc_C_2_ref ) / Enhanc_C_2_ref ] ## relative enhancement
    DATA[ , GLB_rati :=   wattGLB / Enhanc_C_2_ref                    ]
}



## __ Vamvakas2020  Criteria  --------------------------------------------------
## +5% from model => enhancements above 15 Wm^2 the instrument uncertainty
DATA[, Enhanc_C_3 := FALSE]
DATA[, Enhanc_C_3_ref := CS_ref * 1.05]
DATA[wattGLB > Enhanc_C_3_ref,
     Enhanc_C_3 := TRUE]

if (SelEnhanc == "Enhanc_C_3") {
    DATA[ , GLB_diff :=   wattGLB - Enhanc_C_3_ref                    ] ## enhancement
    DATA[ , GLB_ench := ( wattGLB - Enhanc_C_3_ref ) / Enhanc_C_3_ref ] ## relative enhancement
    DATA[ , GLB_rati :=   wattGLB / Enhanc_C_3_ref                    ]
}

#+ include=TRUE, echo=FALSE

# hist(DATA[GLB_ench > 0, GLB_ench])
# hist(DATA[GLB_diff > 0, GLB_diff])
# hist(DATA[GLB_rati > 1, GLB_rati])

hist(DATA[, GLB_ench], breaks = 100)
abline(v = 0, col = "red")

hist(DATA[, GLB_diff], breaks = 100)
abline(v = 0, col = "red")

hist(DATA[, GLB_rati], breaks = 100)
abline(v = 1, col = "red")




## Mol2023
## activate when +1% and 10w/m from model reference
## near by values with +0.1 are also accepted


table(DATA$Enhanc_C_1)
table(DATA$Enhanc_C_2)
table(DATA$Enhanc_C_3)


hist(DATA[TYPE == "Cloud", ClearnessIndex_kt])
hist(DATA[TYPE == "Cloud", CS_ref * GLB_ench_THRES + GLB_diff_THRES - wattGLB ])



#    ## __ Group continuous values  ---------------------------------------------
#    DATA[, cnF := cumsum(Enhanc_C_1 == FALSE)]
#    DATA[, cnT := cumsum(Enhanc_C_1 == TRUE) ]
#    ## Init groups logical
#    DATA[, C1G1  := Enhanc_C_1]
#    DATA[, C1G0  := Enhanc_C_1]
#
#    ## Find groups with one gap
#    for (i in 1:nrow(DATA)) {
#        p1 <- i - 1
#        n1 <- i + 1
#        if (p1 > 0 & n1 <= nrow(DATA)) {
#            if (DATA$C1G1[p1] == TRUE  &
#                DATA$C1G1[i]  == FALSE &
#                DATA$C1G1[n1] == TRUE  ) {
#                DATA$C1G1[i]  <- TRUE
#            }
#        }
#    }
#
#    ## Allow one gap group
#    DATA[, C1Grp1 := rleid(c(NA,diff(cumsum(G1))))]
#    DATA[C1G1 == FALSE, C1Grp1 := NA]
#
#    ## No gap group
#    DATA[, C1Grp0 := rleid(c(NA,diff(cumsum(G0))))]
#    DATA[C1G0 == FALSE, C1Grp0 := NA]



## __ Estimate enhancement daily magnitude  ------------------------------------
enh_days <- DATA[get(SelEnhanc) == TRUE,
                 .(Enh_sum      = sum(GLB_ench, na.rm = TRUE),
                   Enh_max      = max(GLB_ench, na.rm = TRUE),
                   Enh_diff_sum = sum(GLB_diff, na.rm = TRUE),
                   Enh_diff_max = max(GLB_diff, na.rm = TRUE)),
                 Day]

hist(enh_days$Enh_sum)
hist(enh_days$Enh_max)
hist(enh_days$Enh_diff_max)
hist(enh_days$Enh_diff_sum)


sunny_days <- DATA[, .(Sunshine = sum(TYPE == "Clear")/max(DayLength, na.rm = TRUE),
                       Energy   = sum(ClearnessIndex_kt, na.rm = TRUE)/sum(TYPE == "Clear"),
                       EC       = sum(get(SelEnhanc)),
                       Cloud    = sum(TYPE == "Cloud")),
                   by = Day]

hist(sunny_days$Sunshine)
hist(sunny_days$Energy)
hist(sunny_days$EC)
hist(sunny_days$Cloud)



## days with maximum values
setorder(enh_days, -Enh_diff_max)
maxenhd <- enh_days[1:20]

## strong total enhancement days
setorder(enh_days, -Enh_sum)
daylist <- enh_days[1:200]
daylist <- daylist[!Day %in% maxenhd$Day]
enhsnd  <- daylist[sample(1:nrow(daylist), 30)]

## select some sunny days
sunnyd  <- sunny_days[Sunshine > 0.79 & Energy > 0.74]
sunnyd  <- sunnyd[!Day %in% maxenhd$Day & !Day %in% enhsnd$Day]
sunnyd  <- sunnyd[sample(1:nrow(sunnyd), 20)]

## cloudy days
clouds <- sunny_days[Sunshine > 0.6 & Energy > 0.6 & EC > 2 & Cloud > 5]
clouds <- clouds[!Day %in% maxenhd$Day & !Day %in% enhsnd$Day & !Day %in% sunnyd$Day]
clouds <- clouds[sample(1:nrow(clouds), 20)]

## some random days
all_days <- data.table(Day=unique(DATA[, Day]))
all_days <- all_days[!Day %in% maxenhd$Day & !Day %in% enhsnd$Day & !Day %in% sunnyd$Day & !Day %in% clouds]
all_days <- all_days[sample(1:nrow(all_days), 30)]




##  Days with strong enhancement cases  ----------------------------------------

#' \FloatBarrier
#' # Plot some days with strong enhancement cases
#'
#+ echo=F, include=T, results="asis"

vecData  <- c("maxenhd",       "enhsnd",             "sunnyd", "clouds",    "all_days")
vecNames <- c("extrene cases", "strong enhancement", "sun",    "clouds ID", "random selection")

for (ii in 1:length(vecData)) {
    cat("\n\\FloatBarrier\n\n")
    cat("\n## Days with", vecNames[ii], "\n\n")
    temp    <- get(vecData[ii])
    daylist <- sort(temp$Day)


    for (aday in daylist) {
        temp <- DATA[Day == aday]
        par(mar = c(4, 4, 1, 1))
        ylim <- range(0, temp$TSIextEARTH_comb * cosde(temp$SZA), temp$wattGLB, na.rm = TRUE)

        # if (aday == "1997-04-23") stop("www")

        plot(temp$Date, temp$wattGLB, col = "green",
             pch  = ".", cex = 2,
             ylim = ylim,
             ylab = expression(Watt/m^2), xlab = "Time (UTC)")

        lines(temp$Date, temp$wattGLB, col = "green")

        lines(temp$Date, temp$wattHOR, col = "blue")

        lines(temp$Date, temp$TSIextEARTH_comb * cosde(temp$SZA))

        # lines(temp$Date, Clearness_Kt_THRES * temp$TSIextEARTH_comb * cosde(temp$SZA), lty = 3)


        lines(temp[, get(paste0(SelEnhanc,"_ref")), Date], col = "red" )


        points(temp[get(SelEnhanc) == TRUE, wattGLB, Date], col = "red")
        points(temp[TYPE == "Cloud", wattGLB, Date], col = "blue", pch = 3, cex = 0.3)

        # if (any(temp$TYPE == "Cloud")) stop("DD")

        title(main = paste(as.Date(aday, origin = "1970-01-01"), temp[get(SelEnhanc) == TRUE, .N], temp[TYPE == "Cloud", .N], vecNames[ii]))
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

        # overplot clearnesindex
        # par(new = T)
        # plot(temp$Date, temp$ClearnessIndex_kt, "l")
        # abline(h = Clearness_Kt_THRES)

        # plot(temp$Date, temp$Clearness_Kt)
        # abline(h=.8,col="red")
        # plot(temp$Date, temp$DiffuseFraction_Kd)
        # plot(temp$Date, temp$GLB_ench)
        # plot(temp$Date, temp$GLB_diff)
    }
}



#'
#' \FloatBarrier
#' # Plot years with enhancement cases
#'
#+ echo=F, include=T, results="asis"

## TODO plot only enhancement cases
## DO it with base plot
##
yearstodo <- unique(year(DATA$Date))

pyear <- 2018
for (pyear in yearstodo) {
    p <-
        ggplot(DATA[year(Date) == pyear],
               aes(get(paste0(SelEnhanc,"_ref")), wattGLB)) +
        geom_point(data   = DATA[year(Date) == pyear & get(SelEnhanc) == F,],
                   colour = "black",
                   na.rm  = TRUE,
                   size   = 0.2) +
        geom_point(data   = DATA[year(Date) == pyear & get(SelEnhanc) == T,],
                   na.rm  = TRUE,
                   size   = 0.2,
                   aes(color = GLB_diff)) +
        scale_colour_gradient(low      = "blue",
                              high     = "red",
                              na.value = NA) +
        labs(title = pyear) +
        xlab(paste0(SelEnhanc, "_ref")) +
        labs(color = "Over\nreference") +
        theme(
            legend.position      = c(.03, .97),
            legend.justification = c("left", "top"),
            legend.box.just      = "right",
            legend.margin        = margin(6, 6, 6, 6)
        ) +
        scale_x_continuous(expand = expansion(mult = c(0.03, 0.03))) +
        scale_y_continuous(breaks = scales::breaks_extended(n = 6),
                           expand = expansion(mult = c(0.03, 0.03)))
    print(p)

    # ggplot(DATA, aes(CS_ref, wattGLB)) +
    #     geom_point(data = DATA[GLB_diff < 0], colour = "black", size = 0.5) +
    #     geom_point(data = DATA[GLB_diff > 0], size = 0.5, aes(color = GLB_diff)) +
    #     scale_colour_gradient(low = "blue", high = "red", na.value = NA)

    # ggplot(DATA[year(Date) == 2018], aes(CS_ref, wattGLB)) +
    #     geom_point(data = DATA[year(Date) == 2018 & GLB_diff < 0], colour = "black", size = 0.5) +
    #     geom_point(data = DATA[year(Date) == 2018 & GLB_diff > 0], size = 0.5, aes(color = GLB_diff)) +
    #     scale_colour_gradient2(low = "black", mid = "yellow", high = "red", na.value = NA)

}





#  Save raw input data  ----------------------------------------------------
saveRDS(DATA, file = Input_data_ID, compress = "xz")
cat("\n  Saved raw input data:", Input_data_ID, "\n\n")






#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
