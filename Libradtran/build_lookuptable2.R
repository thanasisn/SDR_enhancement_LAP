#!/usr/bin/env Rscript
# /* Copyright (C) 2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:  "......."
#' author: "Natsis Athanasios"
#' institute: "AUTH"
#' affiliation: "Laboratory of Atmospheric Physics"
#' abstract: "........."
#' output:
#'   html_document:
#'     toc: true
#'     fig_width:  9
#'     fig_height: 4
#'   pdf_document:
#' date: "`r format(Sys.time(), '%F')`"
#' ---


#+ echo=F, include=T
rm(list = (ls()[ls() != ""]))
Script.Name <- "~/MANUSCRIPTS/02_enhancement/Libradtran/parse_out.R"
dir.create("./runtime/", showWarnings = FALSE)
d <- filelock::lock(paste0("./runtime/", basename(sub("\\.R$",".lock", Script.Name))), timeout = 0)
Sys.setenv(TZ = "UTC")
## standard output
if (!interactive()) {
    pdf( file = paste0("./runtime/",  basename(sub("\\.R$",".pdf", Script.Name))))
    sink(file = paste0("./runtime/",  basename(sub("\\.R$",".out", Script.Name))), split = TRUE)
}
## error notification function
options(error = function() {
    if (interactive()) {
        system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
        system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'An error occurred!'"))
    }
})
tic <- Sys.time()

library(data.table)
library(RlibRadtran)
library(ggplot2)

source("~/FUNCTIONS/R/data.R")

## run files
repo_dir     <- "~/MANUSCRIPTS/02_enhancement/Libradtran/io_repo/"

## empty runs
run_list_rds <- "~/MANUSCRIPTS/02_enhancement/Libradtran/run.Rds"

## all runs
model_cs     <- "~/MANUSCRIPTS/02_enhancement/Libradtran/Model_CS.Rds"


## Based on raw data
DATA <- data.table(readRDS("../data/CE_ID_Input.Rds"))

## Fill with CS approximation model
CS <- data.table(readRDS("./Model_CS.Rds"))
CS[, SZA := sza]

## Create global irradiance W/m^2
CS[, glo := (edir + edn) / 1000 ]

CS <- janitor::remove_constant(CS)


## Keep only relevant
LKUO <- DATA[, .(Date, SZA, sun_dist, wattGLB)]
rm(DATA)

table(CS$type)

warning("check for low data")
##TODO check for low data!!
table(CS$type, CS$month)


table(CS$type, CS$month) == min(table(CS$type, CS$month))


##TODO check for low data!!
table(CS$type, CS$sza)
table(CS$type, CS$sza) == min(table(CS$type, CS$sza))


## Fill data with CS variables
LKUO[, atmosphere_file := date_to_standard_atmosphere_file(Date)]
LKUO[, month := month(Date)]





# https://stackoverflow.com/questions/77875177/fill-a-table-by-interpolating-some-matching-values-from-onother-table-with-inter#comment137295011_77875400
## another approach
# join by
c("month", "atmosphere_file")



library(dplyr)
library(tidyr)

CS %>% filter(type == "Low B")

look <- CS
data <- LKUO

new <- look %>%
    filter(type == "Low B") %>%
    group_by(month, atmosphere_file) %>%
    summarise(fun = list(approxfun(SZA, glo)), .groups = 'keep') %>%
    right_join(nest(data, .by = c(month, atmosphere_file)), by = join_by(month, atmosphere_file))  %>%
    reframe(SZA = data[[1]]$SZA, glo = fun[[1]](SZA))


data_M1 <- cbind(data, glo1 = new$glo)
data_M1[, glo1 := glo1 / sun_dist^2]


## 1. Create an `approxfun` for each `A/B` combination

## The + 0 trick is necessary b/c otherwise data.table does not evaluate C, D
## while creation and thus keeps just the last chunk of it


(fns <- look[type == "Low B", .(f = list(approxfun(SZA + 0, glo + 0))), .(month, atmosphere_file)])
#    A B             f
# 1: A 1 <function[1]>
# 2: B 1 <function[1]>
# 3: C 2 <function[1]>

## 2. Join it to data and apply the function
# data[fns, .(month, atmosphere_file, SZA, glo = Map(\(f, x) f(x), f, SZA)), on = .(month, atmosphere_file)]

data[fns, glo2 := Map(\(f, x) f(x), f, SZA), on = .(month, atmosphere_file)]

data$glo2 <- unlist(data$glo2)


data_M2 <- data
data_M2[, glo2 := glo2 / sun_dist^2]



test <- merge(data_M1, data_M2)

plot(test[1:10000, glo1/glo2])



data_M0 <- readRDS("./CS_LoolUpTable.Rds")


test <- merge(data_M0, test)



plot(test[1:10000, CS_low/glo2])

plot(test[1:10000, CS_low/glo1])



stop()


#
# below <-
#     LKUO |>
#     left_join(CS[type == "Low B"], join_by("month", "atmosphere_file", x$SZA >= y$SZA),
#               suffix = c("", "_below"),
#               relationship = "many-to-many") |>
#     group_by(month, atmosphere_file, SZA) |>
#     filter(C_below == max(C_below)) |>
#     rename(D_below = D) |>
#     ungroup()
#
#
# above <-
#     data |>
#     left_join(look, join_by("A", "B", x$C <= y$C),
#               suffix = c("", "_above"),
#               relationship = "many-to-many") |>
#     group_by(A, B, C) |>
#     filter(C_above == min(C_above)) |>
#     rename(D_above = D) |>
#     ungroup()
#
# res <-
#     below |>
#     inner_join(above, by = join_by(A, B, C)) |>
#     mutate(D = case_when(C_below == C_above ~ D_below,
#                          .default = D_below +
#                              (D_above - D_below)/(C_above - C_below) *
#                              (C - C_below)))
#
#
#
#
#
#
#
# stop()



##  Table for rendering document
#'
#'  # Plots
#'
#+ echo=F, include=T, results="asis"

cc <- 0


# test
# for (aday in sample(unique(as.Date(LKUO[month(Date)==7, Date])))) {
# for (aday in sample(unique(as.Date(LKUO$Date)))) {

for (aday in (unique(as.Date(LKUO$Date)))) {

    cc <- cc + 1
    # LKUO[as.Date(Date) == aday]

    theday  <- as.POSIXct(as.Date(aday, "1970-01-01"))

    cat(paste(theday,"\n"))

    atmfile <- date_to_standard_atmosphere_file(as.POSIXct(as.Date(aday, "1970-01-01")))


    ## choose data and interpolate global
    CS_exact <- CS[atmosphere_file == atmfile &
                       month == month(theday) &
                       type == "Exact B", .(sza, (edir + edn) / 1000 )]

    CS_low   <- CS[atmosphere_file == atmfile &
                       month == month(theday) &
                       type == "Low B",   .(sza, (edir + edn) / 1000 )]

    CS_2_low <- CS[atmosphere_file == atmfile &
                       month == month(theday) &
                       type == "Low 2 B", .(sza, (edir + edn) / 1000 )]


    ## Interpolate to SZA
    CS_2_low_fn <- approxfun(CS_2_low$sza, CS_2_low$V2)
    CS_low_fn   <- approxfun(  CS_low$sza,   CS_low$V2)
    CS_exact_fn <- approxfun(CS_exact$sza, CS_exact$V2)

    LKUO[as.Date(Date) == aday,
         CS_2_low := CS_2_low_fn(LKUO[as.Date(Date) == aday, SZA])]

    LKUO[as.Date(Date) == aday,
         CS_low   := CS_low_fn(  LKUO[as.Date(Date) == aday, SZA])]

    LKUO[as.Date(Date) == aday,
         CS_exact := CS_exact_fn(LKUO[as.Date(Date) == aday, SZA])]


    ## Apply sun - earth distance correction
    LKUO[as.Date(Date) == aday, CS_exact := CS_exact / sun_dist^2]
    LKUO[as.Date(Date) == aday, CS_low   := CS_low   / sun_dist^2]
    LKUO[as.Date(Date) == aday, CS_2_low := CS_2_low / sun_dist^2]


    ## Plot every nth day just for check
    if ( cc %% 60 == 0 ) {
        suppressWarnings({

            p <- ggplot(LKUO[as.Date(Date) == aday], aes(x = Date)) +
                geom_point(aes(y = wattGLB ), col = "green", size = 0.3 ) +
                geom_line( aes(y = CS_low  ), col = "red")     +
                geom_line( aes(y = CS_2_low), col = "cyan")    +
                geom_line( aes(y = CS_exact), col = "magenta") +
                labs( title = theday) +
                theme_bw()
            print(p)
            # plotly::ggplotly(p)

            p <- ggplot(LKUO[as.Date(Date) == aday], aes(x = SZA)) +
                geom_point(aes(y = wattGLB ), col = "green", size = 0.3 ) +
                geom_line( aes(y = CS_low  ), col = "red")     +
                geom_line( aes(y = CS_2_low), col = "cyan")    +
                geom_line( aes(y = CS_exact), col = "magenta") +
                labs( title = theday) +
                theme_bw()
            print(p)
            # plotly::ggplotly(p)

        })
    }
}

LKUO[, wattGLB := NULL ]
saveRDS(LKUO, "/CS_LoolUpTable2.Rds")






#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (difftime(tac, tic, units = "sec") > 30) {
    system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
    system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'FINISHED'"))
}
