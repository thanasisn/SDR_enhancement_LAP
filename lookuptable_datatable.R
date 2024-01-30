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


#'
#' This creates only global and it is intended
#' of building the look up table.
#'
#' - This is fast.
#' - This creates edir, edn and eup
#'
#+ echo=F, include=T
rm(list = (ls()[ls() != ""]))
Script.Name <- "~/MANUSCRIPTS/02_enhancement/lookuptable_datatable.R"
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
# library(RlibRadtran)
library(ggplot2)

source("~/FUNCTIONS/R/data.R")
source("~/Aerosols/RlibRadtran/R/date_to_standard_atmosphere_file.R")


##  Prepare data  --------------------------------------------------------------

## all runs are stored here
model_cs     <- "./data/Model_CS.Rds"

## _ Get raw data we want to create reference for  -----------------------------
DATA <- data.table(readRDS("~/MANUSCRIPTS/02_enhancement/data/CE_ID_Input.Rds"))


## _ Fill with CS approximation model  -----------------------------------------
CS <- data.table(readRDS(model_cs))
CS[, SZA := sza]

## drop some not used data
CS <- janitor::remove_constant(CS)

## Keep only relevant input
LKUO <- DATA[, .(Date, SZA, sun_dist, wattGLB)]

## Fill data with CS variables
LKUO[, atmosphere_file := date_to_standard_atmosphere_file(Date)]
LKUO[, month           := month(Date)]

## clean memory
rm(DATA); dummy <- gc()


##  Inspect data  --------------------------------------------------------------

table(CS$type)

##TODO check for low data!!
table(CS$type, CS$month)
table(CS$type, CS$month) == min(table(CS$type, CS$month))

##TODO check for low data!!
table(CS$type, CS$sza)
table(CS$type, CS$sza) == min(table(CS$type, CS$sza))




##  Data table approach  -------------------------------------------------------

## https://stackoverflow.com/a/77878676/1591737

## _ Do it for all types and variables  ----------------------------------------

types <- unique(CS[, type])
vars  <- c("edir", "edn", "eup")

for (aty in types) {
    for (avr in vars) {
        cat("Interpolation for: ", aty, avr, "\n")

        ## create a combination name
        nnm <- gsub(" ", "_", paste0(aty, ".", avr))

        ## One family of functions for each variable
        fns <- CS[type == aty,   .(f = list(approxfun(SZA + 0, get(avr) + 0))), .(month, atmosphere_file)]

        ## Do the interpolation
        LKUO[fns,  (nnm)   := unlist(Map(\(f, x) f(x), f, SZA)), on = .(month, atmosphere_file)]
    }
}

##  store final lookup table  --------------------------------------------------
LKUO[, wattGLB := NULL ]
saveRDS(LKUO, paste0("./data/", sub(".R", ".Rds", basename(Script.Name))))


summary(LKUO)



## ## This was successful
## ## 1. Create an `approxfun` for each `A/B` combination
##
## ## The + 0 trick is necessary b/c otherwise data.table does not evaluate C, D
## ## while creation and thus keeps just the last chunk of it
##
## fnsLB  <- CS[type == "Low B",   .(f = list(approxfun(SZA + 0, glo + 0))), .(month, atmosphere_file)]
## fnsL2B <- CS[type == "Low 2 B", .(f = list(approxfun(SZA + 0, glo + 0))), .(month, atmosphere_file)]
## fnsEX  <- CS[type == "Exact B", .(f = list(approxfun(SZA + 0, glo + 0))), .(month, atmosphere_file)]
##
## ## 2. Join it to data and apply the function
## # data[fns, .(month, atmosphere_file, SZA, glo = Map(\(f, x) f(x), f, SZA)), on = .(month, atmosphere_file)]
##
## LKUO[fnsLB,  CS_low_v2   := Map(\(f, x) f(x), f, SZA), on = .(month, atmosphere_file)]
## LKUO[fnsL2B, CS_2_low_v2 := Map(\(f, x) f(x), f, SZA), on = .(month, atmosphere_file)]
## LKUO[fnsEX,  CS_exact_v2 := Map(\(f, x) f(x), f, SZA), on = .(month, atmosphere_file)]





## ## Unlist and apply sun distance
## ## This will be done elsewhere!!!
## LKUO[, CS_low_v2   := unlist(CS_low_v2)   / sun_dist^2]
## LKUO[, CS_2_low_v2 := unlist(CS_2_low_v2) / sun_dist^2]
## LKUO[, CS_exact_v2 := unlist(CS_exact_v2) / sun_dist^2]





# # ## compare with dplyr method
# # test1 <- merge(data_M1, LKUO)
# # plot(test1[1:10000, glo1/CS_low_v2])
#
# ## compare with analytic solution
# data_M0 <- readRDS("./CS_LoolUpTable.Rds")
#
#
# test2 <- merge(data_M0, LKUO)
#
# sample(1:nrow(test2), 100000 )
#
# plot(test2[sample(1:nrow(test2), 100000),   CS_low/CS_low_v2  ])
# plot(test2[sample(1:nrow(test2), 100000), CS_2_low/CS_2_low_v2])
# plot(test2[sample(1:nrow(test2), 100000), CS_exact/CS_exact_v2])
#
#
# test2[, max(CS_low/CS_low_v2    )]
# test2[, max(CS_2_low/CS_2_low_v2)]
# test2[, max(CS_exact/CS_exact_v2)]
#
# test2[, max(CS_low   - CS_low_v2  )]
# test2[, max(CS_2_low - CS_2_low_v2)]
# test2[, max(CS_exact - CS_exact_v2)]







##  Dplyr approach  ------------------------------------------------------------
##  This run but maybe wrong
##  https://stackoverflow.com/a/77875400/1591737

# library(dplyr)
# library(tidyr)
#
# CS %>% filter(type == "Low B")
#
# look <- CS
# data <- LKUO
#
# new <- look %>%
#     filter(type == "Low B") %>%
#     group_by(month, atmosphere_file) %>%
#     summarise(fun = list(approxfun(SZA, glo)), .groups = 'keep') %>%
#     right_join(nest(data, .by = c(month, atmosphere_file)), by = join_by(month, atmosphere_file))  %>%
#     reframe(SZA = data[[1]]$SZA, glo = fun[[1]](SZA))
#
# data_M1 <- cbind(data, glo1 = new$glo)
# data_M1[, glo1 := glo1 / sun_dist^2]






##  Dplyr on DuckDB ------------------------------------------------------------
##  https://stackoverflow.com/a/77875532/1591737
##  can not put loukup table in duckdb

# library(dplyr, warn.conflicts = FALSE)
# library(DBI)
#
# db2 <- dbConnect(duckdb::duckdb())
#
# data2 <- tibble(A = c("A","A","A","B","B","B","C","C","C"),
#                B = c(1,1,1,1,1,1,2,2,2),
#                C = rep(c(0.15, 0.22, 0.3), 3)) |>
#     copy_to(db2, df = _, name = "data", overwrite = TRUE)
#
# look2 <- tibble(A = c("A","A","A","B","B","B","C","C","C"),
#                B = c(1,1,1,1,1,1,2,2,2),
#                C = rep(c(0.1, 0.2, 0.3),3),
#                D = c(10, 20, 30, 11,22,33,12,24,36)) |>
#     copy_to(db2, df = _, name = "look", overwrite = TRUE)
#
#
#
# db <- dbConnect(duckdb::duckdb())
#
# data <- LKUO |>
#     copy_to(db, df = _, name = "data", overwrite = TRUE)
#
# look <- CS[type == "Low B"] |>
#     copy_to(db, df = _, name = "look", overwrite = TRUE)
#
#
#
# # below <-
# #     LKUO |>
# #     left_join(CS[type == "Low B"], join_by("month", "atmosphere_file", x$SZA >= y$SZA),
# #               suffix = c("", "_below"),
# #               relationship = "many-to-many") |>
# #     group_by(month, atmosphere_file, SZA) |>
# #     filter(C_below == max(C_below)) |>
# #     rename(D_below = D) |>
# #     ungroup()
#
#
#
# below2 <-
#     data2 |>
#     left_join(look2, join_by("A", "B", x$C >= y$C),
#               suffix = c("", "_below"),
#               relationship = "many-to-many") |>
#     group_by(A, B, C) |>
#     filter(C_below == max(C_below, na.rm = TRUE)) |>
#     rename(D_below = D) |>
#     ungroup()
#
# above2 <-
#     data2 |>
#     left_join(look2, join_by("A", "B", x$C <= y$C),
#               suffix = c("", "_above"),
#               relationship = "many-to-many") |>
#     group_by(A, B, C) |>
#     filter(C_above == min(C_above, na.rm = TRUE)) |>
#     rename(D_above = D) |>
#     ungroup()
#
# res2 <-
#     below2 |>
#     inner_join(above2, by = join_by(A, B, C)) |>
#     mutate(D = case_when(C_below == C_above ~ D_below,
#                          .default = D_below +
#                              (D_above - D_below)/(C_above - C_below) *
#                              (C - C_below))) |>
#     collect()



#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (difftime(tac, tic, units = "sec") > 30) {
    system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
    system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'FINISHED'"))
}
