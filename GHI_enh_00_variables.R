
## Variables for project


#### dictionary ####
dict <- list(DIR_att    = 'Dir. Beam Irrad.',
             HOR_att    = 'Dir. Irrad. horizontal plane',
             DIR_transp = 'Transp. for Dir. Irrad.',
             GLB_att    = "SDR",
             tsi1au_att = "TSI at 1au",
             ALL        = "All sky cond.",
             CLEAR      = "Clear sky cond.",
             CLOUD      = "Cloudy sky cond.")
## function to translate objects names
# translate <- function(...) as.vector(unlist(dict[c(...) == names(dict)]))

translate <- function(x) {
    res <- c()
    for (ax in x) {
        res <- c(res,
                 as.vector(unlist(
                     dict[stringr::str_detect(ax, names(dict))]
                 ))
        )
    }
    return(res)
}




#### Data range ####
## will not include the last/first day
# LAST_DAY                 <- as.Date("2023-06-01") ## Cleaned data
# LAST_DAY                 <- as.Date("2011-04-01") ## similar to Bais analysis
LAST_DAY                 <- as.POSIXct("2023-04-13 11:59:30") ## set the date of last NOAA TSI data
FIRST_DAY                <- as.POSIXct("1900-07-01") ## include all available
# FIRST_DAY                <- as.Date("2005-01-01") ## data inspected by me
# FIRST_DAY                <- as.Date("2016-04-01") ## start of chp1


#### Paths ####
CLEARdir                 <- "~/DATA/Broad_Band/CS_id"
tag                      <- paste0("Natsis Athanasios LAP AUTH ", strftime(Sys.time(), format = "%b %Y" ))
CS_file_13               <- "./data/Clear_Sky_13.Rds"
CS_file_14               <- "./data/Clear_Sky_14.Rds"
CS_file_14               <- "./data/Clear_Sky_14.Rds"
CS_file_14_2             <- "./data/Clear_Sky_14_2.Rds"
common_data_13           <- "./data/common_data_13.Rda"
common_data_14           <- "./data/common_data_14.Rda"
common_data_14_2         <- "./data/common_data_14_2.Rda"
variables_fl             <- "./GHI_enh_0_variables.R"
data_procsess_fl         <- "./GHI_enh_00_raw_data.R"
raw_input_data           <- "./data/Raw_Input.Rds"
Input_data_ID            <- "./data/CE_ID_Input.Rds"
I1_longterm              <- "./data/Input_1_longterm_trends.Rda"
I2_szatrend              <- "./data/Input_2_sza_trends.Rda"
I3_trendsconsist         <- "./data/Input_3_trends_consist.Rda"

#### colors ####
col_DIR_att              <- "#2166ac"
col_HOR_att              <- "#4244ac"
col_DIR_transp           <- "#9970ab"
col_GLB_att              <- "#1a9850"
col_tsi1au_att           <- "#e3e300"


#### parameters ####

## https://www.rapidtables.com/calc/time/days-in-year.html
# Days_of_year             <- 365.25   ## Mean Julian year
Days_of_year              <- 365.2425 ## Mean Gregorian calendar year
running_mean_window_years <-   5
running_mean_window_days  <- running_mean_window_years * Days_of_year

MIN_ELEVA                 <-  -10  ##  no global elevation limit
MIN_N                     <-    4
SEAS_MIN_N                <-    3
BIO_ELEVA                 <-   12  ## 11.55479

Daily_confidence_limit    <-   0.99
SZA_confidence_limit      <-   0.99
Monthly_confidence_limit  <-   0.99

# Daily_aggregation_N_lim   <-  60 * 3 # minutes in a day
Daily_aggregation_N_lim   <-   0       # replaced with relative daylight filter
Monthly_aggegation_N_lim  <-  20
SZA_aggregation_N_lim     <-   4

All_daily_ratio_lim       <- 0.3   # Keep days with daylight data available
Clear_daily_ratio_lim     <- 0.5
Cloud_daily_ratio_lim     <- 0.5



## extra skip ranges for this work
SKIP_cm21 <- matrix(
##     From                  Until                  Comment
    c("1996-02-10 00:00:00", "1996-02-29 23:59:00", "Systematic cut off due to gain error",
      NULL),
    byrow = TRUE,
    ncol = 3)

## Format to data frame
SKIP_cm21 <- data.frame(From    = as.POSIXct(  SKIP_cm21[,1]),
                        Until   = as.POSIXct(  SKIP_cm21[,2]),
                        Comment = as.character(SKIP_cm21[,3]))


## __ Variables  -----------------------------------------------------------


min_elevation      <- 10    ## minimum sun elevation to use
ampl               <- 1.05  ## adjusted HAU amplified threshold
SZA_BIN            <- 1



