
#### dictionary ####
dictionay <- list(
    # Name        Short                             Long
    ST_daily         = c("Daily",                       "Daily statistics",                       ),
    ST_E_daily       = c("Daily Enhancements",           "Daily Enhancements",           ),
    ST_E_daily_seas  = c("Seasonal Daily Enhancements", "Seasonal Daily Enhancements", ),
    ST_extreme_daily = c("Daily Extreme Enhancements",   "Daily Extreme Enhancements",   ),
    empty            = c(" --- empty --- ",             " --- empty --- "              )
)


#' Get nice name for a variable in BB DB
#'
#' @param x    Name of a column
#' @param type What to return one of c("short", "long")
#'
#' @return     A string for a variable
#' @export
#'
tr_var <- function(x, type = "short") {
    require(stringr)
    types <- c("short", "long")

    if (!type %in% types) {
        cat("No such column:", type, "\n")
        return(NA)
    }

    ty  <- which(type == types)
    res <- c()
    for (ax in x) {
        res <- c(res,
                 as.vector(
                     unlist(dictionay[str_detect(ax, names(dictionay))])[[ty]]
                 )
        )
    }
    return(res)
}


tr_var(x = "DIR_att")

