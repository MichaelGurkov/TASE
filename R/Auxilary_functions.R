#' @title Calculate Amihud's iiliquidity ratio
#'
#' @description This function calculates Amihud's iiliquidity ratio from market data
#'
#' @import dplyr
#'
#' @import zoo
#'
#' @param market_df a data frame with price and turnover data at daily frequency

calculate.iiliq = function(market_df){

  market_df = market_df %>%
    arrange(date)

  illiq_df = market_df %>%
    select(date, sec_id, close, turnover) %>%
    group_by(sec_id) %>%
    mutate(sec_ret = c(NA,diff(log(close)))) %>%
    ungroup() %>%
    filter(complete.cases(.)) %>%
    mutate(daily_ratio = abs(sec_ret) / turnover) %>%
    group_by(sec_id, date_yearmon = as.yearmon(date)) %>%
    summarise(illiq = mean(sqrt(na.omit(daily_ratio))),
              .groups = "drop")

  return(illiq_df)

}


#' This function calculates betas and CAR's
#'
#' @import dplyr
#'
#' @import lubridate

calculate.beta.and.car = function(df){

  ta135 = read_csv(paste0(
    file.path(Sys.getenv("USERPROFILE"), fsep = "\\"),
    "\\OneDrive - Bank Of Israel\\Data\\",
    "TASE liquidity\\TA_125_2000_2020.csv")) %>%
    mutate(date = dmy(date))






}
