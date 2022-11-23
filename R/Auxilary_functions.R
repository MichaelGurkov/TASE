#' @title Calculate Amihud's iiliquidity ratio
#'
#' @description This function calculates Amihud's iiliquidity ratio from market data
#'
#' @import dplyr
#'
#' @import zoo
#'
#' @param market_df a data frame with price and turnover data at daily frequency

calculate_illiq = function(market_df){

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



#' This function pivots delisted and control comps into
#' tase_id, date, comp_type format
#'
#' @param match_table a data frame with tase_id_delisted,
#' tase_id_control, ipo_date, delisting_date structure
#'
pivot_to_join_format = function(match_table){

  join_format = match_table %>%
    mutate(time_period = map2(ipo_date, delisting_date,
                              function(start,end){

                                period = seq.Date(from = start,
                                                  to = end,
                                                  by = "quarter") %>%
                                  as.yearqtr()

                                return(period)


                              })) %>%
    select(starts_with("tase_id"), time_period) %>%
    pivot_longer(starts_with("tase_id"),
                 names_to = "comp_type", values_to = "tase_id") %>%
    mutate(comp_type = str_remove_all(comp_type,"tase_id_")) %>%
    distinct() %>%
    unnest(time_period)


  return(join_format)


}
