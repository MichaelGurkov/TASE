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


#' This function calculates raw and adjusted return
#'
calculate_return_df = function(price_df){

  ret_df = price_df %>%
    group_by(id) %>%
    arrange(month) %>%
    mutate(across(c("close","index", "control"),
                  ~ . /lag(.) - 1,
                  .names = "{.col}_ret")) %>%
    ungroup() %>%
    select(-c("close","index","control"))

  adj_ret_df = ret_df %>%
    pivot_longer(c("index_ret","control_ret"),
                 names_to = "benchmark",
                 values_to = "benchmark_ret") %>%
    filter(complete.cases(.)) %>%
    mutate(adjusted_ret = close_ret - benchmark_ret) %>%
    select(-benchmark_ret) %>%
    pivot_wider(names_from = "benchmark",
                values_from = "adjusted_ret") %>%
    pivot_longer(-c(id,month),names_to = "adjustment_type")


  return(adj_ret_df)


}


#' This function calculates cumulative return
#'
calculate_cum_return = function(ret_df){

  avg_adj_ret_df = ret_df%>%
    group_by(month, adjustment_type) %>%
    summarise(across(value,
                     .fns = list(mean = ~mean(.,na.rm = TRUE),
                                 median = ~median(.,na.rm = TRUE)),
                     .names = "{.fn}"), .groups = "drop")


  cum_ret_df = avg_adj_ret_df  %>%
    filter(month <= 120)  %>%
    pivot_longer(c("mean", "median"), names_to = "summary_measure") %>%
    mutate(adjustment_type = factor(
      adjustment_type,
      labels = c("Raw returns",
                 "Abnormal (index adjusted) returns",
                 "Control group returns")
    )) %>%
    filter(complete.cases(.)) %>%
    group_by(adjustment_type, summary_measure) %>%
    arrange(month) %>%
    mutate(value = cumsum(value)) %>%
    ungroup()

  return(cum_ret_df)

}


#' This function mutates the price data
#'
clean_price_data = function(price_vec){

  power_vec = floor(log10(price_vec))

  diff_power_vec = diff(power_vec)

  break_index = which(diff_power_vec == max(diff_power_vec))

  price_vec[1: break_index] = price_vec[1: break_index] * 10 ^ max(diff_power_vec)

  10 ^ power_vec

  price_vec * (10 ^ (-power_vec))






}

