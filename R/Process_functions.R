
#' This function prepares the market variable variables
#'
#' @import dplyr
#'
#' @import slider
#'
make_market_df = function(df){

  # illiq_df = df %>%
  #   calculate_illiq() %>%
  #   group_by(tase_id, sec_id, date = as.yearqtr(date_yearmon)) %>%
  #   summarise(illiq = mean(illiq, na.rm = TRUE), .groups = "drop")

  sd_df = df %>%
    select(tase_id, sec_id,date, close) %>%
    group_by(tase_id, sec_id) %>%
    arrange(date) %>%
    mutate(ret = c(NA,diff(log(close)))) %>%
    group_by(tase_id, sec_id, date = as.yearqtr(date)) %>%
    summarise(volatility = sd(ret, na.rm = TRUE), .groups = "drop")


  market_df = df %>%
    mutate(across(c(market_value, turnover),
                  ~ . * 10 ^ (-6))) %>%
    mutate(size = log(market_value)) %>%
    group_by(tase_id, sec_id, date = as.yearqtr(date)) %>%
    summarise(across(c(size, market_value, turnover, volume),
                     mean, na.rm = TRUE), .groups = "drop")

 market_df = market_df %>%
   # full_join(illiq_df, by = c("sec_id","date")) %>%
   full_join(sd_df, by = c("tase_id", "sec_id","date"))


 return(market_df)


}



#' This function matches delisted and control companies by
#' sector and market cap
#' @param threshold allowed percentage difference between delisted
#'  and control comps
#'
#'
match_control_group = function(market_df, tase_sector_df,
                               stocks_ipo,threshold = 0.1){

  ipo_control_match_df = market_df %>%
    select(tase_id, sec_id, date, market_value) %>%
    inner_join(stocks_ipo %>%
                 select(tase_id), by = "tase_id") %>%
    group_by(tase_id, sec_id) %>%
    summarise(first_date = date[date == min(date)],
              first_market_value = market_value[date == min(date)],
              .groups = "drop") %>%
    filter(!is.na(first_market_value)) %>%
    mutate(date = as.yearmon(first_date)) %>%
    left_join(tase_sector_df, by = c("tase_id","date")) %>%
    filter(!is.na(tase_sector)) %>%
    unite(id, tase_id:sec_id)

  target_market_df = market_df %>%
    select(tase_id, sec_id, date, market_value) %>%
    filter(complete.cases(.)) %>%
    mutate(date_yearmon = as.yearmon(date)) %>%
    left_join(tase_sector_df,
              by = c("tase_id","date_yearmon" = "date")) %>%
    select(-date_yearmon) %>%
    unite(id, tase_id:sec_id)



  ipo_control_match_df = ipo_control_match_df %>%
    left_join(target_market_df,
              by = c("first_date" = "date", "tase_sector"),
              suffix = c("_ipo","_control")) %>%
    group_by(id_ipo) %>%
    mutate(value_diff = abs(market_value / first_market_value - 1)) %>%
    arrange(value_diff) %>%
    slice(2) %>%
    ungroup() %>%
    filter(value_diff <= threshold)


  return(ipo_control_match_df)


}


#' This function matches delisted and control companies by
#' sector and annualiesed market cap
#' @param threshold allowed percentage difference between delisted
#'  and control comps
#'
#'
match_control_group_annual_mcap = function(market_df, tase_sector_df,
                               stocks_ipo,threshold = 0.1){

  tase_sector_df = tase_sector_df %>%
    mutate(year = year(date)) %>%
    select(year,tase_id, tase_sector) %>%
    distinct()

  ipo_control_match_df = market_df %>%
    select(tase_id, sec_id, date, market_value) %>%
    inner_join(stocks_ipo %>%
                 select(tase_id), by = "tase_id") %>%
    mutate(year = year(date)) %>%
    group_by(tase_id, sec_id) %>%
    filter(year == min(year)) %>%
    group_by(tase_id, sec_id, year) %>%
    summarise(market_value = mean(market_value, na.rm = TRUE),
              .groups = "drop") %>%
    filter(!is.na(market_value)) %>%
    left_join(tase_sector_df, by = c("tase_id","year")) %>%
    filter(!is.na(tase_sector)) %>%
    unite(id, tase_id:sec_id)

  target_market_df = market_df %>%
    mutate(year = year(date)) %>%
    group_by(tase_id, sec_id, year) %>%
    summarise(market_value = mean(market_value, na.rm = TRUE),
              .groups = "drop") %>%
    filter(complete.cases(.)) %>%
    left_join(tase_sector_df, by = c("tase_id","year")) %>%
    unite(id, tase_id:sec_id)



  matched_df = ipo_control_match_df %>%
     left_join(target_market_df,
              by = c("year", "tase_sector"),
              suffix = c("_ipo","_control")) %>%
    group_by(id_ipo) %>%
    mutate(value_diff = abs(market_value_ipo / market_value_control - 1)) %>%
    arrange(value_diff) %>%
    slice(2) %>%
    ungroup() %>%
    filter(value_diff <= threshold)


  return(matched_df)


}



#' This functions calculates adjusted price
#'
calculate_adjusted_price = function(df,
                                    base_rate_threshold = 1){

  if("close_rate_adj" %in% names(df)){

    df = df %>% select(-"close_rate_adj")
  }

  adjusted_df = df %>%
    filter(!is.na(base_rate)) %>%
    group_by(tase_id, sec_id) %>%
    filter(abs(base_rate -  base_rate_threshold) > 0.1) %>%
    arrange(date) %>%
    mutate(daily_gross_ret = close_rate / base_rate) %>%
    mutate(close_adjusted = close_rate[1] * c(1,cumprod(daily_gross_ret)[-1])) %>%
    ungroup() %>%
    select(-daily_gross_ret)

  return(adjusted_df)





}


#' This function makes price df by joining price data for securities and
#' benchmarks
#'
make_price_df = function(market_df,ipo_control_match_df,ta_125,
                         tase_sector_df){

  benchmark_df =  market_df %>%
    select(tase_id, sec_id,date, control = close_adjusted) %>%
    unite(id_control,tase_id:sec_id) %>%
    inner_join(ipo_control_match_df %>%
                 select(contains("id")),
               by = "id_control") %>%
    rename(id = id_ipo)


  price_df = market_df %>%
    inner_join(stocks_ipo %>%
                 select(tase_id),by = "tase_id")

  price_df = price_df %>%
    select(tase_id, sec_id,date, close = close_adjusted) %>%
    mutate(date_yearmon = as.yearmon(date)) %>%
    left_join(tase_sector_df,
              by = c("date_yearmon" = "date", "tase_id")) %>%
    select(-date_yearmon)

  price_df = price_df %>%
    left_join(ta_125 %>%
                rename(index = ta_125), by = "date")

  price_df = price_df %>%
    mutate(id = paste(tase_id, sec_id, sep = "_")) %>%
    left_join(benchmark_df,by = c("date", "id")) %>%
    select(-c(tase_id,sec_id,id_control))


  price_df = price_df %>%
    group_by(id) %>%
    mutate(month = as.numeric(date -  min(date)) %/% 22) %>%
    group_by(id, month) %>%
    summarise(across(c("close","index","control"), ~mean(., na.rm = TRUE)),
              tase_sector = unique(tase_sector),
              .groups = "drop")

  return(price_df)



}


