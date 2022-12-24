#' This function prepares the financial reports variables
#'
#' @import dplyr
#'
make_finrep_df = function(raw_df, selected_x_vars = NULL,
                          filter_df = FALSE){

  x_vars = c(
    "leverage",
    "capex_to_revenue",
    "roa",
    "free_cashflow",
    "intangible"
  )

  finrep_df = raw_df %>%
    rename_all(tolower)

  finrep_df = finrep_df %>%
    select(-starts_with("entity")) %>%
    mutate(across(-c(starts_with("tase"),"year") &
                    where(is.character),
                  as.numeric)) %>%
    mutate(leverage = total_liabilities / total_assets) %>%
    mutate(capex_to_revenue = capex_cashflow / revenue) %>%
    mutate(roa = operating_profit / total_assets) %>%
    mutate(free_cashflow = operating_cashflow / total_assets)


  if(!is.null(selected_x_vars)){

    finrep_df = finrep_df %>%
      select(date, tase_id, any_of(selected_x_vars))

  }



  if(filter_df){

    finrep_df = finrep_df %>%
      filter(complete.cases(.)) %>%
      filter(across(where(is.numeric), is.finite))

  }


  return(finrep_df)






}


#' @title Prepare financial reports data
#'
#' @details This function prepares the financial reports
#' variables data from Oracle format. The original values are given in thousands ILS
#' and are scaled down by 1000 so that the result is in millions ILS
#'
#' @import dplyr
#'
make_finrep_oracle_df = function(raw_oracle_df,
                                 raw_finrep_vars,
                                 final_finrep_vars){


  finrep_df = raw_oracle_df %>%
    select(tase_id, date = date_yearqtr, any_of(raw_finrep_vars))

  finrep_df = finrep_df %>%
    mutate(across(-c("tase_id", "date"),as.numeric))

  finrep_df = finrep_df %>%
    mutate(across(where(is.numeric), ~ . * 10 ^ (-3)))

  finrep_df = finrep_df %>%
    mutate(leverage = total_liabilities / total_assets) %>%
    mutate(capex_to_revenue =
             investing_activities / total_revenue) %>%
    mutate(roa = operating_profit / total_assets) %>%
    mutate(free_cashflow = operating_activities / total_assets)

  finrep_df = finrep_df %>%
    select(tase_id, date, any_of(final_finrep_vars))



  return(finrep_df)






}


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


#' This function merges and cleans data to construct reg df
#'
#' @import dplyr
#'
#' @param raw_data list of finrep and market data
#'
#'
make_reg_df = function(market_df, finrep_df,
                       final_vars = NULL){

  aggregated_market_df = market_df %>%
    select(-sec_id) %>%
    group_by(tase_id, date) %>%
    summarise(across(everything(), ~mean(.,na.rm = TRUE)),
              .groups = "drop")

  merged_df = aggregated_market_df %>%
    full_join(finrep_df, by = c("tase_id","date"))


  df = merged_df %>%
    mutate(mb = market_value / total_assets) %>%
    mutate(volume = log(volume))

  if(!is.null(final_vars)){

    df = df %>%
      select(tase_id, date, any_of(final_vars))

  }

  return(df)



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


#' This functions calculates adjusted price
#'
calculate_adjusted_price = function(df,
                                    base_rate_threshold = 1){

  adjusted_df = df %>%
    filter(!is.na(base_rate)) %>%
    group_by(tase_id, sec_id) %>%
    filter(abs(base_rate -  base_rate_threshold) > 0.1) %>%
    arrange(date) %>%
    mutate(daily_gross_ret = close_rate / base_rate) %>%
    mutate(close_adjusted = close_rate[1] * c(1,cumprod(daily_gross_ret)[-1])) %>%
    ungroup()

  return(adjusted_df)





}


#' This function makes price df by joining price data for securities and
#' benchmarks
#'
make_price_df = function(market_df,ipo_control_match_df,ta_125 ){

  benchmark_df =  market_df %>%
    select(tase_id, sec_id,date, control = close_adjusted) %>%
    unite(id_control,tase_id:sec_id) %>%
    inner_join(ipo_control_match_df %>%
                 select(contains("id")),
               by = "id_control") %>%
    rename(id = id_ipo)


  price_df = market_df %>%
    select(tase_id, sec_id,date, close = close_adjusted) %>%
    inner_join(stocks_ipo %>%
                 select(tase_id),by = "tase_id") %>%
    left_join(ta_125 %>%
                rename(index = ta_125), by = "date") %>%
    mutate(id = paste(tase_id, sec_id, sep = "_")) %>%
    left_join(benchmark_df,by = c("date", "id")) %>%
    select(-c(tase_id,sec_id,id_control))


  price_df = price_df %>%
    group_by(id) %>%
    mutate(month = as.numeric(date - date[date == min(date)]) %/% 22 + 1) %>%
    group_by(id, month) %>%
    summarise(across(c("close","index","control"), ~mean(., na.rm = TRUE)),
              .groups = "drop")

  return(price_df)



}

