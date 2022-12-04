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
#' selected varible
#'
#' @param comps_list table with tase_ id, ipo_date, delisted_status
#'
#' @param matching_variable the variable control group is matched on
#'
#' @param threshold allowed percentage difference between delisted
#'  and control comps
#'
#'
match_control_group = function(comps_list, data_df,
                               matching_variable,
                               threshold){

  data_df = data_df %>%
    select(tase_id, date, all_of(matching_variable)) %>%
    filter(complete.cases(.))

  delisted_comps= comps_list %>%
    filter(delisted_status == 1) %>%
    select(tase_id, ipo_date) %>%
    mutate(tase_id = as.character(tase_id)) %>%
    mutate(ipo_date = as.yearqtr(ipo_date)) %>%
    left_join(data_df, by = c("tase_id", "ipo_date" = "date")) %>%
    filter(complete.cases(.))


  control_comps = comps_list %>%
    filter(delisted_status == 0) %>%
    select(tase_id, ipo_date) %>%
    mutate(tase_id = as.character(tase_id)) %>%
    mutate(ipo_date = as.yearqtr(ipo_date)) %>%
    left_join(data_df, by = c("tase_id", "ipo_date" = "date")) %>%
    filter(complete.cases(.))

  del_var = paste0(matching_variable,"_delisted")

  control_var = paste0(matching_variable,"_control")


  matching_table = delisted_comps %>%
    full_join(control_comps,
              by = "ipo_date",
              suffix = c("_delisted","_control")) %>%
    mutate(diff = !!sym(del_var) / !!sym(control_var) - 1) %>%
    filter(abs(diff) <= threshold) %>%
    select("tase_id_delisted","tase_id_control") %>%
    mutate(across(starts_with("tase_id"),as.character))

  return(matching_table)


}

#' This function returns matched df
#'
#'
get_matched_df = function(df, comps_dates,
                          matching_variable,
                          threshold){


  matched_table = comps_dates %>%
    mutate(delisted_status = if_else(is.na(quotation_period),0,1)) %>%
    select(tase_id, ipo_date, delisted_status) %>%
    match_control_group(data_df = df,
                        matching_variable = matching_variable,
                        threshold = threshold)


  matched_comps_df = matched_table %>%
    left_join(comps_dates %>%
                select(tase_id, contains("date")),
              by = c("tase_id_delisted" = "tase_id")) %>%
    pivot_to_join_format() %>%
    left_join(df, by = c("tase_id",c("time_period" = "date")))

  return(matched_comps_df)




}




#' This functions cleans the dirty dataframe
#'
clean_df = function(dirty_df){




}
