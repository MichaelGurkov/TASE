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
                          selected_x_vars = NULL,
                          filter_df = FALSE){

  x_vars = c(
    "leverage",
    "capex_to_revenue",
    "roa",
    "free_cashflow",
    "intangible_assets")


  finrep_df = finrep_df %>%
    select(-any_of(
      c(
        "fsd_period",
        "reporting_period",
        "be_master_gk",
        "be_ver_gk",
        "date_fsd",
        "company_id",
        "business_entity_name",
        "firm_merchantability_heb"
      )
    ))

  finrep_df = finrep_df %>%
    mutate(across(c(-date_yearqtr,-starts_with("is"), -tase_id),
                  as.numeric))

  finrep_df = finrep_df %>%
    mutate(across(where(is.numeric), ~ . * 10 ^ (-3)))

  finrep_df = finrep_df %>%
    mutate(leverage = total_liabilities / total_assets) %>%
    mutate(capex_to_revenue =
             investing_activities / total_revenue) %>%
    mutate(roa = operating_profit / total_assets) %>%
    mutate(free_cashflow = operating_activities / total_assets)


  if(!is.null(selected_x_vars)){

    finrep_df = finrep_df %>%
      select(date = date_yearqtr, tase_id, any_of(selected_x_vars))

  }



  if(filter_df){

    finrep_df = finrep_df %>%
      filter(complete.cases(.)) %>%
      filter(across(where(is.numeric), is.finite))

  }


  return(finrep_df)






}


#' This function prepares the market variable variables
#'
#' @import dplyr
#'
#' @import slider
#'
make_market_df = function(df){

  illiq_df = df %>%
    calculate_illiq() %>%
    group_by(sec_id, date = as.yearqtr(date_yearmon)) %>%
    summarise(illiq = mean(illiq, na.rm = TRUE), .groups = "drop")

  sd_df = df %>%
    select(date,sec_id, close) %>%
    group_by(sec_id) %>%
    arrange(date) %>%
    mutate(ret = c(NA,diff(log(close)))) %>%
    group_by(sec_id, date = as.yearqtr(date)) %>%
    summarise(volatility = sd(ret, na.rm = TRUE), .groups = "drop")


  market_df = df %>%
    select(-close,-tase_id) %>%
    mutate(across(c(market_value, turnover),
                  ~ . * 10 ^ (-6))) %>%
    mutate(size = log(market_value)) %>%
    group_by(sec_id, date = as.yearqtr(date)) %>%
    summarise(across(c(size, market_value, turnover, volume),
                     mean, na.rm = TRUE), .groups = "drop")

 market_df = market_df %>%
   full_join(illiq_df, by = c("sec_id","date")) %>%
   full_join(sd_df, by = c("sec_id","date"))


 return(market_df)


}


#' This function merges and cleans data to construct reg df
#'
#' @import dplyr
#'
#' @param raw_data list of finrep and market data
#'
#'
make_reg_df = function(market_df, finrep_df, secs_catalog){

  merged_df = market_df %>%
    left_join(secs_catalog %>%
                select(sec_id, tase_id) %>%
                distinct(), by = "sec_id") %>%
    inner_join(finrep_df, by = c("tase_id","date"))


  df = merged_df %>%
    mutate(mb = market_value / total_assets) %>%
    mutate(volume_log = log(volume))

  df = secs_catalog %>%
    select(tase_id, tase_branch_eng) %>%
    filter(tase_branch_eng == "technology") %>%
    distinct() %>%
    right_join(df,by = "tase_id")

  return(df)



}
