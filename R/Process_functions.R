#' This function prepares the financial reports variables
#'
#' @import dplyr
#'
make_finrep_df = function(raw_df, selected_x_vars = NULL){

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
    mutate(across(-c(starts_with("tase"),"year") & where(is.character),
                  as.numeric)) %>%
    mutate(leverage = total_liabilities / total_assets) %>%
    mutate(capex_to_revenue = capex_cashflow / revenue) %>%
    mutate(roa = operating_profit / total_assets) %>%
    mutate(free_cashflow = operating_cashflow / total_assets)


  if(!is.null(selected_x_vars)){

    finrep_df = finrep_df %>%
      select(date, tase_id, any_of(selected_x_vars))

  }



  finrep_df = finrep_df %>%
    filter(complete.cases(.)) %>%
    filter(across(where(is.numeric), is.finite))


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
    calculate.iiliq() %>%
    group_by(sec_id, date_yearqtr = as.yearqtr(date_yearmon)) %>%
    summarise(illiq = mean(illiq, na.rm = TRUE), .groups = "drop")

  market_df = df %>%
    select(-close_rate,-tase_id) %>%
    mutate(across(c(market_value, turnover),
                  ~ . * 10 ^ (-6))) %>%
    mutate(size = log(market_value)) %>%
    group_by(sec_id, date_yearqtr = as.yearqtr(date)) %>%
    summarise(across(c(size, market_value, turnover),
                     mean, na.rm = TRUE), .groups = "drop")%>%
    filter_at(.vars = vars(size,market_value,turnover),
              any_vars(!is.na(.)))

 market_df = market_df %>%
   full_join(illiq_df, by = c("sec_id","date_yearqtr"))


 return(market_df)







  return()






}
