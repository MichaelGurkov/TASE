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

  ta135 = read_csv(paste0(
    file.path(Sys.getenv("USERPROFILE"), fsep = "\\"),
    "\\OneDrive - Bank Of Israel\\Data\\",
    "TASE liquidity\\TA_125_2000_2020.csv")) %>%
    mutate(date = dmy(date))

  illiq_df = df %>%
    calculate.iiliq()

  ret_df = market_df %>%
    group_by(sec_id) %>%
    mutate(sec_ret = slide_dbl(close_rate,~(.[2] / .[1] - 1),
                               .before = 1)) %>%
    group_by(sec_id, year = year(date)) %>%
    summarise(sec_sd = sd(sec_ret, na.rm = TRUE),.groups = "drop")





  return(finrep_df)






}
