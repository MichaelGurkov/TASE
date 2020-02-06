#' @title Calculate Amihud's iiliquidity ratio
#'
#' @description This function calculates Amihud's iiliquidity ratio from market data
#'
#' @import dplyr
#'
#' @import xts
#'
#' @param market_df a data frame with price and turnover data at daily frequency

calculate.iiliq = function(market_df){

  market_df = market_df %>%
    arrange(Date)

  market_df = market_df %>%
   mutate(Sec_Ret = c(NA,diff(log(Close)))) %>%
    mutate(Daily_Ratio = abs(Sec_Ret) / Turnover)

  illiq_df = market_df %>%
    mutate(Year_Month = as.yearmon(Date)) %>%
    group_by(Year_Month) %>%
    summarise(Illiq = mean(sqrt(na.omit(Daily_Ratio))))

  return(illiq_df)

}

#' @title Get market variables
#'
#' @description This function takes market data and returns data frame with market cap
#' turnover and illiquidity measure variables
#'
#' @param market_df a data frame with price ,turnover  and market capitalization
#' data at daily frequency
#'
#' @import dplyr
#'
#' @import xts

get.market.variables = function(market_df){

  market_df = market_df %>%
    mutate(Date = as.Date(DATE_VALUE)) %>%
    select(-DATE_VALUE)%>%
    rename(Sec_ID = SECURITY_IDENT_NUM_TASE, Close = CLOSE_RATE,
           Market_Cap = MARKET_VALUE, Turnover = TURNOVER,
           Comp_ID = TASE_ISSUER_ID)

  illiq_df_list = lapply(split(market_df[,c("Date","Close","Turnover")], market_df$Comp_ID),
                         calculate.iiliq)

  illiq_df = lapply(names(illiq_df_list), function(temp_name){

    temp_df = illiq_df_list[[temp_name]]

    temp_df = temp_df %>%
      rename(Date = Year_Month) %>%
      mutate(Date = as.yearqtr(Date)) %>%
      group_by(Date) %>%
      summarise(Illiq = mean(Illiq, na.rm = TRUE)) %>%
      mutate(Comp_ID = temp_name) %>%
      ungroup()

    return(temp_df)


  }) %>%
    bind_rows() %>%
    mutate(Date = as.yearqtr(Date))

  df = market_df %>%
    mutate(Date = as.yearqtr(Date)) %>%
    mutate(Comp_ID = as.character(Comp_ID)) %>%
    group_by(Date, Comp_ID, Sec_ID) %>%
    summarise(Market_Cap = mean(Market_Cap, na.rm = TRUE),
              Turnover = mean(Turnover, na.rm = TRUE))

  df = full_join(df, illiq_df, by = c("Date" = "Date", "Comp_ID" = "Comp_ID"))

  return(df)

}
