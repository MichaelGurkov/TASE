
#' This function cleans market df and filters out irrelevant data
#'
#' @param market_df a data frame with the following structure:
#'  date,tase_id,sec_id,close_adjusted,market_value
#'
clean_market_df = function(market_df){

  duplicated_comps = market_df %>%
    select(tase_id, sec_id) %>%
    distinct() %>%
    count(tase_id) %>%
    filter(n > 1)


  market_df = market_df %>%
    anti_join(duplicated_comps, by = "tase_id")

  market_df = market_df %>%
    filter(!tase_id == 106)


  market_df = market_df %>%
    select(-sec_id) %>%
    rename(price = close_adjusted)


  return(market_df)

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


#' This function makes price df by merging price data for securities and
#' benchmarks
#'
#' @param price_df a data frame with column named id
#'
#' @param matched_sample a data frame with columns id_ipo and id_control
#'
merge_ipo_control_prices = function(price_df,matched_sample){

  price_df_ipo = price_df %>%
    inner_join(matched_sample, by = c("id" = "id_ipo"))

  price_df_ipo_control = price_df_ipo %>%
    left_join(price_df, by = c("id_control" = "id", "date"),
              suffix = c("_ipo","_control")) %>%
    filter(complete.cases(.))

  return(price_df_ipo_control)



}


