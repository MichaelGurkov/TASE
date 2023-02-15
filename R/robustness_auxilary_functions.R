#' @title Match control comps to IPO comps
#'
#' @description This function matches control comps by market value
#'
#' @details The matching is performed on mature firms (\code{age_treshold}
#' parameter)
#'
match_comps_by_market_value = function(ipo_comps, market_df,
                                       age_threshold = 1,
                                       market_threshold = 0.1,
                                       single_match = TRUE){


  ipo_sample = market_df %>%
    inner_join(ipo_comps, by = "tase_id") %>%
    unite(id,c("tase_id","sec_id")) %>%
    group_by(id) %>%
    summarise(market_value = market_value[date == min(date)],
              date = min(date),
              .groups = "drop") %>%
    filter(complete.cases(.))

  target_sample = market_df %>%
    filter(!is.na(market_value)) %>%
    unite(id, c("tase_id","sec_id")) %>%
    select(date, id, market_value) %>%
    group_by(id) %>%
    mutate(age = as.numeric(date - min(date)) / 252) %>%
    ungroup() %>%
    filter(age >= age_threshold)


  matched_sample = ipo_sample %>%
    left_join(target_sample, by = c("date"),
              suffix = c("_ipo","_control")) %>%
    filter(complete.cases(.)) %>%
    mutate(matching_diff = abs(market_value_ipo/market_value_control - 1)) %>%
    filter(matching_diff < market_threshold)

  if(single_match){

    matched_sample = matched_sample %>%
      group_by(id_ipo) %>%
      slice_min(order_by = matching_diff) %>%
      ungroup() %>%
      select(id_ipo, id_control)


  } else {

    matched_sample = matched_sample %>%
       select(id_ipo, id_control)


  }

 return(matched_sample)


}



#' This function takes returns df and mathed table and calculates
#' benchmark performance
#'
#'
#'

calculate_bhar_from_price_df = function(price_df,
                                           target_duration){

  bhar_df = price_df %>%
    group_by(id, id_control) %>%
    arrange(date) %>%
    mutate(duration = as.numeric(date - min(date))) %>%
    filter(duration <= target_duration) %>%
    slice(1, length(duration)) %>%
    summarise(bhar_ipo = (price_ipo[2] / price_ipo[1]) - 1,
              bhar_control = (price_control[2] / price_control[1]) - 1,
              bhar_abnormal = bhar_ipo - bhar_control,
              duration = duration[2], .groups = "drop")


  return(bhar_df)



}
