


#' This function takes returns df and mathed table and calculates
#' benchmark performance
#'
#'
#'

calculate_bhar_from_price_df = function(price_df,
                                        target_duration,
                                        duration_tolerance = 0.05){

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

  bhar_df = bhar_df %>%
    mutate(dur_diff = target_duration / duration - 1) %>%
    filter(dur_diff <= duration_tolerance) %>%
    select(-duration, -dur_diff)


  return(bhar_df)



}


#' This function calculates periodic returns from price df. In order to do that
#' the frequency is reduced by taking the endpoints of the period
#'
calculate_period_ret_from_price_df = function(price_df,
                                                  period_freq,
                                                  period_thresh = NULL){

  period_df = price_df %>%
    group_by(id,id_control) %>%
    arrange(date) %>%
    mutate(duration = as.numeric(date - min(date))) %>%
    mutate(period = duration %/% period_freq) %>%
    {if(!is.null(period_thresh)) filter(., period <= period_thresh) else .} %>%
    group_by(period) %>%
    slice_min(duration) %>%
    ungroup()


  ret_df = period_df %>%
    group_by(id, id_control) %>%
    arrange(period) %>%
    mutate(across(starts_with("price"), ~. / lag(.) - 1)) %>%
    ungroup() %>%
    rename_with(.cols = starts_with("price"),
                ~str_replace(.,"price","ret")) %>%
    select(id, id_control,period, starts_with("ret")) %>%
    filter(complete.cases(.)) %>%
    mutate(ret_abnormal = ret_ipo - ret_control)

  return(ret_df)


}
