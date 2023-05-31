
#' This function calculates buy and hold returns from
#' price df (matched ipo and control)
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

#' This function calculates average abnormal returns from
#' price df (matched ipo and control)
#'
average_abnormal_return = function(price_df){

  abnormal_df = price_df %>%
    group_by(id, id_control) %>%
    arrange(date) %>%
    mutate(across(c("price_ipo","price_control"), ~./dplyr::lag(.) - 1)) %>%
    ungroup() %>%
    rename_with(~str_replace_all(.,"price","ret")) %>%
    filter(complete.cases(.)) %>%
    mutate(abnormal_return = ret_ipo - ret_control)

  average_df = abnormal_df %>%
    group_by(date) %>%
    summarise(average_abnormal_return = mean(abnormal_return),
              .groups = "drop")


  return(average_df)



}

#' This function transforms price df into trading months
#'
convert_to_trading_months = function(price_df){

  first_day_df = price_df %>%
    group_by(id, id_control) %>%
    filter(date == min(date)) %>%
    mutate(date = 0)

  monthly_df = price_df %>%
    group_by(id, id_control) %>%
    mutate(date = as.numeric(date - min(date))) %>%
    filter(!date == 0) %>%
    mutate(date = (date %/% 22) + 1) %>%
    ungroup()

  monthly_df = monthly_df %>%
    bind_rows(first_day_df) %>%
    arrange(id,id_control, date)

  monthly_df = monthly_df %>%
    group_by(date, id, id_control) %>%
    summarise(across(everything(), mean), .groups = "drop")

  return(monthly_df)

}
