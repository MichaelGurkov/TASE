
#' This function calculates buy and hold returns from
#' price df (matched ipo and control)
#'
#'
#' @return a buy-and-hold return for each ipo firm and each of the
#'  control group firms for this ipo firm for three horizons

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
              duration = duration[2], .groups = "drop")

  bhar_df = bhar_df %>%
    mutate(dur_diff = target_duration / duration - 1) %>%
    filter(dur_diff <= duration_tolerance) %>%
    select(-duration, -dur_diff) %>%
    group_by(id) %>%
    summarise(across(contains("bhar"), mean), .groups = "drop") %>%
    mutate(bhar_abnormal = bhar_ipo - bhar_control)


  return(bhar_df)



}

#' This function calculates abnormal returns from
#' price df (matched ipo and control)
#'
#'
#' @return an abnormal return for each ipo firm and each of the
#'  control group firms for this ipo firm for three horizons

calculate_car_from_price_df = function(price_df){


  ret_df = price_df %>%
    group_by(id, id_control) %>%
    arrange(date) %>%
    mutate(across(c("price_ipo", "price_control"),
                  ~ . / dplyr::lag(.,1) - 1)) %>%
    ungroup() %>%
    filter(complete.cases(.)) %>%
    rename_with(~str_replace_all(.,"price","ret")) %>%
    mutate(ret_abnormal = ret_ipo - ret_control)

  cumret_df = ret_df %>%
    group_by(id, id_control) %>%
    arrange(date) %>%
    mutate(across(contains("ret"),cumsum)) %>%
    ungroup() %>%
    rename_with(~str_replace_all(.,"ret","cum_ret"))

  cumret_df = cumret_df %>%
    group_by(date, id) %>%
    summarise(across(contains("cum_ret"),mean), .groups = "drop")

  return(cumret_df)


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
    mutate(date = (date %/% 21) + 1) %>%
    ungroup()

  monthly_df = monthly_df %>%
    bind_rows(first_day_df) %>%
    arrange(id,id_control, date)

  monthly_df = monthly_df %>%
    group_by(date, id, id_control) %>%
    summarise(across(everything(), mean), .groups = "drop")

  return(monthly_df)

}


#' This function calculates skewness adjusted t statistics
#'
t_skew_adjusted = function(ar_vec, n_comps){

  mean_ar = mean(ar_vec)

  sd_ar = sd(ar_vec)

  S = mean_ar / sd_ar

  gamma_hat = sum((ar_vec - mean_ar) ^ 3) / (n_comps * sd_ar ^ 3)

  t_sa = sqrt(n_comps) * (S + (gamma_hat * S^2) / 3 + gamma_hat / (6 * n_comps))

  return(t_sa)



}

#' This function calculates t statistic
#'
t_statistic = function(ar_vec, n_comps){

mean_ar = mean(ar_vec)

sd_ar = sd(ar_vec)

se = sd_ar / sqrt(n_comps)

t_stat = mean_ar / se

return(t_stat)



}

