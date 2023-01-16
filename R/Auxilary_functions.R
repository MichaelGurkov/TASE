
#' This function calculates raw and adjusted return
#'
calculate_return_df = function(price_df){

  ret_df = price_df %>%
    group_by(id) %>%
    arrange(month) %>%
    mutate(across(c("close","index", "control"),
                  ~ . /lag(.) - 1,
                  .names = "{.col}_ret")) %>%
    ungroup() %>%
    select(-c("close","index","control")) %>%
    filter(!is.na(close_ret))

  return(ret_df)


}

#' This function adjustes (removes) for benchmark
#'
adjust_returns_to_benchmark = function(ret_df){

  adj_ret_df = ret_df %>%
    pivot_longer(c("index_ret","control_ret"),
                 names_to = "benchmark",
                 values_to = "benchmark_ret") %>%
    filter(complete.cases(.)) %>%
    mutate(adjusted_ret = close_ret - benchmark_ret) %>%
    select(-benchmark_ret) %>%
    pivot_wider(names_from = "benchmark",
                values_from = "adjusted_ret") %>%
    pivot_longer(-c(id,month),names_to = "adjustment_type")

  adj_ret_df = adj_ret_df %>%
    mutate(adjustment_type = factor(
      adjustment_type,
      levels = c("close_ret", "index_ret", "control_ret"),
      labels = c(
        "Raw returns",
        "Index adjusted returns",
        "Control adjusted returns"
      )
    ))

  return(adj_ret_df)



}

#' This function calculates cumulative return
#'
calculate_cum_return = function(ret_df){

  adj_ret_df = adjust_returns_to_benchmark(ret_df)

  avg_adj_ret_df = adj_ret_df %>%
    group_by(month, adjustment_type) %>%
    summarise(value = mean(value, na.rm = TRUE),
              num_comps = length(id), .groups = "drop")


  cum_ret_df = avg_adj_ret_df  %>%
    filter(month <= 120) %>%
    filter(complete.cases(.)) %>%
    group_by(adjustment_type) %>%
    arrange(month) %>%
    mutate(value = cumsum(value)) %>%
    ungroup()

  return(cum_ret_df)

}


#' This function calculates holding return
#'
calculate_holding_return = function(ret_df){

  hold_ret = ret_df %>%
    group_by(id) %>%
    arrange(month) %>%
    mutate(across(ends_with("ret"),~ cumprod(1 + .) - 1)) %>%
    ungroup()

  adj_hold_ret_df = adjust_returns_to_benchmark(hold_ret)

  avg_adj_hold_ret_df = adj_hold_ret_df %>%
    filter(month <= 120) %>%
    group_by(month, adjustment_type) %>%
    summarise(value = mean(value, na.rm = TRUE),
              num_comps = length(id), .groups = "drop")



  return(avg_adj_hold_ret_df)

}


#' This function calculates holding return
#'
calculate_holding_return_by_indicator = function(ret_df, ind_df){

  hold_ret = ret_df %>%
    group_by(id) %>%
    arrange(month) %>%
    mutate(across(ends_with("ret"),~ cumprod(1 + .) - 1)) %>%
    ungroup()

  adj_hold_ret_df = adjust_returns_to_benchmark(hold_ret)

  hold_ret_by_indicator = adj_hold_ret_df %>%
    filter(month == 36) %>%
    separate(id,into = c("tase_id","sec_id"), sep = "_") %>%
    left_join(ind_df %>%
                select(tase_id, indicator) %>%
                distinct(), by = "tase_id") %>%
    filter(complete.cases(.)) %>%
    # filter(adjustment_type == "Raw returns") %>%
    group_by(indicator, adjustment_type) %>%
    summarise(value = mean(value), num_comps = length(indicator),
              .groups = "drop")

  return(hold_ret_by_indicator)

}
