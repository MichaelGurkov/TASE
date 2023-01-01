
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
    filter(complete.cases(.))

  return(ret_df)


}


#' This function calculates cumulative return
#'
calculate_cum_return = function(ret_df){

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

  avg_adj_ret_df = adj_ret_df %>%
    group_by(month, adjustment_type) %>%
    summarise(value = mean(value, na.rm = TRUE),
              num_comps = length(id), .groups = "drop")


  cum_ret_df = avg_adj_ret_df  %>%
    filter(month <= 120)  %>%
    mutate(adjustment_type = factor(
      adjustment_type,
      labels = c("Raw returns",
                 "Abnormal (index adjusted) returns",
                 "Control group returns")
    )) %>%
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

 hold_ret =  hold_ret %>%
   group_by(month) %>%
   summarise(across(ends_with("ret"), ~mean(., na.rm = TRUE)),
             num_comps = length(id),
             .groups = "drop") %>%
   mutate(hold_ret = close_ret - control_ret) %>%
   select(month, hold_ret, num_comps)


  return(hold_ret)

}


