
#' This function takes returns df and mathed table and calculates
#' benchmark performance
#'
#'
#'

calculate_benchmark_performance = function(returns_df, matched_sample,
                                           target_duration){

  ipo_returns_df = returns_df %>%
    inner_join(select(matched_sample, "id_ipo"), by = c("id" = "id_ipo")) %>%
    group_by(id) %>%
    mutate(dur_diff = abs(duration - target_duration)) %>%
    filter(dur_diff == min(dur_diff)) %>%
    ungroup()

  benchmark_performance_df = matched_sample %>%
    left_join(ipo_returns_df, by = c("id_ipo" = "id")) %>%
    left_join(returns_df, by = c("id_control" = "id"),
              suffix = c("_ipo","_control"))

  benchmark_performance_df = benchmark_performance_df %>%
    mutate(duration_diff = abs(duration_ipo - duration_control)) %>%
    select(-c("duration_ipo","duration_control")) %>%
    group_by(id_control) %>%
    filter(duration_diff == min(duration_diff)) %>%
    ungroup() %>%
    mutate(wealth_relative = (1 + bhar_ipo) / (1 + bhar_control)) %>%
    select(contains("id"), contains("bhar"), wealth_relative, duration_diff)

  return(benchmark_performance_df)

}
