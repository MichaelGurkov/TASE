#' @title Match control comps to IPO comps
#'
#' @description This function matches control comps by market value
#'
#' @details The matching is performed on mature firms (\code{age_treshold}
#' parameter)
#'
match_by_target_value = function(ipo_comps, target_df,
                                       age_threshold,
                                       target_threshold = 0.1,
                                       single_match = TRUE){


  ipo_sample = target_df %>%
    inner_join(ipo_comps, by = "id") %>%
    group_by(id) %>%
    summarise(target_value = target_value[date == min(date)],
              date = min(date),
              .groups = "drop") %>%
    filter(complete.cases(.))

  target_sample = target_df %>%
    group_by(id) %>%
    mutate(age = as.numeric(date - min(date))) %>%
    ungroup() %>%
    filter(age >= age_threshold)


  matched_sample = ipo_sample %>%
    left_join(target_sample, by = c("date"),
              suffix = c("_ipo","_control"),
              relationship = "many-to-many") %>%
    filter(complete.cases(.)) %>%
    mutate(matching_diff = abs(target_value_ipo/target_value_control - 1)) %>%
    filter(matching_diff < target_threshold)

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

