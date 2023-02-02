#' This function matches



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
