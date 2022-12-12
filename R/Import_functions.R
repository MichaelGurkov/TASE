#' @title Import BoI format financial report data
#'
#' @description  This function imports financial report data from BOI format
#'
#' @param filepath the path to financial report data (in csv format)
#'
#' @import dplyr
#'
#' @import xts

import.boi.finrep.data = function(filepath = NULL){

  if(is.null(filepath)){filepath = paste0(file.path(
    Sys.getenv("USERPROFILE"),fsep="\\"),
    "\\OneDrive - Bank Of Israel\\Data\\",
    "TASE liquidity\\FinancialReports.csv")}

  temp_df = read.csv(filepath, encoding = "UTF-8",
                     header = FALSE,
                     stringsAsFactors = FALSE)

  names_vec = c(
    "Year",
    "Date",
    "TASE_branch",
    "TASE_ID",
    "Entity_Name",
    "Entity_ID",
    "Total_Assets",
    "Current_Assets",
    "Cash_Equivalent",
    "Non_Current_Assets",
    "Total_Liabilities",
    "Current_Liabilities",
    "Non_current_Liabilities",
    "Equity",
    "Minority_rights",
    "Revenue",
    "Total_Cost",
    "Operating_Profit",
    "Financing_cost",
    "Profit_before_tax",
    "Tax",
    "Net_Profit",
    "Minority_Profit",
    "Operating_CashFlow",
    "Capex_CashFlow",
    "Financial_CashFlow"
  )

  df = temp_df %>%
    slice(-1) %>%
    setNames(names_vec) %>%
    mutate(Date = as.yearqtr(Date, format = "Q%q/%Y")) %>%
    mutate(TASE_branch = factor(TASE_branch))

  Sys.setlocale(locale = "hebrew")

  levels(df$TASE_branch) = list(
    "Biomed" = "ביומד",
    "Insurance" = "ביטוח",
    "Banks" = "בנקים",
    "Investment_Holdings" = "השקעה ואחזקות",
    "Technology" = "טכנולוגיה",
    "Services" = "מסחר ושרותים",
    "Real_estate" = "נדל\"ן ובינוי",
    "Fin_services" = "שרותים פיננסיים",
    "Industry" = "תעשיה"
  )


  return(df)

}


#' @title Import BoI format market report data
#'
#' @description  This function imports market data from BOI format
#'
#' @param filepath the path to financial report data (in csv format)
#'
#' @import dplyr
#'
#' @import readr
#'
#' @import lubridate
#'
#' @export


import_boi_market_data = function(filepath){


  temp_df = read_rds(filepath)


  df = temp_df %>%
    rename_all(tolower) %>%
    rename(tase_id = tase_issuer_id,
           sec_id = security_ident_num_tase,
           date = date_value) %>%
    mutate(date = as_date(date)) %>%
    mutate(sec_id = as.character(as.numeric(sec_id)))


  df = df %>%
    mutate(across(-c(sec_id,tase_id, date), as.numeric))


  return(df)

}

#' @title Import trading status data
#'
#' @description This function imports data set about listing status of TASE companies
#'
#' @param filepath the path to financial report data (in xlsx format)
#'
#' @import readxl
#'
#' @import dplyr
#'
#' @import xts
#'
#' @export


import.old.regression.data = function(filepath = NULL){

  if(is.null(filepath)){filepath = paste0(
    file.path(Sys.getenv("USERPROFILE"),fsep="\\"),
    "\\OneDrive - Bank Of Israel\\Data",
    "\\TASE liquidity\\quarterly regression updated.csv")}

  temp_df = read.csv(filepath, encoding = "UTF-8", stringsAsFactors = FALSE)

  temp_df = temp_df %>%
    rename(TASE_ID = MSP_HVR,
           TASE_branch = ANAF_HVR,
           Date = DATE,
           Illiq = illq,
           Market_Cap = erh_shuk,
           Turnover = mhzr_shk,
           Public_Share = public_share,
           Total_Assets = sh_necs,
           Operating_Profit = rvh_tefulli,
           Net_Profit = rvh_naki,
           Revenue = sh_hahn,
           Equity = honazmi,
           Leverage = minuf_aroh,
           Total_Liabilities = sh_hth) %>%
    mutate(Date = as.yearqtr(Date, format = "%d/%m/%Y")) %>%
    mutate(TASE_ID = as.character(TASE_ID),
           TASE_branch = as.character(TASE_branch)) %>%
    mutate_at(.vars = vars(Total_Assets, Total_Liabilities,
                           Revenue, Net_Profit, Operating_Profit, Equity),
              .funs = list(~./1000)) %>%
    mutate(Public_Share = Public_Share * 100)

  return(temp_df)

}

#' @title Import Nimrod dataset
#'
#' @description This function imports clean (and filtered) version of
#' Nimrod's stata df
#'
#' @param filepath string a path to stata data file
#'
#' @param vars_names = vector of variables names to return
#'
#' @import haven
#'
#' @import dplyr

import.nimrod.stata.df = function(filepath,
                                  vars_names = NULL){



  raw_df = read_dta(filepath) %>%
    rename_all(tolower)

  names_table = read_csv(paste0(
    file.path(Sys.getenv("USERPROFILE"),fsep = "\\"),
    "\\OneDrive - Bank Of Israel\\Data\\",
    "TASE liquidity\\michael files\\convert_names_table.csv")) %>%
    filter(old_name %in% names(raw_df))

  df = raw_df %>%
    filter(!is.na(newdate)) %>%
    rename_at(vars(names_table$old_name),~names_table$new_name) %>%
    rename(date = newdate) %>%
    mutate(date = as.yearqtr(date)) %>%
    mutate(tase_id = as.character(tase_id)) %>%
    mutate(tase_branch = as_factor(tase_branch))

  df = df %>%
    mutate(across("turnover", ~ . * (10 ^ -6)))


  if(!is.null(vars_names)){

    df = df %>%
      select(any_of(vars_names))


  }

  return(df)


}

#' This function imports comps data that includes tase_id,
#' ipo_date, delisting_date and quotation_period
#'
#' @param ipo_dates_filepath
#' @param delisting_dates_filepath

import_comps_dates_and_status = function(ipo_dates_filepath,
                             delisting_dates_filepath){

  ipo_dates = read_csv(ipo_dates_filepath,
                       show_col_types = FALSE) %>%
    select(tase_id, ipo_date) %>%
    mutate(ipo_date = dmy(ipo_date)) %>%
    arrange(tase_id)


  delisted_dates = read_csv(delisting_dates_filepath,
                            show_col_types = FALSE) %>%
    arrange(tase_id) %>%
    mutate(delisting_date = mdy(delisting_date))


  comps_data = ipo_dates %>%
    full_join(delisted_dates, by = "tase_id") %>%
    mutate(quotation_period = difftime(delisting_date, ipo_date)) %>%
    mutate(quotation_period = time_length(quotation_period,"years"))  %>%
    mutate(across(starts_with("tase_id"), as.character)) %>%
    filter(quotation_period >= 0 | is.na(quotation_period)) %>%
    group_by(tase_id, ipo_date) %>%
    arrange(ipo_date, delisting_date) %>%
    slice(1) %>%
    ungroup() %>%
    filter(year(ipo_date) >= 2005)

  return(comps_data)



}
