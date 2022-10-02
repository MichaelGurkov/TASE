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


import.boi.market.data = function(filepath = NULL){

  if(is.null(filepath)){filepath = paste0(file.path(
    Sys.getenv("USERPROFILE"),fsep="\\"),
    "\\OneDrive - Bank Of Israel\\Data\\",
    "TASE liquidity\\Rdata files\\stocks_full_data.rds")}

  temp_df = read_rds(filepath)


  df = temp_df %>%
    rename_all(tolower) %>%
    rename(tase_id = tase_issuer_id,
           sec_id = security_ident_num_tase,
           close = close_rate_adj,
           date = date_value) %>%
    mutate(date = ymd(ymd_hms(date))) %>%
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

import_TASE_comps_status = function(filepath = NULL){

  if(is.null(filepath)){
    filepath = paste0(Sys.getenv("USERPROFILE"),
                      "\\OneDrive - Bank Of Israel",
                      "\\Data\\TASE liquidity",
                      "\\Trading_Companies_Status.xlsx")}

  sheets_names = excel_sheets(filepath)

  sheet_list = lapply(sheets_names,function(temp_name){

    temp_sheet = read_xlsx(filepath,sheet = temp_name) %>%
      mutate(Year = temp_name) %>%
      rename(Start_Year = Start, End_Year = End, Issued_Comps = Issued,
             Delisted_Comps = Delisted) %>%
      select(Year, Sector, everything())

  })

  status_df = bind_rows(sheet_list)

  return(status_df)

}


#' @title Import BoI format financial report data
#'
#' @description  This function imports financial report data from BOI format
#'
#' @param filepath the path to financial report data (in csv format)
#'
#' @import dplyr

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
    "TASE liquidity\\convert_names_table.csv")) %>%
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


#' @title Import Orcale format financial report data
#'
#' @description  This function imports financial report data from Oracle format
#'
#' @param filepath the path to financial report data (in csv format)
#'
#' @import dplyr
#'
#' @import zoo
#'
#' @import stringr
#'
#' @export

import.boi.oracle.finrep.data = function(filepath = NULL,
                                         data_frequency){

  if(missing(data_frequency)){stop("data frequency argument is required")}


  if(is.null(filepath)){filepath = paste0(file.path(
    Sys.getenv("USERPROFILE"),fsep="\\"),
    "\\OneDrive - Bank Of Israel\\Data\\",
    "TASE liquidity\\Rdata files\\finrep_oracle_data.rds")}

  temp_df = read_rds(filepath)

  df = temp_df %>%
    rename_all(tolower) %>%
    rename_all(~str_replace_all(.,"__","_"))

  df = df %>%
    mutate(fsd_period = nchar(fsd_period)) %>%
    mutate(fsd_period = recode(fsd_period,
                               `4` = "annual",
                               `6` = "quarterly",
                               `8` = "semiannual")) %>%
    filter(fsd_period == data_frequency)

  df = df  %>%
    mutate(date_yearqtr = as.yearqtr(date_fsd,
                                     format = "%Y%q"))%>%
    rename(total_assets = total_balance,
           tase_id = tase_issuer_id)

  df = df %>%
    mutate(across(!c("tase_id","date_yearqtr") & where(~!is.numeric(.)),
                  as.numeric))

  return(df)

}

