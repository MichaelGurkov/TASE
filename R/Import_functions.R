#' @title Import BoI format financial report data
#'
#' @description  This function imports financial report data from BOI format
#'
#' @param filepath the path to financial report data (in csv format)
#'
#' @import dplyr

import.boi.finrep.data = function(filepath = NULL){

  if(is.null(filepath)){filepath = paste0(file.path(Sys.getenv("USERPROFILE"),
                                                    "Documents",fsep="\\"),
                                          "\\Data\\BOI\\FinancialReports.csv")}

  temp_df = read.csv(filepath, encoding = "UTF-8", header = FALSE,
                     stringsAsFactors = FALSE)

  names_vec = c("Year","Date","TASE_branch","TASE_ID","Entity_Name",
                "Entity_ID","Total_Assets","Current_Assets","Cash_Equivalent",
                "Non_Current_Assets","Total_Liabilities","Current_Liabilities",
                "Non_current_Liabilities","Equity","Minority_rights","Revenue",
                "Total_Cost","Operating_Profit", "Financing_cost",
                "Profit_before_tax","Tax","Net_Profit","Minority_Profit",
                "Operating_CashFlow",
                "Capex_CashFlow","Financial_CashFlow")

  df = temp_df %>%
    slice(-1) %>%
    setNames(names_vec) %>%
    mutate(Date = as.yearqtr(Date, format = "Q%q/%Y")) %>%
    mutate(TASE_branch = factor(TASE_branch))

  levels(df$TASE_branch) = list("Biomed" = "ביומד",
                                "Insurance" = "ביטוח",
                                "Banks" = "בנקים",
                                "Investment_Holdings" = "השקעה ואחזקות",
                                "Technology" = "טכנולוגיה",
                                "Services" = "מסחר ושרותים",
                                "Real_estate" = "נדל\"ן ובינוי",
                                "Fin_services" = "שרותים פיננסיים",
                                "Industry" = "תעשיה")


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

import.TASE.comps_status = function(filepath = NULL){

  if(is.null(filepath)){filepath = paste0(file.path(Sys.getenv("USERPROFILE"),"Documents",
                                                    fsep="\\"),
                                          "\\Data\\TASE\\TASE_Liquidity\\",
                                          "Trading_Companies_Status.xlsx")}

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
    file.path(Sys.getenv("USERPROFILE"),
              fsep="\\"),
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


#' Import augmented (with data from market and finrep df) data frame
#'
#' @description  This wrapper function takes the old data (collected by Nimrod)
#' and augments it (by coalsce new data first) with market df and finrep df
#'
#' @import dplyr

import.augmented.data = function(){

  if(file.exists(paste0(file.path(Sys.getenv("USERPROFILE"),fsep="\\"),
                        "\\OneDrive - Bank Of Israel\\Data\\",
                        "TASE liquidity\\market_df.RDS"))){

    market_df = read_rds(paste0(file.path(Sys.getenv("USERPROFILE"),fsep="\\"),
                                "\\OneDrive - Bank Of Israel\\Data\\",
                                "TASE liquidity\\market_df.RDS"))



    } else {

    market_df = read_rds(paste0(file.path(Sys.getenv("USERPROFILE"),
                                          "Documents",fsep="\\"),
                                "\\Data\\TASE\\Market Data",
                                "\\Market_data_stocks_2001-2019.Rds")) %>%
      get.market.variables()

  }


  if(file.exists(paste0(file.path(Sys.getenv("USERPROFILE"),fsep="\\"),
                        "\\OneDrive - Bank Of Israel\\Data\\",
                        "TASE liquidity\\finrep_df.RDS"))){

    finrep_df = read_rds(paste0(file.path(Sys.getenv("USERPROFILE"),fsep="\\"),
                                "\\OneDrive - Bank Of Israel\\Data\\",
                                "TASE liquidity\\finrep_df.RDS"))

  } else {

    finrep_df = import.boi.finrep.data() %>%
      mutate_if(((!names(.) %in% c("Date","TASE_branch","TASE_ID")) & !is.numeric(.)),
                as.numeric) %>%
      select(Date, TASE_ID, TASE_branch, Operating_Profit,Net_Profit,
             Equity,Revenue,Total_Assets, Total_Liabilities, Capex_CashFlow,
             Operating_CashFlow) %>%
      mutate(Capex_to_revenue = Capex_CashFlow / Revenue) %>%
      mutate(ROA = Operating_Profit / Total_Assets) %>%
      mutate(Free_CashFlow = Operating_CashFlow / Total_Assets)


  }

  df = full_join(finrep_df, market_df, by = c("Date" = "Date",
                                              "TASE_ID" = "Comp_ID"))

  df = df %>%
    mutate(MB = Market_Cap / Equity) %>%
    mutate(Market_Cap_log = log(Market_Cap)) %>%
    mutate(Turnover_log = log(Turnover)) %>%
    mutate(Leverage = Total_Liabilities / Total_Assets)


  old_df = import.old.regression.data() %>%
    mutate(TASE_branch = as.factor(TASE_branch))

  merged_df = full_join(df,old_df[,names(old_df)[names(old_df) %in% names(df)]],
                        by = c("Date","TASE_ID"),
                        suffix = c("_new","_old"))

  final_df = merged_df %>%
    mutate(TASE_branch_new = as.character(TASE_branch_new)) %>%
    mutate(TASE_branch_old = as.character(TASE_branch_old))

  for(varname in grep("_new$",names(final_df),value = TRUE)){

    main_var = sym(varname)

    backup_var = sym(str_replace(varname,"new","old"))

    final_df = final_df %>%
      mutate(!!varname := coalesce(!!main_var,!!backup_var))

    rm(main_var, backup_var)

  }

  final_df = final_df %>%
    select(-ends_with("_old")) %>%
    rename_at(.vars = vars(ends_with("_new")),
              .funs = list(~str_remove(.,"_new"))) %>%
    mutate_if(is.numeric,.funs = ~ifelse(abs(.) == Inf,NA,.))

  return(final_df)



}
