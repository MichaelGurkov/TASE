#' @title Import BoI format financial report data
#'
#' @description  This function imports financial report data from BOI format
#'
#' @param filepath the path to financial report data (in csv format)
#'
#' @import dplyr

import.boi.finrep.data = function(filepath = NULL){

  if(is.null(filepath)){filepath = paste0(file.path(Sys.getenv("USERPROFILE"),"Documents",
                                                    fsep="\\"),
                                          "\\Data\\BOI\\FinancialReports.csv")}

  temp_df = read.csv(filepath, encoding = "UTF-8", header = FALSE, stringsAsFactors = FALSE)

  names_vec = c("Year","Date","TASE_branch","TASE_ID","Entity_Name",
                "Entity_ID","Total_Assets","Current_Assets","Cash_Equivalent",
                "Non_Current_Assets","Total_Liabilities","Current_Liabilities",
                "Non_current_Liabilities","Equity","Minority_rights","Revenue",
                "Total_Cost","Operating_Profit", "Financing_cost","Profit_before_tax",
                "Tax","Net_Profit","Minority_Profit", "Operating_CashFlow",
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


