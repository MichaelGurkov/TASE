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

  temp_df = read.csv("C:\\Users\\internet\\Documents\\Data\\BOI\\FinancialReports.csv",
                     encoding = "UTF-8", header = FALSE, stringsAsFactors = FALSE)

  names_vec = c("Year","Date","TASE_branch","TASE_ID","Entity_Name",
                "Entity_ID","Total_Assets","Current_Assets","Cash_Equivalent",
                "Non_Current_Assets","Total_Liabilities","Current_Liabilities",
                "Non_current_Liabilities","Equity","Minority_rights","Revenue",
                "Total_Cost","Operational_Profit", "Financing_cost","Profit_before_tax",
                "Tax","Net_Profit","Minority_Profit", "Operational_CashFlow",
                "Capex_CashFlow","Financial_CashFlow")

  df = temp_df %>%
    slice(-1) %>%
    setNames(names_vec) %>%
    mutate(Date = as.yearqtr(Date, format = "Q%q/%Y"))

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


