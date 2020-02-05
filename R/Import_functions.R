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
                "Entity_ID","Total_Assets","Current","Cash","Non_current_Property",
                "Total_Liabilities","Current_Liabilities","Non_current_Liabilities",
                "Equity","Minority_rights","Revenue","Total_Cost","Operational_Profit",
                "Cap_expences","Profit_before_tax","Tax","Net_Profit","Minority_Profit",
                "Current_CashFlow","Investment_CashFlow","Finance_CashFlow")
  
  df = temp_df %>% 
    slice(-1) %>% 
    setNames(names_vec) %>% 
    mutate(Date = as.yearqtr(Date, format = "Q%q/%Y"))
  
  return(df)
  
  
  
}



