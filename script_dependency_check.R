rm(list = ls())

# Output is a data frame with line numbers, line text and function used in the package
get_lib_depend <- function(script_path,package_name){
  
  # Read in the script file
  script_lines <- readLines(script_path)
  
  # Check if package is installed - if not installed, install it and load it
  if (!(package_name %in% installed.packages())){
    install.packages(package_name)
  }
  invisible(eval(parse(text =  paste0("library(",package_name,")"))))
  
  # Get list of functions within that package
  func_list <- eval(parse(text = paste0("lsf.str(\"package:",package_name,"\")")))
  df_list <- list()
  
  # String search for function usage
  get_func_depend <- function(func_name){
    logic_vec <- grepl(paste0(func_name,"\\("),script_lines)
    line_nums <- (1:length(script_lines))[logic_vec]
    line_text <- script_lines[logic_vec]
    if (length(line_nums)>0){
      dependency_df <- cbind.data.frame(line_nums,line_text,stringsAsFactors = F)
      dependency_df$fn_name <- func_name
    } else {
      dependency_df <- data.frame()
    }
    return(dependency_df)
  }
  
  df_list <- lapply(func_list, get_func_depend)
  
  final_df <- do.call(rbind.data.frame,df_list)
  rownames(final_df) <- NULL
  if (nrow(final_df) > 0){
    final_df$package_name <- package_name
    final_df <- final_df[,c("package_name","fn_name","line_nums","line_text")]
  } else {
    final_df <- data.frame()
  }
  return(final_df)
}

# Unit test

script_path <- "E://Unilever Growth Driver CCBT Analysis (18-AIM-2817)/Ankit B/v2_testing/Functions_BSTS.R"
package_names <- c("dplyr", "reshape", "tidyr", "openxlsx", "xlsx", "tools", "readxl", "readr", "stringr",
                    "gtools", "igraph", "tibble", "combinat", "data.table", "forecast", "zoo", "smooth",
                    "janitor", "Metrics", "lubridate", "bsts", "caret", "xgboost",
                    "mpmi", "roxygen2", "devtools", "tidyverse", "rstudioapi", "filesstrings", "foreach", "doParallel",
                    "fBasics","reshape2", "gridExtra", "grid","pdftools")
output_path <- dirname(script_path)
dependency_list <- list()
for (package_name in package_names){
  dependency_df_temp <- get_lib_depend(script_path,package_name)
  dependency_list[[package_name]] <- dependency_df_temp
  rm(dependency_df_temp)
}

overall_dependency <- do.call(rbind.data.frame,dependency_list)
rownames(overall_dependency) <- NULL


package_name_df <- cbind.data.frame("package_name" = package_names,"loaded" = c(T))
overall_dependency <- merge(overall_dependency,package_name_df,by = "package_name",all.y = T)

write.csv(overall_dependency,file = file.path(output_path,"dependency_check.csv"),row.names = F)
