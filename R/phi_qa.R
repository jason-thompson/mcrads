#' QA for PHI/Tableau ready standards using R.
#'
#' @description
#' This functions seeks to ensure that the data being assessed meet the detailed PHI
#' Tableau Ready standards detailed on our network drive:
#' "//phshare01/epe_share/WORK/PHI Visualizations/Tableau Ready Output Format_v2.xlsx"
#'
#' @details
#' This function ensures that the structure of the data matches all PHI Tableau Ready
#' specifications. QA for data quality vis-à-vis previous production data, CHAT, or any other source,
#' must be performed separately
#'
#'
#' @param phi_est Name of a data.table or data.frame containing the prepared data to be pushed to SQL
#' @param phi_meta Name of a data.table or data.frame containing the metadata to be pushed to SQL
#' @param acs Logical. Indicates whether it is ACS data (which does not have / need varnames)
#' @param ignore_trends Logical. Indicates whether the time_trends column should be ignored when checking for missing data.
#' @param verbose Logical. Should the function be talkative?
#'
#' @return If there are no problems, a printed statement of success. Otherwise, it will stop and provide informative
#' feedback every time there is an error.
#'
#' @export
#'
#' @keywords PHI, Tableau, Production
#'
#' @importFrom data.table is.data.table ':=' setDT setDF data.table setorder copy setnames setorder dcast setcolorder fread shift "%between%"
#' @importFrom glue glue
#' @importFrom utils write.table
#' @importFrom yaml yaml.load
#'
#' @examples
#'
#' \dontrun{
#' # create sample data
#'
#' # run function
#' }

# phi_qa function ----

phi_qa <- function(phi_est = NULL, phi_meta = NULL, acs = F, ignore_trends = T, verbose = FALSE){

  #global variables used by data.table declared as NULL here to play nice with devtools::check()
    indicator_key <- result_type <- result <- upper_bound <- lower_bound <- rse <- caution <- tab <- suppression <- time_trends <- NULL

  # Check that both the results and the metadata were provided ----
    if(is.null(phi_est)){
      stop("You must provide the name of a data.frame or data.table that contains the PHI results (e.g., phi_est = phi_2018")
    }

    if(is.null(phi_meta)){
      stop("You must provide the name of a data.frame or data.table that contains the PHI metadata (e.g., phi_meta = phi_meta_2018")
    }

    phi_est <- data.table::setDT(copy(phi_est))
    phi_meta <- data.table::setDT(copy(phi_meta))

  ## Load reference YAML ----
    url <- "https://raw.githubusercontent.com/jason-thompson/mcrads/main/ref/phi_qa.yaml"
    response <- httr::GET(url)
    yaml_content <- httr::content(response, "text")
    phi.yaml <- yaml::yaml.load(yaml_content)


  ## Check columns ----
      # Confirm that all column names are unique ----
          if(length(names(phi_est)) != length(unique(names(phi_est)))) {
            stop("You submitted a dataset where at least two columns have the same name.
                 All names in phi_est must be unique.")
          }

          if(length(names(phi_meta)) != length(unique(names(phi_meta)))) {
            stop("You submitted a metadata table where at least two columns have the same name.
                       All names in phi_meta must be unique.")
          }

      # Confirm all necessary columns exist ----
          missing.var <- setdiff(names(phi.yaml$vars), names(phi_est))
          if(length(missing.var) > 0){
            missing.var <- paste(missing.var, collapse = ", ")
            stop(glue::glue("You are missing the following critical columns(s) in phi_est: {missing.var}"))
          }

          missing.var <- setdiff(names(phi.yaml$metadata), names(phi_meta))
          if(length(missing.var) > 0){
            missing.var <- paste(missing.var, collapse = ", ")
            stop(glue::glue("You are missing the following critical columns(s) in phi_meta: {missing.var}"))
          }

      # Confirm that there are no additional variables ----
          extra.var <- setdiff(names(phi_est), names(phi.yaml$vars))
          if(length(extra.var) > 0){
            extra.var <- paste(extra.var, collapse = ", ")
            stop(glue::glue("Your dataset contains the following columns that are not PHI compliant: {extra.var}.
                            Please drop these variables from phi_est before attempting to QA the data again."))
          }

          extra.var <- setdiff(names(phi_meta), names(phi.yaml$meta))
          if(length(extra.var) > 0){
            extra.var <- paste(extra.var, collapse = ", ")
            stop(glue::glue("Your metadata table contains the following columns that are not PHI compliant: {extra.var}.
                            Please drop these variables from phi_meta before attempting to QA the data again."))
          }

  ## Confirm that variables are of the proper class ----
          if(verbose) message("Validating PHI estimates: ")
          validate_yaml_data(DF = phi_est, YML = phi.yaml, VARS = "vars") # check PHI estimate table

          if(verbose) message(paste("", "Validating PHI metadata: ", sep = "\n"))
          validate_yaml_data(DF = phi_meta, YML = phi.yaml, VARS = "metadata") # check PHI metadata table

          if(verbose) message(paste("", "", sep = "\n"))

  ## Confirm that critical columns are not missing any values ----
          for(mycol in c("indicator_key", "year", "data_source", "tab", "cat1", "cat1_group", "run_date")){
            if(nrow(phi_est[is.na(get(mycol))]) > 0){stop(paste0("'", mycol, "' is missing in at least one row but is a critical identifier column in PHI data. \n",
                                                            "Fix the error and run this QA script again."))}
          }

          for(mycol in c("result", "lower_bound", "upper_bound", "se", "rse", "numerator", "denominator", "phi", "source_date", "run_date", "comparison_with_mc")){
            if(nrow(phi_est[is.na(get(mycol)) & is.na(suppression)]) > 0){message(paste0("\U00026A0 Warning: '", mycol, "' is missing in at least one row of the PHI data."))}
          }

          for(mycol in setdiff(names(phi.yaml$metadata), c("latest_year_mc_pop", "latest_year_count"))){
            if(nrow(phi_meta[is.na(get(mycol))]) > 0){stop(paste0("'", mycol, "' is missing in at least one row but is a critical identifier column in PHI metadata. \n",
                                                                 "Fix the error and run this QA script again."))}
          }

          for(mycol in c("latest_year_mc_pop", "latest_year_count")){
            if(nrow(phi_meta[is.na(get(mycol))]) > 0){message(paste0("\U00026A0 Warning: '", mycol, "' is missing in at least one row of the metadata."))}
          }

  ## Set the columns in standard order ----
        setcolorder(phi_est, names(phi.yaml$vars))
        setcolorder(phi_meta, names(phi.yaml$meta))

  ## Basic logic checks for estimates ----
      # Check for infinite values, which cannot be pushed to SQL ----
        for(var in c("result", "lower_bound", "upper_bound", "se", "rse",
                     "numerator", "denominator")){
          if(nrow(phi_est[is.infinite(get(var))]) > 0 ){
            stop(glue::glue("There is at least one row where is.infinite({var}) == T.
                     Please fix this problem before rerunning phi_qa() (e.g., by setting it equal to NA)
                     You can view the problematic data by typing something like: View(phi_est[is.infinite({var}), ])"))
          }
        }

      # proportions should always be between zero and one ----
        phi_est <- merge(phi_est, phi_meta[, list(indicator_key, result_type)], by = "indicator_key", all.x = TRUE, all.y = FALSE) # merge on result_type
        if(nrow(phi_est[result_type=="proportion" & !result %between% c(0, 1)]) > 0){
          stop("There is at least one row where where the metadata states that the indicator is a proportion but the result is outside [0,1].
               Please fix either the metadata table or the PHI estimates and try again.")
        }

      # upper_bound should be greater than lower_bound ----
        if(nrow(phi_est[upper_bound < lower_bound, ])){
          stop("There is at least one row where the upper_bound is less than the lower_bound.
               Please fix this error prior to re-running the phi_qa() function.
               You can view the problematic data by typing something like: View(phi_est[upper_bound < lower_bound, ])")
        }

      # result should be less than or equal to the upper bound ----
        if(nrow(phi_est[!(result <= upper_bound)])){
          stop("There is at least one row where the result is not less than or equal to the upper_bound.
               Please fix this error prior to rerunning the phi_qa() function.
               You can view the problematic data by typing something like: View(phi_est[!(result <= upper_bound)])")
        }

      # result should be greater than or equal to the lower_bound ----
        if(nrow(phi_est[!(result >= lower_bound)])){
          stop("There is at least one row where the result is not greater than or equal to the lower_bound.
           Please fix this error prior to rerunning the phi_qa() function.
           You can view the problematic data by typing something like: View(phi_est[!(result >= lower_bound)])")
        }

      # lower_bound should never be less than zero ----
        if(nrow(phi_est[lower_bound < 0])){
          stop("There is at least one row where the lower_bound is less than zero (i.e., it is negative).
           Please fix this error prior to rerunning the phi_qa() function.
          You can view the problematic data by typing something like: View(phi_est[lower_bound < 0])")
        }

      # RSE should always be between 0 and 100 ----
          # confirmed with Abby 2/7/2020 that want RSE * 100
          if(nrow(phi_est[!rse %between% c(0, 100)]) > 0 ){
            message(paste("\U00026A0 Warning: There is at least one row where the RSE (relative standard error) is outside the range of (0, 100].",
                 "This is not necessarily an error, but you should examine the data to make sure it makes sense.",
                 "You can view the data in question by typing something like: View(phi_est[!rse %between% c(0, 100)])", sep = "\n"))
          }

      # RSE should be on scale of 0-100 (i.e., the proportion should have been multiplied by 100) ----
          if(nrow(phi_est[!is.na(rse)]) == nrow(phi_est[rse <=1])){
            stop("All RSEs are within the range (0, 1]. PHI Tableau Ready standards necessitate that these proportions
               be mutliplied by 100. I.e., .12345 >> 12.345
               Please fix this error prior to rerunning the phi_qa() function.")
          }

      # Caution flag should be toggled if RSE >= 30% ----
          if(nrow(phi_est[rse>=30 & (caution != "!" | is.na(caution)) ]) > 0 ){
            stop("There is at least one row where a caution flag ('!') is not used and rse >= 30% or is.na(rse) == T.
                 Please fix this error prior to rerunning the phi_qa() function.
                 You can view the problematic data by typing something like: View(phi_est[(rse>=30 | is.na(rse)) & (caution != '!' | is.na(caution))])")
          }

  ## Ensure proper rounding ----
      # result should be to three digits ----
          if(sum(phi_est$result != round2(phi_est$result, 3), na.rm = T) != 0) {
            stop("The 'result' column does not appear to be rounded to 3 digits, as specified in the PHI standards")
          }

      # lower_bound should be to three digits ----
          if(sum(phi_est$lower_bound != round2(phi_est$lower_bound, 3), na.rm = T) != 0) {
            stop("The 'lower_bound' column does not appear to be rounded to 3 digits, as specified in the PHI standards")
          }

      # upper_bound should be to three digits ----
          if(sum(phi_est$upper_bound != round2(phi_est$upper_bound, 3), na.rm = T) != 0) {
            stop("The 'upper_bound' column does not appear to be rounded to 3 digits, as specified in the PHI standards")
          }

      # rse should be to three digits ----
          if(sum(phi_est$rse != round2(phi_est$rse, 3), na.rm = T) != 0) {
            stop("The 'rse' column does not appear to be rounded to 3 digits, as specified in the PHI standards")
          }

      # se should be rounded to four digits ----
          if(sum(phi_est$se != round2(phi_est$se, 4), na.rm = T) != 0) {
            stop("The 'se' column does not appear to be rounded to 3 digits, as specified in the PHI standards")
          }

  ## Check that core identification variables are all present ----
      for(var in c("indicator_key", "tab", "year", "cat1", "cat1_group",
                   "source_date", "run_date")){
        if(nrow(phi_est[is.na(get(var))]) > 0 ){
          stop(glue::glue("There is at least one row where '{var}' is missing.
                          Please fill in the missing value before rerunning phi_qa()"))
        }
      }

      if(acs==F){
        if(nrow(phi_est[is.na("cat1_varname")]) > 0 ){
          stop(glue::glue("There is at least one row where 'cat1_varname' is missing.
                        Please fill in the missing value before rerunning phi_qa()"))
        }
      }

  ## Check that crosstab identification variables are all present ----
      for(var in c("cat2", "cat2_group")){
        if(nrow(phi_est[tab=="crosstabs" & is.na(get(var))]) > 0 ){
          stop(glue::glue("There is at least one row where tab=='crosstabs' & where '{var}' is missing.
                          Please fill in the missing value before rerunning phi_qa()"))
        }
      }

      if(acs==F){
        if(nrow(phi_est[tab=="crosstabs" & is.na("cat2_varname")]) > 0 ){
          stop(glue::glue("There is at least one row where 'cat2_varname' is missing.
                      Please fill in the missing value before rerunning phi_qa()"))
        }
      }

  ## Check that results are always present if row is not suppressed ----
      for(var in c("result", "lower_bound", "upper_bound", "se", "rse", "numerator", "denominator", "comparison_with_mc")){
        if(nrow(phi_est[suppression != "^" & is.na(get(var))]) > 0 ){
          stop(glue::glue("There is at least one row that is not suppressed & where '{var}' is missing.
                          Please fill in the missing value before rerunning phi_qa()"))
        }
      }

  ## Check that time_trends are always provided when tab=="trends" ----
        if(ignore_trends == F){
          if(nrow(phi_est[tab == "trends" & is.na(time_trends)]) > 0 ){
            stop(glue::glue("There is at least one row where tab=='trends' & where 'time_trends' is missing.
                            Please fill in the missing value before rerunning phi_qa()"))}
        }

  ## Compare with previous year's results (FOR FUTURE???)----
      # in function arguments, have user submit most recent and comparison year(s). Submit as character b/c can be 2013-2017, not just 2017
      # if both are null, skip the comparison
      # it not submitted, merge newer data on old data and identify rows with > 3% absolute difference
      # save this dataset for manual review by the user
  ## Compare with a CSV (FOR FUTURE) ----
      # sometimes want to compare with external data source ---
      # must identify year of interest as above
      # instead of submitting a reference year, the user specifies a reference file
      # reference file will attempt to match on all columns that have the same name, except those with results (i.e., results, lower_bound, se, etc.)
      # actual comparison code should be the same as when comparing to a previous year, so write a small funcion to do this

  ## Print success statement!!!!!!!! ####
    if(verbose) message(paste("\n\U0001f389Congratulations!\U0001f973\n",
          "Your data has passed all PHI Tableau Ready formatting, style, and logic checks.\n",
          "However, you are encouraged to read any warnings and consider whether you need to revise the data."))

} # close function

