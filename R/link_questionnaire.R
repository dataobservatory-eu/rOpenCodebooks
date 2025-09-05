#' Cross-check questionnaire and responses
#'
#' Ensures variables in responses match those defined in the questionnaire.
#'
#' @param responses A dataset_df with responses.
#' @param questionnaire A dataset_df with questionnaire definitions.
#'
#' @return TRUE if valid, otherwise a list of mismatches.
#' @export
link_questionnaire <- function(responses, questionnaire) {
  q_vars <- questionnaire$variable
  issues <- setdiff(names(responses), q_vars)
  if (length(issues)) {
    return(list(extra_vars = issues))
  }
  TRUE
}

