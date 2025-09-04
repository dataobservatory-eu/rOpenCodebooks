#' Attach metadata to a variable in a dataset
#'
#' @param responses A dataset_df with responses.
#' @param variable The name of the variable to annotate.
#' @param variable_uri A URI for the variable.
#' @param code_list_uri Optional URI of the associated code list.
#'
#' @return The modified dataset_df.
#' @export
link_variable <- function(responses,
                          variable,
                          variable_uri,
                          code_list_uri = NULL) {
  stopifnot(variable %in% names(responses))
  attr(responses[[variable]], "variable_uri") <- variable_uri
  if (!is.null(code_list_uri)) {
    attr(responses[[variable]], "code_list_uri") <- code_list_uri
  }
  responses
}
