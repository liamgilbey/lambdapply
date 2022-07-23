#' Get a lambda config
#'
#' This function will find and return the current lambda config. This avoids us
#' having to explicitly passing the config as an argument to every \code{lambdapply}
#' call.
#'
#' @param env Environment to search for the lambda config
#'
#' @examples
#'
#' config <- lambda_config()$new()
#'
#' # -----------------
#' #
#' get_lambda_config()
get_lambda_config <- function(
  env = globalenv()
){
  # get the items from the environment, and deduce their class
  items <- ls(envir = env)
  item_classes <- sapply(items, function(x){class(env[[x]])})

  # test whether any are valid lambda configs
  potential_configs <- sapply(item_classes, function(x){"lambda_config" %in% x})
  configs <- which(potential_configs)

  if(length(configs) == 0){
    .error_message("Cannot locate a valid lambda_config")
  } else if(length(configs) > 1){
    .error_message("Multiple lambda_config objects exist in the chosen environment")
  } else{
    env[[names(configs)]]
  }
}
