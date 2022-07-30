# Lambda config
lambda_config <- R6::R6Class(
  classname = "lambda_config",
  public = list(
    initialize = function(
      aws_function,
      s3_bucket,
      s3_prefix,
      s3_config,
      lambda_config
    ){
      # set private elements
      private$aws_function <- aws_function
      private$s3_bucket <- s3_bucket
      private$s3_prefix <- s3_prefix
      private$s3_config <- s3_config
      private$lambda_config <- lambda_config
      self
    },

    get = function(){
      as.list(private)
    },

    print = function(){
      .lambda_config_print(as.list(private))
    }
  ),
  private = list(
    aws_function = NULL,
    s3_bucket = NULL,
    s3_prefix = NULL,
    s3_config = NULL,
    lambda_config = NULL
  )
)


#' Lambda config printing helper function
#'
#' @param private The private element of the lambda R6 class
#'
#' @keywords internal
.lambda_config_print <- function(private){
  header <- "<-- Lambda config -->"

  lists <- sapply(private, is.list)
  private[lists] <- "list()"

  body <- paste(
    crayon::blue(.aligner(names(private))),
    private,
    collapse = "\n  "
  )

  output <- paste(
    crayon::blue(header),
    body,
    sep = "\n  "
  )
  cat(output)
}


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
