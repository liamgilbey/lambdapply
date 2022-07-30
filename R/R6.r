

.val_f <- function(
  f
){
  stopifnot(is.function(f))
}

.val_data <- function(
  d
){
  stopifnot(is.list(d))
}

#' Class for collection of lambda items
#'
lambdar <- R6::R6Class(
  classname = "lambdar",
  public = list(
    initialize = function(
      data,
      f,
      aws_config = get_lambda_config()
    ){

      # validate
      if(!is.null(f)){
        .val_f(f)
      }

      if(!is.null(f)){
        .val_data(data)
      }

      # set private values
      new_uuid <- uuid::UUIDgenerate()
      f_string <- deparse(f) %>% paste0(collapse = "\n")
      private$uuid <- new_uuid
      private$data <- data
      private$aws_config <- aws_config
      private$f <- f_string

      # generate our specific lambdas
      private$n_iterations <- length(data)
      lambda_names <- if(is.null(names(data))){
        .warning_message(
          'Data supplied is not a named list, using numeric sequence instead'
        )
        as.character(seq(from = 1, to = length(data), by = 1))
      } else{
        names(data)
      }

      private$lambdas <- mapply(
        FUN = lambda_item$new,
        uuid = rep(new_uuid, length(data)),
        item_name = lambda_names,
        data = data,
        MoreArgs = list(
          f_string = f_string,
          aws_config = aws_config
        ),
        SIMPLIFY = FALSE
      )

      self
    },

    get_lambdas = function(){
      private$lambdas
    },

    invoke = function(){
      lapply(
        private$lambdas,
        function(x){
          x$invoke()
        }
      )
    },

    summary = function(force_check = F){
      r <- lapply(
        private$lambdas,
        function(x){
          x$summary(force_check)
        }
      )

      r <- do.call(rbind, r)
      r
    }
  ),
  private = list(
    uuid = NULL,
    data = NULL,
    f = NULL,
    aws_config = NULL,
    lambdas = NULL,
    n_iterations = NULL
  )
)

.build_lambda_function <- function(
  mapper
){
  l_function <- glue::glue(
    '
      function(d, file_name, s3_bucket, s3_destination, lambda_dir, s3, ...){
        # define the function we are iterating over
        f <- <<mapper>>

        # run the mapper
        result <- f(d, ...)

        # save the output
        saveRDS(result, file.path(lambda_dir, file_name))

        # upload to S3
        s3$put_object(
          Body = file.path(lambda_dir, file_name),
          Bucket = s3_bucket,
          Key = s3_destination
        )
        list(
          file_name = file_name
        )

      }
    ',
    .open = "<<",
    .close = ">>"
  )
  l_function
}

.build_payload <- function(
  l_function,
  d,
  file_name,
  s3_bucket,
  s3_destination,
  s3
){
  l <- list(
    f = l_function,
    d = d,
    file_name = file_name,
    s3_bucket = s3_bucket,
    s3_destination = s3_destination,
    lambda_dir = "/tmp",
    s3 = s3
  )
  jsonlite::toJSON(
    l,
    auto_unbox = TRUE
  )
}

# An individual lambda item
lambda_item <- R6::R6Class(
  classname = "lamda_item",
  public = list(
    initialize = function(
      uuid,
      item_name,
      data,
      f_string,
      aws_config = get_lambda_config()
    ){
      # unpack the config
      config <- aws_config$get()

      private$uuid <- uuid
      private$item_name <- item_name
      private$item_id <- paste(uuid, as.character(item_name), sep = '-')
      private$data <- data
      private$f <- f_string
      private$aws_function <- config$aws_function
      private$s3_prefix <- config$s3_prefix
      private$suffix <- ".rds"
      private$file_name <- paste0(private$item_id, private$suffix)
      private$s3_destination <- file.path(private$s3_prefix, uuid, private$file_name)
      private$s3_bucket <- config$s3_bucket
      private$s3_config <- config$s3_config
      config$lambda_config = config$lambda_config
      private$l_function <- .build_lambda_function(
        mapper = f_string
      )
      private$payload <- .build_payload(
        private$l_function,
        d = data,
        file_name = private$file_name,
        s3_bucket = private$s3_bucket,
        s3_destination = private$s3_destination,
        s3 = config$s3_config
      )

      self
    },

    get_id = function(){
      private$item_id
    },

    get_data = function(){
      private$data
    },

    get_filename = function(){
      private$file_name
    },

    get_s3_destination = function(){
      private$s3_destination
    },

    get_l_function = function(){
      private$l_function
    },

    get_payload = function(){
      private$payload
    },

    invoke = function(){
      logger::log_info(
        glue::glue("Invoking lambda function: {private$aws_function} for item: {private$item_id}")
      )
      l <- paws::lambda(
        config = list(region = 'ap-southeast-2')
      )
      l$invoke(
        FunctionName = private$aws_function,
        InvocationType = 'Event',
        Payload = private$payload
      )
    },

    summary = function(force_check = FALSE){
      if(force_check){

      }
      data.frame(
        item_name = private$item_name,
        status = private$status
      )
    }
  ),
  private = list(
    uuid = NULL,
    item_name = NULL,
    item_id = NULL,
    data = NULL,
    f = NULL,
    aws_function = NULL,
    s3_bucket = NULL,
    s3_prefix = NULL,
    suffix = NULL,
    file_name = NULL,
    s3_destination = NULL,
    l_function = NULL,
    s3_config = NULL,
    lambda_config = NULL,
    payload = NULL,
    status = 'INCOMPLETE'
  )
)

## examples

f <- function(x){
  model <- lm(Sepal.Width ~ Sepal.Length, data = x)
  summary(model)
}

aws_config <- lambda_config$new(
  aws_function = 'r-test',
  s3_bucket = 'data-storage',
  s3_prefix = 'r-testing',
  s3_config = paws::s3(),
  lambda_config = paws::lambda()
)

data_split <- split(iris, as.factor(iris$Species))

l <- lambdar$new(
  data = data_split,
  f = f,
  aws_config
)
l$summary()
l$invoke()
x <- l$get_lambdas()[[1]]
x$invoke()
x$summary()
x$get_data()
x$get_payload()

new_f <- eval(parse(text = x$get_l_function()))
new_f(x$get_data(), x$get_filename(), "data-science-model-storage-dev", x$get_s3_destination(), "/tmp", list(region = 'ap-aoutheast-2'))
readRDS(
  file.path(
    "/tmp",
    x$get_filename()
  )
)
x

