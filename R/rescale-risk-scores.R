#' Scaling range transformation function (re-scaling)
#'
#' This function supports multiple scaling methods to transform risk values from
#' an existing range to  another. By default the output range is between 0 and 100.
#'
#' Below are the equations used for each method:
#'
#' **Linear Scaling**
#' \deqn{f(x) = \frac{x - 0}{12 - 0} \cdot (100 - 0) + 0}
#'
#' **Quadratic Scaling**
#' \deqn{f(x) = \left(\frac{x - 0}{12 - 0}\right)^2 \cdot (100 - 0) + 0}
#'
#' **Exponential Scaling**
#' \deqn{f(x) = \frac{\exp(x / 12) - 1}{\exp(1) - 1} \cdot (100 - 0) + 0}
#'
#' **Sigmoid Scaling**
#' \deqn{f(x) = \frac{100}{1 + \exp(-10 \cdot (\frac{x}{12} - 0.5))}}
#'
#'
#' @param dataset dataset to add scaled risk column to.
#' @param cols Column containing numeric vector of risk values in the range \[0, 12\] OR a named
#' vector, names will be used as the `names_to` arg, giving new names to rescaled columns.
#' @param from existing range of possible values for `risk_col` that will be converted to
#' @param to new range of possible values for `risk_col`, by default it is 0 to 100.
#' @param method The scaling method to apply. Options are `"linear"`, `"quadratic"`,
#' `"exponential"`, `"sigmoid"`, or `"complementary"`.
#' @param inverse boolean to inverse the transformation function (e.g., quadratic becomes
#' square root, exponential becomes logarithmic). This affects the shape of the
#' transformation curve.
#' @param reverse boolean to reverse the risk scale direction before transformation,
#' flipping low risk to high risk and vice versa (e.g., a value of 3/12 becomes 9/12).
#' This is independent of `inverse` and can be combined with it.
#' @param names_prefix string, prefix `cols` names to have new names for scaled columns.
#' @param names_to string vector, provide new name for rescaled columns.
#' @param keep_cols default TRUE, whether to keep `cols` columns after rescaling is done or to remove.
#' @return A dataset with new column containing  numeric vector of scaled values in the range \[0, 100\].
#' @examples
#' # rescale_risk -----
#' @example examples/rescale_risk.R
#' @export
rescale_risk_scores <- function(
    dataset,
    cols = NULL,
    from = NULL,
    to = c(0, 100),
    method = c("linear", "quadratic", "exponential", "sigmoid"),
    inverse = FALSE,
    reverse = FALSE,
    names_prefix = NULL,
    names_to = NULL,
    keep_cols = FALSE
) {

  if (!is.null(cols) && is.null(attr(dataset, "risk_col"))) {
    cols <- cols
  } else if (is.null(cols) && !is.null(attr(dataset, "risk_col"))) {
    cols <- attr(dataset, "risk_col")
  } else if (!is.null(cols) && !is.null(attr(dataset, "risk_col"))) {
    if(attr(dataset, "risk_col") != cols){
      if (!shinyIsRunning()) {
        cli_warn("{.arg risk_col} attribute of {.arg dataset} is being overwritten by {.arg cols}")
      }
    }
  } else {
    cli_abort("{.args cols} needs to be provided as {.arg dataset} has no {.arg risk_col} attribute")
  }
  if (!is.null(from) && is.null(attr(dataset, "scale"))) {
    from <- from
  } else if (is.null(from) && !is.null(attr(dataset, "scale"))) {
    from <- attr(dataset, "scale")
  } else if (!is.null(from) && !is.null(attr(dataset, "scale"))) {
    if (!shinyIsRunning()) {
      cli_inform("{.arg scale} attribute of {.arg dataset} is being overwritten by {.arg from}")
    }
  } else {
    cli_abort("{.args from} needs to be provided as {.arg dataset} has no {.arg scale} attribute")
  }

  cli_abort_if_not(
    "{.arg  dataset} should be a data.frame or tibble" = "data.frame" %in% class(dataset),
    "{.arg inverse} should be TRUE or FALSE" = inverse %in% c(TRUE, FALSE),
    "{.arg reverse} should be TRUE or FALSE" = reverse %in% c(TRUE, FALSE),
    "{.arg from} should have be a vector of length 2" = length(from) == 2,
    "{.arg to} should have be a vector of length 2" = length(from) == 2
  )

  missing_cols <- cols[!cols %in% colnames(dataset)]
  if (length(missing_cols) > 0) {
    cli_abort("These {.arg  cols} not found in {.arg dataset}: {quote_and_collapse(missing_cols, quote_char = '\"')}")
  }
  if (rlang::is_named(cols) && !is.null(names_to)){
    cli_abort("When {.arg cols} is a named list, {.arg names_to} should not be provided.")
  }
  if (!is.null(names_to) && length(names_to) != length(cols)) {
    cli_abort("{.arg names_to} must be the same length as {.arg cols}.")
  }

  new_cols <- names_to %||% names(cols) %||% cols
  new_cols <- paste0(names_prefix, new_cols)
  table_name <- attr(dataset, "table_name")

  method <- match.arg(method)

  dataset <- rescale_risk(
    dataset = dataset,
    cols = cols,
    new_cols = new_cols,
    method = method,
    from = from,
    to = to,
    inverse = inverse,
    reverse = reverse,
    keep_cols = keep_cols
  )

  # Rescale secondary datasets as well
  if (any(c("borders", "points", "flows") %in% names(attributes(dataset)))) {
    dataset <- rescale_secondary_datasets(
      dataset = dataset,
      method = method,
      from = from,
      to = to,
      inverse = inverse,
      reverse = reverse,
      keep_cols = keep_cols
    )
  }
  attr(dataset, "scale") <- to
  attr(dataset, "risk_col") <- unique(new_cols)
  attr(dataset, "table_name") <- table_name
  dataset
}

rescale_risk <- function(dataset, cols, new_cols, method, from, to, inverse, reverse, keep_cols){

  old_min <- from[[1]] # existing min
  old_max <- from[[2]]
  new_min <- to[[1]] # new min
  new_max <- to[[2]]
  if (!inverse) {
    transformation <- switch(method,
                             linear = function(x) x,
                             quadratic = function(x) x^2,
                             exponential = function(x) {
                               k <- 1
                               (1 - exp(-k * x)) / (1 - exp(-k))
                             },
                             sigmoid = function(x) 1 / (1 + ((1 - x) / x)^2)
    )
  } else {
    transformation <- switch(method,
                             linear = function(x) x,
                             quadratic = function(x) sqrt(x),
                             exponential = function(x) {
                               k <- 1
                               -log(1 - x * (1 - exp(-k))) / k
                             },
                             sigmoid = function(x) 1 / (sqrt((1 / x) - 1) + 1)
    )
  }

  for (i in seq_along(cols)){
    dat <- data.frame(x = dataset[[cols[[i]]]])

    # Reverse the scale if requested (flip low/high risk)
    if (reverse) {
      dat$x <- old_max + old_min - dat$x
    }

    dat$normalised <- (dat$x - old_min) / (old_max - old_min)
    dat$transformed <- transformation(dat$normalised)
    dat$rescaled <- dat$transformed * (new_max - new_min) + new_min
    dataset[[new_cols[[i]]]] <- dat$rescaled
  }

  if (!keep_cols && !all(cols == new_cols)) {
    dataset <- dataset |> dplyr::select(-all_of(cols))
  }

  dataset
}



#' Rescale secondary datasets stored as attributes
#'
#' Helper function to rescale risk columns in secondary datasets (borders, points, flows)
#' that are stored as attributes of the main dataset.
#'
#' @param dataset Main dataset with secondary datasets as attributes
#' @param from Original scale range
#' @param to New scale range
#' @param method Scaling method
#' @param inverse Whether to apply inverse transformation
#' @param reverse Whether to reverse the risk scale direction
#' @return Dataset with rescaled secondary datasets as attributes
#' @noRd
rescale_secondary_datasets <- function(
    dataset,
    method,
    from,
    to,
    inverse,
    reverse,
    keep_cols
) {
  if (!is.null(attr(dataset, "borders"))) {
    borders <- attr(dataset, "borders")
    borders_rescaled <- rescale_risk(
      borders,
      cols = attr(borders, "risk_col"),
      from = attr(borders, "scale"),
      to = to,
      method = method,
      new_cols = attr(borders, "risk_col"),
      inverse = inverse,
      reverse = reverse,
      keep_cols = FALSE
    )
    attr(borders_rescaled, "scale") <- to
    attr(dataset, "borders") <- borders_rescaled

  } else if (!is.null(attr(dataset, "points"))) {
    points <- attr(dataset, "points")
    points_rescaled <- rescale_risk(
      points,
      cols = attr(points, "risk_col"),
      from = from,
      to = to,
      new_cols = attr(points, "risk_col"),
      method = method,
      inverse = inverse,
      reverse = reverse,
      keep_cols = FALSE
    )
    attr(points_rescaled, "scale") <- to
    attr(dataset, "points") <- points_rescaled

  } else if (!is.null(attr(dataset, "flows"))) {
    flows <- attr(dataset, "flows")
    flows_rescaled <- rescale_risk(
      flows,
      cols = attr(flows, "risk_col"),
      from = from,
      to = to,
      new_cols = attr(flows, "risk_col"),
      method = method,
      inverse = inverse,
      reverse = reverse,
      keep_cols = FALSE
    )
    attr(flows_rescaled, "scale") <- to
    attr(dataset, "flows") <- flows_rescaled
  }

  dataset
}

#' Sigmoid and inverse sigmoid
#'
#' We use the logistic function, rescaled to map non-negative values into [0, 1).
#'
#' The function increases asymptotically up to 1, taking the values .46, .76, .90 and .96
#' at 1, 2, 3 and 4 respectively.
#'
#' @param x Numeric vector.
#'
#' @return A numeric vector with values between -1 and 1.
#' @examples
#' x <- (0:500)/100
#' plot(x, sigmoid(x), type = 'l')
#' @importFrom stats plogis
#' @keywords internal
#' @rdname sigmoid
sigmoid <- function(x) {
  2 * plogis(x) - 1
}

#' @keywords internal
#' @name sigmoid
#' @importFrom stats qlogis
inv_sigmoid <- function(x) {
  qlogis((x + 1)/2)
}

#' Add scale attribute to a dataset
#'
#' @description
#' This function adds a scale attribute to a dataset, which defines the range
#' of risk values for the dataset. The scale attribute is used by other functions
#' in the analysis pipeline to understand the value range when performing
#' operations like rescaling or visualization.
#'
#' The scale attribute is then used in other riskintroanalysis functions such as
#' [plot_risk()] or [rescale_risk_scores()].
#'
#' @param dataset A dataset to add the scale attribute to.
#' @param scale A numeric vector of length 2 specifying the minimum and maximum
#'   values of the scale range (e.g., c(0, 100) for a 0-100 scale).
#'
#' @return The input dataset with the scale attribute added.
#'
#' @examples
#' # Create sample data
#' risk_data <- data.frame(
#'   region = c("A", "B", "C"),
#'   risk_score = c(2.5, 7.8, 4.1)
#' )
#'
#' # Add scale attribute indicating values range from 0 to 12
#' risk_data_with_scale <- add_scale(risk_data, c(0, 12))
#'
#' # Check the scale attribute
#' attr(risk_data_with_scale, "scale")
#'
#' @export
#' @importFrom rlang inherits_any
add_scale <- function(dataset, scale){
  cli_abort_if_not(
    "{.arg dataset} must be a {.cls data.frame}." = inherits(dataset,"data.frame"),
    "{.arg scale} must be numeric." = is.numeric(scale),
    "{.arg scale} must be length 2." = length(scale) == 2
  )
  attr(dataset, "scale") <- scale
  dataset
}
