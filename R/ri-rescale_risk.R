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
#' @param risk_col Column containing numeric vector of risk values in the range \[0, 12\].
#' @param from existing range of possible values for `risk_col` that will be converted to
#' @param to new range of possible values for `risk_col`, by default it is 0 to 100.
#' @param method The scaling method to apply. Options are `"linear"`, `"quadratic"`,
#' `"exponential"`, `"sigmoid"`, or `"complementary"`.
#' @param inverse boolean to inverse the risk values, i.e. low becomes high, and
#' high becomes low. Similar to the `"complementary"` value of method, but can be
#' added to quadratic.
#' @param new_col string, The name of the new scaled column, defaults to `{method}_scaled_{risk_col}`.
#' If set to `NA` the scaled column will replace the existing column.
#' @return A dataset with new column containing  numeric vector of scaled values in the range \[0, 100\].
#' @examples
#' # rescale_risk -----
#' @example examples/rescale_risk.R
rescale_risk <- function(
    dataset,
    risk_col,
    method = c("linear", "quadratic", "exponential", "sigmoid"),
    inverse = FALSE,
    from,
    to = c(0, 100),
    new_col = NULL
    ) {


  stopifnot(
    'risk_col not found in dataset' = risk_col %in% names(dataset),
    'inverse should be TRUE or FALSE' = inverse %in% c(TRUE, FALSE)
  )

  method <- match.arg(method)

  if (is.null(new_col)) {
    new_col <- paste0(method, "_scaled_", risk_col)
  }

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

  dat <- data.frame(x = dataset[[risk_col]])
  dat$normalised <- (dat$x - old_min) / (old_max - old_min)
  dat$transformed <- transformation(dat$normalised)
  dat$rescaled <- dat$transformed * (new_max - new_min) + new_min

  dataset[[new_col]] <- dat$rescaled
  dataset
}



#' Sigmoid function
#'
#' We use the logistic function, rescaled to map non-negative values into [0, 1).
#'
#' The function increases asymptotically up to 1, taking the values .46, .76, .90 and .96
#' at 1, 2, 3 and 4 respectively.
#'
#' @param x Numeric vector.
#'
#' @return A numeric vector with values between -1 and 1.
#' @noRd
#' @examples
#' x <- (0:500)/100
#' plot(x, sigmoid_1_100(x), type = 'l')
#' @importFrom stats plogis
#' @rdname sigmoid
#' @noRd
sigmoid <- function(x) {
  2 * plogis(x) - 1
}

#' Inverse sigmoid function
#'
#' The invesrse of the sigmoid function.
#'
#' @param x Numeric vector with values between -1 and 1.
#'
#' @return A numeric vector with real values.
#' @noRd
#' @examples
#' x <- c(0.01, 0.88, 0.247)
#' inv_sigmoid(x)
#' @importFrom stats qlogis
#' @rdname sigmoid
inv_sigmoid <- function(x) {
  qlogis((x + 1)/2)
}


#' @importFrom legendry primitive_bracket bracket_square key_range_manual
plot_rescale_risk_density <- function(dat,
                                      linear_col = "emission_risk_linear",
                                      rescaled_col = "emission_risk_scaled",
                                      scaling_label = "new scaling") {

  pre <- dat |>
    select(y = .data[[linear_col]])|>
    mutate(cat = "linear")

  post <- dat |>
    select(y = .data[[rescaled_col]]) |>
    mutate(cat = scaling_label)

  prepost <- bind_rows(pre, post) |>
    mutate(x = row_number(), .by = cat) |>
    mutate(cat = factor(cat, levels = c("linear", scaling_label)))

  ranges <- key_range_manual(
    start = c(0, 25, 50, 75),
    end   = c(25, 50, 75, 100),
    name  = c("Negligible", "Low", "Medium", "High"),
    level = 1,
    colour = c("#2B83BA","#ABDDA4","#FDAE61","#D7191C")
  )

  ggplot() +
    guides(x.sec = primitive_bracket(ranges, bracket = bracket_square)) +
    geom_density(
      data = prepost,
      mapping = aes(
        x = .data[["y"]],
        color = .data[["cat"]],
        fill = .data[["cat"]],
        linetype = .data[["cat"]]
      ),
      position = "identity",
      alpha = 0.5,
      size = 0.5
    ) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    scale_x_continuous(name = "Risk scores", limits = c(0, 100), expand = c(0, 0))+
    scale_y_continuous(name = "Density", expand = c(0, 0)) +
    .my_theme +
    theme(
      legend.position = c(.95, .95),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(6, 6, 6, 6),
      legend.title=element_blank(),
      legend.box.background = element_rect(color="grey", size=0.5)
    )

}

plot_rescale_risk_points <- function(dat, fun, inverse, raw_col, scaled_col, from, to, is_road_access_risk = FALSE){

  if(is_road_access_risk) {
    from <- c(0, 100)
    raw_col <- "linear_risk"
  }


  lines_df <- rescale_risk(
    dataset = data.frame(x = seq(from[1], from[2], 0.25)),
    risk_col = "x", new_col = "y",
    method = fun,
    inverse = inverse,
    from = from,
    to = to
    )

  gg_obj <- ggplot(
    data = dat,
    mapping = aes(
      x = .data[[raw_col]],
      y = .data[[scaled_col]]
    )
  )

  gg_obj <- gg_obj+
    geom_smooth(data = lines_df, aes(x=.data$x, y =.data$y), method = 'loess', formula = 'y ~ x', na.rm = TRUE)

  gg_obj <- gg_obj +
    geom_point(alpha = 0.20, color = "black", size = 4, na.rm = TRUE)

  gg_obj <- gg_obj +
    scale_x_continuous(
      breaks = seq(from[1], from[2], round(from[2] * 0.10, 0)),
      limits = c(from[1], from[2])
      )

  gg_obj <- gg_obj +
    scale_y_continuous(
      limits = to,
      breaks = seq(to[1], to[2], to[2]/10)
      )


  if(is_road_access_risk) {
    gg_obj <- gg_obj +
      labs(
        x = "Unscaled risk score (before)",
        y = "Scaled risk score (after)"
      )
  } else {
    gg_obj <- gg_obj +
      labs(
        x = "Linear scaling",
        y = "New scaling"
      )
  }

  gg_obj <- gg_obj +
    .my_theme

  gg_obj
}


plot_rescale_raster_points <- function(dat, fun, inverse, raw_col, scaled_col, from, to = c(0,100)){

  xmax <- from[2]

  lines_df <- rescale_risk(
    dataset = data.frame(x = seq(0, xmax + 25, 0.25)), risk_col = "x", new_col = "y",
    method = fun, inverse = inverse, from = from, to = to
  )

  ggplot(
    data = dat,
    mapping = aes(
      x = .data[[raw_col]],
      y = .data[[scaled_col]]
    )
  ) +
    geom_smooth(data = lines_df, aes(x=.data$x, y =.data$y), method = 'loess', formula = 'y ~ x', na.rm = TRUE) +

    geom_point(alpha = 0.20, color = "black", size = 4) +
    scale_x_continuous(
      name = "Unscaled risk score (before)",
      expand = c(0, 0),
      limits = c(from[1] - 1, xmax + 1)
    )+
    scale_y_continuous(
      name = "Scaled risk score (after)",
      limits = c(to[1]-0.5, to[2] + 10),
      breaks = seq(to[1], to[2], to[2]/10),
      expand = c(0, 0)
    ) +
    .my_theme
}
