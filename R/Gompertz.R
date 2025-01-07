#' SSgompertzDummy: Self-starting Gompertz with dummy term
#'
#' This self-starting function extends the classic Gompertz model by adding a linear
#' term for a dummy variable. The expression is:
#' \deqn{y = \mathrm{Asym} \, e^{-b_2 \cdot e^{-b_3 x}} + \beta \cdot \mathrm{dummy}.}
#'
#' It allows \code{nls()} to automatically compute initial values for
#' \code{Asym}, \code{b2}, \code{b3}, and \code{Beta}, so long as your data
#' and dummy are reasonably set up.
#'
#' @param x A numeric vector representing the independent variable.
#' @param dummy A numeric or integer vector (0/1) indicating which observations belong
#'   to the "dummy" group. Must have same length as \code{x}.
#' @param Asym A numeric parameter representing the asymptotic maximum.
#' @param b2 A numeric parameter (often related to the rate of growth).
#' @param b3 A numeric parameter (often related to the time scaling).
#' @param Beta A numeric parameter multiplying the dummy (vertical shift).
#'
#' @details
#' Internally, this function has two parts:
#' \enumerate{
#'   \item A model expression: \code{Asym * exp(-b2 * exp(-b3 * x)) + Beta * dummy}.
#'   \item An \code{initial} function for guessing starting values.
#' }
#'
#' The gradient attribute is also defined so that \code{nls()} can use analytical
#' gradients, potentially improving convergence and performance.
#'
#' @return A numeric vector of the same length as \code{x} with the modeled values,
#'   along with attributes for self-starting and gradient calculation.
#'
#' @examples
#' \dontrun{
#' # Generate some data
#' set.seed(123)
#' n <- 50
#' x <- seq(1, 10, length.out = n)
#' dummy_vec <- rep(c(0,1), each = n/2)
#'
#' # True parameters for simulation:
#' # Asym = 100, b2 = 2, b3 = 0.3, Beta = 20
#' # We'll create a "Gompertz" pattern plus 20 for half the data
#' y <- 100 * exp(-2 * exp(-0.3 * x)) + 20 * dummy_vec + rnorm(n, 0, 3)
#'
#' df <- data.frame(x = x, y = y, dummy = dummy_vec)
#'
#' # Fit with nls
#' fit_dummy <- nls(
#'   formula = y ~ SSgompertzDummy(x, dummy, Asym, b2, b3, Beta),
#'   data = df,
#'   start = list(Asym = 100, b2 = 2, b3 = 0.3, Beta = 0) # or rely on selfStart
#' )
#'
#' summary(fit_dummy)
#' }
#'
#' @seealso \code{\link{nls}}, \code{\link{selfStart}}, \code{\link{SSgompertz}}
#' @export
SSgompertzDummy <- selfStart(
  # 1) Model expression
  function(x, dummy, Asym, b2, b3, Beta) {
    expr <- Asym * exp(-b2 * exp(-b3 * x)) + Beta * dummy
    expr
  },

  # 2) Initialization function
  initial = function(mCall, data, LHS, ...) {
    # Evaluate the arguments in the provided data environment
    x_val    <- eval(mCall[["x"]], data)
    dummy_val<- eval(mCall[["dummy"]], data)
    y_val    <- eval(LHS, data)

    # Simple heuristic for Asym: max of y where dummy=0
    # Adjust as needed for your use case
    Asym  <- max(y_val[dummy_val == 0], na.rm = TRUE)
    b2    <- 2
    b3    <- 0.02
    Beta  <- 0

    list(Asym = Asym, b2 = b2, b3 = b3, Beta = Beta)
  },

  # 3) List of parameter names
  parameters = c("Asym", "b2", "b3", "Beta")
)

#' Define analytic gradient for SSgompertzDummy
#'
#' This gradient function helps nls() converge faster by avoiding numerical
#' approximation of derivatives.
#'
#' @keywords internal
#' @noRd
attr(SSgompertzDummy, "gradient") <- function(x, dummy, Asym, b2, b3, Beta) {
  # f(x) = Asym * exp(-b2 * exp(-b3*x)) + Beta*dummy
  # Let w = exp(-b3*x)
  # Then: f(x) = Asym * exp(-b2*w) + Beta*dummy
  # Derivatives:
  # df/dAsym = exp(-b2 * w)
  # df/db2   = -Asym * w * exp(-b2 * w)
  # df/db3   = Asym * b2 * x * w * exp(-b2 * w)
  # df/dBeta = dummy

  w <- exp(-b3 * x)
  g1 <- exp(-b2 * w)        # common factor
  df_dAsym <- g1
  df_db2   <- -Asym * w * g1
  df_db3   <-  Asym * b2 * x * w * g1
  df_dBeta <-  dummy

  cbind(df_dAsym, df_db2, df_db3, df_dBeta)
}


#' fit_gompertz_dummy: Example function to fit a Gompertz + dummy model
#'
#' This function showcases how to use \code{SSgompertzDummy} in an \code{nls}
#' model. It receives a dataset with columns \code{x}, \code{y}, and \code{dummy},
#' and fits:
#' \deqn{ y = Asym \exp(-b_2 \exp(-b_3 x)) + \beta \cdot \mathrm{dummy} }
#'
#' @param data A data frame (or tibble) containing the columns \code{x}, \code{y},
#'   and \code{dummy}.
#' @param start A named list of starting values for the parameters
#'   \code{Asym, b2, b3, Beta}. If \code{NULL}, the selfStart mechanism will attempt
#'   to guess them. However, providing good starts can improve convergence.
#' @param ... Additional arguments passed to \code{nls}.
#'
#' @return An object of class \code{nls}, with estimated parameters and summary stats.
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' n <- 50
#' x_vals <- seq(1, 10, length.out = n)
#' dummy_vec <- rep(c(0, 1), each = n/2)
#' # True: Asym=100, b2=2, b3=0.3, Beta=20
#' y_vals <- 100 * exp(-2 * exp(-0.3 * x_vals)) + 20 * dummy_vec + rnorm(n, 0, 3)
#' df <- data.frame(x = x_vals, y = y_vals, dummy = dummy_vec)
#'
#' # Fit
#' fit <- fit_gompertz_dummy(df, start = list(Asym=90, b2=2, b3=0.2, Beta=10))
#' summary(fit)
#' }
#'
#' @seealso \code{\link{SSgompertzDummy}}, \code{\link{nls}}
#' @export
fit_gompertz_dummy <- function(data, start = NULL, ...) {
  stopifnot(is.data.frame(data))
  if (!all(c("x", "y", "dummy") %in% names(data))) {
    stop("Data must have columns named 'x', 'y', and 'dummy'.")
  }
  if (is.null(start)) {
    # If user doesn't provide starts, nls will rely on selfStart's 'initial' function
    start_list <- list(Asym = NA, b2 = NA, b3 = NA, Beta = NA)
  } else {
    start_list <- start
  }

  mod <- nls(
    formula = y ~ SSgompertzDummy(x, dummy, Asym, b2, b3, Beta),
    data = data,
    start = start_list,
    ...
  )
  return(mod)
}
