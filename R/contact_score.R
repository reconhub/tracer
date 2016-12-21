#' Compute the tracing score of a contact
#'
#' This function compute the tracing score of a contact, i.e. an individual who
#' has had at least one recorded exposure to a case. For a contact, the tracing
#' score is defined as the probability that this individual will show symptoms
#' for the first time on day 't'.
#'
#' @export
#'
#' @author Thibaut Jombart (\email{thibautjombart@@gmail.com})
#'
#' @param x A vector of integers indicating dates of onset of the cases
#'     causing exposure.
#'
#' @param R The average effective reproduction number, i.e. the average number
#'     of secondar cases seeded by an infected individual.
#'
#' @param lambda The average number of contacts (infectious or not) reported by
#'     cases.
#'
#' @param w The probability mass function of the serial interval, i.e. the delay
#'     from primary to secondary symptom onset. We recommend using the
#'     \code{distcrete} package to generate discretized distributions (see
#'     details).
#'
#'
#' @details See the \code{distcrete} package for generating discretized
#'     distributions at: \url{http://github.com/reconhub/distcrete}.
#'
#' @return A function with two arguments:
#' \itemize{
#'
#' \item \code{t}: the date for which the score should be computed.
#'
#' \item \code{visit_days_ago}: the number of days since the last visit to the
#' contact, defaulting to \code{t - 1}.
#'
#' }
#' 
#' @examples
#' if (require(distcrete)) {
#'  ## generate serial interval distribution
#'  SI <- distcrete("gamma", 1L, w = 0, 10, 0.65)$d
#'  plot(SI, type="h", xlim=c(0,50), xlab = "Days")
#'  title("Serial interval distribution")
#'
#'  ## get tracing score function for:
#'  ## - exposure to cases with onsets: 1, 10, 25
#'  ## - R = 2.1
#'  ## - lambda = 3.5
#'  f <- contact_score(c(1,10,25), R=2.1, lambda=3.5, SI)
#'
#'  ## score for various days
#'  f(0) # day 0
#'  f(10) # day 10
#'  f(10:20)
#'
#'  ## plot score for various days
#'  plot(f, type = "h", xlim = c(0,60), col = pal1(100),
#'       xlab = "Date", ylab = "P(new symptoms)")
#'  title("Contact score over time")
#' }
contact_score <- function(x, R, lambda, w) {
    ## The returned object will be a function with enclosed data; its only
    ## argument 't' is a vector of integer dates for which the tracing score is
    ## computed.

    if (length(x) < 1) {
        stop("'x' must have at least one value.")
    }
    if (any(!is.finite(x))) {
        stop("All values in 'x' need to be finite, non-NA numbers.")
    }
    if (!is.finite(R)) {
        stop("'R' must be a finite number.")
    }
    if (!is.finite(lambda)) {
        stop("'lambda' must be a finite number.")
    }
    if (R < 0) {
        stop("'R' cannot be less than 0.")
    }
    if (lambda < 0 ) {
        stop("'lambda' cannot be less than 0.")
    }
    if (!is.function(w)) {
        stop("'w' must be a function.")
    }

    ## Rc is the probability that an exposure leads to a new case.
    Rc <- min(R / lambda, 1)
    
    function(t, visit_days_ago = 1L) {
        days_to_try <- c()
        rates <- Rc * vapply(t, function(day) sum(w(day - x)), double(1))
        1 - exp(-rates)
    }
}

