#' Compute the tracing score of a group of contacts
#'
#' This function compute the tracing score of a group of contacts, i.e. a set of
#' individual who have been exposed to one or several cases whose dates of
#' onsets are known. The tracing score is defined as the expected number of
#' individuals that will show symptoms for the first time on day 't'.
#'
#' @export
#'
#' @author Thibaut Jombart (\email{thibautjombart@@gmail.com})
#'
#' @inheritParams contact_score
#' 
#' @param x A list of integer vectors indicating dates of onset of the cases
#'     causing exposure. Each item of the list corresponds to a different
#'     individual.
#'
#' @details See the \code{distcrete} package for generating discretized
#'     distributions at: \url{http://github.com/reconhub/distcrete}.
#'
#' @return A function with one single argument 't', which is the date for which
#'     the score should be computed.
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
group_score <- function(x, R, lambda, w) {
    ## The returned object will be a function with enclosed data; its only
    ## argument 't' is a vector of integer dates for which the tracing score is
    ## computed.

    if (length(x) < 1) {
        stop("'x' must contain at least one item.")
    }
    if (any(!is.finite(unlist(x)))) {
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

    ## This function computes p(x_i = t) for one individual
    score_one_indiv <- function(onset, Rc, w, t) {
        if (length(t) > 1) {
            rates <- Rc * vapply(t, function(day) sum(w(day - onset)), double(1))
            1 - exp(-rates)
        } else {
            rate <- Rc * sum(w(t - onset))
            1 - exp(-rate)
        }
    }

    
    function(t) {
        indiv_scores_over_t <- lapply(x, score_one_indiv, Rc, w, t)
        Reduce("+", indiv_scores_over_t)
    }
}
