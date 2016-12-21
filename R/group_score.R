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
#' @return A function with two arguments:
#' \itemize{
#'
#' \item \code{t}: the date for which the score should be computed.
#'
#' \item \code{visit_days_ago}: the number of days since the last visit to the
#' contact, defaulting to \code{1}, which means that the last visit happened the
#' day before \code{t}.
#'
#' }
#'
#' @examples
#' if (require(distcrete)) {
#' ## simulate data
#' set.seed(1)
#' x <- replicate(30, sample(0:30, sample(1:5), replace = TRUE))
#' head(x)
#'
#'  ## generate serial interval distribution
#'  SI <- distcrete("gamma", 1L, w = 0, 10, 0.65)$d
#'  plot(SI, type="h", xlim=c(0,50), xlab = "Days")
#'  title("Serial interval distribution")
#'
#' ## get scoring function
#' g <- group_score(x, R, lambda, SI$d)
#' g(c(10,20,30)) # Exp nb of new cases at t=10,20,30
#'
#' plot(g, xlim = c(0, 100), type = "h", 
#'     main = "Expected number of new cases", 
#'     xlab = "Current time", ylab = "Number of cases")
#'
#' set.seed(1)
#' ## early wave
#' x1 <- replicate(30, sample(0:20, sample(1:6), replace = TRUE))
#'
#' ## large middle wave
#' x2 <- replicate(140, sample(15:70, sample(1:3), replace = TRUE))
#'
#' ## late wave
#' x3 <- replicate(140, sample(65:80, sample(1:4), replace = TRUE))
#'
#' ## get scoring functions for every group
#' list_g <- lapply(list(x1, x2, x3), group_score, R, lambda, SI$d)
#'
#' ## get predictions for days 1:120
#' pred_nb_cases <- sapply(list_g, function(g) g(1:120))
#'
#' barplot(t(pred_nb_cases), col = pal1(3), border = "grey", 
#'         main = "Predicted new cases per groups", xlab = "Current time", 
#'         ylab = "Number of new cases (stacked)")
#' axis(side=1)
#' 
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

    ## This function computes p(x_i = t) for one individual.  This function is
    ## similar to contact_score except that it does not enclose inputs. See
    ## comments in contact_score.R for details.

    score_one_indiv <- function(onset, Rc, w, t, visit_days_ago) {
        if (visit_days_ago < 1L) {
            stop("'visit_days_ago' cannot be less than 1.")
        }

        days_ago_to_consider <- seq(0, visit_days_ago - 1L, by = 1L)
        days_list <- lapply(days_ago_to_consider, function(i) t - i)
        out <- double(length(t))
        for (days in days_list) {
            rates <- Rc * vapply(days, function(day) sum(w(day - onset)), double(1))
            out <- out + 1 - exp(-rates)
        }
        return(out)
    }

    
    function(t, visit_days_ago = 1L) {
        indiv_scores_over_t <- lapply(x, score_one_indiv, Rc, w, t, visit_days_ago)
        Reduce("+", indiv_scores_over_t)
    }
}

