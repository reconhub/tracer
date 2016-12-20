##' Color palettes used in tracer
##'
##' These functions are color palettes used in tracer.
##'
##' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
##'
##' @param n a number of colors
##'
##' @rdname palettes
##' @aliases palettes pal1
##'
##' @export
##' @importFrom grDevices colorRampPalette
##'
##' @details
##' \itemize{
##' \item pal1: purples (dark to light)
##' }
##' @examples
##'
##' plot(1:100, cex=8, pch=20, col = pal1(100),
##'      main="palette: pal1")
##'
pal1 <- function(n){ # purple
    if(!is.numeric(n)) stop("'n' is not a number.")
    colors <- c("#400080", "#e6ccff")
    return(colorRampPalette(colors)(n))
}
