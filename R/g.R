#' Apply G(n) function.
#'
#' @description Apply G(n) function on a given number (n) for a
#' given # of samples (n_samples).
#'
#' @param n float. The number to compute G
#' @param n_samples integer. The # of samples in dataset.
#'
#' @usage g(n, n_samples)
#'
#' @return The calculus of G function for the inputs.
#'
#' @examples
#' g(2, 10)
#' g(3, 15)
#'
#' @export g
g <- function(n, n_samples){
    return (n*(n_samples+1))
}
