#' Euclidian algorithm.
#'
#' This function uses euclidian algorithm to find the greatest common divisor.
#' adapted from https://en.wikipedia.org/wiki/Euclidean_algorithm .
#'
#' @param x the first number.
#' @param y the second number.
#' @export
#' @examples
#' euclidean()
#' d <- euclidean(x=100, y=1000)
euclidean <- function(x=100, y=1000)
{
  num1 <- x;
  num2 <- y;
  while(num2 != 0) {
    rem      = num1%%num2; 
    num1    = num2;       
    num2    = rem;
  }
  return(num1)
}
