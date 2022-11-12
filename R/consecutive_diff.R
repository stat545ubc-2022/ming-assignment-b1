#' Calculate the consecutive difference for a sequence of numbers
#'
#' @description given a sequence such as (x1, x2, x3, x4, x5, ...),
#' consecutive_diff() can calculate all the consecutive differences as {(x2-x1), (x3-x2), (x4-x3), ...}

#' @param number_sequence the number sequence the user chooses to put in this function. The input should be a numeric vector, or list of single numerics.
#'
#' @return consecutive_diff() returns a numeric vector
#'
#' @examples
#' # create an array of consecutive squares and calculate the consecutive differences
#' lst <- c(0, 1, 4, 9, 16, 25, 36, 49, 64)
#' consecutive_diff(lst)
#' # generate a series of random integers and calculate the consecutive difference
#' consecutive_diff(sample.int(100,20))
#' @export
consecutive_diff <- function(number_sequence){
  # special case, check if the input vector is empty
  if(length(number_sequence) == 0){
    stop("Sorry, the input number sequence is empty, please input a non-empty number sequence!")
  }

  # special case, check if the input vector only contains one number
  if(length(number_sequence) == 1){
    stop("Sorry, your output vector has a length of 0,
         please input a number sequence with at least two numbers for the function to work properly:)")
  }

  # if the sequence contains NAs, provide a message.
  if (any(is.na(number_sequence))) {
    message("The input number sequence contains NAs.")
  }
  x2 <- number_sequence[-1]
  x1 <- number_sequence[-length(number_sequence)]
  return(x2 - x1)
}

