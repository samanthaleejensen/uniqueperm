#---------------------MAIN METHODS---------------------#

#-------------------------------------------------------------------------
#                         User Interface Method
#-------------------------------------------------------------------------

#' Space efficient unique permutation generation
#'
#' Efficiently generate truly unique and random permutations of binary data in
#' R.
#'
#' @param original an integer (0/1 only) or logical vector representing the
#'   statuses of the true observations.
#' @param number_permutations integer; desired number of permutations to find.
#'
#' @return A 0/1 matrix with \code{number_permutations} rows and
#'   \code{length(original)} columns. If there are fewer possible permutations
#'   than are requested, will return as many as possible.
#'
#' @details This function uses C++ bitsets to decrease memory load and
#'   \code{random_shuffle} to increase speed. It is capable of generating
#'   1,000,000 unique permutations of a 1000 observation binary vector in
#'   around 30 seconds.
#'
#'   If you are getting errors while using this method it is likely because this
#'   method has a C++ implementation. Check to make sure that you have the
#'   necessary tools to compile C++ code.
#'
#' @examples
#' example_states <- c(0,0,0,0,1,1,1,1)
#' permute(example_states, 34)
#'
#' example_states <- c(rep(1, 500), rep(0,500))
#' permute(example_states, 1000)
#'
#'#TIMING AND MEMORY USAGE EXAMPLES
#'
#' library(R.utils) # for timing
#' library(profmem) # for memory usage information
#'
#' ptm <- proc.time()
#' permute(example_states, 1000)
#' print(proc.time() - ptm)
#'
#' print(total(profmem(permute(example_states, 1000))))
#'
#' @author PJ Tatlow, Samantha Jensen
#'
#' @useDynLib uniqueperm
#' @importFrom Rcpp sourceCpp
#'
#' @export
permute <- function(original, number_permutations, fast = TRUE, memory_efficient = TRUE, truly_random = TRUE, method = "default") {

  # checking that generating requested permutations is possible
  maximum_permutations <- choose(length(original), min(table(original)))# determine max possible permutations

  if (number_permutations > maximum_permutations - 1) { # if not possible, do as many as possible.
    number_permutations = maximum_permutations - 1
  }

  bitsetpermute(original, number_permutations)
}
