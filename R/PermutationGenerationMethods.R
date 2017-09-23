#---------------------PERMUTATION GENERATION METHODS---------------------#

#-------------------------------------------------------------------------
#                            Piccolo Method
#-------------------------------------------------------------------------

#' Fast unique permutation generation
#'
#' This method is the default method upon calling
#' \code{uniqueperm::get_unique_perms}. It is the second-fastest method in the
#' package (after \code{pseudorandom_generation}), but requires more memory than
#' \code{bitset_generation}.
#'
#' @param original an integer (0/1 only) or logical vector representing the
#'   statuses of the true observations.
#' @param number_permutations integer; desired number of permutations to find.
#'
#' @return A 0/1 matrix with \code{number_permutations} rows and
#'   \code{length(original)} columns. If there are fewer possible permutations
#'   than are requested, only as many as are possible will be generated.
#'
#' @details This method is a helper function not meant for general use. Use
#'   \code{get_unique_perms} instead, as it has more parameter checking and
#'   safeguards.
#'
#' @examples
#' example_states <- c(0,0,0,0,1,1,1,1)
#' fast_generation(example_states, 34)
#' get_unique_perms(example_states, 34) #preferred
#' get_unique_perms(example_states, 34, method = "default")
#'
#' example_states <- c(rep(1, 500), rep(0,500))
#' fast_generation(example_states, 1000)
#'
#' @author
#' Dr. Stephen Piccolo, Samantha Jensen
fast_generation <- function(original, number_permutations) {
  number_observations <- length(original) #number of observations
  maximum_permutations <- choose(number_observations, min(table(original)))# determine max possible permutations

  #find which group is larger
  grouped_observations <- sort(table(original)) # group cases and controls and count
  minority <- as.integer(names(grouped_observations)[1]) # group with least observations
  majority <- as.integer(names(grouped_observations)[2]) # group with more observations

  number_minority <- grouped_observations[names(grouped_observations)[1]] # this is the least number of indices we need to change for each permutation

  #create list of number_permutations/maximum_permutations permutations (list of lists of indices of minority group)
  permutations <- list(which(original==minority)) # original observations are first element of list to avoid duplication
  to_generate <- ceiling(number_permutations * 1.01) # generate a few more permutations than necessary because some will be duplicates

  #sub-function to generate a group of unique permutations
  bulk_generate <- function() {
    # a sub-sub-function to return a sorted randomly generated list of indices
    sample_sort <- function(max_index) {
      sample <- sample.int(max_index, size = number_minority) # get number_minority values between 1 and number_observations
      return(sort2(sample)) # return a sorted list of indices
    }

    observations_per_permutation <- rep(number_observations, to_generate) # get a list of the number of observations per each new permutation (they will all be the same)
    new_permutations <- sapply(observations_per_permutation, sample_sort, USE.NAMES = FALSE, simplify = FALSE)
    return(unique(c(permutations, new_permutations))) # add newly generated permutations to previously generated ones
  }

  permutations <- bulk_generate() # should return almost number_permutations distinct permutations

  to_generate <- ceiling(number_permutations * 0.10) # keep adding 10% to permutation list until proper length

  while ((length(permutations) < maximum_permutations) & (length(permutations) < (number_permutations + 1))) {
    permutations <- bulk_generate() #generate to_generate permutations at once
  }

  #remove original observations and any extra permutations
  permutations <- permutations[2:(min(number_permutations, (maximum_permutations - 1)) + 1)]

  #make and return matrix of permutations
  permutations_matrix <- matrix(majority, ncol=number_observations, nrow = length(permutations)) # initially fill entirely with majority element
  for (permutation in 1:length(permutations)) {
    permutations_matrix[permutation,permutations[[permutation]]] <- minority # set all indices in permutation to minority element in this row
  }

  return(permutations_matrix)
}

#-------------------------------------------------------------------------
#                            Wright Method
#-------------------------------------------------------------------------

#' Fast pseudorandom unique permutation generation
#'
#' Called when \code{uniqueperm::get_unique_perms(method = "pseudo")}. It is the
#' fastest method in the package, but due to the way that permutations are
#' selected is not strictly random. If the permutations generated don't need to
#' be distinct from another set generated at a different time, use this option.
#'
#' @param original an integer (0/1 only) or logical vector representing the
#'   statuses of the true observations.
#' @param number_permutations integer; desired number of permutations to find.
#'
#' @return A 0/1 matrix with \code{number_permutations} rows and
#'   \code{length(original)} columns. If there are fewer possible permutations
#'   than are requested, an error may be thrown.
#'
#' @details This method is a helper function not meant for general use. Use
#'   \code{get_unique_perms} instead, as it has more parameter checking and
#'   safeguards.
#'
#' @examples
#' example_states <- c(0,0,0,0,1,1,1,1)
#' pseudorandom_generation(example_states, 34)
#' get_unique_perms(example_states, 34, method = "pseudo")
#'
#' example_states <- c(rep(1, 500), rep(0,500))
#' pseudorandom_generation(example_states, 1000)
#'
#' @author Sage Wright, Samantha Jensen
pseudorandom_generation <- function() {

}

#-------------------------------------------------------------------------
#                            Tatlow Method
#-------------------------------------------------------------------------

#' Space efficient unique permutation generation
#'
#' Called when \code{uniqueperm::get_unique_perms(method = "bitset")}. It is the
#' most space efficient method in the package, but slower than the others. If
#' you are attempting to generate a lot of permutations with limited disc space,
#' use this method.
#'
#' @param original an integer (0/1 only) or logical vector representing the
#'   statuses of the true observations.
#' @param number_permutations integer; desired number of permutations to find.
#'
#' @return A 0/1 matrix with \code{number_permutations} rows and
#'   \code{length(original)} columns. If there are fewer possible permutations
#'   than are requested, an error may be thrown.
#'
#' @details This method is a helper function not meant for general use. Use
#'   \code{get_unique_perms} instead, as it has more parameter checking and
#'   safeguards.
#'
#'   If you are getting errors while using this method it is likely because this
#'   method has a C++ implementation. Check to make sure that you have the
#'   necessary tools to compile C++ code.
#'
#' @examples
#' example_states <- c(0,0,0,0,1,1,1,1)
#' bitset_generation(example_states, 34)
#' get_unique_perms(example_states, 34, method = "bitset")
#'
#' example_states <- c(rep(1, 500), rep(0,500))
#' bitset_generation(example_states, 1000)
#'
#' @author PJ Tatlow, Samantha Jensen
bitset_generation <- function(original, number_permutations) {

}
