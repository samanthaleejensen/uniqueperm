#-------------------------------------------------------------------------
#                       Package Documentation
#-------------------------------------------------------------------------

#'uniqueperm: A package for efficient unique random permutation of case-control
#'data.
#'
#' Biologists often depend on statistical methods that use random
#' permutations to quantify significance. These methods produce an empirical null
#' distribution by repeatedly permuting class labels and performing the
#' statistical test; the results obtained using the actual class labels are then
#' compared against the empirical null distribution. To avoid biases in
#' performing such calculations, the permutations should be sampled without
#' replacement. However, existing software packages sample with replacement due
#' to the computational complexity of ensuring that permuted vectors are unique.
#' Phipson and Smyth have demonstrated that sampling with replacement often
#' results in empirical p-values that are understated, thus increasing the type I
#' error rate. To date, no computationally efficient method for ensuring the
#' uniqueness of permutation vectors is widely used. To address this problem, we
#' used a crowd-sourcing approach, extending the challenge of creating an
#' efficient algorithm for generating unique permutations to 26 undergraduate
#' students in Brigham Young University’s capstone bioinformatics class. Analysis
#' of the time and space efficiency of these methods led us to identify three
#' distinct algorithms that each can generate 1,000,000 unique permutations of a
#' binary vector of 1000 observations in around 30 seconds. This package is the
#' implementation of the best of these algorithms, which utilizes C++ bitsets to
#' decrease memory load and \code{random_shuffle} to increase speed.
#'
#//#'@section Permutation with and without replacement: words
#//#' //#'@sectionImportance of accurate _p_-values: also words
#'@references Phipson, B. & Smyth, G. K. Permutation P-values should never be
#'  zero: calculating exact P-values when permutations are randomly drawn.
#'  _Stat. Appl. Genet. Mol. Biol._ __9__, 39 (2010).
#'@docType package
#'@name uniqueperm
NULL

#-------------------------------------------------------------------------
#                     Loading and Unloading Package
#-------------------------------------------------------------------------

#make sure that all C++ stuff gets unloaded properly
.onUnload <- function (libpath) {
  library.dynam.unload("uniqueperm", libpath)
}

#-------------------------------------------------------------------------
#                         C++ Interface Method
#-------------------------------------------------------------------------

#This is the main function of the package and the only function that users
#should be calling. It figures out the maximum number of possible permutations
#and then calls the C++ function bitsetpermute.

#'Space efficient unique permutation generation
#'
#'Efficiently generate truly unique and random permutations of binary data in R.
#'
#'@param original a string, integer, or logical vector representing the labels
#'  to be permuted (currently we are only able to handle two factor levels).
#'@param number_permutations integer; desired number of permutations to find.
#'
#'@return A 0/1 matrix with \code{number_permutations} rows and
#'  \code{length(original)} columns. If there are fewer possible permutations
#'  than are requested, will return as many as possible.
#'
#'@details This function uses C++ bitsets to decrease memory load and
#'  \code{random_shuffle} to increase speed. It is capable of generating
#'  1,000,000 unique permutations of a 1000 observation binary vector in around
#'  30 seconds.
#'
#'  If you are getting errors while using this method it is likely because this
#'  method has a C++ implementation. Check to make sure that you have the
#'  necessary tools to compile C++ code.
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
#'@author PJ Tatlow, Samantha Jensen
#'
#'@useDynLib uniqueperm
#'@importFrom Rcpp sourceCpp
#'
#'@export
permute <- function(original, number_permutations) {

  #check number of factors
  original <- as.factor(original)

  if(length(levels(original)) != 2)
  {
    print("This package is currently capable only of permuting classifications with only two possible levels. Please drop a level to continue.")
  }
  else
  {
    #convert to binary format
    zero <- levels(original)[1]
    one <- levels(original)[2]

    original_bin <- original == one

    # checking that generating requested permutations is possible
    maximum_permutations <- choose(length(original), min(table(original)))# determine max possible permutations

    if (number_permutations > maximum_permutations - 1) { # if not possible, do as many as possible.
      number_permutations = maximum_permutations - 1
    }

    permutations <-bitsetpermute(original_bin, number_permutations) # calling C++ method, which will keep going until all number_permutations are found

    #replace original labels
    permutations[permutations==0] <- zero
    permutations[permutations==1] <- one

    #return as a list not a matrix
    return(convertRowsToList(permutations))
  }
}

