#---------------------MAIN METHODS---------------------#

#-------------------------------------------------------------------------
#                         User Interface Method
#-------------------------------------------------------------------------

#' Flexible Unique, Random Permutation Method Caller
#'
#' Efficiently generate truly unique and random permutations of binary data in
#' R.
#'
#' @param original an integer (0/1 only) or logical vector representing the
#'   statuses of the true observations.
#' @param number_permutations integer; desired number of permutations to find.
#' @param fast \code{TRUE}/\code{FALSE}; whether or not speed of permutation generation should
#'   be prioritized. Defaults to \code{TRUE}. Will be ignored if \code{method} is
#'   specified or if \code{truly_random = TRUE}.
#' @param memory_efficient \code{TRUE}/\code{FALSE}; whether or not memory efficiency should
#'   be prioritized. Defaults to \code{TRUE}. Will be ignored if \code{method} is
#'   specified or if \code{fast = TRUE}.
#' @param truly_random \code{TRUE}/\code{FALSE}; whether we care if permutations are truly
#'   randomly generated. Defaults to \code{TRUE}. Will be ignored if \code{method} is
#'   specified or if \code{fast = FALSE}.
#' @param method which type of algorithm to run; options are "default",
#'   "pseudo", and "bitset". Defaults to "default".
#'
#' @return A 0/1 matrix with \code{number_permutations} rows and
#'   \code{length(original)} columns. If there are fewer possible permutations
#'   than are requested, only as many as are possible will be generated.
#'
#' @details
#'
#' This method is the only method in the package that should be called by users.
#' Includes options for memory use, time efficiency, and randomization that will
#' determine the method used. You can also explicitly pick the method you'd like
#' to use, with the \code{method} parameter.
#'
#' \strong{Methods}
#'
#' This package contains three methods for unique permutation generation:
#' \describe{
#'  \item{\code{\link{fast_generation}}}{Default method. Relatively
#'   fast and memory efficient.}
#'  \item{\code{\link{bitset_generation}}}{Most
#'   memory efficient method, slower than the others. Call with \code{method =
#'   "bitset"}.}
#'  \item{\code{\link{pseudorandom_generation}}}{Fastest method, not
#'   truly random. Call with \code{method = "pseudo"}.} }
#'
#'
#' \strong{Automatic method selection}
#'
#' There is no one method that is fast, memory efficient, and truly random. If
#' you are not sure which method will work best for you, you can choose which of
#' these factors matters most to you and indicate such with the parameters
#' \code{fast}, \code{memory_efficient}, and \code{truly_random}. The method will
#' then be automatically selected for you.
#'
#' The below chart represents the combination of parameters that will trigger
#' which method:
# has a lot of tabs to make spacing reasonable...
#' \tabular{lllllllllllllllllllllllllllllllllll}{
#'             \tab \tab \emph{\code{fast}}   \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \emph{\code{memory_efficient}}  \tab \tab \emph{\code{truly_random}} \tab \tab \tab \tab \tab \tab \tab \tab \tab \emph{\code{method}} \tab \tab \tab \tab \tab \emph{calls}                          \cr
#'  \strong{1} \tab \tab \code{TRUE}          \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \code{TRUE}                     \tab \tab \code{TRUE}               \tab \tab \tab \tab \tab \tab \tab \tab \tab "default"            \tab \tab \tab \tab \tab \code{\link{fast_generation}}         \cr
#'  \strong{2} \tab \tab \code{TRUE}          \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \code{TRUE}                     \tab \tab \code{FALSE}              \tab \tab \tab \tab \tab \tab \tab \tab \tab "pseudo"             \tab \tab \tab \tab \tab \code{\link{pseudorandom_generation}} \cr
#'  \strong{3} \tab \tab \code{TRUE}          \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \code{FALSE}                    \tab \tab \code{TRUE}               \tab \tab \tab \tab \tab \tab \tab \tab \tab "default"            \tab \tab \tab \tab \tab \code{\link{fast_generation}}         \cr
#'  \strong{4} \tab \tab \code{FALSE}         \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \code{TRUE}                     \tab \tab \code{TRUE}               \tab \tab \tab \tab \tab \tab \tab \tab \tab "bitset"             \tab \tab \tab \tab \tab \code{\link{bitset_generation}}       \cr
#'  \strong{5} \tab \tab \code{TRUE}          \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \code{FALSE}                    \tab \tab \code{FALSE}              \tab \tab \tab \tab \tab \tab \tab \tab \tab "pseudo"             \tab \tab \tab \tab \tab \code{\link{pseudorandom_generation}} \cr
#'  \strong{6} \tab \tab \code{FALSE}         \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \code{TRUE}                     \tab \tab \code{FALSE}              \tab \tab \tab \tab \tab \tab \tab \tab \tab "bitset"             \tab \tab \tab \tab \tab \code{\link{bitset_generation}}       \cr
#'  \strong{7} \tab \tab \code{FALSE}         \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \code{FALSE}                    \tab \tab \code{TRUE}               \tab \tab \tab \tab \tab \tab \tab \tab \tab "default"            \tab \tab \tab \tab \tab \code{\link{fast_generation}}         \cr
#'  \strong{8} \tab \tab \code{FALSE}         \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \code{FALSE}                    \tab \tab \code{FALSE}              \tab \tab \tab \tab \tab \tab \tab \tab \tab "default"            \tab \tab \tab \tab \tab \code{\link{fast_generation}}         }
#' Note that setting the \code{method} parameter will override any other settings.
#'
#' @examples
#' example_states <- c(0,0,0,0,1,1,1,1)
#'
#' #default settings
#' get_unique_perms(example_states, 34) # runs default method (relatively fast and memory efficient)
#'
#' #AUTOMATICALLY DEFINING METHOD
#'
#' #same as default
#' get_unique_perms(example_states, 34, memory_efficient = FALSE)
#' get_unique_perms(example_states, 34, fast = FALSE, memory_efficient = FALSE)
#' get_unique_perms(example_states, 34, fast = FALSE)
#' get_unique_perms(example_states, 34, fast = FALSE, memory_efficient = FALSE)
#'
#' #triggers pseudorandom generation method
#' get_unique_perms(example_states, 34, truly_random = FALSE)
#' get_unique_perms(example_states, 34, memory_efficient = FALSE, truly_random = FALSE)
#'
#' #triggers bitset generation method
#' get_unique_perms(example_states, 34, fast = FALSE)
#' get_unique_perms(example_states, 34, fast = FALSE, truly_random = FALSE)
#'
#' #EXPLICITLY DEFINING METHOD
#'
#' get_unique_perms(example_states, 34, method = "default") # specify default
#' get_unique_perms(example_states, 34, method = "pseudo") # fast pseudorandom method
#' get_unique_perms(example_states, 34, method = "bitset") # memory efficient method
#'
#' #TIMING AND MEMORY USAGE EXAMPLES
#'
#' library(R.utils) # for timing
#' library(profmem) # for memory usage information
#'
#' example_states <- c(rep(1, 500), rep(0,500)) #larger dataset
#'
#' #default
#' ptm <- proc.time()
#' get_unique_perms(example_states, 1000)
#' print(proc.time() - ptm)
#'
#' print(total(profmem(get_unique_perms(example_states, 1000))))
#'
#' #pseudo
#' ptm <- proc.time()
#' get_unique_perms(example_states, 1000, method = "pseudo")
#' print(proc.time() - ptm)
#'
#' print(total(profmem(get_unique_perms(example_states, 1000, method = "pseudo"))))
#'
#' #bitset
#' ptm <- proc.time()
#' get_unique_perms(example_states, 1000, method = "bitset")
#' print(proc.time() - ptm)
#'
#' print(total(profmem(get_unique_perms(example_states, 1000, method = "bitset"))))
#'
#' @author Dr. Stephen Piccolo, Samantha Jensen
#' @export
get_unique_perms <- function(original, number_permutations, fast = TRUE, memory_efficient = TRUE, truly_random = TRUE, method = "default") {

  # checking that generating requested permutations is possible
  maximum_permutations <- choose(length(original), min(table(original)))# determine max possible permutations

  # set random number generator seeds to different number
  set.seed(Sys.time())

  if (number_permutations > maximum_permutations - 1) { # if not possible, do as many as possible.
    number_permutations = maximum_permutations - 1
  }

  if (method == "pseudo") {
    return(pseudorandom_generation(original, number_permutations))
  }
  else if (method == "bitset") {
    return(bitset_generation(original, number_permutations))
  }
  else if (memory_efficient & !fast){
    return(bitset_generation(original, number_permutations))
  }
  else if (fast & !truly_random){
    return(pseudorandom_generation(original, number_permutations))
  }
  else {
    return(fast_generation(original, number_permutations))
  }
}
