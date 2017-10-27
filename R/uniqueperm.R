#DOCUMENTATION FOR THE PACKAGE

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

#FUNCTIONS FOR LOADING AND UNLOADING THE PACAKGE

#make sure that all C++ stuff gets unloaded properly
.onUnload <- function (libpath) {
  library.dynam.unload("uniqueperm", libpath)
}
