// [[Rcpp::plugins(cpp11)]]`
#include <unordered_set>
#include <RcppArmadillo.h> // this is a slightly different Rcpp library that provides faster Rcpp data structures
//[[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::mat bitsetpermute(Rcpp::LogicalVector original, unsigned long number_permutations) {
	// set total to the size of the given input vector
  unsigned int total = original.size();

  // here we create a bitset based on the vector passed in to make sure
  // that we don't duplicate it later, as well as count the number of ones
  // in the given input vector "original"
  //unsigned int ones = 0;
	//std::bitset<n> given;
	std::vector<bool> given(total);

	for(int i = 0; i < original.size(); i++) {
        given[i] = original[i];
  }

  // create the set of bitsets
	std::unordered_set<std::vector<bool> > sets;

  // put in the initial vector provided by the user (we'll remove it later)
  sets.insert(given);

  // initalize the bitset we'll use to create the permutations
  std::vector<bool> bits = given;

  // until we've created enough permutations, keep looping.
  // enough permutations is either the maximum number of permutations
  // or the number of permutations requested
  while (sets.size() < (number_permutations + 1)) {

        Rcpp::checkUserInterrupt(); // makes it so that we can exit out while executing from R

	      std::random_shuffle(bits.begin(), bits.end());

        // put the bitset into the set
        sets.insert(bits);
  }

  // now that we've generated all our permutations, remove the original
  sets.erase(given);

  // create an R matrix initialized to zeros, the proper size that we need
  arma::mat matrix = arma::zeros(sets.size(),total);

  // iterate through every element in the set
  std::unordered_set< std::vector<bool> >::iterator it = sets.begin();
  for (unsigned int i=0; i < sets.size(); i++) {
      // for each bit in the bitset, set the value in the matrix to the value of the bit
      for (unsigned int j=0; j < total; j++) {
          int bit = (int) it->operator[](j);
          matrix(i,j) = bit;
      }
      ++it;
  }
  return matrix;
}

