// [[Rcpp::plugins(cpp11)]]`
#include <unordered_set>
#include <RcppArmadillo.h> // this is a slightly different Rcpp library that provides slightly faster Rcpp data structures
//[[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::mat bitset_permutation(Rcpp::LogicalVector x, unsigned int ones, unsigned long permutations) {
	// set total to the size of the given input vector
  unsigned int total = x.size();

  // here we create a bitset based on the vector passed in to make sure
  // that we don't duplicate it later, as well as count the number of ones
  // in the given input vector "x"
  //unsigned int ones = 0;
	//std::bitset<n> given;
	std::vector<bool> given(total);

	for(int i = 0; i < x.size(); i++) {
        given[i] = x[i];
  }

	GetRNGstate(); // we have to use R's random number interface to avoid package issues

  // create the set of bitsets
	std::unordered_set<std::vector<bool> > sets;

  // put in the initial vector provided by the user (we'll remove it later)
  sets.insert(given);

  // initalize the bitset we'll use to create the permutations
  std::vector<bool> bits(total);

  // until we've created enough permutations, keep looping.
  // enough permutations is either the maximum number of permutations
  // or the number of permutations requested
  while (sets.size() < (permutations + 1)) {

	      // generate list of indices to flip
        std::unordered_set<int> to_flip;
        while(to_flip.size() < ones){
            to_flip.insert((int)norm_rand() % total);
        }

        for(const auto& index : to_flip){
            bits[index].flip();
        }

        // put the bitset into the set
        sets.insert(bits);
        // set all the bits to zero
        bits.assign(total, false);
  }

  PutRNGstate(); // we're done with R's random number interface

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

