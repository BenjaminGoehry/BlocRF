// test_nonoverlapping.cpp
#include <cmath>
#include <iostream>
#include <vector>
#include "globals.h"
#include "bootstrap_moving_block.cpp"
#include "bootstrap_circular_block.cpp"
#include "bootstrap_non_overlapping_block.cpp"
#include "bootstrap_stationary_block.cpp"

using namespace std;

int main() {
  // general parameters
  int num_samples = 200;
  double sample_fraction = 0.133;
  int block_size = 10;
  uint seed = 2;
  bool sample_rep = false;

  // initialise necessary variables
  std::vector<size_t> oob_sampleIDs;
  std::vector<size_t> inbag_counts;

  std::vector<size_t> sampleIDs = bootstrap_non_overlapping_block(seed, num_samples, sample_fraction, block_size, oob_sampleIDs, inbag_counts, sample_rep);
  //std::vector<size_t> sampleIDs = bootstrap_circular_block(seed, num_samples, sample_fraction, block_size, oob_sampleIDs, inbag_counts);
  //std::vector<size_t> sampleIDs = bootstrap_stationary_block(seed, num_samples, sample_fraction, block_size, oob_sampleIDs, inbag_counts);
  //std::vector<size_t> sampleIDs = bootstrap_moving_block(seed, num_samples, sample_fraction, block_size, oob_sampleIDs, inbag_counts);
  //size_t size = bootstrap_stationary_block(seed, num_samples, sample_fraction, block_size, oob_sampleIDs, inbag_counts);

  /*
  std::cout << "inbag_counts contains:";
  for (size_t i=0;i < inbag_counts.size();i++)
    std::cout << ' ' << inbag_counts[i];
  std::cout << '\n';

  std::cout << "oob_sampleIDs contains:";
  for (size_t i=0;i < oob_sampleIDs.size();i++)
    std::cout << ' ' << oob_sampleIDs[i];
  std::cout << '\n';
  */

  std::cout << "sampleIDs contains:";
  for (size_t i=0;i < sampleIDs.size();++i)
    std::cout << ' ' << sampleIDs[i];
  //std::cout << sampleIDs.size();
  //std::cout << size;
  std::cout << '\n';

  return 0;
}
