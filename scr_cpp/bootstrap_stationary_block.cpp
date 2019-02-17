#include <random>
//size_t  bootstrap_stationary_block(uint seed, int num_samples, double sample_fraction, int block_size, std::vector<size_t> oob_sampleIDs, std::vector<size_t> inbag_counts) {
std::vector<size_t>  bootstrap_stationary_block(uint seed, int num_samples, double sample_fraction, int block_size, std::vector<size_t> oob_sampleIDs, std::vector<size_t> inbag_counts) {
  std::mt19937_64 random_number_generator;
  random_number_generator.seed(seed);
// Use fraction (default 63.21%) of the samples
  size_t num_samples_inbag = (size_t) num_samples * sample_fraction;

// Reserve space, reserve a little more to be save)
  std::vector<size_t> sampleIDs;
  sampleIDs.reserve(num_samples_inbag);
  oob_sampleIDs.reserve(num_samples * (exp(-1 * sample_fraction) + 0.1));

  std::uniform_int_distribution<size_t> unif_dist(0, num_samples - 1);
  std::uniform_int_distribution<double> unif_dist_bis(0, 1);
  // TODO the probability should be adjusted manually by user
  std::geometric_distribution<size_t> geom_dist(1.0 / block_size);

// Start with all samples OOB
  inbag_counts.resize(num_samples, 0);

// Draw num_samples samples with replacement (num_samples_inbag out of n) as inbag and mark as not OOB
  //double proba = 1.0 / block_size;
   while (sampleIDs.size() < num_samples_inbag) {
    size_t draw = unif_dist(random_number_generator);
    size_t len = geom_dist(random_number_generator);
    // loop to take the selected block
    for (size_t i = 0; i < len; ++i) {
      // same idea as circular
      size_t ind = (size_t) (draw + i) % num_samples;
      if (sampleIDs.size() < num_samples_inbag) {
        sampleIDs.push_back(ind);
        ++inbag_counts[ind];
      }
    }
  }

// Save OOB samples
  for (size_t s = 0; s < inbag_counts.size(); ++s) {
    if (inbag_counts[s] == 0) {
      oob_sampleIDs.push_back(s);
    }
  }
//  num_samples_oob = oob_sampleIDs.size();
  //return(geom_dist(random_number_generator));
  return(sampleIDs);
}
