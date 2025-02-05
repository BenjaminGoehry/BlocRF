#include <random>
std::vector<size_t> bootstrap_seasonal_block(uint seed, int num_samples, double sample_fraction, int block_size, std::vector<size_t> oob_sampleIDs, std::vector<size_t> inbag_counts, bool sample_with_replacement, int seasonality) {
  std::mt19937_64 random_number_generator;
  random_number_generator.seed(seed);
// Use fraction (default 63.21%) of the samples
  size_t num_samples_inbag = (size_t) num_samples * sample_fraction;
  size_t num_seasonality = (size_t) ceil((double) num_samples / seasonality);
// Reserve space, reserve a little more to be save)
  std::vector<size_t> sampleIDs;
  sampleIDs.reserve(num_samples_inbag);
  oob_sampleIDs.reserve(num_samples * (exp( -1 * sample_fraction) + 0.1));

// Start with all samples OOB
  size_t k = (size_t) ceil((double) num_samples_inbag / block_size);
  inbag_counts.resize(num_samples, 0);


if (sample_with_replacement) {
  std::uniform_int_distribution<size_t> unif_dist(0, num_seasonality - 1);
  // Draw num_samples samples with replacement (num_samples_inbag out of n) as inbag and mark as not OOB
    for (size_t s = 0; s < k; ++s) {
      size_t draw = unif_dist(random_number_generator);
      // loop to take the selected block
      // stop when the inbag sample is full
      for (size_t i = 0; i < block_size; ++i) {
        size_t ind = draw * seasonality + i;
        if (sampleIDs.size() < num_samples_inbag && ind < num_samples) {
          sampleIDs.push_back(ind);
          ++inbag_counts[ind];
        }
      }
    }
} else {
  // initialise block index vector, fill it from 0 to num_seasonality - 1
  std::vector<size_t> index_season(num_seasonality);
  std::iota(index_season.begin(), index_season.end(), 0);

  // shuffle the block index
  std::shuffle(index_season.begin(), index_season.end(), random_number_generator);
  index_season.resize(k);
  index_season.shrink_to_fit();

  for (auto & s : index_season) {
    for (size_t i = 0; i < block_size; ++i) {
      size_t ind = (size_t) s * seasonality + i;
      if (sampleIDs.size() < num_samples_inbag && ind < num_samples) {
        sampleIDs.push_back(ind);
        ++inbag_counts[ind];
      }
    }
  }
}


// Save OOB samples

  for (size_t s = 0; s < inbag_counts.size(); ++s) {
    if (inbag_counts[s] == 0) {
      oob_sampleIDs.push_back(s);
    }
  }
  size_t num_samples_oob = oob_sampleIDs.size();
  oob_sampleIDs.push_back(num_samples_oob);
  oob_sampleIDs.push_back(inbag_counts.size());
  num_samples_inbag = num_samples - num_samples_oob;
  sampleIDs.resize(num_samples_inbag);
  sampleIDs.shrink_to_fit();
  oob_sampleIDs.push_back(sampleIDs.size());

  return(oob_sampleIDs);
  //return(num_samples_inbag);
}
