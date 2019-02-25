std::vector<size_t> bootstrap_non_overlapping_block(uint seed, int num_samples, double sample_fraction, int block_size, std::vector<size_t> oob_sampleIDs, std::vector<size_t> inbag_counts, bool sample_with_replacement) {
  std::mt19937_64 random_number_generator;
  random_number_generator.seed(seed);

// Use fraction (default 63.21%) of the samples
  size_t num_samples_inbag = (size_t) num_samples * sample_fraction;

// Reserve space, reserve a little more to be save)
  std::vector<size_t> sampleIDs;
  sampleIDs.reserve(num_samples_inbag);
  oob_sampleIDs.reserve(num_samples * (exp(-1 * sample_fraction) + 0.1));
  // Start with all samples OOB
  size_t num_block_inbag = ceil(num_samples_inbag / block_size);
  size_t num_block = ceil(num_samples / block_size);
  inbag_counts.resize(num_samples, 0);

  if (sample_with_replacement) {
    std::uniform_int_distribution<size_t> unif_dist(0, num_block - 1);

    // Draw num_samples samples with replacement (num_samples_inbag out of n) as inbag and mark as not OOB
    for (size_t s = 0; s <= num_block_inbag; ++s) {
      size_t draw = unif_dist(random_number_generator);

      // loop to take the selected block
      for (size_t i = 0; i < block_size; ++i) {
        size_t ind = (size_t) draw * block_size + i;
        if (sampleIDs.size() < num_samples_inbag && ind < num_samples) {
          sampleIDs.push_back(ind);
          ++inbag_counts[ind];
        }
      }
    }
  } else {

    // initialise block index vector, fill it from 0 to num_block - 1
    std::vector<size_t> index_nonoverlap(num_block);
    std::iota(index_nonoverlap.begin(), index_nonoverlap.end(), 0);

    // shuffle the block index
    std::shuffle(index_nonoverlap.begin(), index_nonoverlap.end(), random_number_generator);
    index_nonoverlap.resize(num_block_inbag + 1);
    index_nonoverlap.shrink_to_fit();

    // fill the sampleIDs and inbag_counts
    for (auto & s : index_nonoverlap) {
      for (size_t i = 0; i < block_size; ++i) {
        size_t ind = (size_t) s * block_size + i;
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
  /*
  num_samples_oob = oob_sampleIDs.size();

  if (!keep_inbag) {
    inbag_counts.clear();
    inbag_counts.shrink_to_fit();
  }*/
  return(sampleIDs);
}
