void Tree::bootstrap_moving_block() {

// Use fraction (default 63.21%) of the samples
  size_t num_samples_inbag = (size_t) num_samples * (*sample_fraction)[0];

// Reserve space, reserve a little more to be save)
  sampleIDs.reserve(num_samples_inbag);
  oob_sampleIDs.reserve(num_samples * (exp(-(*sample_fraction)[0]) + 0.1));

  std::uniform_int_distribution<size_t> unif_dist(0, num_samples - block_size);

// Start with all samples OOB
// TODO need to add parameter to Tree class : bool activate_ts, and block_size
  size_t k = ceil(num_samples_inbag / block_size);
  inbag_counts.resize(num_samples, 0);

// Draw num_samples samples with replacement (num_samples_inbag out of n) as inbag and mark as not OOB
  for (size_t s = 0; s < k; ++s) {
    size_t draw = unif_dist(random_number_generator);
    // loop to take the selected block
    // stop when the inbag sample is full
    for (size_t i = 0; i < block_size; ++i) {
      if (sampleIDs.size() < num_samples_inbag) {
        sampleIDs.push_back(draw + i);
        ++inbag_counts[draw + i];
      }
    }
  }

// Save OOB samples
  for (size_t s = 0; s < inbag_counts.size(); ++s) {
    if (inbag_counts[s] == 0) {
      oob_sampleIDs.push_back(s);
    }
  }
  num_samples_oob = oob_sampleIDs.size();

  if (!keep_inbag) {
    inbag_counts.clear();
    inbag_counts.shrink_to_fit();
  }
}
