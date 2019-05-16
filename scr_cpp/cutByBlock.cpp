void cutByBlock(std::vector<size_t>& oob_sampleIDs_block, int block_size) {
  size_t num_samples_oob = oob_sampleIDs_block.size();
  std::vector<size_t> oob_block;
  oob_block.reserve(num_samples_oob);
  std::vector<size_t> tmp_block;
  tmp_block.reserve(block_size);

  if (num_samples_oob > 1) {
    // build the index vector
    size_t count = 1;
    size_t i = 0;
    while ((i + count) < num_samples_oob) {
      size_t diff = oob_sampleIDs_block[i + count] - oob_sampleIDs_block[i + count - 1];
      if (diff != 1) {
        ++i;
      } else {
        tmp_block.push_back(oob_sampleIDs_block[i + count - 1]);
        ++count;
      }

      if (count == block_size) {
        tmp_block.push_back(oob_sampleIDs_block[i + count - 1]);
        oob_block.insert(oob_block.end(), tmp_block.begin(), tmp_block.end());
        tmp_block.clear();
        count = 1;
        i = i + block_size;
      }
    }
    oob_block.shrink_to_fit();
    oob_sampleIDs_block = oob_block;
    oob_sampleIDs_block.shrink_to_fit();
    oob_block.clear();
  }
}
