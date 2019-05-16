void permuteByBlock(std::vector<size_t>& permutations, int block_size, uint seed) {
  std::mt19937_64 random_number_generator;
  random_number_generator.seed(seed);
  size_t block_oob_size = permutations.size();
  size_t num_block = (size_t) floor((double) block_oob_size / block_size);
  // initialization
  std::vector<size_t> index_block(num_block);
  std::iota(index_block.begin(), index_block.end(), 0);

  std::vector<size_t> permuted;
  permuted.reserve(block_oob_size);

  if (num_block > 1) {
    std::shuffle(index_block.begin(), index_block.end(), random_number_generator);
    // build the permutations vector
    size_t index_block_size = index_block.size();

    for (size_t j = 0; j < index_block_size; ++j) {
      for (size_t m = 0; m < block_size; ++m) {
        permuted.push_back(permutations[index_block[j] * block_size + m]);
      }
    }
    permuted.shrink_to_fit();
    permutations = permuted;
    permuted.clear();
  }
}
