#ifndef DIP_LAZYARRAY_SCHEDULE_H
#define DIP_LAZYARRAY_SCHEDULE_H

#include "common.h"

class ScheduledIndex {
  
public:
  ScheduledIndex(
    const bool& block_indexed,
    const std::vector<std::vector<int64_t>>& block_location,
    const std::vector<int64_t>& partition_index,
    const std::vector<int64_t>& schedule_index,
    const std::vector<int64_t>& schedule_dimension,
    const std::vector<int64_t>& block_dimension,
    const std::vector<int64_t>& block_schedule,
    const int64_t& block_schedule_start,
    const int64_t& block_schedule_end
  ):
  block_indexed(block_indexed),
  block_location(block_location),
  partition_index(partition_index),
  schedule_index(schedule_index),
  schedule_dimension(schedule_dimension),
  block_dimension(block_dimension),
  block_schedule(block_schedule)
  {
    partition_counts = partition_index.size();
    schedule_counts_per_part = schedule_index.size();
    block_ndims = block_dimension.size();
    
    block_prod_dim = std::vector<int64_t>(block_ndims, 1);
    block_expected_length = 1;
    
    for(R_xlen_t ii = 1; ii < block_ndims; ii++){
      block_prod_dim[ii] = block_prod_dim[ii-1] * block_dimension[ii - 1];
      block_expected_length *= (block_location[ii]).size();
    }
    block_length = block_prod_dim[block_ndims-1] * block_dimension[block_ndims-1];
    
    
  }
  
  bool block_indexed;                   // whether block_schedule can be trusted
  // partition level
  int64_t partition_counts;               // the last dimension - n files to iterate through
  std::vector<int64_t> partition_index;    // detailed indexes   - always exists
  
  // schedule level
  int64_t schedule_counts_per_part; // for each partition, number of blocks to run
  std::vector<int64_t> schedule_index;      // indices to schedule run blocks
  std::vector<int64_t> schedule_dimension; // [schedule dim, partition counts]
  
  // block level
  int64_t block_ndims;                    // length(block dim)
  std::vector<int64_t> block_dimension; // [block dim], full version
  std::vector<int64_t> block_prod_dim; // prod([1, block dim]), used to locate indices when block is too large to index
  std::vector<int64_t> block_schedule; // given a flattened block (full version), which indices to subset?
  int64_t block_schedule_start;
  int64_t block_schedule_end;      // min, max of block_schedule
  
  int64_t block_length;                  // # elements in a block (full version) = prod(block_dimension)
  int64_t block_expected_length;// # elements in a block (subset version) = length(block_schedule)
  
  std::vector<std::vector<int64_t>> block_location;           // subset of locational indices of blocks
  
};

class ParsedIndex {
  
public:
  ParsedIndex(
    const int& subset_mode, const std::vector<int64_t>& target_dimension,
    const std::vector<bool>& negative_subscript,
    const std::vector<std::vector<int64_t>>& location_indices,
    const ScheduledIndex& schedule
  ):
  subset_mode(subset_mode), 
  target_dimension(target_dimension),
  negative_subscript(negative_subscript), 
  location_indices(location_indices),
  schedule(schedule) {
    expected_length = std::accumulate(target_dimension.begin(), target_dimension.end(), INTEGER64_ONE, std::multiplies<int64_t>());
  }
  
  
  int subset_mode;
  std::vector<int64_t> target_dimension;
  std::vector<bool> negative_subscript;
  std::vector<std::vector<int64_t>> location_indices;
  
  int64_t expected_length;
  ScheduledIndex schedule;
  
  
  
};
  
  
#endif // DIP_LAZYARRAY_SCHEDULE_H
