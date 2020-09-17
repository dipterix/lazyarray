#ifndef DIP_LAZYARRAY_SCHEDULE_H
#define DIP_LAZYARRAY_SCHEDULE_H

#include "common.h"

class ScheduledIndex {
  
public:
  
  ScheduledIndex() {
    block_indexed = false;
  }
  
  ScheduledIndex(
    const bool _block_indexed,
    const std::vector<int64_t> _dimension,
    const std::vector<std::pair<std::vector<int64_t>, bool>> _block_location,
    const std::vector<int64_t>& _partition_index,
    const std::vector<int64_t> _schedule_index,
    const std::vector<int64_t> _schedule_dimension,
    const std::vector<int64_t> _block_dimension,
    const std::vector<int64_t> _block_schedule,
    const int64_t _block_schedule_start,
    const int64_t _block_schedule_end
  );
  
  ScheduledIndex(SEXP locations, const std::vector<int64_t>& dim, bool forceSchedule = false, int64_t hint = -1);
  
  ~ScheduledIndex(){
#ifdef LAZYARRAY_DEBUG
    print(wrap("A ScheduledIndex is destroyed"));
#endif
  }
  
  bool block_indexed;                   // whether block_schedule can be trusted
  std::vector<int64_t> dimension;
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
  
  std::vector<std::pair<std::vector<int64_t>, bool>> block_location;           // subset of locational indices of blocks
  
  
  Rcpp::List asList();
};

class ParsedIndex {
  
public:
  ParsedIndex(
    const int& subset_mode, const std::vector<int64_t>& target_dimension,
    const std::vector<bool>& negative_subscript,
    const std::vector<std::pair<std::vector<int64_t>, bool>>& location_indices,
    ScheduledIndex* schedule
  ):
  subset_mode(subset_mode), 
  target_dimension(target_dimension),
  negative_subscript(negative_subscript), 
  location_indices(location_indices),
  schedule(schedule) {
    expected_length = std::accumulate(target_dimension.begin(), target_dimension.end(), INTEGER64_ONE, std::multiplies<int64_t>());
  }
  
  ~ParsedIndex(){
    if(this->schedule != nullptr){
      delete schedule;
      this->schedule = nullptr;
    }
#ifdef LAZYARRAY_DEBUG
    print(wrap("A ParsedIndex is destroyed"));
#endif
  }
  
  ParsedIndex(const SEXP listOrEnv, const std::vector<int64_t>& dim, bool pos_subscript);
  
  
  int subset_mode;
  std::vector<int64_t> target_dimension;
  std::vector<bool> negative_subscript;
  std::vector<std::pair<std::vector<int64_t>, bool>> location_indices;
  
  int64_t expected_length;
  ScheduledIndex *schedule;
  
  Rcpp::List asList();
  
};


#endif // DIP_LAZYARRAY_SCHEDULE_H
