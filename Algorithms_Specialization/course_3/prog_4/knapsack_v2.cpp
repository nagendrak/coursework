#include <iostream>
#include <cassert>
#include <fstream>
#include <sstream>
#include <vector>
#include <cstring>

#define MAX(a,b) a >= b ? a : b
#define MIN(a,b) a <= b ? a : b

struct sITEM {
  long value;
  long weight;
};

struct sCHANGE {
  long total_value;
  long x;
};

long get_entry(std::vector<sCHANGE>& last_i_changes, long x) {
  int i;
  for (i = 1; i < last_i_changes.size(); i++) {
    if (last_i_changes[i-1].x <= x && last_i_changes[i].x > x)
      break;
  }
  return last_i_changes[i-1].total_value;
}

long next_probable_change_x(std::vector<sCHANGE>& last_i_changes, long this_weight, long old_x,
    long knapsack_size) {
  long next_x = knapsack_size + 2;

  for (auto& change: last_i_changes) {
    long min_valid_next_x = change.x - this_weight;
    if (min_valid_next_x <= old_x)
      min_valid_next_x = change.x;
    if (min_valid_next_x <= old_x)
      min_valid_next_x = change.x + this_weight;
    if (min_valid_next_x <= old_x)
      min_valid_next_x = knapsack_size + 2;

    next_x = MIN(next_x, min_valid_next_x);
  }

  if (next_x == old_x || next_x > knapsack_size + 1)
    next_x = knapsack_size + 1;

  return next_x;
}

int main(const int argc, const char* argv[]) {
  assert(argc == 2);
  std::string input_filename = argv[1];

  std::ifstream input_file(input_filename);

  std::string line;
  long knapsack_size;
  long total_num_items;
  std::vector<sITEM> items;

  if (input_file.is_open()) {
    if (getline(input_file, line)) {
      std::istringstream iss_header(line);
      iss_header >> knapsack_size >> total_num_items;

      items.resize(total_num_items + 1);
      long i = 1;

      while (getline(input_file, line)) {
        std::istringstream iss_item(line);
        iss_item >> items[i].value >> items[i].weight;
        i++;
      }
    }
  }

  std::vector<sCHANGE> last_i_change;
  sCHANGE first;
  first.total_value = 0;
  first.x = 0;
  last_i_change.push_back(first);

  for (long i = 1; i <= total_num_items; i++) {
    std::vector<sCHANGE> this_i_change;

    long weight_i = items[i].weight;
    long value_i = items[i].value;

    long x = 0;
    long last_entry_this_i = -1;

    while (x != knapsack_size + 1) {
      long new_entry = x >= weight_i ? get_entry(last_i_change, x - weight_i) + value_i : 0;
      long new_total_value = MAX(get_entry(last_i_change, x), new_entry);

      if (last_entry_this_i != new_total_value) {
        last_entry_this_i = new_total_value;
        sCHANGE new_change;
        new_change.total_value = new_total_value;
        new_change.x = x;
        this_i_change.push_back(new_change);
      }

      x = next_probable_change_x(last_i_change, weight_i, x, knapsack_size);
    }

    last_i_change = this_i_change;
  }

  std::cout << last_i_change[last_i_change.size() - 1].total_value << "\n";

  return 0;
}
