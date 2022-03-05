#include <iostream>
#include <cassert>
#include <fstream>
#include <sstream>
#include <vector>

#define MAX(a,b) a >= b ? a : b
struct sITEM {
  long value;
  long weight;
};

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

  long table[total_num_items + 1][knapsack_size + 1] = {};
  for (long i = 1; i <= total_num_items; i++) {
    long weight_i = items[i].weight;
    long value_i = items[i].value;

    for (long x = 0; x <= knapsack_size; x++) {
      long new_entry = x >= weight_i ? table[i - 1][x - weight_i] + value_i : 0;
      table[i][x] = MAX(table[i - 1][x], new_entry);
    }
  }

  std::cout << table[total_num_items][knapsack_size] << "\n";

  return 0;
}
