#include <iostream>
#include <cassert>
#include <fstream>
#include <vector>

int main(const int argc, char* argv[]) {
  assert(argc == 2);
  std::string input_filename = argv[1];

  std::ifstream input_file(input_filename);
  long num_vertices;
  std::vector<long> vertex_weights;

  if (input_file.is_open()) {
    std::string line;
    if (getline(input_file, line)) {
      num_vertices = stol(line);
      vertex_weights.resize(num_vertices + 1);

      long i = 1;
      while (getline(input_file, line)) {
        vertex_weights[i++] = stol(line);
      }

      assert(i == num_vertices + 1);
    }
  }

  std::vector<long> sub_mwis(num_vertices + 1);
  sub_mwis[0] = 0;
  sub_mwis[1] = vertex_weights[1];

  for (long i = 2; i <= num_vertices; i++) {
    long new_wis = sub_mwis[i - 2] + vertex_weights[i];
    sub_mwis[i] = new_wis > sub_mwis[i - 1] ? new_wis : sub_mwis[i - 1];
  }

  std::vector<bool> is_included(num_vertices + 1, false);
  for (long i = num_vertices; i >= 1; i--) {
    if (sub_mwis[i - 1] < sub_mwis[i - 2] + vertex_weights[i]) {
      is_included[i] = true;
      i--;
    }
  }

  long test_indices[] = {1, 2, 3, 4, 17, 117, 517, 997};
  std::string output = "00000000";

  for (int i = 0; i < 8; i++) {
    if (test_indices[i] < is_included.size())
      output[i] = is_included[test_indices[i]] ? '1' : '0';
  }

  std::cout << output << "\n";

  return 0;
}

