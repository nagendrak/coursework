#include <iostream>
#include <cassert>
#include <fstream>
#include <sstream>
#include <climits>
#include <vector>
#include <list>
#include <algorithm>
#include <unordered_map>
#include <unordered_set>

#define DESIRED_MIN_DIST 3

const long get_root_leader(std::vector<long>& leaders, const long num) {
  long root_leader = num;
  while (root_leader != leaders[root_leader]) {
    root_leader = leaders[root_leader];
  }

  return root_leader;
}

int main(int argc, char* argv[]) {
  assert(argc == 2);
  std::string input_filename = argv[1];
  // std::string input_filename = "input_random_1_10.txt";

  std::ifstream input_file(input_filename);
  long num_nodes;
  long num_bits;
  std::vector<std::string> nodes;
  std::vector<long> node_leaders;
  std::unordered_map<std::string, long> nodes_map;

  if (input_file.is_open()) {
    std::string line;
    getline(input_file, line);
    std::istringstream iss_base(line);

    iss_base >> num_nodes >> num_bits;

    long node_num = 0;
    while (getline(input_file, line)) {
      std::istringstream iss_node(line);

      char node_bits[num_bits];
      for (int i = 0; i < num_bits; i++) {
        iss_node >> node_bits[num_bits - i - 1];
      }

      std::string node_str = node_bits;

      /** Insert node into vector */
      nodes.push_back(node_str);
      node_leaders.push_back(node_num);

      /** Add entry to map */
      nodes_map[node_str] = node_num++;
    }
  }

  /** Check for node with 0 bit different */
  long node_num = 0;
  for (auto& node : nodes) {
    node_leaders[node_num++] = nodes_map[node];
  }

  /** Check for node with 1 bit different */
  node_num = 0;
  for (auto& node : nodes) {
    // std::cout << "node: " << node_num << "\n";
    long node_index = nodes_map[node];
    for (int i = 0; i < num_bits; i++) {
      std::string off_node = node;
      off_node[i] = node[i] == '0' ? '1' : '0';

      if (nodes_map.find(off_node) != nodes_map.end()) {
        long off_node_index = nodes_map[off_node];
        if (node_leaders[node_num] != node_leaders[off_node_index]) {
          // long max_node = nodes_map[off_node] > nodes_map[node] ? nodes_map[off_node] : nodes_map[node];
          // nodes_map[off_node] = max_node;
          // nodes_map[node] = max_node;
          long max_node = node_leaders[node_num] > node_leaders[off_node_index]
                          ? node_leaders[node_num]
                          : node_leaders[off_node_index];
          long root_leader = get_root_leader(node_leaders, max_node);
          node_leaders[node_num] = root_leader;
          node_leaders[off_node_index] = root_leader;

          // std::cout << "One difference neighbor found " << off_node_index << "\n";
        }
      }
    }
    node_num++;
  }

  /** Check for node with 2 bits different */
  node_num = 0;
  for (auto& node : nodes) {
    // std::cout << "node: " << node_num << "\n";
    long node_index = nodes_map[node];
    for (int i = 0; i < num_bits; i++) {
      std::string off_node = node;
      off_node[i] = node[i] == '0' ? '1' : '0';

      for (int j = i + 1; j < num_bits; j++) {
        off_node[j] = node[j] == '0' ? '1' : '0';
        // std::cout << off_node << "\n";

        if (nodes_map.find(off_node) != nodes_map.end()) {
          long off_node_index = nodes_map[off_node];
          if (node_leaders[node_num] != node_leaders[off_node_index]) {
            // long max_node = nodes_map[off_node] > nodes_map[node] ? nodes_map[off_node] : nodes_map[node];
            // nodes_map[off_node] = max_node;
            // nodes_map[node] = max_node;
            long max_node = node_leaders[node_num] > node_leaders[off_node_index]
                            ? node_leaders[node_num]
                            : node_leaders[off_node_index];
            long root_leader = get_root_leader(node_leaders, max_node);
            node_leaders[node_num] = root_leader;
            node_leaders[off_node_index] = root_leader;
            // std::cout << "Two difference neighbor found " << off_node_index << "\n";
          }

        }
        off_node[j] = node[j] == '0' ? '0' : '1';
      }
    }
    node_num++;
  }

  /** Count number of unique leaders */
  long count = 0;
  for (long i = 0; i < node_leaders.size(); i++) {
    if (node_leaders[i] == i)
      count++;
  }

  std::cout << count << "\n";

  return 0;
}

