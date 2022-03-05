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

typedef struct sNODE {
  std::string str;
  long id;
  long leader;
  long size;

  sNODE () : str(""), id(-1), leader(-1), size(1) {}
  sNODE (const std::string s, const long i) : str(s), id(i), leader(i) {}
} *NODE;

struct sBIG_GRAPH {
  std::vector<sNODE> nodes;
  std::unordered_map<std::string, long> map;

  const long find(const long i) {
    if (nodes[i].leader == i) {
      return i;
    } else {
      nodes[i].leader = find(nodes[i].leader);
      return nodes[i].leader;
    }
  }

  const long get_index(std::string s) { return map.find(s) != map.end() ? map[s] : -1; }

  void merge(const long i, const long j) {
    long x = find(i);
    long y = find(j);

    if (x != y) {
      if (nodes[x].size < nodes[y].size) {
        long temp = x;
        x = y;
        y = temp;
      }

      nodes[y].leader = x;
      nodes[x].size = nodes[x].size + nodes[y].size;
    }
  }

  const long count_clusters() {
    long count = 0;
    for (auto& node : nodes) {
      if (node.leader == node.id) {
        count++;
      }
    }
    return count;
  }
};

int main(int argc, char* argv[]) {
  assert(argc == 2);
  std::string input_filename = argv[1];
  // std::string input_filename = "input_random_1_10.txt";

  std::ifstream input_file(input_filename);

  sBIG_GRAPH bg;

  long num_nodes;
  long num_bits;

  if (input_file.is_open()) {
    std::string line;
    getline(input_file, line);
    std::istringstream iss_base(line);

    iss_base >> num_nodes >> num_bits;

    bg.nodes.resize(num_nodes);

    long node_num = 0;
    while (getline(input_file, line)) {
      std::istringstream iss_node(line);

      char node_bits[num_bits];
      for (int i = 0; i < num_bits; i++) {
        iss_node >> node_bits[num_bits - i - 1];
      }

      std::string node_str = node_bits;

      sNODE node(node_str, node_num);

      /** Insert node into vector */
      bg.nodes[node_num] = node;

      /** Add entry to map */
      bg.map[node_str] = node_num++;
    }
  }

  /** Check for node with 0 bit different */
  for (int index = 0; index < bg.nodes.size(); index++) {
    const sNODE node = bg.nodes[index];
    const long off_index = bg.get_index(node.str);
    if (off_index > 0) {
      bg.merge(index, off_index);
    }
  }

  /** Check for node with 1 bit different */
  for (int index = 0; index < bg.nodes.size(); index++) {
    const std::string node_str = bg.nodes[index].str;
    for (int i = 0; i < num_bits; i++) {
      std::string off_node_str = node_str;
      off_node_str[i] = node_str[i] == '0' ? '1' : '0';
      const long off_index = bg.get_index(off_node_str);

      if (off_index > 0) {
        bg.merge(index, off_index);
      }
    }
  }

  /** Check for node with 2 bits different */
  for (int index = 0; index < bg.nodes.size(); index++) {
    const std::string node_str = bg.nodes[index].str;
    for (int i = 0; i < num_bits; i++) {
      std::string off_node_str = node_str;
      off_node_str[i] = node_str[i] == '0' ? '1' : '0';

      for (int j = i + 1; j < num_bits; j++) {
        off_node_str[j] = node_str[j] == '0' ? '1' : '0';
        const long off_index = bg.get_index(off_node_str);
        // std::cout << off_node_str << "\n";

        if (off_index > 0) {
          bg.merge(index, off_index);
        }

        off_node_str[j] = node_str[j];
      }
    }
  }

  std::cout << bg.count_clusters() << "\n";

  return 0;
}

