#include <iostream>
#include <cassert>
#include <fstream>
#include <sstream>
#include <climits>
#include <vector>
#include <list>
#include <algorithm>

struct cEDGE {
  long node_1;
  long node_2;
  long length;
};

int main(int argc, char* argv[]) {
  assert(argc == 2);
  std::string input_filename = argv[1];
  // std::string input_filename = "input_random_1_10.txt";

  std::ifstream input_file(input_filename);
  long num_nodes;
  long num_edges;
  std::vector<cEDGE> edges;

  if (input_file.is_open()) {
    std::string line;
    getline(input_file, line);
    std::istringstream iss_base(line);

    iss_base >> num_nodes >> num_edges;

    while (getline(input_file, line)) {
      std::istringstream iss_edge(line);

      cEDGE edge;
      iss_edge >> edge.node_1 >> edge.node_2 >> edge.length;
      edges.push_back(edge);
    }
  }

  std::list<long> nodes_X;
  std::vector<cEDGE> mst;

  /** Step 1: Initialize X and V */
  /** For first node in X, use first vertex of first edge */
  long first_node = edges[0].node_1;
  nodes_X.push_back(first_node);

  /** Step 2: Loop over all nodes in X and pick shortest length frontier crossing edge */
  while (nodes_X.size() != num_nodes) {
    long node_to_move;
    long smallest_edge_length = LONG_MAX;
    cEDGE smallest_edge;
    for (auto& edge : edges) {
      bool new_smaller_edge_found = false;
      if (edge.length < smallest_edge_length) {
        if (std::find(nodes_X.begin(), nodes_X.end(), edge.node_1) != nodes_X.end()
            && std::find(nodes_X.begin(), nodes_X.end(), edge.node_2) == nodes_X.end()) {
          node_to_move = edge.node_2;
          new_smaller_edge_found = true;
        } else if (std::find(nodes_X.begin(), nodes_X.end(), edge.node_1) == nodes_X.end()
            && std::find(nodes_X.begin(), nodes_X.end(), edge.node_2) != nodes_X.end()) {
          node_to_move = edge.node_1;
          new_smaller_edge_found = true;
        }
        
        if (new_smaller_edge_found) {
          smallest_edge_length = edge.length;
          smallest_edge = edge;
        }
      }
    }

    nodes_X.push_back(node_to_move);
    mst.push_back(smallest_edge);
    // edges.remove(smallest_edge);
  }

  /** Step 3: Measure length of MST */
  long mst_length = 0;
  for (auto& edge : mst) {
    mst_length += edge.length;
    // std::cout << mst_length << " ";
  }

  std::cout << mst_length << "\n";
  return 0;
}

