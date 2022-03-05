#include <iostream>
#include <cassert>
#include <fstream>
#include <sstream>
#include <climits>
#include <vector>
#include <list>
#include <algorithm>

#define DESIRED_CLUSTERS 4

struct cEDGE {
  long node_1;
  long node_2;
  long length;
};

bool sort_edges_by_length(cEDGE& edge_1, cEDGE& edge_2) {
  return edge_1.length < edge_2.length;
}

void update_leaders(std::vector<long>& node_leader, long old_leader, long new_leader) {
  for (long i = 0; i < node_leader.size(); i++) {
    if (node_leader[i] == old_leader)
      node_leader[i] = new_leader;
  }
}

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

  std::vector<long> node_leader(num_nodes + 1);
  for (int i = 0; i <= num_nodes; i++) {
    node_leader[i] = i;
  }

  /** Sort edges by length */
  std::sort(edges.begin(), edges.end(), sort_edges_by_length);

  long num_clusters = num_nodes;

  long i = 0;
  while (num_clusters > DESIRED_CLUSTERS) {
    cEDGE edge = edges[i++];

    if (node_leader[edge.node_1] != node_leader[edge.node_2]) {
      update_leaders(node_leader, node_leader[edge.node_2], node_leader[edge.node_1]);
      num_clusters--;
    }
  }

  /** Now advance i till we find an edge connecting two clusters */
  cEDGE edge = edges[i];
  while (node_leader[edge.node_1] == node_leader[edge.node_2]) {
    edge = edges[++i];
  }

  std::cout << edges[i].length << "\n";

  return 0;
}

