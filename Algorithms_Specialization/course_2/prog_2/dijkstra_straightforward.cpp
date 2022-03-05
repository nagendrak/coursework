#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <list>
#include <algorithm>

#define DEFAULT_SCORE 1000000

typedef class cEDGE {
private:
  int from_node;
  int to_node;
  int length;
public:
  cEDGE (const int f, const int t, const int len) : from_node (f), to_node(t), length(len) {}

  const int get_from_node() const { return from_node; }

  const int get_to_node() const { return to_node; }

  const int get_length() const { return length; }
} *EDGE;

typedef class cNODE {
private:
  int id;
  int shortest_path_score = DEFAULT_SCORE;
  std::vector<cEDGE> adjacency_list;
  cNODE *shortest_path_head = NULL;

public:
  cNODE (const int i) : id(i) {}

  std::vector<cEDGE>& get_adjacency_list() { return adjacency_list; }

  const int get_id() const { return id; }

  const int num_edges() const { return adjacency_list.size(); }

  void set_shortest_path_score (int s) { shortest_path_score = s; }

  void set_shortest_path_head (cNODE* n) { shortest_path_head = n; }

  const int get_shortest_path_score () { return shortest_path_score; }
} *NODE;

class cGRAPH {
private:
  std::vector<NODE> nodes;
public:
  cGRAPH(std::string input_filename);

  void compute_shortest_paths_from_node(int i);

  void print_all_shortest_paths() const;

  void print_graph () const;

  int num_nodes () const { return nodes.size(); }

  NODE get_connected_node (const EDGE e) { return nodes[e->get_to_node()]; }

  int get_dijkstra_greedy_score (const EDGE e) {
    return nodes[e->get_from_node()]->get_shortest_path_score() + e->get_length(); }
};

cGRAPH::cGRAPH(std::string input_filename) {
  std::ifstream input_file(input_filename);

  std::string line;

  if (input_file.is_open()) {
    while (getline(input_file, line)) {
      // std::cout << line << "\n";
      std::istringstream iss_line(line);

      /** Get the node number (first entry on every line) */
      int node_num;
      iss_line >> node_num;

      NODE new_node = new cNODE(node_num - 1);

      /** Get adjacency list for this node */
      std::string str;
      while (iss_line >> str) {
        std::istringstream iss_adj_node(str);
        std::string token;
        while (getline(iss_adj_node, token, ',')) {
          int to_node = stoi(token);
          getline(iss_adj_node, token, ',');
          int length = stoi(token);

          cEDGE new_edge(node_num - 1, to_node - 1, length);
          new_node->get_adjacency_list().push_back(new_edge);
          // std::cout << "Node: " << node_num << " adj: " << to_node << " of length " << length << "\n";
        }
      }

      /** Add new node to the graph */
      this->nodes.push_back(new_node);
    }
  }
}

void add_all_edges_from_node(std::list<EDGE>& edge_list, NODE node) {
  for (auto& edge : node->get_adjacency_list()) {
    edge_list.push_back(&edge);
  }
}

void delete_edges_within_frontier(std::list<EDGE>& edge_list, std::list<int>& nodes_in_X) {
  std::list<EDGE> edges_to_remove;

  for (auto& edge : edge_list) {
    if (std::find(nodes_in_X.begin(), nodes_in_X.end(), edge->get_from_node()) != nodes_in_X.end() 
        && std::find(nodes_in_X.begin(), nodes_in_X.end(), edge->get_to_node()) != nodes_in_X.end()) {
      edges_to_remove.push_back(edge);
    }
  }

  for (auto& edge : edges_to_remove) {
    edge_list.remove(edge);
  }
}

void cGRAPH::compute_shortest_paths_from_node(int i) {
  i = i - 1;

  NODE base_node = this->nodes[i];

  /** Define lists to keep track of things */
  std::list<int> nodes_in_X;
  std::list<EDGE> frontier_crossing_edges;

  /** Initialize values for base node */
  nodes_in_X.push_back(i);
  base_node->set_shortest_path_score(0);
  base_node->set_shortest_path_head(NULL);

  NODE new_node_in_X = base_node;
  while (new_node_in_X) {
    add_all_edges_from_node(frontier_crossing_edges, new_node_in_X);

    delete_edges_within_frontier(frontier_crossing_edges, nodes_in_X);

    /** Update scores from all frontier crossing edges */
    for (auto& edge : frontier_crossing_edges) {
      NODE connected_node = get_connected_node(edge);
      int score = this->get_dijkstra_greedy_score(edge);

      if (score < connected_node->get_shortest_path_score()) {
        connected_node->set_shortest_path_score(score);
        connected_node->set_shortest_path_head(this->nodes[edge->get_from_node()]);
      }
    }

    /** Add node with lowest score in V - X */
    int new_node_id = -1;
    int lowest_score = DEFAULT_SCORE;
    for (auto& node : this->nodes) {
      if (std::find(nodes_in_X.begin(), nodes_in_X.end(), node->get_id()) == nodes_in_X.end()) {
        if (node->get_shortest_path_score() < lowest_score) {
          lowest_score = node->get_shortest_path_score();
          new_node_id = node->get_id();
        }
      }
    }

    if (new_node_id != -1) {
      new_node_in_X = this->nodes[new_node_id];
      nodes_in_X.push_back(new_node_id);
    } else {
      new_node_in_X = NULL;
    }
  }
}

void cGRAPH::print_all_shortest_paths() const {
  for (auto& node : nodes) {
    std::cout << node->get_id() + 1 << " " << node->get_shortest_path_score() << "\n";
  }
}

void cGRAPH::print_graph() const {
  for (auto& node : this->nodes) {
    std::cout << "Printing details of node " << node->get_id();
    std::cout << " with " << node->num_edges() << " connections\n";

    for (auto& edge : node->get_adjacency_list()) {
      std::cout << "Connected to " << edge.get_to_node() << " with length " << edge.get_length() << "\n";
    }
  }
}

int main()
{
  // std::string input_filename = "mytest.txt";
  std::string input_filename = "dijkstraData.txt";

  cGRAPH graph = cGRAPH(input_filename);

  // graph.print_graph();

  graph.compute_shortest_paths_from_node(1);

  graph.print_all_shortest_paths();

  return 0;
}

