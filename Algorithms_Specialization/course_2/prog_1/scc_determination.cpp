#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>
#include <list>
#include <unordered_map>

using namespace std;

#define MAX(a, b) a > b ? a : b

class cEDGE;
std::vector<cEDGE> edges;

class cEDGE {
private:
  int tail;
  int head;

public:
  cEDGE(int t, int h) : tail(t), head(h) {}

  void print() { cout << tail << " " << head << "\n"; }

  int get_tail() { return tail; }

  int get_head() { return head; }
};

typedef class cNODE {
private:
  int leader;
  std::list<cEDGE> adjacency_list;
  std::list<cEDGE> reverse_adjacency_list;

public:
  void set_leader(int i) { leader = i; }
  
  int get_leader() { return leader; }

  template <bool IS_FINISHING_TIME_LOOP>
  std::list<cEDGE>& get_adjacency_list() {
    if constexpr (IS_FINISHING_TIME_LOOP)
      return reverse_adjacency_list;
    else
      return adjacency_list;
  }
} *NODE;

class cGRAPH {
private:
  std::vector<cNODE> nodes;
  std::vector<bool> is_node_explored;
  std::vector<int> node_finishing_times;

public:
  void reset();

  void construct_from_edges(int n, std::vector<cEDGE> edges);

  void determine_sccs();

  int number_of_nodes() { return nodes.size(); }

  template <bool IS_FINISHING_TIME_LOOP>
  void dfs(int *t, int i, int s);

  template <bool IS_FINISHING_TIME_LOOP>
  void dfs();

  void print_sccs();
};

void cGRAPH::construct_from_edges(int n, std::vector<cEDGE> edges) {
  this->nodes.assign(n, cNODE());
  // for (int i = 0; i < n; i++) {
  //   this->nodes.push_back(NULL);
  // }

  for (auto& edge : edges) {
    cEDGE offset_edge(edge.get_tail() - 1, edge.get_head() - 1);
    std::list<cEDGE>& adjacency_list = this->nodes[offset_edge.get_tail()].get_adjacency_list<false>();
    adjacency_list.push_back(offset_edge);

    cEDGE reverse_offset_edge(offset_edge.get_head(), offset_edge.get_tail());
    std::list<cEDGE>& reverse_adjacency_list = this->nodes[offset_edge.get_head()].get_adjacency_list<true>();
    reverse_adjacency_list.push_back(reverse_offset_edge);
  }

  // for (int i = 0; i < n; i++) {
  //   if (this->node_list.find(i) == this->node_list.end()) {
  //     // cout << "Adding empty node " << i << "\n";
  //     this->node_list[i] = cNODE();
  //   }
  // }
}

void read_data(cGRAPH *graph, const string input_filename) {
  string line;
  ifstream input(input_filename);

  int max_node_id = -1;

  if (input.is_open()) {
    while (getline(input, line)) {
      int tail;
      int head;

      istringstream iss(line);
      iss >> tail >> head;

      cEDGE new_edge(tail, head);
      edges.push_back(new_edge);

      max_node_id = MAX(max_node_id, tail);
      max_node_id = MAX(max_node_id, head);
    }
  }

  // for (auto& edge : edges) {
  //   edge.print();
  // }

  graph->construct_from_edges(max_node_id, edges);
  edges.clear();
}

void cGRAPH::reset() {
  int n = number_of_nodes();

  is_node_explored.assign(n, false);
}

template <bool IS_FINISHING_TIME_LOOP>
void cGRAPH::dfs(int *t, int i, int s) {
  // cout << "About to explore " << i + 1 << "\n";
  is_node_explored[i] = true;
  if constexpr (!IS_FINISHING_TIME_LOOP) {
    nodes[i].set_leader(s);
    // cout << "Setting leader for " << i + 1 << " as " << s + 1 << "\n";
  }

  for (auto& edge : nodes[i].get_adjacency_list<IS_FINISHING_TIME_LOOP>()) {
    int j = edge.get_head();
    if (!is_node_explored[j]) {
      dfs<IS_FINISHING_TIME_LOOP>(t, j, s);
      // cout << "Returned from last dfs call with t = " << *t << "\n";
    }
  }

  if constexpr (IS_FINISHING_TIME_LOOP) {
    // cout << "Done exploring " << i + 1 << ", finishing time: " << *t + 1 << "\n";
    node_finishing_times.push_back(i);
    *t += 1;
  }
}

template <bool IS_FINISHING_TIME_LOOP>
void cGRAPH::dfs() {
  this->reset();

  int n = number_of_nodes();

  int t = -1;
  int s = -1;

  // Traverse nodes in reverse
  for (int i = n - 1; i >= 0; i--) {
    int k;
    if constexpr (IS_FINISHING_TIME_LOOP) {
      k = i;
    } else {
      k = node_finishing_times[i];
    }

    if (!is_node_explored[k]) {
      s = k;
      dfs<IS_FINISHING_TIME_LOOP>(&t, k, s);
    }
  }
}

void cGRAPH::determine_sccs() {
  dfs<true>();

  dfs<false>();
}

void cGRAPH::print_sccs() {
  std::unordered_map<int, int> count_nodes_in_sccs;
  for (int i = 0; i < number_of_nodes(); i++) {
    int leader = nodes[i].get_leader();

    if (count_nodes_in_sccs.find(leader) == count_nodes_in_sccs.end()) {
      count_nodes_in_sccs[leader] = 1;
    } else {
      count_nodes_in_sccs[leader] += 1;
    }
  }

  std::list<int> count_list;
  for (auto& [leader, count] : count_nodes_in_sccs) {
    count_list.push_back(count);
  }
  count_list.sort(std::greater<int>());

  int i = 0;
  for (auto& sorted_count : count_list) {
    i++;
    cout << sorted_count << ",";
    if (i > 10)
      break;
  }
}

int main()
{
  cGRAPH graph;

  // string input_filename = "SCC.txt";
  string input_filename = "mytest.txt";
  // string input_filename = "input_mostlyCycles_3_8.txt";
  // string input_filename = "input_mostlyCycles_10_32.txt";

  read_data(&graph, input_filename);

  graph.determine_sccs();

  graph.print_sccs();

  return 0;
}

