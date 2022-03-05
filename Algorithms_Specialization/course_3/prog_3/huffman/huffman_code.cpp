#include <iostream>
#include <cassert>
#include <fstream>
#include <vector>

struct sSYMBOL {
  long weight;
  long id;
};

struct sNODE;
typedef sNODE* NODE;

struct sNODE {
  NODE left;
  NODE right;
  NODE root;

  long weight;
  long id;
  long depth;

  sNODE() {}
  sNODE(const long w, const long i) : weight(w), id(i), left(NULL), right(NULL), root(NULL) {}
  sNODE(const NODE l, const NODE r) {
    assert(l->weight <= r->weight);
    left = l;
    right = r;
    weight = l->weight + r->weight;
    id = -1;
  }
};

class cHEAP {
private:
  std::vector<NODE> node_vec;

  const long get_weight(const long i) { assert(i < size()); return node_vec[i]->weight; }

public:
  const long parent(const long i) const { return (i + 1) / 2 - 1; }

  const long left_child(const long i) const { return i * 2 + 1; }

  const long right_child(const long i) const { return (i + 1) * 2; }

  const long size() const { return node_vec.size(); }

  void swap(const long i, const long j) {
    NODE temp_node = node_vec[i];
    node_vec[i] = node_vec[j];
    node_vec[j] = temp_node;
  }

  void insert(const NODE n) {
    node_vec.push_back(n);

    long i = size() - 1;
    long j = parent(i);

    while (true) {
      if (i == 0)
        break;

      if (node_vec[i]->weight < node_vec[j]->weight) {
        swap(i, j);
        i = j;
        j = parent(i);
      } else {
        i = 0;
      }
    }
  }

  void heapify(const long root) {
    assert(root <= size());

    const long l = left_child(root);
    const long r = right_child(root);

    const bool root_has_left_child = l < size();
    const bool root_has_right_child = r < size();

    if (root_has_left_child) {
      long smaller_child_index = l;

      if (root_has_right_child
          && get_weight(r) < get_weight(l)) {
        smaller_child_index = r;
      }

      bool is_swap_needed = get_weight(root) > get_weight(smaller_child_index);

      if (is_swap_needed) {
        swap(root, smaller_child_index);
        heapify(smaller_child_index);
      }
    }
  }

  const NODE extract_min() {
    if (size() == 0)
      return NULL;

    const NODE root_node = node_vec[0];
    node_vec[0] = node_vec[size() - 1];
    node_vec.pop_back();
    heapify(0);

    return root_node;
  }
};

void update_node_depth_via_DFS(std::vector<sNODE>& nodes, const NODE root_node, long root_count) {
  NODE left_child = root_node->left;
  NODE right_child = root_node->right;

  if (left_child == NULL && right_child == NULL) {
    assert(root_node->id != -1);
    nodes[root_node->id].depth = root_count;
  } else {
    update_node_depth_via_DFS(nodes, left_child, root_count + 1);
    update_node_depth_via_DFS(nodes, right_child, root_count + 1);
  }
}

int main(const int argc, char* argv[]) {
  assert(argc == 2);
  std::string input_filename = argv[1];

  std::ifstream input_file(input_filename);
  long num_symbols;
  std::vector<sNODE> nodes;
  cHEAP node_heap;

  if (input_file.is_open()) {
    std::string line;
    if (getline(input_file, line)) {
      num_symbols = stol(line);

      nodes.resize(num_symbols);

      long i = 0;
      while (getline(input_file, line)) {
        const long weight = stol(line);

        sNODE new_node = sNODE(weight, i);
        nodes[i] = new_node;
        node_heap.insert(&nodes[i]);
        i++;
      }

      assert(i == num_symbols);
    }
  }

  std::vector<sNODE> merged_nodes(num_symbols - 1);

  long merged_node_id = 0;
  while (node_heap.size() > 1) {

    NODE smallest_node = node_heap.extract_min();
    NODE next_smallest_node = node_heap.extract_min();

    sNODE merged_node = sNODE(smallest_node, next_smallest_node);
    merged_nodes[merged_node_id] = merged_node;

    node_heap.insert(&merged_nodes[merged_node_id]);

    merged_node_id++;
  }

  sNODE tree = merged_nodes[merged_nodes.size() - 1];
  update_node_depth_via_DFS(nodes, &tree, 0);

  long min = 1000000;
  long max = 0;
  for (auto& node : nodes) {
    min = node.depth < min ? node.depth : min;
    max = node.depth > max ? node.depth : max;
  }

  std::cout << max << "\n";
  std::cout << min << "\n";

  return 0;
}
