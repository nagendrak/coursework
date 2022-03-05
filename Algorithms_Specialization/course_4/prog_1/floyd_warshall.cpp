#include <iostream>
#include <fstream>
#include <cassert>
#include <vector>
#include <sstream>
#include <climits>
#include <cstring>

#define INF INT_MAX
#define MIN(a,b) a < b ? a : b

struct sEDGE {
  int tail;
  int head;
  int length;
};

int main(const int argc, const char* argv[]) {
  assert(argc == 2);
  std::string input_filename = argv[1];
  std::ifstream input_file(input_filename);

  int num_vertices;
  int num_edges;
  std::vector<sEDGE> edges;

  if (input_file.is_open()) {
    std::string line;

    getline(input_file, line);
    std::istringstream iss(line);

    iss >> num_vertices >> num_edges;

    while (getline(input_file, line)) {
      sEDGE new_edge;
      std::istringstream iss_edge(line);
      iss_edge >> new_edge.tail >> new_edge.head >> new_edge.length;
      edges.push_back(new_edge);
    }

    assert(edges.size() == num_edges);
  }

  int A[2][num_vertices+1][num_vertices+1];
  // int ***A = new int** [2];
  // A[0] = new int* [num_vertices + 1];
  // A[1] = new int* [num_vertices + 1];
  // for (int i = 0; i <= num_vertices; i++)
  //   A[0][i] = new int [num_vertices + 1];

  /** Initialize A, first with all infinities, then self-edges and finally actual edges */
  for (int i = 0; i <= num_vertices; i++) {
    for (int j = 0; j <= num_vertices; j++) {
      A[0][i][j] = INT_MAX / 10;
    }
  }

  for (int i = 1; i <= num_vertices; i++) {
    A[0][i][i] = 0;
  }

  for (auto& edge : edges) {
    int i = edge.tail;
    int j = edge.head;
    A[0][i][j] = MIN(A[0][i][j], edge.length);
  }

  // for (int i = 1; i <= num_vertices; i++) {
  //   for (int j = 1; j <= num_vertices; j++) {
  //     std::cout << "\t" << A[0][i][j];
  //   }
  //   std::cout << "\n";
  // }
  // std::cout << "\n";

  for (int k = 1; k <= num_vertices; k++) {
    int new_k = k % 2;
    int old_k = new_k ^ 1;
    for (int i = 1; i <= num_vertices; i++) {
      for (int j = 1; j <= num_vertices; j++) {
        A[new_k][i][j] = MIN(A[old_k][i][j], A[old_k][i][k] + A[old_k][k][j]);
        // std::cout << "\t" << A[new_k][i][j];
      }
      // std::cout << "\n";
    }
    // std::cout << "\n";
  }

  int last_k = num_vertices % 2;
  int min_entry = INT_MAX;
  for (int i = 1; i <= num_vertices; i++) {
    for (int j = 1; j <= num_vertices; j++) {
      min_entry = MIN(min_entry, A[last_k][i][j]);
    }
  }

  bool negative_cycle_present = false;
  for (int i = 1; i <= num_vertices; i++) {
    if (A[last_k][i][i] < 0) {
      negative_cycle_present = true;
      break;
    }
  }

  if (negative_cycle_present) {
    std::cout << "NULL" << "\n";
  } else {
    std::cout << min_entry << "\n";
  }

  return 0;
}
