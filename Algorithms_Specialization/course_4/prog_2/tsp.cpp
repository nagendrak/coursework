#include <iostream>
#include <cassert>
#include <fstream>
#include <sstream>
#include <climits>
#include <vector>
#include <unordered_map>
#include <cmath>

#define MIN(a,b) a < b ? a : b

#define INF INT_MAX

void maybe_swap(int& i, int& j) {
  if (i > j) {
    int temp = i;
    i = j;
    j = temp;
  }
}

const int comb_size(const int n, const int k) {
  assert(k <= n);

  if (2 * k > n) return comb_size (n, n - k);

  long numerator = 1;
  long denominator = 1;

  for (int i = 0; i < k; i++) {
    numerator *= (n - i);
    denominator *= (i + 1);
  }

  return numerator / denominator;
}

static long g_index = 0;

void combinations (int v[], const int start, const int n, const int k, const int maxk,
    std::vector<std::vector<int> >& full_v) {
  /* k here counts through positions in the maxk-element v.
   * if k > maxk, then the v is complete and we can use it.
   */
  if (k > maxk) {
    g_index++;

    full_v[g_index][0] = 0;
    for (int i = 1; i <= maxk; i++) {
      full_v[g_index][i] = v[i];
    }

    return;
  }

  /* for this k'th element of the v, try all start..n
   * elements in that position
   */
  for (int i = start; i <= n; i++) {
    v[k] = i;

    /* recursively generate combinations of integers
     * from i+1..n
     */
    combinations (v, i+1, n, k+1, maxk, full_v);
  }
}

void load_all_combinations(std::vector<std::vector<int> >& full_v, const int n, const int m) {
  int arr[n + 1];

  combinations(arr, 1, n, 1, m, full_v);
}

struct sBITSET {
  std::vector<std::vector<int> > v;
  std::unordered_map<std::string, long> map;
  int num_cities;

  sBITSET (const int n) {
    assert(n > 1);
    num_cities = n;
    g_index = 0;
    const long num_subsets = 1 << (n - 1);
    v.resize(num_subsets);

    v[0].resize(1, 0);
    long index = 1;
    for (int m = 2; m <= n; m++) {
      const int num_combinations_this_m = comb_size(n - 1, m - 1);
      for (int j = 0; j < num_combinations_this_m; j++) {
        v[index++].resize(m, -1);
      }
      load_all_combinations(v, n - 1, m - 1);
    }

    populate_subset_map();
  }

  std::string get_subset_string(const long i) {
    std::string subset_string(num_cities, '0');
    auto& subset = v[i];

    for (int i = 0; i < subset.size(); i++) {
      const int j = subset[i];
      subset_string[j] = '1';
    }

    return subset_string;
  }

  void populate_subset_map() {
    for (long index = 0; index < v.size(); index++) {
      std::string subset_string = get_subset_string(index);
      map[subset_string] = index;
    }
  }

  const long index_without(const int si, const int j) {
    std::string subset_string_without = get_subset_string(si);
    subset_string_without[j] = '0';
    return map.at(subset_string_without);
  }
};

struct sCITY {
  double x;
  double y;
};

struct sCITY_LIST {
  int total_num;
  std::vector<sCITY> v;
  std::unordered_map<int, double> distances;

  const int get_index(int i, int j) {
    assert(total_num < 100);
    maybe_swap(i, j);
    return 100 * i + j;
  }

  void populate_all_distances() {
    for (int i = 0; i < total_num; i++) {
      for (int j = i + 1; j < total_num; j++) {
        int index = get_index(i, j);
        double dist = (v[i].x - v[j].x) * (v[i].x - v[j].x)
                      + (v[i].y - v[j].y) * (v[i].y - v[j].y);
        dist = sqrt(dist);
        distances[index] = dist;
      }
    }
  }

  const double get_distance(int i, int j) {
    return distances[get_index(i, j)];
  }
};

int main(const int argc, const char* argv[]) {
  assert(argc == 2);
  std::string input_filename = argv[1];

  std::ifstream input_file(input_filename);
  sCITY_LIST cities;

  if (input_file.is_open()) {
    std::string line;
    getline(input_file, line);

    cities.total_num = stoi(line);

    int city_count = 0;
    while (getline(input_file, line)) {
      sCITY new_city;
      std::istringstream iss(line);
      iss >> new_city.x >> new_city.y;

      cities.v.push_back(new_city);

      // std::cout << new_city.x << " " << new_city.y << "\n";
      city_count++;
    }

    assert(city_count == cities.total_num);
  }

  cities.populate_all_distances();

  long num_subsets = 1 << (cities.total_num - 1);
  std::vector<std::vector<double> > A(num_subsets, std::vector<double>(cities.total_num, INF));
  sBITSET all_subsets(cities.total_num);
  A[0][0] = 0;

  for (long subset_index = 1; subset_index < all_subsets.v.size(); subset_index++) {
    if (subset_index % 100000 == 0) std::cout << "At subset index " << subset_index << " out of " << num_subsets << "\n";
    const std::vector<int>& subset = all_subsets.v[subset_index];

    for (int jj = 0; jj < subset.size(); jj++) {
      const int j = subset[jj];
      if (j == 0) continue;

      double min_value = INF;
      for (int kk = 0; kk < subset.size(); kk++) {
        const int k = subset[kk];
        if (k == j) continue;

        min_value = MIN(min_value, A[all_subsets.index_without(subset_index, j)][k] + cities.get_distance(k, j));
      }
      A[subset_index][j] = min_value;
    }
  }

  double min_value = INF;
  for (int j = 1; j < cities.total_num; j++) {
    min_value = MIN(min_value, A[num_subsets - 1][j] + cities.get_distance(0, j));
  }

  std::cout << long(min_value) << "\n";

  return 0;
}
