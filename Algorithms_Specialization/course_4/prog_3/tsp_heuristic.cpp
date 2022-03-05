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

struct sCITY {
  double x;
  double y;
};

struct sCITY_LIST {
  int total_num;
  std::vector<sCITY> v;
  int num_cities_covered;
  std::vector<bool> is_city_covered;
  std::vector<int> tour;

  void init (const int n) {
    total_num = n;
    num_cities_covered = 0;
    is_city_covered.resize(total_num);
    tour_city(0);
  }

  const bool are_all_cities_covered() { return num_cities_covered == total_num; }

  const int get_last_city_toured() { return tour[tour.size() - 1]; }

  bool city_already_toured(const int c) { return is_city_covered[c]; }

  double get_distance_sqrd (int i, int j) {
    assert(i != j);
    return (v[i].x - v[j].x) * (v[i].x - v[j].x) + (v[i].y - v[j].y) * (v[i].y - v[j].y);
  }

  void set_city_covered (const int c) {
    assert(is_city_covered[c] == false);
    is_city_covered[c] = true;
  }

  void tour_city (const int c) {
    assert(!city_already_toured(c));
    tour.push_back(c);
    set_city_covered(c);
    num_cities_covered++;
  }

  void tour_next_nearest_city() {
    const int current_city = get_last_city_toured();
    double min_dist_sqrd = INF;
    int nearest_city = -1;
    for (int i = 0; i < total_num; i++) {
      if (city_already_toured(i)) continue;

      double new_dist_sqrd = get_distance_sqrd(i, current_city);
      if (new_dist_sqrd < min_dist_sqrd) {
        min_dist_sqrd = new_dist_sqrd;
        nearest_city = i;
      }
    }
    tour_city(nearest_city);
  }

  double get_tour_distance () {
    double tour_distance = 0.0;
    for (int i = 0; i < total_num - 1; i++) {
      const int city_A = tour[i];
      const int city_B = tour[i + 1];
      tour_distance += sqrt(get_distance_sqrd(city_A, city_B));
    }
    tour_distance += sqrt(get_distance_sqrd(tour[total_num - 1], tour[0]));

    return tour_distance;
  }
};

int main(const int argc, const char* argv[]) {
  assert(argc == 2);
  std::string input_filename = argv[1];

  std::ifstream input_file(input_filename);

  std::string line;
  sCITY_LIST cities;

  if (input_file.is_open()) {
    getline(input_file, line);

    int total_num = stoi(line);
    cities.init(total_num);

    int city_count = 0;
    while (getline(input_file, line)) {
      int id;
      sCITY new_city;
      std::istringstream iss(line);
      iss >> id >> new_city.x >> new_city.y;

      cities.v.push_back(new_city);

      // std::cout << new_city.x << " " << new_city.y << "\n";
      city_count++;
    }

    assert(city_count == cities.total_num);
  }

  while (!cities.are_all_cities_covered()) {
    cities.tour_next_nearest_city();
  }

  const double tour_distance = cities.get_tour_distance();

  std::cout << long(tour_distance) << "\n";

  return 0;
}
