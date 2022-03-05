#include <iostream>
#include <fstream>
#include <unordered_map>
#include <vector>
#include <cassert>
#include <list>

#define TARGET_MIN -10000
#define TARGET_MAX 10000

int main(int argc, char* argv[]) {
  assert(argc == 2);
  std::string input_filename = argv[1];
  // std::string input_filename = "mytest.txt";
  std::ifstream input_file(input_filename);

  std::vector<long> nums;
  std::unordered_map<long, long> map;

  if (input_file.is_open()) {
    std::string line;
    long i = 0;
    while (getline(input_file, line)) {
      long num = stol(line);
      nums.push_back(num);
      map[num] = i++;
    }
  }

  int count = 0;
  std::list<int> targets;
  for (int t = TARGET_MIN; t <= TARGET_MAX; t++) {
    targets.push_back(t);
  }

  for (auto& num : nums) {
    for (std::list<int>::iterator i = targets.begin(); i != targets.end(); i++) {
      int target = *i;
      long val = target - num;
      if (map.find(val) != map.end() && num != val) {
        count++;
        targets.erase(i++);
      }
    }
  }

  std::cout << count << "\n";
  return 0;
}

