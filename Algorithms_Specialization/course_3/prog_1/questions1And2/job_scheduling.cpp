#include <iostream>
#include <cassert>
#include <fstream>
#include <sstream>
#include <vector>
#include <list>
#include <algorithm>

using namespace std;

enum STRATEGY {
  STRATEGY_1,
  STRATEGY_2
};

class cJOB {
private:
  int id;
  int weight;
  int length;
  long completion_time;

public:
  cJOB (const int i, const int w, const long l) : id(i), weight(w), length(l) {}

  const int get_id() { return id; }
  const int get_weight() { return weight; }
  const int get_length() { return length; }
  void set_completion_time (const long c) { completion_time = c; }
  const long get_completion_time () { return completion_time; }
};

void sorted_insert(std::list<int>& job_order, const int value, const int id) {
  job_order.push_back(id);
}

bool sorting_1(cJOB& job_1, cJOB& job_2) {
  int value_1 = job_1.get_weight() - job_1.get_length();
  int value_2 = job_2.get_weight() - job_2.get_length();
  if (value_1 > value_2) {
    return true;
  } else if (value_1 < value_2) {
    return false;
  } else {
    return job_1.get_weight() >= job_2.get_weight() ? true : false;
  }
}

bool sorting_2(cJOB& job_1, cJOB& job_2) {
  double value_1 = (double)job_1.get_weight() / job_1.get_length();
  double value_2 = (double)job_2.get_weight() / job_2.get_length();
  if (value_1 > value_2) {
    return true;
  } else if (value_1 < value_2) {
    return false;
  } else {
    return job_1.get_weight() >= job_2.get_weight() ? true : false;
  }
}

void sort_jobs_by_strategy(std::vector<cJOB>& jobs,
    const int strategy) {
  switch (strategy) {
    case STRATEGY_1:
      std::sort(jobs.begin(), jobs.end(), sorting_1);
      break;
    case STRATEGY_2:
      std::sort(jobs.begin(), jobs.end(), sorting_2);
      break;
    default:
      return;
  }
}

void compute_completion_times(std::vector<cJOB>& jobs) {
  long current_time = 0;
  for (auto& job : jobs) {
    current_time += job.get_length();
    job.set_completion_time(current_time);
  }
}

const long compute_sum_of_weighted_completion_times(std::vector<cJOB>& jobs) {
  compute_completion_times(jobs);

  long sum = 0;

  for (auto& job : jobs) {
    sum += job.get_weight() * job.get_completion_time();
  }

  return sum;
}

int main(int argc, char* argv[]) {
  assert(argc == 2);
  std::string input_filename = argv[1];
  // std::string input_filename = "mytest.txt";
  // std::string input_filename = "input_random_41_10000.txt";

  std::ifstream input_file(input_filename);

  std::string line;
  int num_jobs;
  std::vector<cJOB> jobs;

  if (input_file.is_open()) {
    if (getline(input_file, line)) {
      num_jobs = stoi(line);

      int count_jobs = 0;
      while (getline(input_file, line)) {
        std::istringstream iss(line);
        int weight;
        int length;

        iss >> weight >> length;
        cJOB job(count_jobs, weight, length);
        jobs.push_back(job);
        count_jobs++;
      }

      assert(num_jobs == count_jobs);
    }
  }

  sort_jobs_by_strategy(jobs, STRATEGY_1);
  long output_1 = compute_sum_of_weighted_completion_times(jobs); 

  sort_jobs_by_strategy(jobs, STRATEGY_2);
  long output_2 = compute_sum_of_weighted_completion_times(jobs); 

  std::cout << output_1 << " " << output_2 << "\n";
  return 0;
}
