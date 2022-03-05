#include <iostream>
#include <bitset>
#include <cassert>
#include <fstream>
#include <vector>
#include <sstream>
#include <cmath>
#include <cstdlib>
#include <ctime>
#include <chrono>
#include <unordered_map>
#include <algorithm>

struct sCLAUSE {
  int var_1;
  int var_2;

  sCLAUSE (const int v_1, const int v_2) : var_1(v_1), var_2(v_2) {}
};

struct sVARIABLES {
  std::vector<int> var_list;
  std::unordered_map<int, bool> var_state;
  std::vector<sCLAUSE> clauses;
  int last_failed_clause;

  void init (const int n) {
    for (int i = 1; i <= n; i++) {
      var_list.push_back(i);
    }
  }

  void add_clause (const int i, const int cl_var_1, const int cl_var_2) {
    sCLAUSE new_clause(cl_var_1, cl_var_2);
    clauses.push_back(new_clause);
  }

  void random_initialize () {
    for (auto& var : var_list) {
      var_state[var] = rand() % 2 == 1;
    }
  }

  bool test_var (const int value) {
    return std::signbit(value) ? !var_state[abs(value)] : var_state[abs(value)];
  }

  bool test_clause (const int c) {
    return test_var(clauses[c].var_1) || test_var(clauses[c].var_2);
  }

  bool full_test () {
    int num_passed_clauses = 0;
    std::vector<int> untested_clauses(clauses.size());
    for (int i = 0; i < untested_clauses.size(); i++) {
      untested_clauses[i] = i;
    }

    while (num_passed_clauses != clauses.size()) {
      const int c = rand() % (clauses.size() - num_passed_clauses);
      if (test_clause(untested_clauses[c])) {
        untested_clauses.erase(untested_clauses.begin() + c);
        num_passed_clauses++;
      } else {
        last_failed_clause = untested_clauses[c];
        return false;
      }
    }

    return true;
  }

  void flip (const int value) {
    const int index = abs(value);
    if (var_state[index]) {
      var_state[index] = false;
    } else {
      var_state[index] = true;
    }
  }

  void random_flip () {
    if (rand() % 2) {
      flip(clauses[last_failed_clause].var_1);
    } else {
      flip(clauses[last_failed_clause].var_2);
    }
  }

  bool is_var_always_same (const int var, std::vector<int>& clauses_with_var) {
    bool is_negative_present = false;
    bool is_positive_present = false;
    for (int c = 0; c < clauses.size(); c++) {
      if (clauses[c].var_1 == -var || clauses[c].var_2 == -var 
          || clauses[c].var_1 == var || clauses[c].var_2 == var) {

        clauses_with_var.push_back(c);

        if (clauses[c].var_1 == -var || clauses[c].var_2 == -var) {
          is_negative_present = true;
        }
        if (clauses[c].var_1 == var || clauses[c].var_2 == var) {
          is_positive_present = true;
        }
      }

      if (is_negative_present && is_positive_present) return false;
    }
    return true;
  }

  void compute_reductions () {
    int total_reductions = 0;
    int num_clause_reductions_this_iteration = -1;
    int num_var_reductions_this_iteration = -1;
    while (num_var_reductions_this_iteration != 0
        && num_clause_reductions_this_iteration != 0) {
      num_var_reductions_this_iteration = 0;
      num_clause_reductions_this_iteration = 0;

      for (std::vector<int>::iterator it = var_list.begin(); it != var_list.end(); ) {
        int num_reductions_this_var = 0;
        const int var = *it;

        std::vector<int> clauses_with_var;
        clauses_with_var.clear();

        if (is_var_always_same(var, clauses_with_var)) {
          for (auto& c_to_delete : clauses_with_var) {
            clauses.erase(clauses.begin() + c_to_delete - num_reductions_this_var++);
            num_clause_reductions_this_iteration++;
          }
          it = var_list.erase(it);
          num_var_reductions_this_iteration++;
        } else {
          it++;
        }
      }
      total_reductions += num_clause_reductions_this_iteration;
      // std::cout << "Vars: " << var_list.size() << " Clauses: " << clauses.size() << "\n";
    }
  }
};

int papadimitriou (sVARIABLES& variables) {
  for (int i = 0; i < log2(variables.var_list.size()) + 1; i++) {
    variables.random_initialize();

    for (int j = 0; j < 2 * variables.var_list.size() * variables.var_list.size(); j++) {
      if (variables.full_test()) return 1;

      variables.random_flip();
    }
  }
  return variables.var_list.size() == 0 ? 1 : 0;
}

int main(const int argc, const char* argv[]) {
  assert(argc == 2);
  std::string input_filename = argv[1];
  std::ifstream input_file(input_filename);

  unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
  srand(seed);

  int n_variables;
  sVARIABLES variables;

  if (input_file.is_open()) {
    std::string line;
    getline(input_file, line);

    n_variables = stoi(line);
    variables.init(n_variables);

    int line_count = 0;
    while (getline(input_file, line)) {
      std::istringstream iss(line);
      int var_1;
      int var_2;
      iss >> var_1 >> var_2;
      variables.add_clause(line_count, var_1, var_2);

      line_count++;
    }

    assert(line_count == n_variables);
  }

  variables.compute_reductions();
  int answer = papadimitriou(variables);

  std::cout << answer << "\n";

  return 0;
}
