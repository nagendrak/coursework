#include <iostream>
#include <vector>
#include <fstream>
#include <cassert>

#define MIN 0
#define MAX 1

#define parent(i) (i - 1) / 2
#define left_child(i) 2 * i + 1
#define right_child(i) 2 * i + 2

template <int TYPE>
class cHEAP {
private:
  std::vector<int> arr;

public:
  int size() { return arr.size(); }
  int get_element (const int i) { assert(i < size()); return arr.at(i); }
  void set_element (const int i, const int val) { assert(i < size()); arr.at(i) = val; }
  void swap(const int a, const int b);
  void insert(const int num);
  void heapify(const int root);
  int extract();
  void remove_last_element() { assert(size() > 0); arr.pop_back(); }
  int get_root() { assert(size() > 0); return get_element(0); }
  int get_last_element() { assert(size() > 0); return get_element(size() - 1); }
};

template <int TYPE>
void cHEAP<TYPE>::swap(const int a, const int b) {
  int temp = get_element(a);
  set_element(a, get_element(b));
  set_element(b, temp);
}

template <int TYPE>
void cHEAP<TYPE>::insert(const int num) {
  /** Step 1: Insert new number at the end */
  arr.push_back(num);

  /** Step 2: Bubble up till number is at correct location */
  int i = size() - 1;
  int j = parent(i);

  while (true) {
    if (i == 0)
      break;

    bool swap_with_parent;
    if constexpr (TYPE == MIN) {
      swap_with_parent = get_element(i) < get_element(parent(i));
    } else {
      swap_with_parent = get_element(i) > get_element(parent(i));
    }

    if (swap_with_parent) {
      swap(i, j);
      i = j;
      j = parent(i);
    } else {
      i = 0;
    }
  }
}

template <int TYPE>
void cHEAP<TYPE>::heapify(const int root) {
  int l = left_child(root);
  int r = right_child(root);

  bool root_has_at_least_left_child = l < size();

  if (root_has_at_least_left_child) {
    bool root_has_right_child = r < size();
    int slot_to_swap = l;

    if (root_has_right_child) {
      if constexpr (TYPE == MIN) {
        slot_to_swap = get_element(l) < get_element(r) ? l : r;
      } else if constexpr (TYPE == MAX) {
        slot_to_swap = get_element(l) > get_element(r) ? l : r;
      }
    }

    bool is_swap_needed;

    if constexpr (TYPE == MIN)
      is_swap_needed = get_element(root) < get_element(slot_to_swap) ? false : true;
    else if constexpr (TYPE == MAX)
      is_swap_needed = get_element(root) > get_element(slot_to_swap) ? false : true;

    if (is_swap_needed) {
      swap(root, slot_to_swap);
      heapify(slot_to_swap);
    }
  }
}

template <int TYPE>
int cHEAP<TYPE>::extract() {
  assert(size() > 0);

  /** Step 1: get the root to be popped */
  int value = get_root();

  /** Step 2: move last entry into root and delete it */
  set_element(0, get_last_element());
  remove_last_element();

  /** Step 3: now recursively call heapify to ensure valid heap */
  heapify(0);

  return value;
}

void add_num_to_heaps(cHEAP<MAX>& low_heap,
    cHEAP<MIN>& high_heap, const int num) {
  if (low_heap.size() == high_heap.size()) {
    if (low_heap.size() > 0 && num > high_heap.get_root()) {
      high_heap.insert(num);
      low_heap.insert(high_heap.extract());
    } else {
      low_heap.insert(num);
    }
  } else {
    assert(low_heap.size() == high_heap.size() + 1);
    if (num < low_heap.get_root()) {
      low_heap.insert(num);
      high_heap.insert(low_heap.extract());
    } else {
      high_heap.insert(num);
    }
  }
}

int get_median_from_stream(cHEAP<MAX>& low_heap,
    cHEAP<MIN>& high_heap, const int num) {
  add_num_to_heaps(low_heap, high_heap, num);

  return low_heap.get_root();
}

int main() {
  cHEAP<MAX> low_heap;
  cHEAP<MIN> high_heap;

  // std::string input_filename = "mytest.txt";
  // std::string input_filename = "input_random_10_40.txt";
  // std::string input_filename = "input_random_44_10000.txt";
  std::string input_filename = "median.txt";

  std::ifstream input_file(input_filename);
  std::string line;
  int sum_of_medians = 0;

  if (input_file.is_open()) {
    while (getline(input_file, line)) {
      int num = stoi(line);
      sum_of_medians += get_median_from_stream(low_heap, high_heap, num);
    }
  }

  std::cout << sum_of_medians % 10000 << "\n";
  return 0;
}

