#include <iostream>
#include <vector>
#include <fstream>
#include <cassert>

#define MIN 0
#define MAX 1

#define parent(i) (i - 1) / 2
#define left_child(i) 2 * i + 1
#define right_child(i) 2 * i + 2

void swap(int *a, int *b) {
  int temp = *a;
  *a = *b;
  *b = temp;
}

template <int MIN_MAX>
void insert_num_to_heap(std::vector<int>& heap, const int num) {
  /** Step 1: Insert new number at the end */
  heap.push_back(num);

  /** Step 2: Bubble up till number is at correct location */
  int i = heap.size() - 1;
  int j = parent(i);

  while (true) {
    if (i == 0)
      break;

    bool swap_with_parent;
    if constexpr (MIN_MAX == MIN) {
      swap_with_parent = heap.at(i) < heap.at(parent(i));
    } else {
      swap_with_parent = heap.at(i) > heap.at(parent(i));
    }

    if (swap_with_parent) {
      swap(&heap.at(i), &heap.at(j));
      i = j;
      j = parent(i);
    } else {
      i = 0;
    }
  }
}

template <int MIN_MAX>
void heapify(std::vector<int>& heap, const int root) {
  int l = left_child(root);
  int r = right_child(root);

  bool root_has_at_least_left_child = l < heap.size();

  if (root_has_at_least_left_child) {
    bool root_has_right_child = r < heap.size();
    int slot_to_swap = l;

    if (root_has_right_child) {
      if constexpr (MIN_MAX == MIN) {
        slot_to_swap = heap.at(l) < heap.at(r) ? l : r;
      } else if constexpr (MIN_MAX == MAX) {
        slot_to_swap = heap.at(l) > heap.at(r) ? l : r;
      }
    }

    bool is_swap_needed;

    if constexpr (MIN_MAX == MIN)
      is_swap_needed = heap.at(root) < heap.at(slot_to_swap) ? false : true;
    else if constexpr (MIN_MAX == MAX)
      is_swap_needed = heap.at(root) > heap.at(slot_to_swap) ? false : true;

    if (is_swap_needed) {
      swap(&heap.at(root), &heap.at(slot_to_swap));
      heapify<MIN_MAX>(heap, slot_to_swap);
    }
  }
}

template <int MIN_MAX>
int extract_from_heap(std::vector<int>& heap) {
  assert(heap.size() > 0);

  /** Step 1: get the root to be popped */
  int value = heap.at(0);

  /** Step 2: move last entry into root and delete it */
  heap.at(0) = heap.at(heap.size() - 1);
  heap.pop_back();

  /** Step 3: now recursively call heapify to ensure valid heap */
  heapify<MIN_MAX>(heap, 0);

  return value;
}

void add_num_to_heaps(std::vector<int>& low_heap,
    std::vector<int>& high_heap, const int num) {
  if (low_heap.size() == high_heap.size()) {
    if (low_heap.size() > 0 && num > high_heap.at(0)) {
      insert_num_to_heap<MIN>(high_heap, num);
      insert_num_to_heap<MAX>(low_heap, extract_from_heap<MIN>(high_heap));
    } else {
      insert_num_to_heap<MAX>(low_heap, num);
    }
  } else {
    assert(low_heap.size() == high_heap.size() + 1);
    if (num < low_heap.at(0)) {
      insert_num_to_heap<MAX>(low_heap, num);
      insert_num_to_heap<MIN>(high_heap, extract_from_heap<MAX>(low_heap));
    } else {
      insert_num_to_heap<MIN>(high_heap, num);
    }
  }
}

int get_median_from_stream(std::vector<int>& low_heap,
    std::vector<int>& high_heap, const int num) {
  add_num_to_heaps(low_heap, high_heap, num);

  return low_heap[0];
}

int main() {
  std::vector<int> low_heap;
  std::vector<int> high_heap;

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

