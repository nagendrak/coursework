from enum import Enum

class Strategy(Enum):
    FIRST = 1
    LAST = 2
    MEDIAN_OF_THREE = 3

def read_number_list(filename):
    num_list_file = open(filename, "r")

    num_array = []
    for line in num_list_file:
        num_array.append(int(line))
    #num_array = [3, 4, 2, 1]

    return num_array

def choose_pivot(num_list, strategy, l, r):
    if (strategy == Strategy.FIRST):
        return l
    elif (strategy == Strategy.LAST):
        return r
    elif (strategy == Strategy.MEDIAN_OF_THREE):
        middle_index = l + ((r - l) // 2)

        x = num_list[l] - num_list[middle_index]
        y = num_list[middle_index] - num_list[r]
        z = num_list[l] - num_list[r]
        if x * y > 0:
            return middle_index
        if x * z > 0:
            return r
        return l

def swap(num_list, i, j):
    temp = num_list[i]
    num_list[i] = num_list[j]
    num_list[j] = temp

def partition(num_list, strategy, l, r):
    pivot_index = choose_pivot(num_list, strategy, l, r)
    #print("l %d, r %d, p %d" %(l, r, pivot_index))

    if (pivot_index != l):
        swap(num_list, l, pivot_index)

    pivot = num_list[l]
    i = l + 1 # last index of left array
    for j in range(l + 1, r + 1):
        # print("loop beg j ", j, num_list)
        if num_list[j] < pivot:
            swap(num_list, i, j)
            i += 1
        # print("loop", num_list)

    swap(num_list, l, i - 1)
    return i - 1

def quick_sort_and_count_comparisons(num_list, strategy, l, r):
    #print("called for ", l, r)
    if r - l <= 0:
        return 0

    else:
        #print("Starting ", num_list)
        num_of_comparisons = r - l
        new_pivot_index = partition(num_list, strategy, l, r)
        #print("After partition", num_list)

        #print("left call", l, new_pivot_index - 1)
        num_of_comparisons += quick_sort_and_count_comparisons(num_list, strategy, l, new_pivot_index - 1)
        #print("right call", new_pivot_index + 1, r)
        num_of_comparisons += quick_sort_and_count_comparisons(num_list, strategy, new_pivot_index + 1, r)

    return num_of_comparisons

num_list = read_number_list("QuickSort.txt")
#num_list = read_number_list("input_dgrcode_02_5.txt")
pivot_strategy = Strategy.FIRST
num_of_comparisons = quick_sort_and_count_comparisons(num_list, pivot_strategy, 0, len(num_list) - 1)
print(pivot_strategy, " would use %d comparisons" %(num_of_comparisons))
#print(num_list)

num_list = read_number_list("QuickSort.txt")
#num_list = read_number_list("input_dgrcode_02_5.txt")
pivot_strategy = Strategy.LAST
num_of_comparisons = quick_sort_and_count_comparisons(num_list, pivot_strategy, 0, len(num_list) - 1)
print(pivot_strategy, " would use %d comparisons" %(num_of_comparisons))
#print(num_list)

num_list = read_number_list("QuickSort.txt")
#num_list = read_number_list("input_dgrcode_02_5.txt")
pivot_strategy = Strategy.MEDIAN_OF_THREE
num_of_comparisons = quick_sort_and_count_comparisons(num_list, pivot_strategy, 0, len(num_list) - 1)
print(pivot_strategy, " would use %d comparisons" %(num_of_comparisons))
#print(num_list)
