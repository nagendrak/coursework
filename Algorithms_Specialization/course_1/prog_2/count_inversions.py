def read_number_list(filename):
    num_list_file = open(filename, "r")

    num_array = []
    for line in num_list_file:
        num_array.append(int(line))

    return num_array

def merge(left_list, right_list):
    num_left = len(left_list)
    num_right = len(right_list)

    merged_list = []

    i = 0
    j = 0
    num_split_inversions = 0
    for k in range(num_left + num_right):
        if i == num_left:
            merged_list.append(right_list[j])
            j += 1
            continue

        if j == num_right:
            merged_list.append(left_list[i])
            i += 1
            continue

        if left_list[i] <= right_list[j]:
            merged_list.append(left_list[i])
            i += 1
        else:
            merged_list.append(right_list[j])
            j += 1
            num_split_inversions += num_left - i
        # print(merged_list)

    return merged_list, num_split_inversions

def sort_and_count_inversions(num_list):
    n = len(num_list) # number of elements in array

    if n <= 1:
        return num_list, 0
    else:
        m = n // 2 # first index of right array (approximately mid-point)

        left_list = num_list[:m]
        right_list = num_list[m:]

        left_list, left_inversions = sort_and_count_inversions(left_list)
        right_list, right_inversions = sort_and_count_inversions(right_list)

        # print('Starting with:', num_list, left)
        num_list, split_inversions = merge(left_list, right_list)
        # print('Ending with:', num_list, left_inversions + right_inversions + split_inversions)

        return num_list, left_inversions + right_inversions + split_inversions

num_list = read_number_list("IntegerArray.txt")
# num_list = read_number_list("input_beaunus_10_16.txt")

num_list, num_of_inversions = sort_and_count_inversions(num_list)

print("Number of inversions in given list: %d" %(num_of_inversions))
