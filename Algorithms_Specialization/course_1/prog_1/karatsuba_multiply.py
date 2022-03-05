import sys

def pad_with_zeros_right(number, n):
    return str(number) + (n * '0')

def split_numbers(num1, num2, n):
    '''Splits given numbers at location n counting from right'''

    assert (len(num1) >= n), "Length of %s should be greater than %d for split" %(num1, n)
    assert (len(num2) >= n), "Length of %s should be greater than %d for split" %(num2, n)

    num_a = num1[:-n] if len(num1) > 1 else '0'
    num_b = num1[-n:]
    num_c = num2[:-n] if len(num2) > 1 else '0'
    num_d = num2[-n:]

    return num_a, num_b, num_c, num_d

def sum_string_numbers(num1, num2):
    #print("Computing sum of %s and %s" %(num1, num2))

    int_num1 = string_to_int_array(num1)
    int_num2 = string_to_int_array(num2)

    rev_num1 = int_num1[::-1]
    rev_num2 = int_num2[::-1]

    result = ''
    carry = 0
    while len(rev_num1) > 0 or len(rev_num2) > 0:
        top_num = rev_num1[0] if len(rev_num1) > 0 else 0
        bot_num = rev_num2[0] if len(rev_num2) > 0 else 0

        part_sum = top_num + bot_num + carry
        result += str(part_sum % 10)
        carry = part_sum // 10

        rev_num1 = rev_num1[1:]
        rev_num2 = rev_num2[1:]

    if carry > 0:
        result += str(carry)

    #print("Sum is %s" %(result[::-1]))
    return result[::-1]

def string_to_int_array(string_of_digits):
    return [int(i) for i in string_of_digits]

def sub_string_numbers(num1, num2):
    #print("Computing sub of %s and %s" %(num1, num2))

    int_num1 = string_to_int_array(num1)
    int_num2 = string_to_int_array(num2)

    rev_num1 = int_num1[::-1]
    rev_num2 = int_num2[::-1]

    result = ''
    while len(rev_num1) > 0:
        top_num = rev_num1[0]
        bot_num = rev_num2[0] if len(rev_num2) > 0 else 0

        if top_num < bot_num:
            top_num += 10
            rev_num1[1] -= 1

        result += str(top_num - bot_num)

        rev_num1 = rev_num1[1:]
        rev_num2 = rev_num2[1:]

    #print("Sub is %s" %(result[::-1]))
    return result[::-1]

def trim_leading_zeros(number):
    num = number

    while num[0] == '0' and len(num) > 1:
        num = num[1:]

    return num

def karatsuba_multiply(num1, num2):
    #print("Invoking karatsuba for %s and %s" %(num1, num2))
    if len(num1) == 1 and len(num2) == 1:
        return str(int(num1) * int(num2))
    else:
        m = min(len(num1), len(num2))
        m2 = (m + 1) // 2 # Add 1 to ensure m2 is at least 1

        num_a, num_b, num_c, num_d = split_numbers(num1, num2, m2)
        #print("Splits :", num_a, num_b, num_c, num_d)

        a_times_c = karatsuba_multiply(num_a, num_c)
        b_times_d = karatsuba_multiply(num_b, num_d)

        a_plus_b = sum_string_numbers(num_a, num_b)
        c_plus_d = sum_string_numbers(num_c, num_d)
        a_plus_b_times_c_plus_d = karatsuba_multiply(a_plus_b, c_plus_d)

        a_times_c_plus_b_times_d = sum_string_numbers(a_times_c, b_times_d)
        a_times_d_plus_b_times_c = sub_string_numbers(a_plus_b_times_c_plus_d, a_times_c_plus_b_times_d)

        z1 = pad_with_zeros_right(a_times_c, m2 + m2)
        z2 = pad_with_zeros_right(a_times_d_plus_b_times_c, m2)
        z3 = b_times_d

        product = sum_string_numbers(z1, z2)
        product = sum_string_numbers(product, z3)

        #print("Final product is", trim_leading_zeros(product))
        return trim_leading_zeros(product)

assert (karatsuba_multiply('1', '8') == '8'), "1 x 8 is wrong"
assert (karatsuba_multiply('2', '5') == '10'), "2 x 5 is wrong"
assert (karatsuba_multiply('12', '85') == '1020'), "12 x 85 is wrong"
assert (karatsuba_multiply('1254', '8573') == '10750542'), "1254 x 8573 is wrong"

assert (karatsuba_multiply('1254', '2') == '2508'), "1254 x 2 is wrong"
assert (karatsuba_multiply('1254', '83') == '104082'), "1254 x 83 is wrong"
assert (karatsuba_multiply('1254', '853') == '1069662'), "1254 x 853 is wrong"

assert (karatsuba_multiply('1254', '58573') == '73450542'), "1254 x 58573 is wrong"
assert (karatsuba_multiply('1254', '158573') == '198850542'), "1254 x 158573 is wrong"

assert (karatsuba_multiply('123', '4567') == '561741'), "123 x 4567 is wrong"
first = input("Enter first number: ")
second = input("Enter second number: ")

# Check if entered inputs are positive whole numbers
if not first.isdigit() or not second.isdigit():
    print("Inputs should be positive numbers, found non-digit characters")
    sys.exit()

result = karatsuba_multiply(first, second)

print("%s x %s = %s" %(first, second, result))
