
import itertools

def find_combinations_with_sum(data, target_sum):
    for r in range(1, len(data) + 1):
        for combination in itertools.combinations(data, r):
            if sum(combination) == target_sum:
                print(combination)

data = [1, 2, 3, 7, 11, 13, 17, 19, 23, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113]
target_sum = 390

find_combinations_with_sum(data, target_sum)

