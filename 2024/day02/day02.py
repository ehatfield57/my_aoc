#!/usr/bin/env python3

import unittest

def read_numbers(filename):
  all_numbers = []
  with open(filename, 'r') as f:
    for line in f:
      numbers = [int(x) for x in line.split()]
      all_numbers.append(numbers)
  return all_numbers

def differences(numbers):
    differences = []
    for i in range(1, len(numbers)):
        differences.append(numbers[i] - numbers[i-1])
    return differences

def calculate_differences(all_numbers):
  all_differences = []
  for numbers in all_numbers:
      all_differences.append(differences(numbers))
  return all_differences


def is_safe(numbers):
    safe = False
    if all(n > 0 for n in numbers):
        if all(n in [1,2,3] for n in numbers):
            safe = True
    if all(n < 0 for n in numbers):
        if all(n in [-1,-2,-3] for n in numbers):
            safe = True
    return safe

def part1(filename):
    count = 0
    differences = calculate_differences(read_numbers(filename))
    for numbers in differences:
      if is_safe(numbers):
          count += 1
    return count

def part2(filename):
    count = 0
    for numbers in read_numbers(filename):
        if is_safe(differences(numbers)):
            count += 1
        else:
          for i, num in enumerate(numbers):
              new_list = numbers[:i] + numbers[i+1:]
              if is_safe(differences(new_list)):
                  count += 1
                  break
    return count


class TestDay02(unittest.TestCase):
    def test_part1_test(self):
        self.assertEqual(part1("test.txt"), 2)
    def test_part1_real(self):
        self.assertEqual(part1("input.txt"), 359)
    def test_part2_test(self):
        self.assertEqual(part2("test.txt"), 4)
    def test_part2_real(self):
        self.assertEqual(part2("input.txt"), 418)

if __name__ == '__main__':
    unittest.main()


