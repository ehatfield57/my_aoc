# Problem 10 - Adapter relay

from collections import Counter

# Part 1

with open('input.txt', 'r') as f:
    jolts = [int(line.rstrip()) for line in f]

NumOfAdapters = len(jolts)  # Number of adapters to be used
DeviceJolts = max(jolts) + 3
SortedAdapters = sorted(jolts)
Sequence = [0] + SortedAdapters + [DeviceJolts]
JoltsDiff = [Sequence[n] - Sequence[n-1] for n in range(1, len(Sequence))]
DiffDict = dict(Counter(JoltsDiff))

Product = DiffDict[1] * DiffDict[3]
print("Part 1: The product of the amount of 1 jolts and 3 jolts differences is: " + str(Product))

# Part 2
jolts.sort()                    # Sort the adapter list numerically
jolts.append(jolts[-1] + 3)     # Add the target adapter value to the end of the list
counter = {0: 1}                # Dict that will count how many ways to reach a jolts value (1 way to reach 0)

for adapter in jolts:
    # The loop will iterate through the list of all adapters and will count the amount of ways to reach that
    # specific adapter jolts value. Counting will default to 0, if there is no way to reach that value.
    counter[adapter] = counter.get(adapter - 3, 0) + counter.get(adapter - 2, 0) + counter.get(adapter - 1, 0)

print("Part 2: The number of ways to reach jolts value of " + str(jolts[-1]) + " is " + str(counter[jolts[-1]]))
