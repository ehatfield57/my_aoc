import numpy as np


with open('./day21-data.txt') as f:
    instructions = f.readlines()

mappings = {}
start = '.#./..#/###'


def translate_to_np(s):
    return np.array([[c == '#' for c in l]
                     for l in s.split('/')])

for line in instructions:
    k, v = map(translate_to_np, line.strip().split(' => '))
    for a in (k, np.fliplr(k)):
        for r in range(4):
            mappings[np.rot90(a, r).tobytes()] = v


def enhance(grid):
    size = len(grid)
    by = 2 if size % 2 == 0 else 3
    resize = lambda x: x * (by+1) // by
    new_size = resize(size)
    solution = np.empty((new_size, new_size), dtype=bool)
    squares = range(0, size, by)
    new_squares = range(0, new_size, by+1)

    for i, ni in zip(squares, new_squares):
        for j, nj in zip(squares, new_squares):
            square = grid[i:i+by, j:j+by]
            enhanced = mappings[square.tobytes()]
            solution[ni:ni+by+1, nj:nj+by+1] = enhanced
    return solution

def solve(part):
    grid = translate_to_np(start)
    iterations = 5 if part == 1 else 18
    for _ in range(iterations):
        grid = enhance(grid)
    return int(grid.sum())


print(solve(part=1))
print(solve(part=2))
