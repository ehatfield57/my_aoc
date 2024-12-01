defmodule AdventOfCode.Day17.Part1 do
  @moduledoc """
  Solution for Advent of Code 2019, Day 17, Part 1
  """

  def main do
    # Read the input Intcode program from 'day17-data.txt'
    input = File.read!("day17-data.txt") |> String.trim()
    program = parse_input(input)

    # Run the Intcode program
    output = Intcode.run(program, [])

    # Convert the ASCII output to a grid
    grid = parse_grid(output)

    # Find intersections and calculate the sum of alignment parameters
    sum = find_intersections(grid)

    IO.puts("Sum of alignment parameters: #{sum}")
  end

  # Parse the Intcode program from a comma-separated string
  defp parse_input(input) do
    input
    |> String.split(",", trim: true)
    |> Enum.map(&String.to_integer/1)
  end

  # Convert the list of ASCII codes to a 2D grid
  defp parse_grid(output) do
    output
    |> Enum.map(&<<&1>>)
    |> Enum.join()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.graphemes/1)
  end

  # Find intersections and calculate the sum of their alignment parameters
  defp find_intersections(grid) do
    rows = length(grid)
    cols = grid |> List.first() |> length()

    for y <- 1..(rows - 2),
        x <- 1..(cols - 2),
        is_intersection?(grid, x, y) do
      x * y
    end
    |> Enum.sum()
  end

  # Determine if the current position is an intersection
  defp is_intersection?(grid, x, y) do
    current = grid |> Enum.at(y) |> Enum.at(x)
    up = grid |> Enum.at(y - 1) |> Enum.at(x)
    down = grid |> Enum.at(y + 1) |> Enum.at(x)
    left = grid |> Enum.at(y) |> Enum.at(x - 1)
    right = grid |> Enum.at(y) |> Enum.at(x + 1)

    current == "#" and up == "#" and down == "#" and left == "#" and right == "#"
  end
end
