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

    # Uncomment the following lines to print the grid for debugging
    # IO.puts("Scaffold Map:")
    # Enum.each(grid, fn row ->
    #   IO.puts(Enum.join(row, ""))
    # end)

    # Find intersections and calculate the sum of alignment parameters
    {sum, intersections} = find_intersections_with_sum(grid)

    # Uncomment the following lines to print intersection coordinates
    # IO.inspect(intersections, label: "Intersections (x, y)")

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
    |> Enum.map(&String.trim_trailing(&1, "\r"))
    |> Enum.map(&String.graphemes/1)
  end

  # Find intersections and calculate the sum of their alignment parameters
  defp find_intersections_with_sum(grid) do
    rows = length(grid)
    cols = grid |> List.first() |> length()

    intersections =
      for y <- 1..(rows - 2),
          x <- 1..(cols - 2),
          is_intersection?(grid, x, y) do
        {x, y}
      end

    sum =
      intersections
      |> Enum.map(fn {x, y} -> x * y end)
      |> Enum.sum()

    {sum, intersections}
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

# To execute the solution, ensure that your Intcode program is saved in "day17-data.txt" in the same directory.
# Then, you can run the following in your Elixir environment:

AdventOfCode.Day17.Part1.main()
