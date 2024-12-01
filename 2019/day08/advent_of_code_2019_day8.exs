defmodule AdventOfCode2019Day8 do
  @moduledoc """
  Solution for Advent of Code 2019, Day 8, Part One.
  """

  @doc """
  Main function to execute the solution.

  ## Parameters
  - `input`: A string of digits representing the image data.
  - `width`: The width of the image.
  - `height`: The height of the image.

  ## Example

      iex> input = "123456789012"
      iex> AdventOfCode2019Day8.solve(input, 3, 2)
      1

  """
  def solve(input, width, height) do
    layers = parse_layers(input, width * height)

    # Find the layer with the fewest number of '0' digits
    layer_with_fewest_zeros =
      layers
      |> Enum.min_by(fn layer ->
        count_digits(layer, "0")
      end)

    # Count the number of '1' and '2' digits in the selected layer
    count_1 = count_digits(layer_with_fewest_zeros, "1")
    count_2 = count_digits(layer_with_fewest_zeros, "2")

    count_1 * count_2
  end

  @doc """
  Parses the input string into layers based on the specified layer size.

  ## Parameters
  - `input`: The input string containing all the digits.
  - `layer_size`: The number of digits per layer.

  ## Returns
  A list of strings, each representing a layer.
  """
  defp parse_layers(input, layer_size) do
    input
    |> String.trim()
    |> String.graphemes()
    |> Enum.chunk_every(layer_size)
    |> Enum.map(&Enum.join/1)
  end

  @doc """
  Counts the number of occurrences of a specific digit in a layer.

  ## Parameters
  - `layer`: A string representing a single layer.
  - `digit`: The digit to count as a string.

  ## Returns
  The count of the specified digit in the layer.
  """
  defp count_digits(layer, digit) do
    layer
    |> String.graphemes()
    |> Enum.count(&(&1 == digit))
  end
end

# To run the solution, you can use the following code.
# Replace `your_input_string` with your actual input.

if __ENV__.file == __ENV__.file do
  # Example usage:
  # You can replace this input with your actual puzzle input.
  input = "123456789012"

  # Define the image dimensions
  width = 3
  height = 2

  result = AdventOfCode2019Day8.solve(input, width, height)
  IO.puts("The result is: #{result}")
end
