defmodule Day08 do
  @moduledoc """
  Solution for Advent of Code 2019, Day 8, Part One.
  """

  @doc """
  Main function to execute the solution.

  ## Parameters
  - `file_path`: The path to the input file containing the image data.
  - `width`: The width of the image.
  - `height`: The height of the image.

  ## Example

      iex> Day08.solve("day08-data.txt", 25, 6)
      1234
  """
  def solve(file_path, width, height) do
    input = read_input(file_path)
    layers = parse_layers(input, width * height)

    # Find the layer with the fewest number of '0' digits
    layer_with_fewest_zeros =
      layers
      |> Enum.min_by(&count_digits(&1, "0"))

    # Count the number of '1' and '2' digits in the selected layer
    count_1 = count_digits(layer_with_fewest_zeros, "1")
    count_2 = count_digits(layer_with_fewest_zeros, "2")

    count_1 * count_2
  end

  @doc """
  Reads the input from the specified file.

  ## Parameters
  - `file_path`: The path to the input file.

  ## Returns
  A string containing the image data.
  """
  def read_input(file_path) do
    case File.read(file_path) do
      {:ok, content} -> String.trim(content)
      {:error, reason} -> 
        IO.puts("Error reading file #{file_path}: #{reason}")
        System.halt(1)
    end
  end

  # Parses the input string into layers based on the specified layer size.
  defp parse_layers(input, layer_size) do
    input
    |> String.graphemes()
    |> Enum.chunk_every(layer_size)
    |> Enum.map(&Enum.join/1)
  end

  # Counts the number of occurrences of a specific digit in a layer.
  defp count_digits(layer, digit) do
    layer
    |> String.graphemes()
    |> Enum.count(&(&1 == digit))
  end
end

# Execute the solution when the script is run directly
if __ENV__.file == __ENV__.file do
  # Define the image dimensions
  width = 25
  height = 6

  # Define the input file path
  input_file = "day08-data.txt"

  # Calculate the result
  result = Day08.solve(input_file, width, height)

  # Output the result
  IO.puts("The result is: #{result}")
end
