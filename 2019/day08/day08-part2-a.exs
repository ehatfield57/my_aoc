defmodule Day08 do
  @moduledoc """
  Solution for Advent of Code 2019, Day 8, Parts One and Two.
  """

  @doc """
  Solves Part One of the puzzle.

  ## Parameters
  - `file_path`: The path to the input file containing the image data.
  - `width`: The width of the image.
  - `height`: The height of the image.

  ## Returns
  The product of the number of '1' digits and the number of '2' digits in the layer with the fewest '0' digits.
  """
  def solve_part_one(file_path, width, height) do
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
  Solves Part Two of the puzzle.

  ## Parameters
  - `file_path`: The path to the input file containing the image data.
  - `width`: The width of the image.
  - `height`: The height of the image.

  ## Returns
  Renders the decoded image to the console.
  """
  def solve_part_two(file_path, width, height) do
    input = read_input(file_path)
    layers = parse_layers(input, width * height)
    final_image = decode_image(layers, width, height)
    render_image(final_image, width, height)
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

  @doc """
  Decodes the final image by determining the color of each pixel.

  ## Parameters
  - `layers`: A list of layers, each being a string of digits.
  - `width`: The width of the image.
  - `height`: The height of the image.

  ## Returns
  A list representing the final image pixels, where each element is "0" or "1".
  """
  def decode_image(layers, width, height) do
    total_pixels = width * height

    0..(total_pixels - 1)
    |> Enum.map(fn pos ->
      layers
      |> Enum.map(&String.at(&1, pos))
      |> Enum.find(fn pixel -> pixel != "2" end)
      |> case do
        nil -> "2"  # If all layers have "2" (transparent), default to "2"
        pixel -> pixel
      end
    end)
  end

  @doc """
  Renders the decoded image to the console.

  ## Parameters
  - `final_image`: A list representing the final image pixels.
  - `width`: The width of the image.
  - `height`: The height of the image.

  ## Returns
  Prints the image to the console.
  """
  def render_image(final_image, width, height) do
    final_image
    |> Enum.chunk_every(width)
    |> Enum.each(fn row ->
      row
      |> Enum.map(&pixel_to_char/1)
      |> Enum.join()
      |> IO.puts()
    end)
  end

  # Converts a pixel value to a display character.
  defp pixel_to_char("0"), do: " "  # Black
  defp pixel_to_char("1"), do: "█"  # White
  defp pixel_to_char(_), do: " "      # Treat transparent as black or space
end

# Execute the solution when the script is run directly
if __ENV__.file == __ENV__.file do
  # Define the image dimensions
  # Adjust these values if your puzzle uses different dimensions
  width = 25
  height = 6

  # Define the input file path
  input_file = "day08-data.txt"

  # Solve Part One
  part_one_result = Day08.solve_part_one(input_file, width, height)
  IO.puts("Part One Result: #{part_one_result}")

  # Solve Part Two
  IO.puts("\nPart Two Result:")
  Day08.solve_part_two(input_file, width, height)
end
