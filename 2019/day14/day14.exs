defmodule Day14Part1 do
  def parse_file(filename) do
    # Read the file line by line
    filename
    |> File.stream!()
    |> Stream.map(&String.trim/1)
    |> Stream.map(&parse_line/1)
    |> Enum.into(%{})
  end

  def parse_line(line) do
    # Split the line into parts
    [left, right] = String.split(line, " => ")
    {quantity, key} = extract_quantity_and_key(right)
    ingredients = extract_ingredients(left)
    {key, {quantity, ingredients}}
  end

  def extract_quantity_and_key(str) do
    # Extract the quantity and key from the right side of the "=>"
    [quantity_str, key] = String.split(str, " ")
    {String.to_integer(quantity_str), key}
  end

  def extract_ingredients(str) do
    # Extract the ingredients from the left side of the "=>"
    str
    |> String.split(", ")
    |> Enum.map(&parse_ingredient/1)
  end

  def parse_ingredient(str) do
    # Parse each ingredient into a tuple {quantity, ingredient}
    [quantity_str, ingredient] = String.split(str, " ")
    {String.to_integer(quantity_str), ingredient}
  end
end

# Get the filename from the command line arguments
filename = System.argv() |> List.first()

# Parse the file and print the resulting map
Day14Part1.parse_file(filename) |> IO.inspect()

