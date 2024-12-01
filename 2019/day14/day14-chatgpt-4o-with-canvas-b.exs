# Advent of Code 2019, Day 14, Part 1
# Problem: Given a list of chemical reactions, determine how much ORE is required to produce exactly 1 FUEL.

defmodule AdventOfCode.Day14 do
  @moduledoc """
  Solution for Advent of Code 2019, Day 14, Part 1 in Elixir.
  """

  # Parses the input into a map of chemical reactions
  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_reaction/1)
    |> Enum.into(%{})
  end

  # Parses a single line of reaction
  defp parse_reaction(line) do
    [inputs, output] = String.split(line, " => ")
    {output_quantity, output_chemical} = parse_quantity_chemical(output)

    inputs =
      inputs
      |> String.split(", ")
      |> Enum.map(&parse_quantity_chemical/1)

    {output_chemical, {output_quantity, inputs}}
  end

  # Parses a quantity and chemical string, such as "2 ORE"
  defp parse_quantity_chemical(str) do
    [quantity, chemical] = String.split(str, " ")
    {String.to_integer(quantity), chemical}
  end

  # Calculates the amount of ORE needed to produce the required quantity of a chemical
  def ore_required(input, target \\ {1, "FUEL"}) do
    reactions = parse_input(input)
    calculate_ore(reactions, target, %{})
  end

  # Recursive calculation of ORE required
  defp calculate_ore(_reactions, {needed_quantity, "ORE"}, _extra) do
    needed_quantity
  end

  defp calculate_ore(reactions, {needed_quantity, chemical}, extra) do
    {extra, needed_quantity} = handle_extra(chemical, needed_quantity, extra)
    {output_quantity, inputs} = reactions[chemical]
    times = Integer.ceil_div(needed_quantity, output_quantity)

    extra = Map.update(extra, chemical, times * output_quantity - needed_quantity, &(&1 + times * output_quantity - needed_quantity))

    inputs
    |> Enum.map(fn {q, chem} -> {q * times, chem} end)
    |> Enum.reduce(0, fn {q, chem}, acc -> acc + calculate_ore(reactions, {q, chem}, extra) end)
  end

  # Handles surplus chemicals
  defp handle_extra(chemical, needed_quantity, extra) do
    case Map.get(extra, chemical, 0) do
      0 -> {extra, needed_quantity}
      available when available >= needed_quantity -> {Map.put(extra, chemical, available - needed_quantity), 0}
      available -> {Map.put(extra, chemical, 0), needed_quantity - available}
    end
  end
end

# Example Usage
input = """
10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 FUEL
"""

IO.puts(AdventOfCode.Day14.ore_required(input)) # Should output the amount of ORE required to produce 1 FUEL