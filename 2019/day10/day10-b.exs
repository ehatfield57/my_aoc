defmodule MonitoringStation do
  @moduledoc """
  Solution for Advent of Code 2019, Day 10, Part 1.
  Reads the asteroid map from 'day10-data.txt' and determines the best location for the monitoring station.
  """

  @doc """
  Reads the asteroid map from the specified file and returns its content as a string.
  """
  def read_map_from_file(file_path) do
    case File.read(file_path) do
      {:ok, content} ->
        content

      {:error, reason} ->
        IO.puts("Error reading file #{file_path}: #{reason}")
        System.halt(1)
    end
  end

  @doc """
  Parses the asteroid map and returns a list of {x, y} coordinates where asteroids are present.
  """
  def parse_map(map) do
    map
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.flat_map(fn {line, y} ->
      line
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.filter(fn {char, _x} -> char == "#" end)
      |> Enum.map(fn {_char, x} -> {x, y} end)
    end)
  end

  @doc """
  Calculates the number of other asteroids visible from each asteroid.
  Returns a map with asteroid coordinates as keys and visibility counts as values.
  """
  def calculate_visibility(asteroids) do
    Enum.reduce(asteroids, %{}, fn asteroid, acc ->
      other_asteroids = List.delete(asteroids, asteroid)

      angles =
        other_asteroids
        |> Enum.map(fn other ->
          dx = elem(other, 0) - elem(asteroid, 0)
          dy = elem(other, 1) - elem(asteroid, 1)
          :math.atan2(dy, dx)
        end)
        |> Enum.uniq()

      Map.put(acc, asteroid, length(angles))
    end)
  end

  @doc """
  Finds the asteroid with the maximum number of detectable asteroids.
  Returns a tuple with the best asteroid's coordinates and the count.
  """
  def find_best_location(map) do
    asteroids = parse_map(map)
    visibility = calculate_visibility(asteroids)

    {best_asteroid, max_count} =
      visibility
      |> Enum.max_by(fn {_asteroid, count} -> count end)

    {best_asteroid, max_count}
  end

  @doc """
  The main entry point of the script.
  Reads the asteroid map from 'day10-data.txt', finds the best location, and prints the result.
  """
  def main do
    file_path = "day10-data.txt"
    map = read_map_from_file(file_path)
    {best_location, count} = find_best_location(map)
    IO.puts("Best Location: #{inspect(best_location)} with #{count} detectable asteroids.")
  end
end

# Execute the main function when the script is run
MonitoringStation.main()
