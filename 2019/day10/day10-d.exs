defmodule MonitoringStation do
  @moduledoc """
  Solution for Advent of Code 2019, Day 10, Parts 1 and 2.
  Reads the asteroid map from 'day10-data.txt', determines the best location for the monitoring station,
  and simulates the vaporization of asteroids using a laser.
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
  Calculates the angle from the station to each asteroid.
  Adjusts the angle so that 0 radians points upwards and increases clockwise.
  """
  def calculate_angle_and_distance({x, y}, {station_x, station_y}) do
    dx = x - station_x
    dy = y - station_y

    # Invert dy to make upward direction as positive
    angle = :math.atan2(dx, -dy)

    # Adjust angle to be in the range [0, 2π)
    adjusted_angle =
      if angle < 0 do
        angle + 2 * :math.pi()
      else
        angle
      end

    distance = :math.sqrt(dx * dx + dy * dy)
    {adjusted_angle, distance, {x, y}}
  end

  @doc """
  Simulates the vaporization process and returns the list of vaporized asteroids in order.
  """
  def vaporize_asteroids(asteroids, station) do
    # Exclude the station from the list
    targets = List.delete(asteroids, station)

    # Calculate angle and distance for each asteroid relative to the station
    targets_with_angles =
      targets
      |> Enum.map(&calculate_angle_and_distance(&1, station))

    # Group asteroids by angle
    grouped =
      targets_with_angles
      |> Enum.group_by(
        fn {angle, _distance, _coord} -> angle end,
        fn {_angle, distance, coord} -> {distance, coord} end
      )

    # Sort the angles in ascending order (clockwise)
    sorted_angles = grouped |> Map.keys() |> Enum.sort()

    # For each angle, sort the asteroids by distance (closest first)
    sorted_grouped =
      grouped
      |> Enum.map(fn {angle, asteroids} ->
        sorted_asteroids = Enum.sort_by(asteroids, fn {distance, _coord} -> distance end)
        {angle, sorted_asteroids}
      end)
      |> Enum.sort_by(fn {angle, _asteroids} -> angle end)

    vaporization_order = []
    remaining = sorted_grouped

    vaporize_loop(vaporization_order, remaining)
  end

  @doc """
  Recursively vaporizes asteroids in the sorted groups.
  """
  defp vaporize_loop(vaporization_order, []), do: vaporization_order

  defp vaporize_loop(vaporization_order, sorted_grouped) do
    {new_order, new_grouped} =
      Enum.reduce(sorted_grouped, {vaporization_order, []}, fn {angle, asteroids}, {acc_order, acc_grouped} ->
        case asteroids do
          [] ->
            {acc_order, acc_grouped}

          [head | tail] ->
            # Vaporize the closest asteroid
            new_order = [elem(head, 1) | acc_order]

            # If there are more asteroids in this angle, keep them
            if tail == [] do
              {new_order, acc_grouped}
            else
              {new_order, acc_grouped ++ [{angle, tail}]}
            end
        end
      end)

    # Continue the loop with the updated order and remaining asteroids
    vaporize_loop(new_order, new_grouped)
  end

  @doc """
  Finds the Nth vaporized asteroid.
  """
  def find_nth_vaporized(asteroids, station, n) do
    vaporization_order = vaporize_asteroids(asteroids, station)

    vaporization_order
    |> Enum.reverse()
    |> Enum.at(n - 1)
  end

  @doc """
  The main entry point of the script.
  Reads the asteroid map from 'day10-data.txt', finds the best location, vaporizes asteroids,
  and prints the 200th vaporized asteroid.
  """
  def main do
    file_path = "day10-data.txt"
    map = read_map_from_file(file_path)
    {best_location, count} = find_best_location(map)
    IO.puts("Best Location: #{inspect(best_location)} with #{count} detectable asteroids.")

    # Part 2: Vaporization
    asteroids = parse_map(map)
    {x_station, y_station} = best_location
    nth = 200

    case find_nth_vaporized(asteroids, best_location, nth) do
      nil ->
        IO.puts("Less than #{nth} asteroids were vaporized.")

      {x, y} ->
        result = 100 * x + y
        IO.puts("#{nth}th Vaporized Asteroid: #{inspect({x, y})} => #{result}")
    end
  end
end

# Execute the main function when the script is run
MonitoringStation.main()
