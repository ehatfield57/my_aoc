defmodule MoonSimulator do
  @moduledoc """
  Simulates the motion of moons in 3D space and calculates the total energy after a specified number of steps.
  """

  # Define a Moon struct with position and velocity
  defmodule Moon do
    defstruct pos: {0, 0, 0}, vel: {0, 0, 0}
  end

  @doc """
  Parses a list of input strings into a list of Moon structs.
  
  ## Example

      iex> input = ["<x=-1, y=0, z=2>", "<x=2, y=-10, z=-7>", "<x=4, y=-8, z=8>", "<x=3, y=5, z=-1>"]
      iex> MoonSimulator.parse_input(input)
      [
        %MoonSimulator.Moon{pos: {-1, 0, 2}, vel: {0, 0, 0}},
        %MoonSimulator.Moon{pos: {2, -10, -7}, vel: {0, 0, 0}},
        %MoonSimulator.Moon{pos: {4, -8, 8}, vel: {0, 0, 0}},
        %MoonSimulator.Moon{pos: {3, 5, -1}, vel: {0, 0, 0}}
      ]

  """
  def parse_input(lines) do
    Enum.map(lines, fn line ->
      # Extract numbers using regex
      [x, y, z] =
        Regex.run(~r/<x=(-?\d+), y=(-?\d+), z=(-?\d+)>/, line, capture: :all_but_first)
        |> Enum.map(&String.to_integer/1)

      %Moon{pos: {x, y, z}, vel: {0, 0, 0}}
    end)
  end

  @doc """
  Simulates the motion of moons for a given number of steps and returns the list of moons after simulation.
  """
  def simulate(moons, steps) do
    Enum.reduce(1..steps, moons, fn _, acc -> step(acc) end)
  end

  @doc """
  Performs a single time step: applies gravity and then velocity.
  """
  def step(moons) do
    moons
    |> apply_gravity()
    |> apply_velocity()
  end

  @doc """
  Applies gravity to update velocities based on positions.
  """
  def apply_gravity(moons) do
    # Initialize velocity changes as a list of {dx, dy, dz} for each moon
    velocity_changes = Enum.map(moons, fn _ -> {0, 0, 0} end)

    # Generate all unique pairs of indices
    pairs = for i <- 0..(length(moons) - 2), j <- (i + 1)..(length(moons) - 1), do: {i, j}

    # Calculate velocity changes for each pair
    velocity_changes = Enum.reduce(pairs, velocity_changes, fn {i, j}, acc ->
      moon1 = Enum.at(moons, i)
      moon2 = Enum.at(moons, j)

      {dv1, dv2} = compare_positions(moon1.pos, moon2.pos)

      # Update the velocity changes for moon i
      {vx1, vy1, vz1} = Enum.at(acc, i)
      acc = List.replace_at(acc, i, add_tuples({vx1, vy1, vz1}, dv1))

      # Update the velocity changes for moon j
      {vx2, vy2, vz2} = Enum.at(acc, j)
      List.replace_at(acc, j, add_tuples({vx2, vy2, vz2}, dv2))
    end)

    # Apply the accumulated velocity changes to each moon's velocity
    Enum.zip(moons, velocity_changes)
    |> Enum.map(fn {%Moon{vel: vel}, dv} ->
      add_tuples(vel, dv)
    end)
    |> Enum.zip(moons)
    |> Enum.map(fn {new_vel, %Moon{} = moon} ->
      %Moon{moon | vel: new_vel}
    end)
  end

  @doc """
  Compares positions of two moons and returns the velocity changes.
  """
  def compare_positions({x1, y1, z1}, {x2, y2, z2}) do
    delta_x = compare_coord(x1, x2)
    delta_y = compare_coord(y1, y2)
    delta_z = compare_coord(z1, z2)

    { {delta_x, delta_y, delta_z}, { -delta_x, -delta_y, -delta_z} }
  end

  @doc """
  Compares two coordinates and returns the velocity change.
  """
  def compare_coord(a, b) when a < b, do: 1
  def compare_coord(a, b) when a > b, do: -1
  def compare_coord(_a, _b), do: 0

  @doc """
  Adds two 3D tuples element-wise.
  """
  def add_tuples({a1, a2, a3}, {b1, b2, b3}), do: {a1 + b1, a2 + b2, a3 + b3}

  @doc """
  Applies velocity to update positions based on current velocities.
  """
  def apply_velocity(moons) do
    Enum.map(moons, fn moon ->
      {x, y, z} = moon.pos
      {vx, vy, vz} = moon.vel
      %Moon{moon | pos: {x + vx, y + vy, z + vz}}
    end)
  end

  @doc """
  Calculates the total energy in the system.
  """
  def total_energy(moons) do
    Enum.reduce(moons, 0, fn moon, acc ->
      potential = energy(moon.pos)
      kinetic = energy(moon.vel)
      acc + potential * kinetic
    end)
  end

  @doc """
  Calculates the sum of absolute values of a tuple's elements.
  """
  def energy({a, b, c}), do: abs(a) + abs(b) + abs(c)

  @doc """
  Main function to run the simulation.
  
  Reads input from 'day12-data.txt' located in the same directory.
  """
  def run do
    # Read input from 'day12-data.txt' and split into lines
    input =
      "day12-data.txt"
      |> File.read!()
      |> String.split("\n", trim: true)

    moons = parse_input(input)
    final_moons = simulate(moons, 1000)
    IO.puts("Total energy after 1000 steps: #{total_energy(final_moons)}")
  end
end

# To execute the simulation, you can run:
# MoonSimulator.run()

# If you're using the Elixir interactive shell (IEx), you can paste the above module and then run:
# MoonSimulator.run()
