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
    # Generate all unique pairs of moons
    pairs = for {moon1, idx1} <- Enum.with_index(moons),
                {moon2, idx2} <- Enum.with_index(moons),
                idx2 > idx1,
                do: {moon1, moon2}

    # Calculate velocity changes for each pair
    velocity_changes = Enum.reduce(pairs, %{}, fn {m1, m2}, acc ->
      {dv1, dv2} = compare_positions(m1.pos, m2.pos)
      acc
      |> Map.update(m1, dv1, fn {vx, vy, vz} -> add_tuples({vx, vy, vz}, dv1) end)
      |> Map.update(m2, dv2, fn {vx, vy, vz} -> add_tuples({vx, vy, vz}, dv2) end)
    end)

    # Update velocities based on the accumulated changes
    Enum.map(Enum.with_index(moons), fn {moon, idx} ->
      case Map.get(velocity_changes, moon) do
        nil -> moon
        {dvx, dvy, dvz} ->
          {vx, vy, vz} = moon.vel
          %Moon{moon | vel: {vx + dvx, vy + dvy, vz + dvz}}
      end
    end)
  end

  @doc """
  Compares positions of two moons and returns the velocity changes.
  """
  def compare_positions({x1, y1, z1}, {x2, y2, z2}) do
    {delta_x, delta_y, delta_z} =
      {compare_coord(x1, x2), compare_coord(y1, y2), compare_coord(z1, z2)}
    
    {delta_x, delta_y, delta_z}, { -delta_x, -delta_y, -delta_z}
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
  
  Replace the `input` variable with your puzzle input as a list of strings.
  """
  def run do
    input = [
      # Replace these lines with your actual input
      "<x=-1, y=0, z=2>",
      "<x=2, y=-10, z=-7>",
      "<x=4, y=-8, z=8>",
      "<x=3, y=5, z=-1>"
    ]

    moons = parse_input(input)
    final_moons = simulate(moons, 1000)
    IO.puts("Total energy after 1000 steps: #{total_energy(final_moons)}")
  end
end

# To execute the simulation, you can run:
# MoonSimulator.run()

# If you're using the Elixir interactive shell (IEx), you can paste the above module and then run:
# MoonSimulator.run()
