defmodule MoonSimulator do
  @moduledoc """
  Simulates the motion of moons in 3D space and calculates the total energy after a specified number of steps.
  Additionally, it finds the number of steps required for the system to return to a previous state.
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
      acc = List.replace_at(acc, j, add_tuples({vx2, vy2, vz2}, dv2))

      acc
    end)

    # Apply the accumulated velocity changes to each moon's velocity
    Enum.zip(moons, velocity_changes)
    |> Enum.map(fn {%Moon{vel: vel}, dv} ->
      add_tuples(vel, dv)
    end)
    |> Enum.zip(moons)
    |> Enum.map(fn {new_vel, moon} ->
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
  Finds the number of steps required for the system to return to a previous state.

  ## Example

      iex> input = [
      ...>   "<x=-1, y=0, z=2>",
      ...>   "<x=2, y=-10, z=-7>",
      ...>   "<x=4, y=-8, z=8>",
      ...>   "<x=3, y=5, z=-1>"
      ...> ]
      iex> moons = MoonSimulator.parse_input(input)
      iex> MoonSimulator.find_repeat_step(moons)
      2772

  """
  def find_repeat_step(moons) do
    # Extract initial states for each axis
    initial_x = extract_axis_state(moons, :x)
    initial_y = extract_axis_state(moons, :y)
    initial_z = extract_axis_state(moons, :z)

    # Find periods for each axis
    period_x = find_axis_period(moons, :x, initial_x)
    period_y = find_axis_period(moons, :y, initial_y)
    period_z = find_axis_period(moons, :z, initial_z)

    # Compute the Least Common Multiple of the three periods
    lcm(lcm(period_x, period_y), period_z)
  end

  @doc """
  Extracts the state (positions and velocities) for a specific axis.

  Returns a list of tuples `{position, velocity}` for each moon on the specified axis.

  `axis` can be `:x`, `:y`, or `:z`.
  """
  def extract_axis_state(moons, axis) do
    Enum.map(moons, fn %Moon{pos: pos, vel: vel} ->
      {get_axis(pos, axis), get_axis(vel, axis)}
    end)
  end

  @doc """
  Retrieves the value of a specific axis from a tuple.

  `axis` can be `:x`, `:y`, or `:z`.
  """
  def get_axis({x, _y, _z}, :x), do: x
  def get_axis({_x, y, _z}, :y), do: y
  def get_axis({_x, _y, z}, :z), do: z

  @doc """
  Finds the period (number of steps) for a specific axis to repeat its initial state.

  Returns the step count when the axis state matches the initial state again.
  """
  def find_axis_period(initial_moons, axis, initial_state) do
    find_axis_period(initial_moons, axis, initial_state, 0)
  end

  defp find_axis_period(moons, axis, initial_state, step) do
    # Perform one step
    moons = step(moons)

    # Increment step count
    step = step + 1

    # Extract current state for the axis
    current_state = extract_axis_state(moons, axis)

    # Check if current state matches the initial state
    if current_state == initial_state do
      step
    else
      find_axis_period(moons, axis, initial_state, step)
    end
  end

  @doc """
  Computes the Least Common Multiple (LCM) of two integers.
  """
  def lcm(a, b), do: div(a * b, gcd(a, b))

  @doc """
  Computes the Greatest Common Divisor (GCD) of two integers using Euclidean algorithm.
  """
  def gcd(a, 0), do: a
  def gcd(a, b), do: gcd(b, rem(a, b))

  @doc """
  Main function to run the simulation.

  It can perform both Part One and Part Two based on user selection.
  
  - To run Part One (Total Energy after 1000 steps):
      MoonSimulator.run(:part1)
  
  - To run Part Two (Steps to repeat):
      MoonSimulator.run(:part2)
  """
  def run(part \\ :part1) do
    # Read input from 'day12-data.txt' and split into lines
    input =
      "day12-data.txt"
      |> File.read!()
      |> String.split("\n", trim: true)

    moons = parse_input(input)

    case part do
      :part1 ->
        final_moons = simulate(moons, 1000)
        IO.puts("Total energy after 1000 steps: #{total_energy(final_moons)}")

      :part2 ->
        steps_to_repeat = find_repeat_step(moons)
        IO.puts("Steps to reach a previous state: #{steps_to_repeat}")

      _ ->
        IO.puts("Invalid part specified. Use :part1 or :part2.")
    end
  end
end

# Execute the simulation based on command-line arguments
# Allows running either Part One or Part Two by passing arguments
defmodule Main do
  def main(args) do
    case args do
      ["part1"] ->
        MoonSimulator.run(:part1)

      ["part2"] ->
        MoonSimulator.run(:part2)

      _ ->
        IO.puts("""
        Usage:
          elixir moon_simulator.exs part1   # To run Part One
          elixir moon_simulator.exs part2   # To run Part Two
        """)
    end
  end
end

# Invoke the main function with command-line arguments
Main.main(System.argv())
