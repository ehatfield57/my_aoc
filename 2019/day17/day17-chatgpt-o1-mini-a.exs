defmodule AdventOfCode.Day17.Part1 do
  @moduledoc """
  Solution for Advent of Code 2019, Day 17, Part 1
  """

  def main do
    # Read the input Intcode program from a file or define it as a string
    # For demonstration, let's assume it's in a file named "day17-data.txt"
    input = File.read!("day17-data.txt") |> String.trim()
    program = parse_input(input)

    # Run the Intcode program
    output = Intcode.run(program, [])

    # Convert the ASCII output to a grid
    grid = parse_grid(output)

    # Find intersections and calculate the sum of alignment parameters
    sum = find_intersections(grid)

    IO.puts("Sum of alignment parameters: #{sum}")
  end

  defp parse_input(input) do
    input
    |> String.split(",", trim: true)
    |> Enum.map(&String.to_integer/1)
  end

  defp parse_grid(output) do
    output
    |> Enum.map(&List.to_string([&1]))
    |> Enum.join()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.graphemes/1)
  end

  defp find_intersections(grid) do
    rows = length(grid)
    cols = grid |> List.first() |> length()

    for y <- 1..(rows - 2),
        x <- 1..(cols - 2),
        is_intersection?(grid, x, y) do
      x * y
    end
    |> Enum.sum()
  end

  defp is_intersection?(grid, x, y) do
    current = grid |> Enum.at(y) |> Enum.at(x)
    up = grid |> Enum.at(y - 1) |> Enum.at(x)
    down = grid |> Enum.at(y + 1) |> Enum.at(x)
    left = grid |> Enum.at(y) |> Enum.at(x - 1)
    right = grid |> Enum.at(y) |> Enum.at(x + 1)

    current == "#" and up == "#" and down == "#" and left == "#" and right == "#"
  end
end

defmodule Intcode do
  @moduledoc """
  Intcode computer implementation for Advent of Code 2019
  """

  def run(program, input) do
    memory = :array.from_list(program) |> :array.set_bounds(0, length(program) * 10)
    execute(%{memory: memory, ip: 0, relative_base: 0, input: input, output: []})
    |> Enum.reverse()
  end

  defp execute(state) do
    opcode = get(state.memory, state.ip) |> rem(100)
    modes = get(state.memory, state.ip) |> div(100)

    case opcode do
      1 -> # Add
        {a, b, dest} = get_params(state, modes, 3)
        memory = set(state.memory, dest, a + b)
        execute(%{state | memory: memory, ip: state.ip + 4})

      2 -> # Multiply
        {a, b, dest} = get_params(state, modes, 3)
        memory = set(state.memory, dest, a * b)
        execute(%{state | memory: memory, ip: state.ip + 4})

      3 -> # Input
        case state.input do
          [inp | rest] ->
            {_, _, dest} = get_params(state, modes, 1)
            memory = set(state.memory, dest, inp)
            execute(%{state | memory: memory, ip: state.ip + 2, input: rest})

          [] ->
            state.output
            |> Enum.reverse()
        end

      4 -> # Output
        {a, _, _} = get_params(state, modes, 1)
        execute(%{state | memory: state.memory, ip: state.ip + 2, output: [a | state.output]})

      5 -> # Jump-if-true
        {a, b, _} = get_params(state, modes, 2)
        if a != 0 do
          execute(%{state | ip: b})
        else
          execute(%{state | ip: state.ip + 3})
        end

      6 -> # Jump-if-false
        {a, b, _} = get_params(state, modes, 2)
        if a == 0 do
          execute(%{state | ip: b})
        else
          execute(%{state | ip: state.ip + 3})
        end

      7 -> # Less than
        {a, b, dest} = get_params(state, modes, 3)
        val = if a < b, do: 1, else: 0
        memory = set(state.memory, dest, val)
        execute(%{state | memory: memory, ip: state.ip + 4})

      8 -> # Equals
        {a, b, dest} = get_params(state, modes, 3)
        val = if a == b, do: 1, else: 0
        memory = set(state.memory, dest, val)
        execute(%{state | memory: memory, ip: state.ip + 4})

      9 -> # Adjust relative base
        {a, _, _} = get_params(state, modes, 1)
        execute(%{state | relative_base: state.relative_base + a, ip: state.ip + 2})

      99 -> # Halt
        state.output |> Enum.reverse()

      _ ->
        raise "Unknown opcode #{opcode} at position #{state.ip}"
    end
  end

  defp get_params(state, modes, num_params) do
    Enum.map(0..(num_params - 1), fn i ->
      mode = div(modes, :math.pow(10, i) |> trunc()) |> rem(10)
      param = get(state.memory, state.ip + 1 + i)

      case {i, mode} do
        {^i, 0} -> get(state.memory, param) # Position mode
        {^i, 1} -> param # Immediate mode
        {^i, 2} -> get(state.memory, state.relative_base + param) # Relative mode
      end
    end)
    |> case do
      [a, b, dest] ->
        dest_mode = div(modes, 1000) |> rem(10)
        dest =
          case dest_mode do
            0 -> dest
            2 -> state.relative_base + dest
            _ -> dest
          end

        {Enum.at([a, b], 0), Enum.at([a, b], 1), dest}

      [a, dest] ->
        {a, nil, dest}

      [a] ->
        {a, nil, nil}

      _ ->
        {nil, nil, nil}
    end
  end

  defp get(memory, address) do
    case :array.get(address, memory) do
      :undefined -> 0
      val -> val
    end
  end

  defp set(memory, address, value) do
    if address >= :array.size(memory) do
      # Extend the array
      memory = :array.resize(address + 1, memory)
    end
    :array.set(address, value, memory)
  end
end

# To execute the solution, ensure that your Intcode program is saved in "day17-data.txt" in the same directory.
# Then, you can run the following in your Elixir environment:

AdventOfCode.Day17.Part1.main()
