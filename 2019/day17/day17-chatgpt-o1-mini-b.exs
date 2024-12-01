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
    |> Enum.map(&<<&1>>)
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
    memory = Map.new(Enum.with_index(program), fn {val, idx} -> {idx, val} end)
    execute(%{memory: memory, ip: 0, relative_base: 0, input: input, output: []})
    |> Enum.reverse()
  end

  defp execute(state) do
    opcode = get(state.memory, state.ip) |> rem(100)
    modes = div(get(state.memory, state.ip), 100)

    case opcode do
      1 -> # Add
        {a, b, dest} = get_params(state, modes, 3)
        memory = Map.put(state.memory, dest, a + b)
        execute(%{state | memory: memory, ip: state.ip + 4})

      2 -> # Multiply
        {a, b, dest} = get_params(state, modes, 3)
        memory = Map.put(state.memory, dest, a * b)
        execute(%{state | memory: memory, ip: state.ip + 4})

      3 -> # Input
        case state.input do
          [inp | rest] ->
            {_, _, dest} = get_params(state, modes, 1)
            memory = Map.put(state.memory, dest, inp)
            execute(%{state | memory: memory, ip: state.ip + 2, input: rest})

          [] ->
            state.output
            |> Enum.reverse()
        end

      4 -> # Output
        {a, _, _} = get_params(state, modes, 1)
        execute(%{state | output: [a | state.output], ip: state.ip + 2})

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
        memory = Map.put(state.memory, dest, val)
        execute(%{state | memory: memory, ip: state.ip + 4})

      8 -> # Equals
        {a, b, dest} = get_params(state, modes, 3)
        val = if a == b, do: 1, else: 0
        memory = Map.put(state.memory, dest, val)
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
    modes_list = get_modes(modes, num_params)
    params = for i <- 1..num_params, do: get(state.memory, state.ip + i)

    values =
      Enum.zip(modes_list, params)
      |> Enum.map(fn
        {0, param} -> get(state.memory, param) # Position mode
        {1, param} -> param                  # Immediate mode
        {2, param} -> get(state.memory, state.relative_base + param) # Relative mode
      end)

    # Handle destinations (write parameters) separately
    dest =
      case num_params do
        1 ->
          mode = Enum.at(modes_list, 0)
          param = Enum.at(params, 0)
          compute_destination(mode, param, state.relative_base)

        2 ->
          a = Enum.at(values, 0)
          b = Enum.at(values, 1)
          dest = compute_destination(Enum.at(modes_list, 2), Enum.at(params, 2), state.relative_base)
          {a, b, dest}

        3 ->
          a = Enum.at(values, 0)
          b = Enum.at(values, 1)
          c = Enum.at(values, 2)
          dest = compute_destination(Enum.at(modes_list, 2), c, state.relative_base)
          {a, b, dest}

        _ -> 
          raise "Unsupported number of parameters: #{num_params}"
      end

    case num_params do
      1 -> {Enum.at(values, 0), nil, dest}
      2 -> {Enum.at(values, 0), Enum.at(values, 1), dest}
      3 -> {Enum.at(values, 0), Enum.at(values, 1), dest}
    end
  end

  defp get_modes(modes, num_params) do
    for i <- 0..(num_params - 1), do: div(modes, :math.pow(10, i) |> trunc()) |> rem(10)
  end

  defp compute_destination(mode, param, relative_base) do
    case mode do
      0 -> param
      2 -> relative_base + param
      _ -> raise "Invalid mode for destination: #{mode}"
    end
  end

  defp get(memory, address) do
    Map.get(memory, address, 0)
  end
end

# To execute the solution, ensure that your Intcode program is saved in "day17-data.txt" in the same directory.
# Then, you can run the following in your Elixir environment:

AdventOfCode.Day17.Part1.main()
