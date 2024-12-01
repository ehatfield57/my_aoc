defmodule Intcode do
  @moduledoc """
  Intcode computer implementation for Advent of Code 2019.
  """

  def run(program, input \\ []) do
    memory = initialize_memory(program)
    execute(memory, 0, 0, input, [])
  end

  defp initialize_memory(program) do
    # Initialize memory as a map for dynamic access
    Enum.with_index(program)
    |> Enum.reduce(%{}, fn {val, idx}, acc -> Map.put(acc, idx, val) end)
  end

  defp execute(memory, ip, relative_base, input, output) do
    {opcode, modes} = parse_instruction(Map.get(memory, ip, 0))

    case opcode do
      1 ->  # Addition
        {param1, param2, dest} = get_params(memory, ip, modes, relative_base, 3)
        memory = Map.put(memory, dest, param1 + param2)
        execute(memory, ip + 4, relative_base, input, output)

      2 ->  # Multiplication
        {param1, param2, dest} = get_params(memory, ip, modes, relative_base, 3)
        memory = Map.put(memory, dest, param1 * param2)
        execute(memory, ip + 4, relative_base, input, output)

      3 ->  # Input
        case input do
          [in_val | rest_input] ->
            {_, modes_param} = parse_instruction(Map.get(memory, ip, 0))
            dest = get_address(memory, ip + 1, Enum.at(modes_param, 0), relative_base)
            memory = Map.put(memory, dest, in_val)
            execute(memory, ip + 2, relative_base, rest_input, output)
          
          [] ->
            {:need_input, memory, ip, relative_base, output}
        end

      4 ->  # Output
        {param, _, _} = get_params(memory, ip, modes, relative_base, 1)
        execute(memory, ip + 2, relative_base, input, [param | output])

      5 ->  # Jump-if-true
        {param1, param2, _} = get_params(memory, ip, modes, relative_base, 2)
        if param1 != 0 do
          execute(memory, param2, relative_base, input, output)
        else
          execute(memory, ip + 3, relative_base, input, output)
        end

      6 ->  # Jump-if-false
        {param1, param2, _} = get_params(memory, ip, modes, relative_base, 2)
        if param1 == 0 do
          execute(memory, param2, relative_base, input, output)
        else
          execute(memory, ip + 3, relative_base, input, output)
        end

      7 ->  # Less than
        {param1, param2, dest} = get_params(memory, ip, modes, relative_base, 3)
        memory = Map.put(memory, dest, if(param1 < param2, do: 1, else: 0))
        execute(memory, ip + 4, relative_base, input, output)

      8 ->  # Equals
        {param1, param2, dest} = get_params(memory, ip, modes, relative_base, 3)
        memory = Map.put(memory, dest, if(param1 == param2, do: 1, else: 0))
        execute(memory, ip + 4, relative_base, input, output)

      9 ->  # Adjust relative base
        {param, _, _} = get_params(memory, ip, modes, relative_base, 1)
        execute(memory, ip + 2, relative_base + param, input, output)

      99 ->  # Halt
        {:halt, Enum.reverse(output)}

      _ ->
        {:error, "Unknown opcode #{opcode} at position #{ip}"}
    end
  end

  defp parse_instruction(instruction) do
    opcode = rem(instruction, 100)
    modes = instruction |> div(100) |> Integer.digits() |> Enum.reverse()
    {opcode, modes}
  end

  defp get_params(memory, ip, modes, relative_base, count) do
    params = Enum.map(1..count, fn i -> Map.get(memory, ip + i, 0) end)
    Enum.map(params, fn param ->
      mode = Enum.at(modes, Enum.find_index(1..count, fn x -> x == x end), 0)
      case mode do
        0 -> Map.get(memory, param, 0)
        1 -> param
        2 -> Map.get(memory, relative_base + param, 0)
        _ -> 0
      end
    end)
    |> List.to_tuple()
  end

  defp get_address(memory, address, mode, relative_base) do
    case mode do
      0 -> Map.get(memory, address, 0)
      2 -> relative_base + Map.get(memory, address, 0)
      _ -> Map.get(memory, address, 0)
    end
  end
end

defmodule Day13Part1 do
  @moduledoc """
  Solution for Advent of Code 2019 Day 13 Part 1.
  """

  def run do
    program = read_input("day13-data.txt")
    {:halt, output} = Intcode.run(program)
    tiles = parse_output(Enum.reverse(output))
    block_tiles = Enum.count(tiles, fn {_x, _y, tile_id} -> tile_id == 2 end)
    IO.puts("Number of block tiles: #{block_tiles}")
  end

  defp read_input(filename) do
    {:ok, content} = File.read(filename)
    content
    |> String.trim()
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
  end

  defp parse_output([]), do: []
  
  defp parse_output([tile_id, y, x | rest]) do
    [{x, y, tile_id} | parse_output(rest)]
  end

  defp parse_output(_), do: []
end

# To execute the solution, uncomment the following line:
# Day13Part1.run()
