defmodule Intcode do
  defstruct memory: %{}, pointer: 0, relative_base: 0, inputs: [], outputs: []

  @opcode_params %{
    1 => 3,
    2 => 3,
    3 => 1,
    4 => 1,
    5 => 2,
    6 => 2,
    7 => 3,
    8 => 3,
    9 => 1,
    99 => 0
  }

  def run(program, inputs \\ []) do
    memory = parse_program(program)
    initial_state = %Intcode{memory: memory, inputs: inputs}
    execute(initial_state)
  end

  defp parse_program(program) do
    program
    |> String.trim()
    |> String.split(",", trim: true)
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {value, index}, acc ->
      Map.put(acc, index, String.to_integer(value))
    end)
  end

  defp execute(state = %Intcode{pointer: pointer, memory: memory}) do
    {opcode, modes} = parse_instruction(Map.get(memory, pointer, 0))

    cond do
      opcode == 99 ->
        state.outputs

      true ->
        {state, new_pointer} = execute_opcode(state, opcode, modes)
        execute(%{state | pointer: new_pointer})
    end
  end

  defp parse_instruction(instruction) do
    opcode = rem(instruction, 100)
    modes = div(instruction, 100)
    {opcode, modes}
  end

  defp get_param(state, offset, mode) do
    val = Map.get(state.memory, state.pointer + offset, 0)

    case mode do
      0 -> Map.get(state.memory, val, 0) # Position mode
      1 -> val                          # Immediate mode
      2 -> Map.get(state.memory, state.relative_base + val, 0) # Relative mode
    end
  end

  defp get_write_address(state, offset, mode) do
    val = Map.get(state.memory, state.pointer + offset, 0)

    case mode do
      0 -> val
      2 -> state.relative_base + val
      _ -> raise "Invalid mode for write address: #{mode}"
    end
  end

  defp execute_opcode(state, opcode, modes) do
    params = @opcode_params[opcode]
    mode_list = for i <- 0..(params - 1), do: rem(div(modes, :math.pow(10, i) |> trunc), 10)

    case opcode do
      1 -> # Add
        param1 = get_param(state, 1, Enum.at(mode_list, 0))
        param2 = get_param(state, 2, Enum.at(mode_list, 1))
        addr = get_write_address(state, 3, Enum.at(mode_list, 2))
        new_memory = Map.put(state.memory, addr, param1 + param2)
        { %{state | memory: new_memory}, state.pointer + 4 }

      2 -> # Multiply
        param1 = get_param(state, 1, Enum.at(mode_list, 0))
        param2 = get_param(state, 2, Enum.at(mode_list, 1))
        addr = get_write_address(state, 3, Enum.at(mode_list, 2))
        new_memory = Map.put(state.memory, addr, param1 * param2)
        { %{state | memory: new_memory}, state.pointer + 4 }

      3 -> # Input
        case state.inputs do
          [input | rest] ->
            addr = get_write_address(state, 1, Enum.at(mode_list, 0))
            new_memory = Map.put(state.memory, addr, input)
            { %{state | memory: new_memory, inputs: rest }, state.pointer + 2 }

          [] ->
            raise "No input available for opcode 3"
        end

      4 -> # Output
        param = get_param(state, 1, Enum.at(mode_list, 0))
        new_outputs = state.outputs ++ [param]
        { %{state | outputs: new_outputs }, state.pointer + 2 }

      5 -> # Jump-if-true
        param1 = get_param(state, 1, Enum.at(mode_list, 0))
        param2 = get_param(state, 2, Enum.at(mode_list, 1))
        if param1 != 0 do
          {state, param2}
        else
          {state, state.pointer + 3}
        end

      6 -> # Jump-if-false
        param1 = get_param(state, 1, Enum.at(mode_list, 0))
        param2 = get_param(state, 2, Enum.at(mode_list, 1))
        if param1 == 0 do
          {state, param2}
        else
          {state, state.pointer + 3}
        end

      7 -> # Less than
        param1 = get_param(state, 1, Enum.at(mode_list, 0))
        param2 = get_param(state, 2, Enum.at(mode_list, 1))
        addr = get_write_address(state, 3, Enum.at(mode_list, 2))
        value = if param1 < param2, do: 1, else: 0
        new_memory = Map.put(state.memory, addr, value)
        { %{state | memory: new_memory }, state.pointer + 4 }

      8 -> # Equals
        param1 = get_param(state, 1, Enum.at(mode_list, 0))
        param2 = get_param(state, 2, Enum.at(mode_list, 1))
        addr = get_write_address(state, 3, Enum.at(mode_list, 2))
        value = if param1 == param2, do: 1, else: 0
        new_memory = Map.put(state.memory, addr, value)
        { %{state | memory: new_memory }, state.pointer + 4 }

      9 -> # Adjust relative base
        param1 = get_param(state, 1, Enum.at(mode_list, 0))
        new_relative_base = state.relative_base + param1
        { %{state | relative_base: new_relative_base }, state.pointer + 2 }

      _ ->
        raise "Unknown opcode: #{opcode}"
    end
  end
end

defmodule AdventOfCode do
  def part_one(input) do
    Intcode.run(input, [1])
    |> List.last()
  end
end

# To execute the solution, you can use the following code.
# Ensure you have the Intcode and AdventOfCode modules defined above.

# Example usage:
# Assuming you have your Intcode program as a string in a file named "input.txt"

defmodule Solution do
  def run do
    input = File.read!("input.txt")
    output = AdventOfCode.part_one(input)
    IO.puts("BOOST keycode: #{output}")
  end
end

# To run the solution, execute:
# Solution.run()
