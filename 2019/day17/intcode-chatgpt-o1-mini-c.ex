defmodule Intcode do
  @moduledoc """
  Intcode computer implementation for Advent of Code 2019
  """

  def run(program, input) do
    memory = initialize_memory(program)
    execute(%{
      memory: memory,
      ip: 0,
      relative_base: 0,
      input: input,
      output: []
    })
    |> Enum.reverse()
  end

  # Initialize memory as a Map with default value 0 for undefined addresses
  defp initialize_memory(program) do
    program
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {val, idx}, acc ->
      Map.put(acc, idx, val)
    end)
  end

  # Recursive execution of the Intcode program
  defp execute(state) do
    opcode = get(state.memory, state.ip) |> rem(100)
    modes = div(get(state.memory, state.ip), 100)

    case opcode do
      1 -> # Add
        {param1, param2, dest} = get_params(state, modes, [:read, :read, :write])
        memory = Map.put(state.memory, dest, param1 + param2)
        execute(%{state | memory: memory, ip: state.ip + 4})

      2 -> # Multiply
        {param1, param2, dest} = get_params(state, modes, [:read, :read, :write])
        memory = Map.put(state.memory, dest, param1 * param2)
        execute(%{state | memory: memory, ip: state.ip + 4})

      3 -> # Input
        case state.input do
          [inp | rest] ->
            dest = get_params(state, modes, [:write]) |> elem(0)
            memory = Map.put(state.memory, dest, inp)
            execute(%{state | memory: memory, ip: state.ip + 2, input: rest})

          [] ->
            # No input available; halt and return output
            state.output |> Enum.reverse()
        end

      4 -> # Output
        {param1} = get_params(state, modes, [:read])
        execute(%{state | output: [param1 | state.output], ip: state.ip + 2})

      5 -> # Jump-if-true
        {param1, param2} = get_params(state, modes, [:read, :read])
        if param1 != 0 do
          execute(%{state | ip: param2})
        else
          execute(%{state | ip: state.ip + 3})
        end

      6 -> # Jump-if-false
        {param1, param2} = get_params(state, modes, [:read, :read])
        if param1 == 0 do
          execute(%{state | ip: param2})
        else
          execute(%{state | ip: state.ip + 3})
        end

      7 -> # Less than
        {param1, param2, dest} = get_params(state, modes, [:read, :read, :write])
        val = if param1 < param2, do: 1, else: 0
        memory = Map.put(state.memory, dest, val)
        execute(%{state | memory: memory, ip: state.ip + 4})

      8 -> # Equals
        {param1, param2, dest} = get_params(state, modes, [:read, :read, :write])
        val = if param1 == param2, do: 1, else: 0
        memory = Map.put(state.memory, dest, val)
        execute(%{state | memory: memory, ip: state.ip + 4})

      9 -> # Adjust relative base
        {param1} = get_params(state, modes, [:read])
        execute(%{state | relative_base: state.relative_base + param1, ip: state.ip + 2})

      99 -> # Halt
        state.output |> Enum.reverse()

      _ ->
        raise "Unknown opcode #{opcode} at position #{state.ip}"
    end
  end

  # Fetch parameters based on modes and parameter types (:read or :write)
  defp get_params(state, modes, param_types) do
    modes_list = parse_modes(modes, length(param_types))

    Enum.zip(param_types, modes_list)
    |> Enum.with_index()
    |> Enum.map(fn {{type, mode}, idx} ->
      param = get(state.memory, state.ip + idx + 1)
      case {type, mode} do
        {:read, 0} -> get(state.memory, param) # Position mode
        {:read, 1} -> param                   # Immediate mode
        {:read, 2} -> get(state.memory, state.relative_base + param) # Relative mode
        {:write, 0} -> param                  # Position mode for writing
        {:write, 2} -> state.relative_base + param # Relative mode for writing
        {:write, 1} -> raise "Invalid mode for write parameter: 1"
      end
    end)
    |> List.to_tuple()
  end

  # Parse the modes for the given number of parameters
  defp parse_modes(modes, count) do
    for i <- 0..(count - 1), do: div(modes, :math.pow(10, i) |> trunc()) |> rem(10)
  end

  # Get the value from memory, defaulting to 0 if undefined
  defp get(memory, address) do
    Map.get(memory, address, 0)
  end
end
