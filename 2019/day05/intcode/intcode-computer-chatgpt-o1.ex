defmodule IntcodeComputer do
  defstruct memory: %{}, ip: 0, relative_base: 0, inputs: [], outputs: [], halted: false

  def new(program, inputs \\ []) do
    memory =
      program
      |> Enum.with_index()
      |> Map.new(fn {value, index} -> {index, value} end)

    %IntcodeComputer{memory: memory, inputs: inputs}
  end

  def run(computer) do
    if computer.halted do
      computer
    else
      computer
      |> step()
      |> run()
    end
  end

  defp step(%IntcodeComputer{memory: memory, ip: ip} = computer) do
    opcode_full = Map.get(memory, ip, 0)
    opcode = rem(opcode_full, 100)
    modes = Enum.map(2..4, fn i -> div(opcode_full, :math.pow(10, i)) |> rem(10) end)

    case opcode do
      # Addition
      1 ->
        {param1, param2, dest} = get_params(computer, modes, 3)
        value = param1 + param2

        computer
        |> write_memory(dest, value)
        |> advance_ip(4)

      # Multiplication
      2 ->
        {param1, param2, dest} = get_params(computer, modes, 3)
        value = param1 * param2

        computer
        |> write_memory(dest, value)
        |> advance_ip(4)

      # Input
      3 ->
        {dest} = get_write_params(computer, modes, 1)

        case computer.inputs do
          [input_value | rest_inputs] ->
            computer
            |> write_memory(dest, input_value)
            |> advance_ip(2)
            |> Map.put(:inputs, rest_inputs)

          [] ->
            raise "No input available"
        end

      # Output
      4 ->
        {param} = get_params(computer, modes, 1)

        computer
        |> Map.update!(:outputs, &[param | &1])
        |> advance_ip(2)

      # Jump-if-true
      5 ->
        {param1, param2} = get_params(computer, modes, 2)

        if param1 != 0 do
          Map.put(computer, :ip, param2)
        else
          advance_ip(computer, 3)
        end

      # Jump-if-false
      6 ->
        {param1, param2} = get_params(computer, modes, 2)

        if param1 == 0 do
          Map.put(computer, :ip, param2)
        else
          advance_ip(computer, 3)
        end

      # Less than
      7 ->
        {param1, param2, dest} = get_params(computer, modes, 3)
        value = if param1 < param2, do: 1, else: 0

        computer
        |> write_memory(dest, value)
        |> advance_ip(4)

      # Equals
      8 ->
        {param1, param2, dest} = get_params(computer, modes, 3)
        value = if param1 == param2, do: 1, else: 0

        computer
        |> write_memory(dest, value)
        |> advance_ip(4)

      # Adjust relative base
      9 ->
        {param} = get_params(computer, modes, 1)

        computer
        |> Map.update!(:relative_base, &(&1 + param))
        |> advance_ip(2)

      # Halt
      99 ->
        Map.put(computer, :halted, true)

      _ ->
        raise "Unknown opcode: #{opcode}"
    end
  end

  defp get_params(computer, modes, count) do
    Enum.map(1..count, fn offset ->
      mode = Enum.at(modes, offset - 1)
      get_param(computer, mode, offset)
    end)
    |> List.to_tuple()
  end

  defp get_write_params(computer, modes, count) do
    Enum.map(1..count, fn offset ->
      mode = Enum.at(modes, offset - 1)
      get_write_param(computer, mode, offset)
    end)
    |> List.to_tuple()
  end

  defp get_param(%IntcodeComputer{memory: memory, ip: ip, relative_base: rb}, mode, offset) do
    value = Map.get(memory, ip + offset, 0)

    case mode do
      # Position mode
      0 -> Map.get(memory, value, 0)
      # Immediate mode
      1 -> value
      # Relative mode
      2 -> Map.get(memory, rb + value, 0)
      _ -> raise "Unknown parameter mode: #{mode}"
    end
  end

  defp get_write_param(%IntcodeComputer{memory: memory, ip: ip, relative_base: rb}, mode, offset) do
    value = Map.get(memory, ip + offset, 0)

    case mode do
      # Position mode
      0 -> value
      # Relative mode
      2 -> rb + value
      _ -> raise "Invalid mode for writing: #{mode}"
    end
  end

  defp write_memory(computer, address, value) do
    Map.update!(computer, :memory, &Map.put(&1, address, value))
  end

  defp advance_ip(computer, steps) do
    Map.update!(computer, :ip, &(&1 + steps))
  end

  def get_outputs(%IntcodeComputer{outputs: outputs}) do
    Enum.reverse(outputs)
  end
end
