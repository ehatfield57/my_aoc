defmodule AmplificationCircuit do
  # Entry point of the program
  def main do
    # Read the Intcode program from "input.txt"
    {:ok, input} = File.read("input.txt")
    program = parse_input(input)

    # Generate all permutations of phase settings [0,1,2,3,4]
    phase_settings = permutations([0, 1, 2, 3, 4])

    # For each permutation, calculate the thruster signal
    signals =
      phase_settings
      |> Enum.map(fn phases -> run_amplifiers(phases, program) end)

    # Find the maximum signal
    max_signal = Enum.max(signals)
    IO.puts("Max thruster signal: #{max_signal}")
  end

  # Parses the input string into a list of integers
  defp parse_input(input) do
    input
    |> String.trim()
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
  end

  # Generates all permutations of a list
  defp permutations([]), do: [[]]
  defp permutations(list) do
    for elem <- list, rest <- permutations(list -- [elem]), do: [elem | rest]
  end

  # Runs the amplification circuit for a given phase setting
  defp run_amplifiers(phases, program) do
    # Initial input signal is 0
    Enum.reduce(phases, 0, fn phase, input_signal ->
      # Each amplifier takes two inputs: phase and input_signal
      run_intcode(program, [phase, input_signal])
    end)
  end

  # Runs the Intcode program with given inputs and returns the output
  defp run_intcode(program, inputs) do
    run_intcode(program, inputs, 0)
  end

  # Recursive Intcode runner
  defp run_intcode(program, inputs, pointer) do
    opcode = Enum.at(program, pointer)
    {instruction, modes} = parse_opcode(opcode)

    case instruction do
      1 -> # Addition
        {val1, val2, dest} = get_params(program, pointer, modes, 2)
        new_program = List.replace_at(program, dest, val1 + val2)
        run_intcode(new_program, inputs, pointer + 4)

      2 -> # Multiplication
        {val1, val2, dest} = get_params(program, pointer, modes, 2)
        new_program = List.replace_at(program, dest, val1 * val2)
        run_intcode(new_program, inputs, pointer + 4)

      3 -> # Input
        case inputs do
          [input | remaining_inputs] ->
            dest = Enum.at(program, pointer + 1)
            new_program = List.replace_at(program, dest, input)
            run_intcode(new_program, remaining_inputs, pointer + 2)
          [] ->
            raise "No input available for opcode 3 at position #{pointer}"
        end

      4 -> # Output
        val = get_param_value(program, Enum.at(program, pointer + 1), Enum.at(modes, 0))
        # For Part One, we can assume the program halts after output
        val

      5 -> # Jump-if-true
        val = get_param_value(program, Enum.at(program, pointer + 1), Enum.at(modes, 0))
        target = get_param_value(program, Enum.at(program, pointer + 2), Enum.at(modes, 1))
        if val != 0 do
          run_intcode(program, inputs, target)
        else
          run_intcode(program, inputs, pointer + 3)
        end

      6 -> # Jump-if-false
        val = get_param_value(program, Enum.at(program, pointer + 1), Enum.at(modes, 0))
        target = get_param_value(program, Enum.at(program, pointer + 2), Enum.at(modes, 1))
        if val == 0 do
          run_intcode(program, inputs, target)
        else
          run_intcode(program, inputs, pointer + 3)
        end

      7 -> # Less than
        {val1, val2, dest} = get_params(program, pointer, modes, 2)
        new_program = List.replace_at(program, dest, if(val1 < val2, do: 1, else: 0))
        run_intcode(new_program, inputs, pointer + 4)

      8 -> # Equals
        {val1, val2, dest} = get_params(program, pointer, modes, 2)
        new_program = List.replace_at(program, dest, if(val1 == val2, do: 1, else: 0))
        run_intcode(new_program, inputs, pointer + 4)

      99 -> # Halt
        # For Part One, we expect to have produced an output before halting
        raise "Program halted without producing output"

      _ ->
        raise "Unknown opcode #{instruction} at position #{pointer}"
    end
  end

  # Parses the opcode and parameter modes
  defp parse_opcode(opcode) do
    opcode_str = Integer.to_string(opcode) |> String.pad_leading(5, "0")
    instruction = String.slice(opcode_str, 3..4) |> String.to_integer()
    modes =
      opcode_str
      |> String.slice(0..2)
      |> String.graphemes()
      |> Enum.map(&String.to_integer/1)
      |> Enum.reverse()
    {instruction, modes}
  end

  # Retrieves parameters based on modes
  defp get_params(program, pointer, modes, num_params) do
    params = Enum.slice(program, pointer + 1, num_params + 1)

    vals =
      params
      |> Enum.take(num_params)
      |> Enum.with_index()
      |> Enum.map(fn {param, idx} ->
        mode = Enum.at(modes, idx, 0)
        get_param_value(program, param, mode)
      end)

    # Correctly retrieve the destination from the params slice
    dest = Enum.at(params, num_params, 0)

    case num_params do
      2 -> {Enum.at(vals, 0), Enum.at(vals, 1), dest}
      _ -> raise "Unsupported number of parameters: #{num_params}"
    end
  end

  # Retrieves a single parameter value based on its mode
  defp get_param_value(program, param, 0) do
    Enum.at(program, param, 0)
  end

  defp get_param_value(_program, param, 1) do
    param
  end

  # Helper function to start the program
  def run do
    main()
  end
end

# Execute the program
AmplificationCircuit.run()
