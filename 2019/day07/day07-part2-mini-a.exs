defmodule IntcodeState do
  @moduledoc """
  Represents the state of an Intcode computer.
  """
  defstruct program: [], pointer: 0, inputs: [], halted: false
end

defmodule IntcodeComputer do
  @moduledoc """
  Implements the Intcode computer with the ability to pause and resume execution.
  """

  # Runs the Intcode program until it produces an output, needs input, or halts.
  def run_until_output_or_halt(state = %IntcodeState{halted: true}) do
    {:halted, state}
  end

  def run_until_output_or_halt(state = %IntcodeState{}) do
    opcode = Enum.at(state.program, state.pointer, 0)
    {instruction, modes} = parse_opcode(opcode)

    case instruction do
      1 -> # Addition
        {val1, val2, dest} = get_params(state.program, state.pointer, modes, 2)
        new_program = List.replace_at(state.program, dest, val1 + val2)
        new_state = %IntcodeState{state | program: new_program, pointer: state.pointer + 4}
        run_until_output_or_halt(new_state)

      2 -> # Multiplication
        {val1, val2, dest} = get_params(state.program, state.pointer, modes, 2)
        new_program = List.replace_at(state.program, dest, val1 * val2)
        new_state = %IntcodeState{state | program: new_program, pointer: state.pointer + 4}
        run_until_output_or_halt(new_state)

      3 -> # Input
        case state.inputs do
          [input | remaining_inputs] ->
            dest = Enum.at(state.program, state.pointer + 1)
            new_program = List.replace_at(state.program, dest, input)
            new_state = %IntcodeState{
              state
              | program: new_program,
                inputs: remaining_inputs,
                pointer: state.pointer + 2
            }
            run_until_output_or_halt(new_state)

          [] ->
            {:input_needed, state}
        end

      4 -> # Output
        param = Enum.at(state.program, state.pointer + 1, 0)
        val = get_param_value(state.program, param, Enum.at(modes, 0, 0))
        new_state = %IntcodeState{state | pointer: state.pointer + 2}
        {:output, val, new_state}

      5 -> # Jump-if-true
        val = get_param_value(state.program, Enum.at(state.program, state.pointer + 1, 0), Enum.at(modes, 0, 0))
        target = get_param_value(state.program, Enum.at(state.program, state.pointer + 2, 0), Enum.at(modes, 1, 0))
        new_pointer = if val != 0, do: target, else: state.pointer + 3
        new_state = %IntcodeState{state | pointer: new_pointer}
        run_until_output_or_halt(new_state)

      6 -> # Jump-if-false
        val = get_param_value(state.program, Enum.at(state.program, state.pointer + 1, 0), Enum.at(modes, 0, 0))
        target = get_param_value(state.program, Enum.at(state.program, state.pointer + 2, 0), Enum.at(modes, 1, 0))
        new_pointer = if val == 0, do: target, else: state.pointer + 3
        new_state = %IntcodeState{state | pointer: new_pointer}
        run_until_output_or_halt(new_state)

      7 -> # Less than
        {val1, val2, dest} = get_params(state.program, state.pointer, modes, 2)
        new_program = List.replace_at(state.program, dest, if(val1 < val2, do: 1, else: 0))
        new_state = %IntcodeState{state | program: new_program, pointer: state.pointer + 4}
        run_until_output_or_halt(new_state)

      8 -> # Equals
        {val1, val2, dest} = get_params(state.program, state.pointer, modes, 2)
        new_program = List.replace_at(state.program, dest, if(val1 == val2, do: 1, else: 0))
        new_state = %IntcodeState{state | program: new_program, pointer: state.pointer + 4}
        run_until_output_or_halt(new_state)

      99 -> # Halt
        {:halted, %IntcodeState{state | halted: true}}

      _ ->
        raise "Unknown opcode #{instruction} at position #{state.pointer}"
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
end

defmodule AmplificationCircuit do
  @moduledoc """
  Solves Advent of Code 2019 Day 7 Part Two: Amplification Circuit with Feedback Loop.
  """

  def main do
    # Read the Intcode program from "input.txt"
    {:ok, input} = File.read("input.txt")
    program = parse_input(input)

    # Generate all permutations of phase settings [5,6,7,8,9]
    phase_settings = permutations([5, 6, 7, 8, 9])

    # For each permutation, calculate the thruster signal using feedback loop
    signals =
      phase_settings
      |> Enum.map(fn phases -> run_amplifiers_with_feedback(phases, program) end)

    # Find the maximum signal
    max_signal = Enum.max(signals)
    IO.puts("Max thruster signal (Part Two): #{max_signal}")
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

  # Runs the amplification circuit with feedback loop for a given phase setting
  defp run_amplifiers_with_feedback(phases, program) do
    # Initialize five amplifiers with their phase settings as the first input
    amplifiers =
      phases
      |> Enum.map(fn phase ->
        %IntcodeState{program: program, pointer: 0, inputs: [phase], halted: false}
      end)

    # Start the feedback loop with input signal 0
    feedback_loop(amplifiers, 0, 0)
  end

  # Recursive function to manage the feedback loop
  defp feedback_loop(amplifiers, input_signal, last_output) do
    # Iterate over each amplifier, passing the input_signal
    {new_amplifiers, new_input_signal, new_last_output} =
      Enum.reduce(Enum.with_index(amplifiers), {[], input_signal, last_output}, fn {amp, idx}, {acc, current_input, current_last_output} ->
        if amp.halted do
          {acc ++ [amp], current_input, current_last_output}
        else
          # Add the current input to the amplifier's input queue
          updated_amp = %IntcodeState{amp | inputs: amp.inputs ++ [current_input]}

          # Run the amplifier until it outputs, needs input, or halts
          case IntcodeComputer.run_until_output_or_halt(updated_amp) do
            {:output, output, new_state} ->
              # If this is the last amplifier (E), update last_output
              new_last_output = if idx == 4, do: output, else: current_last_output
              # The output becomes the input for the next amplifier
              {acc ++ [new_state], output, new_last_output}

            {:input_needed, new_state} ->
              # If input is needed and not provided, just pass the state along
              {acc ++ [new_state], current_input, current_last_output}

            {:halted, new_state} ->
              {acc ++ [new_state], current_input, current_last_output}
          end
        end
      end)

    # Check if all amplifiers have halted
    if Enum.all?(new_amplifiers, fn amp -> amp.halted end) do
      new_last_output
    else
      # Continue the loop with the new input_signal
      feedback_loop(new_amplifiers, new_input_signal, new_last_output)
    end
  end

  # Helper function to run the program
  def run do
    main()
  end
end

# Execute the program
AmplificationCircuit.run()
