defmodule AdventOfCode2019.Day7 do
  def parse_intcode(input) do
    input
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
  end

  def get_param(mode, param, intcode) do
    case mode do
      0 -> Enum.at(intcode, param, 0)  # Position mode (default to 0 if out of bounds)
      1 -> param  # Immediate mode
    end
  end

  defp ensure_memory_size(intcode, index) do
    if index >= length(intcode) do
      intcode ++ List.duplicate(0, index - length(intcode) + 1)
    else
      intcode
    end
  end

  defp write_memory(intcode, index, value) do
    IO.inspect(%{index: index, value: value, action: "Write"}, label: "Memory Write Operation")
    List.replace_at(intcode, index, value)
  end

  def run_intcode(intcode, inputs, pc \\ 0) do
    opcode = rem(Enum.at(intcode, pc), 100)
    mode1 = rem(div(Enum.at(intcode, pc), 100), 10)
    mode2 = rem(div(Enum.at(intcode, pc), 1000), 10)

    param1 = Enum.at(intcode, pc + 1, 0)
    param2 = Enum.at(intcode, pc + 2, 0)
    param3 = Enum.at(intcode, pc + 3, 0)

    intcode = ensure_memory_size(intcode, param1)
    intcode = ensure_memory_size(intcode, param2)
    intcode = ensure_memory_size(intcode, param3)

    IO.inspect(%{pc: pc, opcode: opcode, intcode_slice: Enum.slice(intcode, pc, 10)}, label: "Intcode Execution State")

    case opcode do
      1 ->
        intcode = write_memory(intcode, param3, get_param(mode1, param1, intcode) + get_param(mode2, param2, intcode))
        run_intcode(intcode, inputs, pc + 4)

      2 ->
        intcode = write_memory(intcode, param3, get_param(mode1, param1, intcode) * get_param(mode2, param2, intcode))
        run_intcode(intcode, inputs, pc + 4)

      3 ->
        case inputs do
          [input | inputs_tail] ->
            intcode = write_memory(intcode, param1, input)
            run_intcode(intcode, inputs_tail, pc + 2)

          [] ->
            {:await_input, intcode, pc}
        end

      4 ->
        output = get_param(mode1, param1, intcode)
        {:output, output, intcode, pc + 2}

      5 ->
        new_pc = if get_param(mode1, param1, intcode) != 0, do: get_param(mode2, param2, intcode), else: pc + 3
        run_intcode(intcode, inputs, new_pc)

      6 ->
        new_pc = if get_param(mode1, param1, intcode) == 0, do: get_param(mode2, param2, intcode), else: pc + 3
        run_intcode(intcode, inputs, new_pc)

      7 ->
        intcode = write_memory(intcode, param3, if get_param(mode1, param1, intcode) < get_param(mode2, param2, intcode), do: 1, else: 0)
        run_intcode(intcode, inputs, pc + 4)

      8 ->
        intcode = write_memory(intcode, param3, if get_param(mode1, param1, intcode) == get_param(mode2, param2, intcode), do: 1, else: 0)
        run_intcode(intcode, inputs, pc + 4)

      99 ->
        {:halt, intcode}

      _ ->
        {:error, "Unknown opcode at pc #{pc}: #{opcode}"}
    end
  end

  def amplifier_loop_sequence(intcode, phases) do
    # Initialize independent states for each amplifier by copying the initial intcode
    states = Enum.map(phases, fn phase ->
      {Enum.clone(intcode), [phase], 0} # Create a new copy of the intcode for each amplifier
    end)

    # Start the feedback loop
    loop(states, 0, 0, nil)
  end

  defp loop(states, current_amp, input, last_output) do
    {intcode, inputs, pc} = Enum.at(states, current_amp)

    case run_intcode(intcode, [input | inputs], pc) do
      {:output, output, new_intcode, new_pc} ->
        new_states = List.replace_at(states, current_amp, {new_intcode, [], new_pc})

        if current_amp == 4 and last_output != nil and output == last_output do
          last_output
        else
          next_amp = rem(current_amp + 1, length(states))
          loop(new_states, next_amp, output, output)
        end

      {:await_input, new_intcode, new_pc} ->
        new_states = List.replace_at(states, current_amp, {new_intcode, [], new_pc})
        next_amp = rem(current_amp + 1, length(states))
        loop(new_states, next_amp, input, last_output)

      {:halt, _} ->
        if current_amp == 4 do
          last_output
        else
          next_amp = rem(current_amp + 1, length(states))
          loop(states, next_amp, input, last_output)
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  def find_best_phase_setting_with_feedback(intcode) do
    [5, 6, 7, 8, 9]
    |> permutations()
    |> Enum.map(fn phases -> amplifier_loop_sequence(intcode, phases) end)
    |> Enum.max_by(fn
      {:error, _} -> -1
      value -> value
    end)
  end

  defp permutations([]), do: [[]]
  defp permutations(list) do
    for elem <- list, rest <- permutations(list -- [elem]), do: [elem | rest]
  end
end

# Read input from the file 'day07-data.txt'
input = File.read!("day07-data.txt")

# Parse the input data and find the best phase setting with feedback loop
intcode = AdventOfCode2019.Day7.parse_intcode(input)
best_output = AdventOfCode2019.Day7.find_best_phase_setting_with_feedback(intcode)

# Print the result, handling errors gracefully
case best_output do
  {:error, reason} ->
    IO.puts("Error: #{reason}")

  result ->
    IO.puts("Best output with feedback loop: #{result}")
end
