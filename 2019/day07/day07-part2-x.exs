defmodule AdventOfCode2019.Day7 do
  def parse_intcode(input) do
    input
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
  end

  def get_param(mode, param, intcode) do
    case mode do
      0 -> Enum.at(intcode, param, 0)  # Position mode
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

  def run_intcode(intcode, inputs, pc \\ 0) do
    # Print the current state of execution for debugging
    IO.inspect(%{pc: pc, opcode: Enum.at(intcode, pc), intcode_slice: Enum.slice(intcode, pc, 10)}, label: "Intcode Execution State")

    opcode = rem(Enum.at(intcode, pc), 100)
    mode1 = rem(div(Enum.at(intcode, pc), 100), 10)
    mode2 = rem(div(Enum.at(intcode, pc), 1000), 10)

    param1 = Enum.at(intcode, pc + 1, 0)
    param2 = Enum.at(intcode, pc + 2, 0)
    param3 = Enum.at(intcode, pc + 3, 0)

    intcode = ensure_memory_size(intcode, param1)
    intcode = ensure_memory_size(intcode, param2)
    intcode = ensure_memory_size(intcode, param3)

    case opcode do
      1 ->
        intcode = List.replace_at(intcode, param3, get_param(mode1, param1, intcode) + get_param(mode2, param2, intcode))
        run_intcode(intcode, inputs, pc + 4)

      2 ->
        intcode = List.replace_at(intcode, param3, get_param(mode1, param1, intcode) * get_param(mode2, param2, intcode))
        run_intcode(intcode, inputs, pc + 4)

      3 ->
        case inputs do
          [input | inputs_tail] ->
            intcode = List.replace_at(intcode, param1, input)
            run_intcode(intcode, inputs_tail, pc + 2)

          [] ->
            {:await_input, intcode, pc}
        end

      4 ->
        output = get_param(mode1, param1, intcode)
        {:output, output, intcode, pc + 2}

      5 ->
        new_pc = if get_param(mode1, param1, intcode) != 0 do
          get_param(mode2, param2, intcode)
        else
          pc + 3
        end
        run_intcode(intcode, inputs, new_pc)

      6 ->
        new_pc = if get_param(mode1, param1, intcode) == 0 do
          get_param(mode2, param2, intcode)
        else
          pc + 3
        end
        run_intcode(intcode, inputs, new_pc)

      7 ->
        intcode = List.replace_at(intcode, param3, if get_param(mode1, param1, intcode) < get_param(mode2, param2, intcode) do
          1
        else
          0
        end)
        run_intcode(intcode, inputs, pc + 4)

      8 ->
        intcode = List.replace_at(intcode, param3, if get_param(mode1, param1, intcode) == get_param(mode2, param2, intcode) do
          1
        else
          0
        end)
        run_intcode(intcode, inputs, pc + 4)

      99 ->
        {:halt, intcode}

      _ ->
        {:error, "Unknown opcode #{opcode} at pc #{pc}"}
    end
  end

  def amplifier_loop_sequence(intcode, phases) do
    states = Enum.map(phases, fn phase ->
      {intcode, [phase], 0}  # Initialize each amplifier with its own copy of intcode and phase input
    end)

    loop_amplifiers(states, 0, 0, [])
  end

  defp loop_amplifiers(states, _amp_index, signal, halted) when length(halted) == length(states) do
    signal
  end

  defp loop_amplifiers(states, amp_index, signal, halted) do
    {intcode, inputs, pc} = Enum.at(states, amp_index)

    case run_intcode(intcode, [signal | inputs], pc) do
      {:output, output, new_intcode, new_pc} ->
        new_states = List.replace_at(states, amp_index, {new_intcode, [], new_pc})
        next_amp_index = rem(amp_index + 1, length(states))
        loop_amplifiers(new_states, next_amp_index, output, halted)

      {:await_input, new_intcode, new_pc} ->
        new_states = List.replace_at(states, amp_index, {new_intcode, [], new_pc})
        next_amp_index = rem(amp_index + 1, length(states))
        loop_amplifiers(new_states, next_amp_index, signal, halted)

      {:halt, _new_intcode} ->
        loop_amplifiers(states, rem(amp_index + 1, length(states)), signal, [amp_index | halted])

      {:error, reason} ->
        {:error, reason}
    end
  end

  def find_best_phase_setting_with_feedback(intcode) do
    [5, 6, 7, 8, 9]
    |> permutations()
    |> Enum.map(&amplifier_loop_sequence(intcode, &1))
    |> Enum.max()
  end

  defp permutations([]), do: [[]]
  defp permutations(list) do
    for elem <- list, rest <- permutations(list -- [elem]), do: [elem | rest]
  end
end

# Read input from the file
input = File.read!("day07-data.txt")
intcode = AdventOfCode2019.Day7.parse_intcode(input)

# Find the best phase setting with feedback loop
best_output = case AdventOfCode2019.Day7.find_best_phase_setting_with_feedback(intcode) do
  {:error, reason} ->
    "Error: #{reason}"

  result ->
    "Best output with feedback loop: #{result}"
end

# Print the result
IO.puts(best_output)
