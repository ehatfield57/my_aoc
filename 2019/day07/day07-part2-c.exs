defmodule AdventOfCode2019.Day7 do
  def parse_intcode(input) do
    input
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
  end

  def get_param(mode, param, intcode) do
    case mode do
      0 -> Enum.at(intcode, param, 0)
      1 -> param
    end
  end

  def run_intcode(intcode, inputs, pc \\ 0, outputs \\ []) do
    opcode = rem(Enum.at(intcode, pc), 100)
    mode1 = rem(div(Enum.at(intcode, pc), 100), 10)
    mode2 = rem(div(Enum.at(intcode, pc), 1000), 10)
    
    param1 = Enum.at(intcode, pc + 1, 0)
    param2 = Enum.at(intcode, pc + 2, 0)
    param3 = Enum.at(intcode, pc + 3, 0)

    case opcode do
      1 ->
        intcode
        |> List.replace_at(param3, get_param(mode1, param1, intcode) + get_param(mode2, param2, intcode))
        |> run_intcode(inputs, pc + 4, outputs)
      
      2 ->
        intcode
        |> List.replace_at(param3, get_param(mode1, param1, intcode) * get_param(mode2, param2, intcode))
        |> run_intcode(inputs, pc + 4, outputs)
      
      3 ->
        case inputs do
          [input | inputs_tail] ->
            intcode
            |> List.replace_at(param1, input)
            |> run_intcode(inputs_tail, pc + 2, outputs)
          [] ->
            {:await_input, intcode, pc, outputs}
        end
      
      4 ->
        output = get_param(mode1, param1, intcode)
        {:output, output, intcode, pc + 2, inputs, outputs}
      
      5 ->
        new_pc = if get_param(mode1, param1, intcode) != 0 do
          get_param(mode2, param2, intcode)
        else
          pc + 3
        end
        run_intcode(intcode, inputs, new_pc, outputs)
      
      6 ->
        new_pc = if get_param(mode1, param1, intcode) == 0 do
          get_param(mode2, param2, intcode)
        else
          pc + 3
        end
        run_intcode(intcode, inputs, new_pc, outputs)
      
      7 ->
        intcode
        |> List.replace_at(param3, if(get_param(mode1, param1, intcode) < get_param(mode2, param2, intcode), do: 1, else: 0))
        |> run_intcode(inputs, pc + 4, outputs)
      
      8 ->
        intcode
        |> List.replace_at(param3, if(get_param(mode1, param1, intcode) == get_param(mode2, param2, intcode), do: 1, else: 0))
        |> run_intcode(inputs, pc + 4, outputs)
      
      99 ->
        {:halt, Enum.reverse(outputs)}
      
      _ ->
        {:error, "Unknown opcode"}
    end
  end

  def permutations([]), do: [[]]
  def permutations(list) do
    for elem <- list, rest <- permutations(list -- [elem]), do: [elem | rest]
  end

  def amplifier_loop_sequence(intcode, phases) do
    # Initial amplifier states
    states = Enum.map(phases, fn phase ->
      {intcode, [phase], 0} # Initial state for each amplifier: intcode, input, and program counter (pc)
    end)

    # Feedback loop
    loop(states, 0, 0)
  end

  defp loop(states, current_amp, input) do
    {intcode, inputs, pc} = Enum.at(states, current_amp)

    case run_intcode(intcode, [input | inputs], pc) do
      {:output, output, new_intcode, new_pc, new_inputs, _} ->
        new_states = List.replace_at(states, current_amp, {new_intcode, new_inputs, new_pc})
        next_amp = rem(current_amp + 1, length(states))

        loop(new_states, next_amp, output)

      {:halt, _outputs} ->
        if current_amp == 4 do
          input
        else
          loop(states, rem(current_amp + 1, length(states)), input)
        end

      {:await_input, new_intcode, new_pc, _outputs} ->
        new_states = List.replace_at(states, current_amp, {new_intcode, [], new_pc})
        next_amp = rem(current_amp + 1, length(states))

        loop(new_states, next_amp, input)

      {:error, reason} ->
        {:error, reason}
    end
  end

  def find_best_phase_setting_with_feedback(intcode) do
    [5, 6, 7, 8, 9]
    |> permutations()
    |> Enum.map(fn phases -> amplifier_loop_sequence(intcode, phases) end)
    |> Enum.max_by(fn
      {:error, _} -> -1 # Treat errors as very low results to avoid crashes
      value -> value
    end)
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
