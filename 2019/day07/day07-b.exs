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
        [input | inputs_tail] = inputs
        intcode
        |> List.replace_at(param1, input)
        |> run_intcode(inputs_tail, pc + 2, outputs)
      
      4 ->
        output = get_param(mode1, param1, intcode)
        run_intcode(intcode, inputs, pc + 2, [output | outputs])
      
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

  def amplifier_sequence(intcode, phases) do
    phases
    |> Enum.reduce(0, fn phase, input ->
      {:halt, [output]} = run_intcode(intcode, [phase, input])
      output
    end)
  end

  def find_best_phase_setting(intcode) do
    [0, 1, 2, 3, 4]
    |> permutations()
    |> Enum.map(fn phases -> amplifier_sequence(intcode, phases) end)
    |> Enum.max()
  end
end

# Example usage:
input = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
intcode = AdventOfCode2019.Day7.parse_intcode(input)
AdventOfCode2019.Day7.find_best_phase_setting(intcode)
