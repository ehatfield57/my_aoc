defmodule IntcodeInterpreter do
  def run(program, input \\ []) do
    run(program, 0, input, [], 0) 
  end

  defp run(program, pc, input, output, relative_base) do
    instruction = program[pc]
    opcode = rem(instruction, 100)

    case opcode do
      99 ->
        { :halt, output } 

      1 -> 
        {p1, p2, p3} = get_params(program, pc, instruction, relative_base)
        program = put_in(program[p3], program[p1] + program[p2])
        run(program, pc + 4, input, output, relative_base)

      2 -> 
        {p1, p2, p3} = get_params(program, pc, instruction, relative_base)
        program = put_in(program[p3], program[p1] * program[p2])
        run(program, pc + 4, input, output, relative_base)

      3 -> 
        [head | tail] = input
        {p1, _, _} = get_params(program, pc, instruction, relative_base)
        program = put_in(program[p1], head)
        run(program, pc + 2, tail, output, relative_base)

      4 -> 
        {p1, _, _} = get_params(program, pc, instruction, relative_base)
        run(program, pc + 2, input, [program[p1] | output], relative_base)

      5 ->
        {p1, p2, _} = get_params(program, pc, instruction, relative_base)
        new_pc = if program[p1] != 0, do: program[p2], else: pc + 3
        run(program, new_pc, input, output, relative_base)

      6 ->
        {p1, p2, _} = get_params(program, pc, instruction, relative_base)
        new_pc = if program[p1] == 0, do: program[p2], else: pc + 3
        run(program, new_pc, input, output, relative_base)

      7 -> 
        {p1, p2, p3} = get_params(program, pc, instruction, relative_base)
        program = put_in(program[p3], if program[p1] < program[p2], do: 1, else: 0)
        run(program, pc + 4, input, output, relative_base)

      8 ->
        {p1, p2, p3} = get_params(program, pc, instruction, relative_base)
        program = put_in(program[p3], if program[p1] == program[p2], do: 1, else: 0)
        run(program, pc + 4, input, output, relative_base)

      9 ->
        {p1, _, _} = get_params(program, pc, instruction, relative_base)
        run(program, pc + 2, input, output, relative_base + program[p1])

      true ->
        {:error, "Invalid opcode: #{opcode}"} 
    end
  end

  defp get_params(program, pc, instruction, relative_base) do
    modes = Integer.digits(instruction // 100) 
            |> Enum.reverse() 
            |> List.to_tuple() 

    params = Enum.map(1..3, fn offset ->
      case elem(modes, offset - 1, 0) do 
        0 -> program[pc + offset] 
        1 -> pc + offset
        2 -> relative_base + program[pc + offset]
      end
    end) 
    List.to_tuple(params) # Convert the list to a tuple
  end
end
