defmodule Intcode do
  defstruct memory: %{}, pointer: 0, relative_base: 0, inputs: [], outputs: [], halted: false

  @opcodes %{
    1 => {:add, 3},
    2 => {:mul, 3},
    3 => {:input, 1},
    4 => {:output, 1},
    5 => {:jump_if_true, 2},
    6 => {:jump_if_false, 2},
    7 => {:less_than, 3},
    8 => {:equals, 3},
    9 => {:adjust_relative_base, 1},
    99 => {:halt, 0}
  }

  def parse_program(input) do
    input
    |> String.trim()
    |> String.split(",", trim: true)
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {value, index}, acc ->
      Map.put(acc, index, String.to_integer(value))
    end)
  end

  def run(intcode = %Intcode{}, input \\ []) do
    execute(intcode, input)
  end

  defp execute(intcode, input) do
    opcode_full = get_memory(intcode, intcode.pointer)
    {opcode, modes} = parse_instruction(opcode_full)

    case Map.get(@opcodes, opcode) do
      nil ->
        IO.puts("Unknown opcode #{opcode} at position #{intcode.pointer}")
        %{intcode | halted: true}

      {instruction, param_count} ->
        params = extract_modes(modes, param_count)

        case instruction do
          :add ->
            param1 = get_param(intcode, intcode.pointer + 1, Enum.at(params, 0, 0))
            param2 = get_param(intcode, intcode.pointer + 2, Enum.at(params, 1, 0))
            dest = get_address(intcode, intcode.pointer + 3, Enum.at(params, 2, 0))
            memory = Map.put(intcode.memory, dest, param1 + param2)
            execute(%{intcode | memory: memory, pointer: intcode.pointer + 4}, input)

          :mul ->
            param1 = get_param(intcode, intcode.pointer + 1, Enum.at(params, 0, 0))
            param2 = get_param(intcode, intcode.pointer + 2, Enum.at(params, 1, 0))
            dest = get_address(intcode, intcode.pointer + 3, Enum.at(params, 2, 0))
            memory = Map.put(intcode.memory, dest, param1 * param2)
            execute(%{intcode | memory: memory, pointer: intcode.pointer + 4}, input)

          :input ->
            case input do
              [first | rest] ->
                dest = get_address(intcode, intcode.pointer + 1, Enum.at(params, 0, 0))
                memory = Map.put(intcode.memory, dest, first)
                execute(%{intcode | memory: memory, pointer: intcode.pointer + 2}, rest)

              [] ->
                %{intcode | inputs: input}
            end

          :output ->
            param = get_param(intcode, intcode.pointer + 1, Enum.at(params, 0, 0))
            execute(%{intcode | pointer: intcode.pointer + 2, outputs: [param | intcode.outputs]}, input)

          :jump_if_true ->
            param1 = get_param(intcode, intcode.pointer + 1, Enum.at(params, 0, 0))
            param2 = get_param(intcode, intcode.pointer + 2, Enum.at(params, 1, 0))

            if param1 != 0 do
              execute(%{intcode | pointer: param2}, input)
            else
              execute(%{intcode | pointer: intcode.pointer + 3}, input)
            end

          :jump_if_false ->
            param1 = get_param(intcode, intcode.pointer + 1, Enum.at(params, 0, 0))
            param2 = get_param(intcode, intcode.pointer + 2, Enum.at(params, 1, 0))

            if param1 == 0 do
              execute(%{intcode | pointer: param2}, input)
            else
              execute(%{intcode | pointer: intcode.pointer + 3}, input)
            end

          :less_than ->
            param1 = get_param(intcode, intcode.pointer + 1, Enum.at(params, 0, 0))
            param2 = get_param(intcode, intcode.pointer + 2, Enum.at(params, 1, 0))
            dest = get_address(intcode, intcode.pointer + 3, Enum.at(params, 2, 0))
            value = if param1 < param2, do: 1, else: 0
            memory = Map.put(intcode.memory, dest, value)
            execute(%{intcode | memory: memory, pointer: intcode.pointer + 4}, input)

          :equals ->
            param1 = get_param(intcode, intcode.pointer + 1, Enum.at(params, 0, 0))
            param2 = get_param(intcode, intcode.pointer + 2, Enum.at(params, 1, 0))
            dest = get_address(intcode, intcode.pointer + 3, Enum.at(params, 2, 0))
            value = if param1 == param2, do: 1, else: 0
            memory = Map.put(intcode.memory, dest, value)
            execute(%{intcode | memory: memory, pointer: intcode.pointer + 4}, input)

          :adjust_relative_base ->
            param = get_param(intcode, intcode.pointer + 1, Enum.at(params, 0, 0))
            execute(%{intcode | relative_base: intcode.relative_base + param, pointer: intcode.pointer + 2}, input)

          :halt ->
            %{intcode | halted: true}
        end
    end
  end

  defp parse_instruction(instruction) do
    opcode = rem(instruction, 100)
    modes = div(instruction, 100)
    {opcode, modes}
  end

  defp extract_modes(modes, count) do
    1..count
    |> Enum.map(fn i -> rem(div(modes, :math.pow(10, i - 1) |> round), 10) end)
  end

  defp get_param(intcode, position, mode) do
    value = get_memory(intcode, position)

    case mode do
      0 -> get_memory(intcode, value)
      1 -> value
      2 -> get_memory(intcode, intcode.relative_base + value)
      _ -> 
        IO.puts("Unknown parameter mode #{mode} at position #{position}")
        0
    end
  end

  defp get_address(intcode, position, mode) do
    value = get_memory(intcode, position)

    case mode do
      0 -> value
      2 -> intcode.relative_base + value
      _ -> 
        IO.puts("Unknown address mode #{mode} at position #{position}")
        value
    end
  end

  defp get_memory(%Intcode{memory: memory}, address) do
    Map.get(memory, address, 0)
  end
end

defmodule ScaffoldRobot do
  def main do
    # Read the Intcode program from day17-data.txt
    program =
      "day17-data.txt"
      |> File.read!()
      |> Intcode.parse_program()

    # Initialize the Intcode computer with the program
    intcode = %Intcode{memory: program, pointer: 0, relative_base: 0, inputs: [], outputs: []}

    # Modify the first memory address to 2 to activate the robot
    intcode = %{intcode | memory: Map.put(intcode.memory, 0, 2)}

    # Define the movement routines
    # **IMPORTANT**: You need to determine these based on your scaffold map.
    # Below are placeholder examples. Replace them with the actual routines.
    main_routine = "A,B,A,C,B,C,A,B,A,C\n"
    function_a = "R,8,R,10,R,8\n"      # Example: Replace with actual
    function_b = "R,4,R,10,L,12\n"     # Example: Replace with actual
    function_c = "L,6,L,8,R,10\n"      # Example: Replace with actual
    video_feed = "n\n"

    # Combine all inputs and convert to ASCII codes
    input_string =
      main_routine <>
        function_a <>
        function_b <>
        function_c <>
        video_feed

    input_ascii = input_string |> String.to_charlist()

    # Run the Intcode program with the input
    final_intcode =
      intcode
      |> Intcode.run(input_ascii)

    # Check if the program has halted
    if final_intcode.halted do
      # The output is the last value in the outputs list
      dust_amount =
        final_intcode.outputs
        |> Enum.reverse()
        |> hd()

      IO.puts("Dust collected: #{dust_amount}")
    else
      IO.puts("Intcode program did not halt properly.")
    end
  end
end

ScaffoldRobot.main()
