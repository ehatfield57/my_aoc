# day17_part2.exs

defmodule Intcode do
  defstruct memory: %{},
            pointer: 0,
            relative_base: 0,
            inputs: [],
            outputs: [],
            halted: false

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

  # Set @debug to true to enable detailed logging
  @debug false

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

    {instruction, param_count} = Map.get(@opcodes, opcode, {nil, 0})

    if instruction == nil do
      IO.puts("[ERROR] Unknown opcode #{opcode} at position #{intcode.pointer}")
      %{intcode | halted: true}
    else
      params = extract_modes(modes, param_count)

      if @debug do
        IO.puts("\n[DEBUG] Pointer: #{intcode.pointer}")
        IO.puts("[DEBUG] Opcode Full: #{opcode_full}")
        IO.puts("[DEBUG] Instruction: #{instruction} with params count: #{param_count}")
        IO.puts("[DEBUG] Parameter Modes: #{inspect(params)}")
      end

      case instruction do
        :add ->
          param1 = get_param(intcode, intcode.pointer + 1, Enum.at(params, 0, 0))
          param2 = get_param(intcode, intcode.pointer + 2, Enum.at(params, 1, 0))
          dest = get_address(intcode, intcode.pointer + 3, Enum.at(params, 2, 0))
          memory = Map.put(intcode.memory, dest, param1 + param2)

          if @debug do
            IO.puts("[DEBUG] Adding #{param1} + #{param2} and storing at #{dest}")
          end

          execute(%{intcode | memory: memory, pointer: intcode.pointer + 4}, input)

        :mul ->
          param1 = get_param(intcode, intcode.pointer + 1, Enum.at(params, 0, 0))
          param2 = get_param(intcode, intcode.pointer + 2, Enum.at(params, 1, 0))
          dest = get_address(intcode, intcode.pointer + 3, Enum.at(params, 2, 0))
          memory = Map.put(intcode.memory, dest, param1 * param2)

          if @debug do
            IO.puts("[DEBUG] Multiplying #{param1} * #{param2} and storing at #{dest}")
          end

          execute(%{intcode | memory: memory, pointer: intcode.pointer + 4}, input)

        :input ->
          case input do
            [first | rest] ->
              dest = get_address(intcode, intcode.pointer + 1, Enum.at(params, 0, 0))
              memory = Map.put(intcode.memory, dest, first)

              if @debug do
                IO.puts("[DEBUG] Inputting value #{first} at address #{dest}")
              end

              execute(%{intcode | memory: memory, pointer: intcode.pointer + 2}, rest)

            [] ->
              if @debug do
                IO.puts("[DEBUG] Waiting for input...")
              end

              %{intcode | inputs: input}
          end

        :output ->
          param = get_param(intcode, intcode.pointer + 1, Enum.at(params, 0, 0))

          if @debug do
            IO.puts("[DEBUG] Output: #{param}")
          end

          execute(%{intcode | pointer: intcode.pointer + 2, outputs: [param | intcode.outputs]}, input)

        :jump_if_true ->
          param1 = get_param(intcode, intcode.pointer + 1, Enum.at(params, 0, 0))
          param2 = get_param(intcode, intcode.pointer + 2, Enum.at(params, 1, 0))

          if @debug do
            IO.puts("[DEBUG] Jump if true: #{param1} != 0 ? Jump to #{param2} : Continue")
          end

          if param1 != 0 do
            execute(%{intcode | pointer: param2}, input)
          else
            execute(%{intcode | pointer: intcode.pointer + 3}, input)
          end

        :jump_if_false ->
          param1 = get_param(intcode, intcode.pointer + 1, Enum.at(params, 0, 0))
          param2 = get_param(intcode, intcode.pointer + 2, Enum.at(params, 1, 0))

          if @debug do
            IO.puts("[DEBUG] Jump if false: #{param1} == 0 ? Jump to #{param2} : Continue")
          end

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

          if @debug do
            IO.puts("[DEBUG] Less Than: #{param1} < #{param2} ? #{value} stored at #{dest}")
          end

          execute(%{intcode | memory: memory, pointer: intcode.pointer + 4}, input)

        :equals ->
          param1 = get_param(intcode, intcode.pointer + 1, Enum.at(params, 0, 0))
          param2 = get_param(intcode, intcode.pointer + 2, Enum.at(params, 1, 0))
          dest = get_address(intcode, intcode.pointer + 3, Enum.at(params, 2, 0))
          value = if param1 == param2, do: 1, else: 0
          memory = Map.put(intcode.memory, dest, value)

          if @debug do
            IO.puts("[DEBUG] Equals: #{param1} == #{param2} ? #{value} stored at #{dest}")
          end

          execute(%{intcode | memory: memory, pointer: intcode.pointer + 4}, input)

        :adjust_relative_base ->
          param = get_param(intcode, intcode.pointer + 1, Enum.at(params, 0, 0))
          new_relative_base = intcode.relative_base + param

          if @debug do
            IO.puts("[DEBUG] Adjusting relative base by #{param}. New relative base: #{new_relative_base}")
          end

          execute(%{intcode | relative_base: new_relative_base, pointer: intcode.pointer + 2}, input)

        :halt ->
          if @debug do
            IO.puts("[DEBUG] Halt instruction encountered. Program halted.")
          end

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
      # Extract each mode digit starting from the least significant digit
      # Pad with 0s if there are not enough mode digits
      modes_digits = Integer.digits(modes) |> Enum.reverse()
      pad_length = count - length(modes_digits)

      # Ensure pad_length is non-negative
      pad_length = if pad_length > 0, do: pad_length, else: 0

      modes_digits = modes_digits ++ List.duplicate(0, pad_length)
      Enum.take(modes_digits, count)
    end

    defp get_param(intcode, position, mode) do
      value = get_memory(intcode, position)

      case mode do
        0 -> get_memory(intcode, value)
        1 -> value
        2 -> get_memory(intcode, intcode.relative_base + value)
        _ ->
          IO.puts("[ERROR] Unknown parameter mode #{mode} at position #{position}")
          0
      end
    end

    defp get_address(intcode, position, mode) do
      value = get_memory(intcode, position)

      case mode do
        0 -> value
        2 -> intcode.relative_base + value
        _ ->
          IO.puts("[ERROR] Unknown address mode #{mode} at position #{position}")
          value
      end
    end

    defp get_memory(%Intcode{memory: memory}, address) do
      Map.get(memory, address, 0)
    end
end

defmodule ScaffoldVisualizer do
  def visualize(file_path) do
    scaffold_map =
      File.read!(file_path)
      |> String.trim()

    scaffold_map
    |> String.split("\n", trim: true)
    |> Enum.each(&IO.puts/1)
  end
end

defmodule ScaffoldRobotPart1 do
  def main do
    # Read the Intcode program from day17-data.txt
    program =
      "day17-data.txt"
      |> File.read!()
      |> Intcode.parse_program()

    # Initialize the Intcode computer with the program
    intcode = %Intcode{
      memory: program,
      pointer: 0,
      relative_base: 0,
      inputs: [],
      outputs: [],
      halted: false
    }

    # Run the Intcode program without inputs to generate the scaffold map
    final_intcode = Intcode.run(intcode, [])

    # Convert outputs to characters and save the scaffold map
    scaffold_map =
      final_intcode.outputs
      |> Enum.reverse()
      |> Enum.map(&(&1))
      |> List.to_string()

    # Save to a file
    File.write!("scaffold_map.txt", scaffold_map)

    IO.puts("Scaffold map saved to scaffold_map.txt")

    # Optionally, visualize the scaffold map
    ScaffoldVisualizer.visualize("scaffold_map.txt")
  end
end

defmodule ScaffoldRobot do
  # Toggle debug mode
  @debug true

  def main do
    # Read the Intcode program from day17-data.txt
    program =
      "day17-data.txt"
      |> File.read!()
      |> Intcode.parse_program()

    # Initialize the Intcode computer with the program
    intcode = %Intcode{
      memory: program,
      pointer: 0,
      relative_base: 0,
      inputs: [],
      outputs: [],
      halted: false
    }

    # Modify the first memory address to 2 to activate the robot
    intcode = %{intcode | memory: Map.put(intcode.memory, 0, 2)}

    # Define the movement routines
    # main_routine = "A,B,A,C,B,C,A,B,A\n"
    # function_a = "R,10,L,8,R,10,R,4\n"
    # function_b = "L,6,L,6,R,10\n"
    # function_c = "L,6,R,12,R,12,R,10\n"
    # [A,B,A,C,B,C,A,B,A,C R,10,L,8,R,10,R,4 L,6,L,6,R,10 L,6,R,12,R,12,R,10] <- from day17.go
    main_routine = "A,B,A,C,B,C,A,B,A,C\n"
    function_a = "R,10,L,8,R,10,R,4\n"
    function_b = "L,6,L,6,R,10\n"
    function_c = "L,6,R,12,R,12,R,10\n"
    video_feed = "n\n"

    # Combine all inputs and convert to ASCII codes
    input_string =
      main_routine <>
        function_a <>
        function_b <>
        function_c <>
        video_feed

    input_ascii = String.to_charlist(input_string)

    if @debug do
      IO.puts("\n[DEBUG] Sending Movement Routines:")
      IO.puts("Main Routine: #{inspect(main_routine)}")
      IO.puts("Function A: #{inspect(function_a)}")
      IO.puts("Function B: #{inspect(function_b)}")
      IO.puts("Function C: #{inspect(function_c)}")
      IO.puts("Video Feed: #{inspect(video_feed)}")
    end

    # Run the Intcode program with the input
    final_intcode = Intcode.run(intcode, input_ascii)

    # Check if the program has halted properly
    if final_intcode.halted do
      # The output is the first value in the outputs list
      dust_amount =
        final_intcode.outputs
        |> hd()

      IO.puts("\n[RESULT] Dust collected: #{dust_amount}")
    else
      IO.puts("\n[ERROR] Intcode program did not halt properly.")
      IO.puts("[DEBUG] Final Outputs: #{inspect(Enum.reverse(final_intcode.outputs))}")
    end
  end
end

# Uncomment the desired module execution below

# To generate and visualize the scaffold map (Part 1):
ScaffoldRobotPart1.main()

# To run Part 2 with movement routines:
ScaffoldRobot.main()
