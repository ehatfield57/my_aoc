defmodule Intcode do
  @moduledoc """
  Intcode computer implementation for Advent of Code 2019.
  All functions are public and include debugging information.
  """

  # Run the Intcode program
  def run(program, input \\ []) do
    memory = initialize_memory(program)
    execute(memory, 0, 0, input, [])
  end

  # Initialize memory as a map for dynamic access
  def initialize_memory(program) do
    Enum.with_index(program)
    |> Enum.reduce(%{}, fn {val, idx}, acc -> Map.put(acc, idx, val) end)
  end

  # Execute the Intcode program
  def execute(memory, ip, relative_base, input, output) do
    instruction = Map.get(memory, ip, 0)
    {opcode, modes} = parse_instruction(instruction)

    IO.puts("\n--- Executing Instruction ---")
    IO.puts("IP: #{ip}, Relative Base: #{relative_base}")
    IO.puts("Opcode: #{opcode}, Modes: #{inspect modes}")

    case opcode do
      1 ->  # Addition
        [param1, param2] = get_parameters(memory, ip, modes, relative_base, 2)
        dest = get_write_address(memory, ip + 3, Enum.at(modes, 2, 0), relative_base)
        IO.puts("Adding #{param1} + #{param2}, storing result at address #{dest}")
        memory = Map.put(memory, dest, param1 + param2)
        execute(memory, ip + 4, relative_base, input, output)

      2 ->  # Multiplication
        [param1, param2] = get_parameters(memory, ip, modes, relative_base, 2)
        dest = get_write_address(memory, ip + 3, Enum.at(modes, 2, 0), relative_base)
        IO.puts("Multiplying #{param1} * #{param2}, storing result at address #{dest}")
        memory = Map.put(memory, dest, param1 * param2)
        execute(memory, ip + 4, relative_base, input, output)

      3 ->  # Input
        case input do
          [in_val | rest_input] ->
            dest = get_write_address(memory, ip + 1, Enum.at(modes, 0, 0), relative_base)
            IO.puts("Input instruction: storing value #{in_val} at address #{dest}")
            memory = Map.put(memory, dest, in_val)
            execute(memory, ip + 2, relative_base, rest_input, output)

          [] ->
            IO.puts("Input instruction: no input available, waiting for input.")
            {:need_input, memory, ip, relative_base, output}
        end

      4 ->  # Output
        [param] = get_parameters(memory, ip, modes, relative_base, 1)
        IO.puts("Output instruction: outputting value #{param}")
        execute(memory, ip + 2, relative_base, input, [param | output])

      5 ->  # Jump-if-true
        [param1, param2] = get_parameters(memory, ip, modes, relative_base, 2)
        if param1 != 0 do
          IO.puts("Jump-if-true: #{param1} is non-zero, jumping to address #{param2}")
          execute(memory, param2, relative_base, input, output)
        else
          IO.puts("Jump-if-true: #{param1} is zero, moving to next instruction")
          execute(memory, ip + 3, relative_base, input, output)
        end

      6 ->  # Jump-if-false
        [param1, param2] = get_parameters(memory, ip, modes, relative_base, 2)
        if param1 == 0 do
          IO.puts("Jump-if-false: #{param1} is zero, jumping to address #{param2}")
          execute(memory, param2, relative_base, input, output)
        else
          IO.puts("Jump-if-false: #{param1} is non-zero, moving to next instruction")
          execute(memory, ip + 3, relative_base, input, output)
        end

      7 ->  # Less than
        [param1, param2] = get_parameters(memory, ip, modes, relative_base, 2)
        dest = get_write_address(memory, ip + 3, Enum.at(modes, 2, 0), relative_base)
        result = if param1 < param2, do: 1, else: 0
        IO.puts("Less than: #{param1} < #{param2} = #{result}, storing at address #{dest}")
        memory = Map.put(memory, dest, result)
        execute(memory, ip + 4, relative_base, input, output)

      8 ->  # Equals
        [param1, param2] = get_parameters(memory, ip, modes, relative_base, 2)
        dest = get_write_address(memory, ip + 3, Enum.at(modes, 2, 0), relative_base)
        result = if param1 == param2, do: 1, else: 0
        IO.puts("Equals: #{param1} == #{param2} = #{result}, storing at address #{dest}")
        memory = Map.put(memory, dest, result)
        execute(memory, ip + 4, relative_base, input, output)

      9 ->  # Adjust relative base
        [param] = get_parameters(memory, ip, modes, relative_base, 1)
        new_relative_base = relative_base + param
        IO.puts("Adjusting relative base by #{param}, new relative base is #{new_relative_base}")
        execute(memory, ip + 2, new_relative_base, input, output)

      99 ->  # Halt
        IO.puts("Halt instruction: program has terminated.")
        {:halt, Enum.reverse(output)}

      _ ->
        IO.puts("Unknown opcode #{opcode} encountered at IP #{ip}. Terminating execution.")
        {:error, "Unknown opcode #{opcode} at position #{ip}"}
    end
  end

  # Parse instruction into opcode and parameter modes
  def parse_instruction(instruction) do
    opcode = rem(instruction, 100)
    modes = instruction
            |> div(100)
            |> Integer.digits()
            |> Enum.reverse()
    {opcode, modes}
  end

  # Get parameters for read operations based on parameter modes
  def get_parameters(memory, ip, modes, relative_base, count) do
    Enum.map(1..count, fn i ->
      param = Map.get(memory, ip + i, 0)
      mode = Enum.at(modes, i - 1, 0)

      case mode do
        0 ->  # Position mode
          Map.get(memory, param, 0)
        1 ->  # Immediate mode
          param
        2 ->  # Relative mode
          Map.get(memory, relative_base + param, 0)
        _ ->
          IO.puts("Error: Unknown parameter mode #{mode} at IP #{ip}")
          0
      end
    end)
  end

  # Get write address based on parameter mode
  def get_write_address(memory, ip, mode, relative_base) do
    param = Map.get(memory, ip, 0)
    case mode do
      0 -> 
        param
      2 -> 
        relative_base + param
      _ ->
        IO.puts("Error: Unsupported parameter mode #{mode} for write operation at IP #{ip}")
        param
    end
  end
end

defmodule Day13Part1 do
  @moduledoc """
  Solution for Advent of Code 2019 Day 13 Part 1.
  Runs the Intcode program and counts block tiles (tile_id=2).
  Includes debugging information to trace computation.
  """

  def run do
    program = read_input("day13-data.txt")
    IO.puts("=== Advent of Code 2019 - Day 13 Part 1 ===")
    IO.puts("Program loaded. Starting Intcode computer execution...")
    
    case Intcode.run(program) do
      {:halt, output} ->
        tiles = parse_output(output)
        block_tiles = Enum.count(tiles, fn {_x, _y, tile_id} -> tile_id == 2 end)
        IO.puts("\n=== Result ===")
        IO.puts("Number of block tiles: #{block_tiles}")

      {:need_input, _memory, _ip, _relative_base, _output} ->
        IO.puts("Intcode program is waiting for input, which was not provided.")

      {:error, message} ->
        IO.puts("An error occurred during Intcode execution: #{message}")
    end
  end

  # Read input from file and parse into list of integers
  def read_input(filename) do
    case File.read(filename) do
      {:ok, content} ->
        content
        |> String.trim()
        |> String.split(",")
        |> Enum.map(&String.to_integer/1)

      {:error, reason} ->
        IO.puts("Error reading file #{filename}: #{reason}")
        []
    end
  end

  # Parse output list into list of {x, y, tile_id} tuples
  def parse_output([]), do: []
  def parse_output([x, y, tile_id | rest]) do
    [{x, y, tile_id} | parse_output(rest)]
  end
  def parse_output(_), do: []
end

# To execute the solution, uncomment the following line:
Day13Part1.run()
