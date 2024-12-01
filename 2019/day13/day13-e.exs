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

    # IO.puts("\n--- Executing Instruction ---")
    # IO.puts("IP: #{ip}, Relative Base: #{relative_base}")
    # IO.puts("Opcode: #{opcode}, Modes: #{inspect modes}")

    case opcode do
      1 ->  # Addition
        [param1, param2] = get_parameters(memory, ip, modes, relative_base, 2)
        dest = get_write_address(memory, ip + 3, Enum.at(modes, 2, 0), relative_base)
        # IO.puts("Adding #{param1} + #{param2}, storing result at address #{dest}")
        memory = Map.put(memory, dest, param1 + param2)
        execute(memory, ip + 4, relative_base, input, output)

      2 ->  # Multiplication
        [param1, param2] = get_parameters(memory, ip, modes, relative_base, 2)
        dest = get_write_address(memory, ip + 3, Enum.at(modes, 2, 0), relative_base)
        # IO.puts("Multiplying #{param1} * #{param2}, storing result at address #{dest}")
        memory = Map.put(memory, dest, param1 * param2)
        execute(memory, ip + 4, relative_base, input, output)

      3 ->  # Input
        case input do
          [in_val | rest_input] ->
            dest = get_write_address(memory, ip + 1, Enum.at(modes, 0, 0), relative_base)
            # IO.puts("Input instruction: storing value #{in_val} at address #{dest}")
            memory = Map.put(memory, dest, in_val)
            execute(memory, ip + 2, relative_base, rest_input, output)

          [] ->
            # IO.puts("Input instruction: no input available, waiting for input.")
            {:need_input, memory, ip, relative_base, output}
        end

      4 ->  # Output
        [param] = get_parameters(memory, ip, modes, relative_base, 1)
        # IO.puts("Output instruction: outputting value #{param}")
        execute(memory, ip + 2, relative_base, input, [param | output])

      5 ->  # Jump-if-true
        [param1, param2] = get_parameters(memory, ip, modes, relative_base, 2)
        if param1 != 0 do
          # IO.puts("Jump-if-true: #{param1} is non-zero, jumping to address #{param2}")
          execute(memory, param2, relative_base, input, output)
        else
          # IO.puts("Jump-if-true: #{param1} is zero, moving to next instruction")
          execute(memory, ip + 3, relative_base, input, output)
        end

      6 ->  # Jump-if-false
        [param1, param2] = get_parameters(memory, ip, modes, relative_base, 2)
        if param1 == 0 do
          # IO.puts("Jump-if-false: #{param1} is zero, jumping to address #{param2}")
          execute(memory, param2, relative_base, input, output)
        else
          # IO.puts("Jump-if-false: #{param1} is non-zero, moving to next instruction")
          execute(memory, ip + 3, relative_base, input, output)
        end

      7 ->  # Less than
        [param1, param2] = get_parameters(memory, ip, modes, relative_base, 2)
        dest = get_write_address(memory, ip + 3, Enum.at(modes, 2, 0), relative_base)
        result = if param1 < param2, do: 1, else: 0
        # IO.puts("Less than: #{param1} < #{param2} = #{result}, storing at address #{dest}")
        memory = Map.put(memory, dest, result)
        execute(memory, ip + 4, relative_base, input, output)

      8 ->  # Equals
        [param1, param2] = get_parameters(memory, ip, modes, relative_base, 2)
        dest = get_write_address(memory, ip + 3, Enum.at(modes, 2, 0), relative_base)
        result = if param1 == param2, do: 1, else: 0
        # IO.puts("Equals: #{param1} == #{param2} = #{result}, storing at address #{dest}")
        memory = Map.put(memory, dest, result)
        execute(memory, ip + 4, relative_base, input, output)

      9 ->  # Adjust relative base
        [param] = get_parameters(memory, ip, modes, relative_base, 1)
        new_relative_base = relative_base + param
        # IO.puts("Adjusting relative base by #{param}, new relative base is #{new_relative_base}")
        execute(memory, ip + 2, new_relative_base, input, output)

      99 ->  # Halt
        # IO.puts("Halt instruction: program has terminated.")
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

defmodule Day13Part2 do
  @moduledoc """
  Solution for Advent of Code 2019 Day 13 Part 2.
  Runs the Intcode program with free play enabled, controls the joystick automatically,
  and outputs the final score after breaking all blocks.
  Includes debugging information to trace computation.
  """

  def run do
    program = read_input("day13-data.txt")
    modified_program = List.replace_at(program, 0, 2)  # Set to free play by setting first value to 2
    IO.puts("=== Advent of Code 2019 - Day 13 Part 2 ===")
    IO.puts("Program loaded and modified for free play. Starting Intcode computer execution...")

    # Initialize game state
    game_state = %{
      score: 0,
      paddle_x: 0,
      ball_x: 0,
      blocks: %{}
    }

    # Start Intcode execution with initial input
    execute_game(modified_program, [], game_state)
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

  # Execute the game by running the Intcode program and controlling the joystick
  def execute_game(program, input, game_state) do
    case Intcode.run(program, input) do
      {:halt, output} ->
        # Process all remaining outputs
        final_game_state = process_output(Enum.reverse(output), game_state)
        IO.puts("\n=== Final Score ===")
        IO.puts("Final score: #{final_game_state.score}")

      {:need_input, memory, ip, relative_base, output} ->
        # Process the current outputs before providing input
        new_game_state = process_output(Enum.reverse(output), game_state)
        # Decide joystick movement based on ball and paddle positions
        joystick = decide_joystick(new_game_state)
        # IO.puts("Providing joystick input: #{joystick}")
        execute_game(program, [joystick], new_game_state)

      {:error, message} ->
        IO.puts("An error occurred during Intcode execution: #{message}")
    end
  end

  # Process the output list and update the game state accordingly
  def process_output([], game_state), do: game_state
  def process_output([x, y, tile_id | rest], game_state) do
    updated_game_state = 
      cond do
        x == -1 and y == 0 ->
          %{game_state | score: tile_id}

        tile_id == 3 ->
          %{game_state | paddle_x: x}

        tile_id == 4 ->
          %{game_state | ball_x: x}

        tile_id == 2 ->
          # Track block positions
          updated_blocks = Map.put(game_state.blocks, {x, y}, tile_id)
          %{game_state | blocks: updated_blocks}

        true ->
          game_state
      end

    process_output(rest, updated_game_state)
  end
  def process_output(_partial, game_state), do: game_state

  # Decide joystick movement based on ball and paddle positions
  def decide_joystick(%{ball_x: ball_x, paddle_x: paddle_x} = _game_state) do
    cond do
      ball_x < paddle_x -> -1
      ball_x > paddle_x -> 1
      true -> 0
    end
  end
end

# To execute the solution, uncomment the following line:
Day13Part2.run()
