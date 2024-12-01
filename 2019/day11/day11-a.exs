defmodule Day11 do
  def main do
    # Read input from file and parse into Intcode program
    program =
      "day11-data.txt"
      |> File.read!()
      |> String.trim()
      |> String.split(",")
      |> Enum.map(&String.to_integer/1)

    # Initialize robot state
    initial_state = %{
      program: program,
      memory: %{},
      pointer: 0,
      relative_base: 0,
      inputs: [],
      outputs: [],
      halted: false
    }

    # Initialize panels map (all black initially)
    panels = %{}

    # Start robot at (0, 0), facing up
    initial_robot = %{
      x: 0,
      y: 0,
      direction: :up,
      panels: panels,
      painted: MapSet.new()
    }

    # Run the simulation
    final_robot = run_robot(initial_state, initial_robot)

    # Output the number of panels painted at least once
    IO.puts("Number of panels painted at least once: #{MapSet.size(final_robot.painted)}")
  end

  def run_robot(state, robot) do
    current_color = Map.get(robot.panels, {robot.x, robot.y}, 0)
    new_state = %{state | inputs: [current_color]}

    case run_intcode(new_state) do
      {:need_input, _} ->
        # Program is waiting for input, which shouldn't happen
        robot

      {:halted, final_state} ->
        robot

      {:output, outputs, final_state} ->
        case outputs do
          [paint_color, turn_direction] ->
            # Paint the current panel
            new_panels = Map.put(robot.panels, {robot.x, robot.y}, paint_color)
            # Record that this panel was painted
            new_painted = MapSet.put(robot.painted, {robot.x, robot.y})
            # Update direction
            new_direction = turn(robot.direction, turn_direction)
            # Move forward
            {new_x, new_y} = move_forward({robot.x, robot.y}, new_direction)
            # Update robot state
            new_robot = %{
              x: new_x,
              y: new_y,
              direction: new_direction,
              panels: new_panels,
              painted: new_painted
            }

            # Continue running with the updated state and robot
            run_robot(final_state, new_robot)

          _ ->
            # Invalid output
            robot
        end
    end
  end

  def turn(:up, 0), do: :left
  def turn(:up, 1), do: :right
  def turn(:right, 0), do: :up
  def turn(:right, 1), do: :down
  def turn(:down, 0), do: :right
  def turn(:down, 1), do: :left
  def turn(:left, 0), do: :down
  def turn(:left, 1), do: :up

  def move_forward({x, y}, :up), do: {x, y - 1}
  def move_forward({x, y}, :down), do: {x, y + 1}
  def move_forward({x, y}, :left), do: {x - 1, y}
  def move_forward({x, y}, :right), do: {x + 1, y}

  # Intcode computer implementation
  def run_intcode(state) do
    execute(state)
  end

  def execute(state) do
    execute(state, state.pointer)
  end

  def execute(state, pointer) do
    opcode = get_mem(state, pointer) |> rem(100)
    modes = get_mem(state, pointer) |> div(100) |> Integer.to_string() |> String.pad_leading(3, "0")
    mode1 = String.at(modes, 2) |> String.to_integer()
    mode2 = String.at(modes, 1) |> String.to_integer()
    mode3 = String.at(modes, 0) |> String.to_integer()

    case opcode do
      1 ->
        # Addition
        param1 = get_param(state, pointer + 1, mode1)
        param2 = get_param(state, pointer + 2, mode2)
        dest = get_dest(state, pointer + 3, mode3)
        new_mem = Map.put(state.memory, dest, param1 + param2)
        execute(%{state | memory: new_mem, pointer: pointer + 4}, pointer + 4)

      2 ->
        # Multiplication
        param1 = get_param(state, pointer + 1, mode1)
        param2 = get_param(state, pointer + 2, mode2)
        dest = get_dest(state, pointer + 3, mode3)
        new_mem = Map.put(state.memory, dest, param1 * param2)
        execute(%{state | memory: new_mem, pointer: pointer + 4}, pointer + 4)

      3 ->
        # Input
        case state.inputs do
          [input | rest_inputs] ->
            dest = get_dest(state, pointer + 1, mode1)
            new_mem = Map.put(state.memory, dest, input)
            execute(%{state | memory: new_mem, pointer: pointer + 2, inputs: rest_inputs}, pointer + 2)

          [] ->
            # Wait for input
            {:need_input, state}
        end

      4 ->
        # Output
        param1 = get_param(state, pointer + 1, mode1)
        new_outputs = state.outputs ++ [param1]
        new_state = %{state | outputs: new_outputs, pointer: pointer + 2}
        # Check if we have two outputs
        if length(new_state.outputs) >= 2 do
          {:output, Enum.take(new_state.outputs, 2), %{new_state | outputs: Enum.drop(new_state.outputs, 2)}}
        else
          execute(new_state, pointer + 2)
        end

      5 ->
        # Jump-if-true
        param1 = get_param(state, pointer + 1, mode1)
        param2 = get_param(state, pointer + 2, mode2)
        if param1 != 0 do
          execute(state, param2)
        else
          execute(state, pointer + 3)
        end

      6 ->
        # Jump-if-false
        param1 = get_param(state, pointer + 1, mode1)
        param2 = get_param(state, pointer + 2, mode2)
        if param1 == 0 do
          execute(state, param2)
        else
          execute(state, pointer + 3)
        end

      7 ->
        # Less than
        param1 = get_param(state, pointer + 1, mode1)
        param2 = get_param(state, pointer + 2, mode2)
        dest = get_dest(state, pointer + 3, mode3)
        value = if param1 < param2, do: 1, else: 0
        new_mem = Map.put(state.memory, dest, value)
        execute(%{state | memory: new_mem, pointer: pointer + 4}, pointer + 4)

      8 ->
        # Equals
        param1 = get_param(state, pointer + 1, mode1)
        param2 = get_param(state, pointer + 2, mode2)
        dest = get_dest(state, pointer + 3, mode3)
        value = if param1 == param2, do: 1, else: 0
        new_mem = Map.put(state.memory, dest, value)
        execute(%{state | memory: new_mem, pointer: pointer + 4}, pointer + 4)

      9 ->
        # Adjust relative base
        param1 = get_param(state, pointer + 1, mode1)
        new_relative_base = state.relative_base + param1
        execute(%{state | relative_base: new_relative_base, pointer: pointer + 2}, pointer + 2)

      99 ->
        # Halt
        {:halted, state}

      _ ->
        IO.puts("Unknown opcode #{opcode} at position #{pointer}")
        {:halted, state}
    end
  end

  def get_param(state, addr, mode) do
    case mode do
      0 ->
        # Position mode
        get_mem(state, get_mem(state, addr))

      1 ->
        # Immediate mode
        get_mem(state, addr)

      2 ->
        # Relative mode
        get_mem(state, state.relative_base + get_mem(state, addr))

      _ ->
        0
    end
  end

  def get_dest(state, addr, mode) do
    case mode do
      0 ->
        # Position mode
        get_mem(state, addr)

      2 ->
        # Relative mode
        state.relative_base + get_mem(state, addr)

      _ ->
        0
    end
  end

  def get_mem(state, addr) when addr < 0 do
    0
  end

  def get_mem(state, addr) do
    cond do
      addr < length(state.program) ->
        Enum.at(state.program, addr)

      Map.has_key?(state.memory, addr) ->
        Map.get(state.memory, addr)

      true ->
        0
    end
  end
end

Day11.main()
