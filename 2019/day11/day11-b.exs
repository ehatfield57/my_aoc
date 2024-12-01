defmodule Intcode do
  defstruct memory: %{}, pointer: 0, relative_base: 0, inputs: [], outputs: [], halted: false

  @opcode_param_count %{
    1 => 3,  # Add
    2 => 3,  # Multiply
    3 => 1,  # Input
    4 => 1,  # Output
    5 => 2,  # Jump-if-true
    6 => 2,  # Jump-if-false
    7 => 3,  # Less than
    8 => 3,  # Equals
    9 => 1,  # Adjust relative base
    99 => 0  # Halt
  }

  def new(program) do
    initial_memory =
      program
      |> Enum.with_index()
      |> Enum.map(fn {v, i} -> {i, v} end)
      |> Map.new()

    %Intcode{memory: initial_memory}
  end

  def run(%Intcode{halted: true} = state), do: state

  def run(state = %Intcode{}) do
    execute(state)
  end

  defp execute(state = %Intcode{}) do
    opcode_full = Map.get(state.memory, state.pointer, 0)
    opcode = rem(opcode_full, 100)
    modes = div(opcode_full, 100)

    case opcode do
      1 -> perform_op(state, :add, modes)
      2 -> perform_op(state, :multiply, modes)
      3 -> perform_input(state, modes)
      4 -> perform_output(state, modes)
      5 -> perform_jump(state, :jump_if_true, modes)
      6 -> perform_jump(state, :jump_if_false, modes)
      7 -> perform_op(state, :less_than, modes)
      8 -> perform_op(state, :equals, modes)
      9 -> perform_relative_base(state, modes)
      99 -> %{state | halted: true}
      _ ->
        IO.puts("Unknown opcode #{opcode} at position #{state.pointer}")
        %{state | halted: true}
    end
  end

  defp perform_op(state, operation, modes) do
    param1 = get_param(state, state.pointer + 1, rem(modes, 10))
    param2 = get_param(state, state.pointer + 2, rem(div(modes, 10), 10))
    dest = get_write_address(state, state.pointer + 3, rem(div(modes, 100), 10))

    result =
      case operation do
        :add -> param1 + param2
        :multiply -> param1 * param2
        :less_than -> if param1 < param2, do: 1, else: 0
        :equals -> if param1 == param2, do: 1, else: 0
      end

    new_memory = Map.put(state.memory, dest, result)
    %{state | memory: new_memory, pointer: state.pointer + 4}
    |> run()
  end

  defp perform_input(state, mode) do
    case state.inputs do
      [input | rest_inputs] ->
        dest = get_write_address(state, state.pointer + 1, rem(mode, 10))
        new_memory = Map.put(state.memory, dest, input)
        %{state | memory: new_memory, pointer: state.pointer + 2, inputs: rest_inputs}
        |> run()

      [] ->
        # Awaiting input
        state
    end
  end

  defp perform_output(state, mode) do
    output = get_param(state, state.pointer + 1, rem(mode, 10))
    new_outputs = state.outputs ++ [output]
    %{state | outputs: new_outputs, pointer: state.pointer + 2}
    |> run()
  end

  defp perform_jump(state, jump_type, modes) do
    param1 = get_param(state, state.pointer + 1, rem(modes, 10))
    param2 = get_param(state, state.pointer + 2, rem(div(modes, 10), 10))

    should_jump =
      case jump_type do
        :jump_if_true -> param1 != 0
        :jump_if_false -> param1 == 0
      end

    new_pointer =
      if should_jump do
        param2
      else
        state.pointer + 3
      end

    %{state | pointer: new_pointer}
    |> run()
  end

  defp perform_relative_base(state, mode) do
    adjustment = get_param(state, state.pointer + 1, rem(mode, 10))
    %{state | relative_base: state.relative_base + adjustment, pointer: state.pointer + 2}
    |> run()
  end

  defp get_param(state, addr, mode) do
    case mode do
      0 -> Map.get(state.memory, Map.get(state.memory, addr, 0), 0)
      1 -> Map.get(state.memory, addr, 0)
      2 -> Map.get(state.memory, state.relative_base + Map.get(state.memory, addr, 0), 0)
      _ ->
        IO.puts("Unknown parameter mode #{mode} at address #{addr}")
        0
    end
  end

  defp get_write_address(state, addr, mode) do
    case mode do
      0 -> Map.get(state.memory, addr, 0)
      2 -> state.relative_base + Map.get(state.memory, addr, 0)
      _ ->
        IO.puts("Invalid write parameter mode #{mode} at address #{addr}")
        Map.get(state.memory, addr, 0)
    end
  end
end

defmodule Robot do
  @directions [:up, :right, :down, :left]

  defstruct x: 0, y: 0, dir_index: 1, panels: %{}, painted: MapSet.new()

  def current_direction(%Robot{dir_index: dir_index}), do: Enum.at(@directions, dir_index)

  def turn(robot = %Robot{}, turn_instruction) do
    new_dir_index =
      case turn_instruction do
        0 -> rem(robot.dir_index - 1 + 4, 4)  # Turn left
        1 -> rem(robot.dir_index + 1, 4)      # Turn right
      end

    %{robot | dir_index: new_dir_index}
  end

  def move_forward(robot = %Robot{}) do
    {x, y} =
      case current_direction(robot) do
        :up -> {robot.x, robot.y - 1}
        :down -> {robot.x, robot.y + 1}
        :left -> {robot.x - 1, robot.y}
        :right -> {robot.x + 1, robot.y}
      end

    %{robot | x: x, y: y}
  end
end

defmodule Day11 do
  def main do
    # Read Intcode program from 'day11-data.txt'
    program =
      "day11-data.txt"
      |> File.read!()
      |> String.trim()
      |> String.split(",")
      |> Enum.map(&String.to_integer/1)

    # Initialize Intcode computer
    intcode = Intcode.new(program)

    # Initialize Robot
    robot = %Robot{}

    # Initialize panel colors (all black initially)
    panels = %{}

    # Start simulation
    final_robot = simulate(intcode, robot)

    # Output the number of panels painted at least once
    painted_count = MapSet.size(final_robot.painted)
    IO.puts("Number of panels painted at least once: #{painted_count}")
  end

  defp simulate(intcode, robot) do
    # Get current panel color (default black)
    current_color = Map.get(robot.panels, {robot.x, robot.y}, 0)

    # Provide input to Intcode
    intcode = %{intcode | inputs: intcode.inputs ++ [current_color]}

    # Run Intcode until it produces output or halts
    intcode = Intcode.run(intcode)

    cond do
      intcode.halted ->
        robot

      length(intcode.outputs) >= 2 ->
        # Retrieve two outputs: paint and turn
        [paint, turn | rest] = intcode.outputs
        new_outputs = Enum.drop(intcode.outputs, 2)

        # Update Intcode state with remaining outputs
        intcode = %{intcode | outputs: new_outputs}

        # Paint the current panel
        new_panels = Map.put(robot.panels, {robot.x, robot.y}, paint)
        # Record that this panel was painted
        new_painted = MapSet.put(robot.painted, {robot.x, robot.y})

        # Update robot's state
        robot = %{robot | panels: new_panels, painted: new_painted}
        |> Robot.turn(turn)
        |> Robot.move_forward()

        # Continue simulation
        simulate(intcode, robot)

      true ->
        # Awaiting more outputs/input
        robot
    end
  end
end

Day11.main()
