defmodule Day13 do
  @moduledoc """
  Solution for Advent of Code 2019, Day 13, Part 2.
  """

  def run(filename) do
    input = File.read!(filename) |> parse_input()
    {_, score} = play(input)
    score
  end

  def parse_input(input) do
    input
    |> String.trim()
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
  end

  def play(program) do
    program = Map.put(program, 0, 2) # Set memory address 0 to 2 for free play
    state = %{0 => program, 1 => 0, 2 => []} # Initialize Intcode state
    play(state, {0, 0}, %{}, 0)
  end

  defp play(state, {ball_x, ball_y}, tiles, score) do
    case Intcode.run(state, []) do
      {:halt, _} ->
        {tiles, score}

      {:output, x, state} ->
        [y, tile_id] = Intcode.run(state, []) |> handle_output()
        state = Intcode.run(state, []) |> handle_output()

        cond do
          x == -1 and y == 0 ->
            play(state, {ball_x, ball_y}, tiles, tile_id)

          tile_id == 4 ->
            play(state, {x, y}, tiles, score)

          true ->
            tiles = Map.put(tiles, {x, y}, tile_id)
            play(state, {ball_x, ball_y}, tiles, score)
        end

      {:input, state} ->
        paddle_x = tiles |> Enum.find(fn {{x, _}, id} -> id == 3 end) |> elem(0) |> elem(0)
        input = joystick(paddle_x, ball_x)
        play(Intcode.run(state, [input]), {ball_x, ball_y}, tiles, score)
    end
  end

  defp handle_output({:output, value, program}) do
    [value | handle_output(program)]
  end

  defp handle_output({:halt, _}), do: []

  defp joystick(paddle_x, ball_x) when paddle_x < ball_x, do: 1
  defp joystick(paddle_x, ball_x) when paddle_x > ball_x, do: -1
  defp joystick(_, _), do: 0
end