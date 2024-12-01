defmodule Day13 do
  @moduledoc """
  Solution for Advent of Code 2019, Day 13, Part 2.
  """

  require Intcode

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

  def play(state, {ball_x, _}, tiles, score) do
    case Intcode.run(state, []) do
      {:halt, _} ->
        {tiles, score}

      {:output, x, state} ->
        [y, tile_id] = Intcode.run(state, []) |> handle_output()
        state = Intcode.run(state, []) |> handle_output()

        cond do
          x == -1 and y == 0 ->
            play(state, {ball_x, y}, tiles, tile_id)

          tile_id == 4 ->
            play(state, {x, y}, tiles, score)

          true ->
            tiles = Map.put(tiles, {x, y}, tile_id)
            play(state, {ball_x, y}, tiles, score)
        end

      {:input, state} ->
        paddle_x = tiles |> Enum.find(fn {{x, _}, id} -> id == 3 end) |> elem(0) |> elem(0)
        input = joystick(paddle_x, ball_x)
        play(Intcode.run(state, [input]), {ball_x, _}, tiles, score)
    end
  end

  def play(program, {ball_x, foo}, tiles, score) do
    case Intcode.run(program, []) do
      {:halt, _} ->
        {tiles, score}

      {:output, x, program} ->
        [y, tile_id] = Intcode.run(program, []) |> handle_output()
        program = Intcode.run(program, []) |> handle_output()

        cond do
          x == -1 and y == 0 ->
            play(program, {ball_x, y}, tiles, tile_id)

          tile_id == 4 ->
            play(program, {x, y}, tiles, score)

          true ->
            tiles = Map.put(tiles, {x, y}, tile_id)
            play(program, {ball_x, y}, tiles, score)
        end

      {:input, program} ->
        paddle_x = tiles |> Enum.find(fn {{x, _}, id} -> id == 3 end) |> elem(0) |> elem(0)
        input = joystick(paddle_x, ball_x)
        play(program, {ball_x, foo}, tiles, score) |> Intcode.run([input])
    end
  end

  def handle_output({:output, value, program}) do
    [value | handle_output(program)]
  end

  def handle_output({:halt, _}), do: []

  def joystick(paddle_x, ball_x) when paddle_x < ball_x, do: 1
  def joystick(paddle_x, ball_x) when paddle_x > ball_x, do: -1
  def joystick(_, _), do: 0
end
