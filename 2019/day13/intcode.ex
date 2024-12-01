defmodule Intcode do
  @moduledoc """
  Intcode computer implementation for Advent of Code 2019.
  """

  def run(program, input \\ [])

  def run(program, input) when is_list(program) and is_list(input) do
    run(%{0 => program, 1 => 0, 2 => input}, input)
  end

  def run(%{0 => program, 1 => ip, 2 => input} = state, _input) do
    case opcode(program, ip) do
      1 -> add(state)
      2 -> multiply(state)
      3 -> take_input(state, input)
      4 -> output(state)
      5 -> jump_if_true(state)
      6 -> jump_if_false(state)
      7 -> less_than(state)
      8 -> equals(state)
      9 -> adjust_relative_base(state)
      99 -> {:halt, state}
    end
  end

  defp opcode(program, ip) do
    get(program, ip) |> rem(100)
  end

  defp modes(program, ip) do
    div(get(program, ip), 100)
    |> Integer.digits()
    |> Enum.reverse()
    |> Enum.map(&(&1 == 1))
  end

  defp get(program, ip, mode \\ false, relative_base \\ 0) do
    value = program[ip] || 0

    case mode do
      true ->
        case program[relative_base + value] do
          nil -> 0
          v -> v
        end

      false ->
        program[value] || 0
    end
  end

  defp put(program, ip, value, mode, relative_base) do
    address = program[ip] || 0

    case mode do
      true -> Map.put(program, relative_base + address, value)
      false -> Map.put(program, address, value)
    end
  end

  defp add(%{0 => program, 1 => ip, 2 => input, 3 => relative_base} = state) do
    [m1, m2, m3] = modes(program, ip)
    p1 = get(program, ip + 1, m1, relative_base)
    p2 = get(program, ip + 2, m2, relative_base)
    program = put(program, ip + 3, p1 + p2, m3, relative_base)
    run(%{state | 0 => program, 1 => ip + 4})
  end

  defp multiply(%{0 => program, 1 => ip, 2 => input, 3 => relative_base} = state) do
    [m1, m2, m3] = modes(program, ip)
    p1 = get(program, ip + 1, m1, relative_base)
    p2 = get(program, ip + 2, m2, relative_base)
    program = put(program, ip + 3, p1 * p2, m3, relative_base)
    run(%{state | 0 => program, 1 => ip + 4})
  end

  defp take_input(%{0 => program, 1 => ip, 2 => [value | input], 3 => relative_base} = state) do
    [m1 | _] = modes(program, ip)
    program = put(program, ip + 1, value, m1, relative_base)
    run(%{state | 0 => program, 1 => ip + 2, 2 => input})
  end

  defp take_input(state, []), do: {:input, state}

  defp output(%{0 => program, 1 => ip, 3 => relative_base} = state) do
    [m1 | _] = modes(program, ip)
    value = get(program, ip + 1, m1, relative_base)
    {:output, value, %{state | 1 => ip + 2}}
  end

  defp jump_if_true(%{0 => program, 1 => ip, 3 => relative_base} = state) do
    [m1, m2 | _] = modes(program, ip)
    p1 = get(program, ip + 1, m1, relative_base)
    p2 = get(program, ip + 2, m2, relative_base)
    ip = if p1 != 0, do: p2, else: ip + 3
    run(%{state | 1 => ip})
  end

  defp jump_if_false(%{0 => program, 1 => ip, 3 => relative_base} = state) do
    [m1, m2 | _] = modes(program, ip)
    p1 = get(program, ip + 1, m1, relative_base)
    p2 = get(program, ip + 2, m2, relative_base)
    ip = if p1 == 0, do: p2, else: ip + 3
    run(%{state | 1 => ip})
  end

  defp less_than(%{0 => program, 1 => ip, 3 => relative_base} = state) do
    [m1, m2, m3] = modes(program, ip)
    p1 = get(program, ip + 1, m1, relative_base)
    p2 = get(program, ip + 2, m2, relative_base)
    value = if p1 < p2, do: 1, else: 0
    program = put(program, ip + 3, value, m3, relative_base)
    run(%{state | 0 => program, 1 => ip + 4})
  end

  defp equals(%{0 => program, 1 => ip, 3 => relative_base} = state) do
    [m1, m2, m3] = modes(program, ip)
    p1 = get(program, ip + 1, m1, relative_base)
    p2 = get(program, ip + 2, m2, relative_base)
    value = if p1 == p2, do: 1, else: 0
    program = put(program, ip + 3, value, m3, relative_base)
    run(%{state | 0 => program, 1 => ip + 4})
  end

  defp adjust_relative_base(%{0 => program, 1 => ip, 3 => relative_base} = state) do
    [m1 | _] = modes(program, ip)
    p1 = get(program, ip + 1, m1, relative_base)
    run(%{state | 1 => ip + 2, 3 => relative_base + p1})
  end
end