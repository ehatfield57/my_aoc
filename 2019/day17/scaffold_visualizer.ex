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
