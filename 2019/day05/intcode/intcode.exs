
defmodule NumberExtractor do
  def extract_numbers_from_file(file_path) do
    file_path
    |> File.read!()
    |> String.split(~r/[^\d-]+/, trim: true)
    |> Enum.map(&String.to_integer/1)
  end
end

