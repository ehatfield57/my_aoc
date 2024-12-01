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
