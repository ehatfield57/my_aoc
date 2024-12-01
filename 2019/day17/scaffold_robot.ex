defmodule ScaffoldRobot do
  # Toggle debug mode
  @debug false

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

    # Modify the first memory address to 2 to activate the robot
    intcode = %{intcode | memory: Map.put(intcode.memory, 0, 2)}

    # Define the movement routines
    # **IMPORTANT**: You need to determine these based on your scaffold map.
    # Below are placeholder examples. Replace them with the actual routines.
    main_routine = "A,B,A,C,B,C,A,B,A\n"
    function_a = "R,10,L,8,R,10,R,4\n"  # Example: Replace with actual
    function_b = "L,6,L,6,R,10\n"       # Example: Replace with actual
    function_c = "L,6,R,12,R,12,R,10\n" # Example: Replace with actual
    video_feed = "n\n"

    # Combine all inputs and convert to ASCII codes
    input_string =
      main_routine <>
        function_a <>
        function_b <>
        function_c <>
        video_feed

    input_ascii = String.to_charlist(input_string)

    if @debug do
      IO.puts("\n[DEBUG] Sending Movement Routines:")
      IO.puts("Main Routine: #{inspect(main_routine)}")
      IO.puts("Function A: #{inspect(function_a)}")
      IO.puts("Function B: #{inspect(function_b)}")
      IO.puts("Function C: #{inspect(function_c)}")
      IO.puts("Video Feed: #{inspect(video_feed)}")
    end

    # Run the Intcode program with the input
    final_intcode = Intcode.run(intcode, input_ascii)

    # Check if the program has halted properly
    if final_intcode.halted do
      # The output is the last value in the outputs list
      dust_amount =
        final_intcode.outputs
        |> Enum.reverse()
        |> hd()

      IO.puts("\n[RESULT] Dust collected: #{dust_amount}")
    else
      IO.puts("\n[ERROR] Intcode program did not halt properly.")
      IO.puts("[DEBUG] Final Outputs: #{inspect(Enum.reverse(final_intcode.outputs))}")
    end
  end
end
