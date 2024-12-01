from itertools import permutations

def parse_intcode(input_data):
    return list(map(int, input_data.split(',')))

def get_param(mode, param, intcode):
    if mode == 0:
        return intcode[param] if param < len(intcode) else 0
    elif mode == 1:
        return param

def ensure_memory_size(intcode, index):
    if index >= len(intcode):
        intcode.extend([0] * (index - len(intcode) + 1))
    return intcode

def run_intcode(intcode, inputs, pc=0):
    while pc < len(intcode):
        opcode = intcode[pc] % 100
        mode1 = (intcode[pc] // 100) % 10
        mode2 = (intcode[pc] // 1000) % 10

        param1 = intcode[pc + 1] if pc + 1 < len(intcode) else 0
        param2 = intcode[pc + 2] if pc + 2 < len(intcode) else 0
        param3 = intcode[pc + 3] if pc + 3 < len(intcode) else 0

        intcode = ensure_memory_size(intcode, param1)
        intcode = ensure_memory_size(intcode, param2)
        intcode = ensure_memory_size(intcode, param3)

        if opcode == 1:  # Addition
            intcode[param3] = get_param(mode1, param1, intcode) + get_param(mode2, param2, intcode)
            pc += 4
        elif opcode == 2:  # Multiplication
            intcode[param3] = get_param(mode1, param1, intcode) * get_param(mode2, param2, intcode)
            pc += 4
        elif opcode == 3:  # Input
            if inputs:
                intcode[param1] = inputs.pop(0)
            else:
                return ('await_input', intcode, pc)
            pc += 2
        elif opcode == 4:  # Output
            output = get_param(mode1, param1, intcode)
            return ('output', output, intcode, pc + 2)
        elif opcode == 5:  # Jump if true
            if get_param(mode1, param1, intcode) != 0:
                pc = get_param(mode2, param2, intcode)
            else:
                pc += 3
        elif opcode == 6:  # Jump if false
            if get_param(mode1, param1, intcode) == 0:
                pc = get_param(mode2, param2, intcode)
            else:
                pc += 3
        elif opcode == 7:  # Less than
            intcode[param3] = 1 if get_param(mode1, param1, intcode) < get_param(mode2, param2, intcode) else 0
            pc += 4
        elif opcode == 8:  # Equals
            intcode[param3] = 1 if get_param(mode1, param1, intcode) == get_param(mode2, param2, intcode) else 0
            pc += 4
        elif opcode == 99:  # Halt
            return ('halt', intcode)
        else:
            return ('error', f"Unknown opcode {opcode} at pc {pc}")

def amplifier_loop_sequence(intcode, phases):
    states = [(intcode[:], [phase], 0) for phase in phases]
    amp_index = 0
    signal = 0
    while True:
        state = states[amp_index]
        result = run_intcode(state[0], [signal] + state[1], state[2])

        if result[0] == 'output':
            signal = result[1]
            states[amp_index] = (result[2], [], result[3])
            amp_index = (amp_index + 1) % len(phases)
        elif result[0] == 'await_input':
            states[amp_index] = (result[1], [], result[2])
            amp_index = (amp_index + 1) % len(phases)
        elif result[0] == 'halt' and amp_index == 0:
            return signal
        else:
            return result

def find_best_phase_setting_with_feedback(intcode):
    phase_settings = permutations([5, 6, 7, 8, 9])
    max_signal = 0
    for phases in phase_settings:
        signal = amplifier_loop_sequence(intcode, phases)
        if isinstance(signal, int) and signal > max_signal:
            max_signal = signal
    return max_signal

# Read input from file
with open('day07-data.txt') as f:
    input_data = f.read().strip()

# Parse the input data
intcode = parse_intcode(input_data)

# Find the best phase setting with feedback loop
best_output = find_best_phase_setting_with_feedback(intcode)

# Print the result
print(f"Best output with feedback loop: {best_output}")
