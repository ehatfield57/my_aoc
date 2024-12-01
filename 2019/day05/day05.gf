\ Advent of Code 2019 - Day 5 Part 2 in gforth

\ Read the Intcode program from input and store it in memory
variable mem-size
create memory 0 ,  \ Initialize memory with a dummy zero

: read-program ( -- )
  begin
    parse-name dup
  while
    s>number? 0= if
      drop ." Invalid number" cr abort
    then
    memory mem-size @ cells + !      \ Store the number in memory
    mem-size @ 1+ mem-size !         \ Increment memory size
  repeat
  drop ;                             \ Clean up

\ Fetch parameter value based on mode
: get-param ( addr mode -- value )
  over @                             \ Get parameter value from memory
  swap
  case
    0 of                             \ Position mode
      memory swap cells + @
    endof
    1 of                             \ Immediate mode
      swap drop                      \ Use the value directly
    endof
    ( default )
      ." Invalid mode" cr abort
  endcase ;

\ The Intcode computer
: execute-program ( -- )
  0 variable ip                      \ Instruction pointer
  begin
    memory ip @ cells + @            \ Fetch instruction
    dup 100 /mod                     \ Extract opcode and modes
    swap 100 mod
    swap                             \ Stack: opcode modes
    dup 99 = if
      drop drop exit                 \ Halt if opcode is 99
    then
    \ Parse parameter modes
    dup 10 /mod swap 10 mod          \ Modes for param1 and param2
    swap                             \ Stack: mode2 mode1 opcode
    >r                               \ Save opcode on return stack
    \ Handle each opcode
    r@
    case
      1 of                           \ Addition
        \ Fetch parameters
        memory ip @ 1+ cells + @ over \ Param1 address and mode1
        get-param                     \ Get val1
        memory ip @ 2+ cells + @ swap \ Param2 address and mode2
        get-param                     \ Get val2
        +                             \ Compute val1 + val2
        memory ip @ 3 + cells + @     \ Param3 (address to store)
        memory swap cells + !         \ Store result
        ip @ 4 + ip !                 \ Move instruction pointer
      endof
      2 of                           \ Multiplication
        \ Similar to addition
        memory ip @ 1+ cells + @ over
        get-param
        memory ip @ 2+ cells + @ swap
        get-param
        *
        memory ip @ 3 + cells + @
        memory swap cells + !
        ip @ 4 + ip !
      endof
      3 of                           \ Input
        \ Read input value (input is 5 for Part Two)
        memory ip @ 1+ cells + @     \ Param (address to store input)
        5                            \ Input value
        memory swap cells + !        \ Store input
        ip @ 2 + ip !
      endof
      4 of                           \ Output
        memory ip @ 1+ cells + @ over \ Param address and mode1
        get-param                     \ Get value to output
        . cr                          \ Output the value
        ip @ 2 + ip !
      endof
      5 of                           \ Jump-if-true
        memory ip @ 1+ cells + @ over
        get-param                    \ val1
        memory ip @ 2+ cells + @ swap
        get-param                    \ val2
        val1 0 <> if
          val2 ip !
        else
          ip @ 3 + ip !
        then
      endof
      6 of                           \ Jump-if-false
        memory ip @ 1+ cells + @ over
        get-param                    \ val1
        memory ip @ 2+ cells + @ swap
        get-param                    \ val2
        val1 0 = if
          val2 ip !
        else
          ip @ 3 + ip !
        then
      endof
      7 of                           \ Less than
        memory ip @ 1+ cells + @ over
        get-param                    \ val1
        memory ip @ 2+ cells + @ swap
        get-param                    \ val2
        val1 val2 < if 1 else 0 then
        memory ip @ 3 + cells + @    \ Param3 (address to store)
        memory swap cells + !
        ip @ 4 + ip !
      endof
      8 of                           \ Equals
        memory ip @ 1+ cells + @ over
        get-param                    \ val1
        memory ip @ 2+ cells + @ swap
        get-param                    \ val2
        val1 val2 = if 1 else 0 then
        memory ip @ 3 + cells + @    \ Param3 (address to store)
        memory swap cells + !
        ip @ 4 + ip !
      endof
      ( default )
        ." Unknown opcode: " r@ . cr abort
    endcase
    rdrop                            \ Clean up return stack
  again ;

\ Main program
read-program
execute-program
