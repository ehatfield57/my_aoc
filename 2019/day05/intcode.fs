\ Advent of Code 2019 - Day 5 Part 2 in gforth

\ Read the Intcode program from input and store it in memory
variable mem-size
create memory 0 ,  \ Initialize memory with a dummy zero

variable ip        \ Instruction pointer

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
: get-param ( mode addr -- value )
  \ mode: 0 = position mode, 1 = immediate mode
  \ addr: address of parameter
  case
    0 of                             \ Position mode
      memory addr @ cells + @        \ Fetch from memory at address
    endof
    1 of                             \ Immediate mode
      addr @                         \ Use the value directly
    endof
    ( default )
      ." Invalid mode" cr abort
  endcase ;

\ Parse instruction into opcode and parameter modes
: parse-instruction ( instruction -- mode3 mode2 mode1 opcode )
  dup 100 mod                        \ opcode = instruction mod 100
  swap 100 /                         \ modes = instruction / 100
  dup 10 mod                         \ mode1 = modes mod 10
  swap 10 /                          \ modes = modes / 10
  dup 10 mod                         \ mode2 = modes mod 10
  swap 10 /                          \ modes = modes / 10
  10 mod                             \ mode3 = modes mod 10
  rot rot ;                          \ Reorder stack to mode3 mode2 mode1 opcode

\ The Intcode computer
: execute-program ( -- )
  0 ip !                             \ Initialize instruction pointer to 0
  begin
    ip @ cells memory + @            \ Fetch instruction
    parse-instruction                \ Get mode3 mode2 mode1 opcode
    >r                               \ Save opcode on return stack
    \ Stack: mode3 mode2 mode1
    r@ 99 = if
      rdrop exit                     \ Halt if opcode is 99
    then
    \ Fetch parameter modes
    \ For opcodes needing parameters, fetch them based on modes
    r@                               \ Fetch opcode
    case
      1 of                           \ Addition
        \ Fetch parameters
        ip @ 1 + cells + >r          \ Address of param1
        ip @ 2 + cells + >r          \ Address of param2
        ip @ 3 + cells + @           \ Address to store result
        \ Get parameter values
        r>                           \ Address of param2
        swap                         \ mode1 under mode2
        get-param                    \ val2
        r>                           \ Address of param1
        swap                         \ mode1 under val2
        get-param                    \ val1 val2
        +                            \ val1 + val2
        memory swap cells + !        \ Store result
        ip @ 4 + ip !                \ Move instruction pointer
      endof
      2 of                           \ Multiplication
        \ Similar to addition
        ip @ 1 + cells + >r
        ip @ 2 + cells + >r
        ip @ 3 + cells + @
        r>
        swap
        get-param
        r>
        swap
        get-param
        *
        memory swap cells + !
        ip @ 4 + ip !
      endof
      3 of                           \ Input
        ip @ 1 + cells + @           \ Address to store input
        5                             \ Input value
        memory swap cells + !         \ Store input
        ip @ 2 + ip !
      endof
      4 of                           \ Output
        ip @ 1 + cells + >r          \ Address of parameter
        swap                         \ mode1 under address
        get-param                    \ Get value to output
        . cr                         \ Output the value
        ip @ 2 + ip !
      endof
      5 of                           \ Jump-if-true
        ip @ 1 + cells + >r          \ Address of param1
        ip @ 2 + cells + >r          \ Address of param2
        r>                           \ Address of param2
        swap                         \ mode1 under address
        get-param                    \ val2
        r>                           \ Address of param1
        swap                         \ mode1 under val2
        get-param                    \ val1 val2
        swap                         \ val2 val1
        dup 0 <> if
          swap ip !                  \ Set ip to val2
        else
          ip @ 3 + ip !              \ Move ip forward by 3
        then
      endof
      6 of                           \ Jump-if-false
        ip @ 1 + cells + >r
        ip @ 2 + cells + >r
        r>
        swap
        get-param                    \ val2
        r>
        swap
        get-param                    \ val1 val2
        swap
        dup 0 = if
          swap ip !                  \ Set ip to val2
        else
          ip @ 3 + ip !              \ Move ip forward by 3
        then
      endof
      7 of                           \ Less than
        ip @ 1 + cells + >r
        ip @ 2 + cells + >r
        ip @ 3 + cells + @           \ Address to store result
        r>
        swap
        get-param                    \ val2
        r>
        swap
        get-param                    \ val1 val2
        <                            \ Compare val1 and val2
        if 1 else 0 then
        memory swap cells + !
        ip @ 4 + ip !
      endof
      8 of                           \ Equals
        ip @ 1 + cells + >r
        ip @ 2 + cells + >r
        ip @ 3 + cells + @
        r>
        swap
        get-param                    \ val2
        r>
        swap
        get-param                    \ val1 val2
        =                            \ Compare val1 and val2
        if 1 else 0 then
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
