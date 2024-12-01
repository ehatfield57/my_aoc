{-# LANGUAGE RecordWildCards #-}

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import System.IO

type Memory = Map.Map Int Int

data ProgramState = ProgramState
  { memory :: Memory
  , ip     :: Int
  , input  :: [Int]
  , output :: [Int]
  } deriving Show

main :: IO ()
main = do
  contents <- getContents
  let intcode = map read $ splitOn "," contents :: [Int]
      initialMemory = Map.fromList $ zip [0..] intcode
      initialState = ProgramState initialMemory 0 [5] []
      finalState = runProgram initialState
  mapM_ print (reverse $ output finalState)
  putStrLn $ "Diagnostic code: " ++ show (head $ output finalState)

runProgram :: ProgramState -> ProgramState
runProgram state@ProgramState{..} =
  let instr = Map.findWithDefault 0 ip memory
      (opcode, modes) = parseInstruction instr
  in case opcode of
       99 -> state
       1  -> -- Addition
         let (param1, param2, dest) = (memory Map.! (ip+1), memory Map.! (ip+2), memory Map.! (ip+3))
             val1 = getParamValue memory param1 (modes !! 0)
             val2 = getParamValue memory param2 (modes !! 1)
             memory' = Map.insert dest (val1 + val2) memory
         in runProgram state { memory = memory', ip = ip + 4 }
       2  -> -- Multiplication
         let (param1, param2, dest) = (memory Map.! (ip+1), memory Map.! (ip+2), memory Map.! (ip+3))
             val1 = getParamValue memory param1 (modes !! 0)
             val2 = getParamValue memory param2 (modes !! 1)
             memory' = Map.insert dest (val1 * val2) memory
         in runProgram state { memory = memory', ip = ip + 4 }
       3  -> -- Input
         let dest = memory Map.! (ip+1)
             inputValue = head input
             memory' = Map.insert dest inputValue memory
         in runProgram state { memory = memory', ip = ip + 2, input = tail input }
       4  -> -- Output
         let param = memory Map.! (ip+1)
             val = getParamValue memory param (modes !! 0)
         in runProgram state { ip = ip + 2, output = val : output }
       5  -> -- Jump-if-true
         let (param1, param2) = (memory Map.! (ip+1), memory Map.! (ip+2))
             val1 = getParamValue memory param1 (modes !! 0)
             val2 = getParamValue memory param2 (modes !! 1)
         in if val1 /= 0
            then runProgram state { ip = val2 }
            else runProgram state { ip = ip + 3 }
       6  -> -- Jump-if-false
         let (param1, param2) = (memory Map.! (ip+1), memory Map.! (ip+2))
             val1 = getParamValue memory param1 (modes !! 0)
             val2 = getParamValue memory param2 (modes !! 1)
         in if val1 == 0
            then runProgram state { ip = val2 }
            else runProgram state { ip = ip + 3 }
       7  -> -- Less than
         let (param1, param2, dest) = (memory Map.! (ip+1), memory Map.! (ip+2), memory Map.! (ip+3))
             val1 = getParamValue memory param1 (modes !! 0)
             val2 = getParamValue memory param2 (modes !! 1)
             memory' = Map.insert dest (if val1 < val2 then 1 else 0) memory
         in runProgram state { memory = memory', ip = ip + 4 }
       8  -> -- Equals
         let (param1, param2, dest) = (memory Map.! (ip+1), memory Map.! (ip+2), memory Map.! (ip+3))
             val1 = getParamValue memory param1 (modes !! 0)
             val2 = getParamValue memory param2 (modes !! 1)
             memory' = Map.insert dest (if val1 == val2 then 1 else 0) memory
         in runProgram state { memory = memory', ip = ip + 4 }
       _  -> error ("Unknown opcode: " ++ show opcode)

parseInstruction :: Int -> (Int, [Int])
parseInstruction instr =
  let opcode = instr `mod` 100
      modes = instr `div` 100
      modeDigits = map (read . (:[])) $ reverse $ show modes
      modesList = modeDigits ++ repeat 0  -- Pad with zeros
  in (opcode, modesList)

getParamValue :: Memory -> Int -> Int -> Int
getParamValue memory param mode =
  case mode of
    0 -> Map.findWithDefault 0 param memory  -- Position mode
    1 -> param                               -- Immediate mode
    _ -> error ("Invalid parameter mode: " ++ show mode)
