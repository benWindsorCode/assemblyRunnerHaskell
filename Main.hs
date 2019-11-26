module Main where

import Text.Read

data Register = Register {
    acc :: Int,
    bak :: Int,
    dat :: Int,
    input :: Int,
    out :: Int
} deriving (Show)

type Label = String 
type Instruction = String
type Destination = String

-- The source of a command can either be an integer or a register to pull a value out of
data Source = Value Int | SourceReg String deriving (Show)

-- A command is either a lable to jump to, a two parameter mov command, or a single param command like ADD, SUB, JEZ etc.
data Command = Label Label | TwoParam Instruction Source Destination | OneParam Instruction Source deriving (Show)

-- Entry point when running with 'cabal v2-run'
main :: IO ()
main = do
    putStrLn $ show $ (runProgram program reg)
    where 
        reg = Just (Register 0 0 0 3 0)
        program = "ADD IN\nADD 5\nMOV ACC DAT"
        
-- Given a loaded string of a program file, execute the commands and return the output
runProgram :: String -> Maybe Register -> Register
runProgram prog Nothing = executeCommands (Register 0 0 0 0 0) (parseFile prog)
runProgram prog (Just initialReg) = executeCommands (initialReg) (parseFile prog)

-- Apply a list of commands to an initial register, and return the resultant register
executeCommands :: Register -> [Command] -> Register
executeCommands reg (command:commands) = executeCommands (executeCommand reg command) commands
executeCommands reg [] = reg

-- Takes a newline separated file, and parses into one Command for each line in the file
parseFile :: String -> [Command]
parseFile input = map parseLine $ lines input

-- Split line into words then pass into parseWords
parseLine :: String -> Command
parseLine input = parseWords . words $ input

-- Given a line of the program that was split on " ", determine the format of the Command and the Source and parse as required
parseWords :: [String] -> Command
parseWords (instruction:source:[]) 
    | processedSource == Nothing = OneParam instruction (SourceReg source)
    | otherwise = OneParam instruction (Value (read source :: Int))
    where 
        processedSource = readMaybe source :: Maybe Int
parseWords (instruction:source:destination:[]) 
    | processedSource == Nothing = TwoParam instruction (SourceReg source) destination
    | otherwise = TwoParam instruction (Value (read source :: Int)) destination
    where 
        processedSource = readMaybe source :: Maybe Int
parseWords (label:[]) = Label label

-- Given a command return the state of the register after that command has been executed
executeCommand :: Register -> Command -> Register
executeCommand reg (Label label) = reg
executeCommand reg (TwoParam instruction source destination) = mov reg source destination
executeCommand reg (OneParam instruction source) 
    | instruction == "ADD" = add reg source
    | instruction == "SUB" = sub reg source

-- Implementation of the ADD command, returning the state of the register after the add has been executed
add :: Register -> Source -> Register
add (Register acc bak dat input out) (Value val) = Register (acc + val) bak dat input out
add reg@(Register acc bak dat input out) (SourceReg name) = Register (acc + (accessRegister reg name)) bak dat input out

-- Implementation of the SUB command, returning the state of the register after the subtraction has been executed
sub :: Register -> Source -> Register
sub (Register acc bak dat input out) (Value val) = Register (acc - val) bak dat input out
sub reg@(Register acc bak dat input out) (SourceReg name) = Register (acc - (accessRegister reg name)) bak dat input out

-- Function to access a single register given a string. Todo: is there a better way to do this?
accessRegister :: Register -> String -> Int
accessRegister (Register acc _ _ _ _) "ACC" = acc
accessRegister (Register _ bak _ _ _) "BAK" = bak
accessRegister (Register _ _ dat _ _) "DAT" = dat
accessRegister (Register _ _ _ input _) "IN" = input
accessRegister (Register _ _ _ _ out) "OUT" = out

-- Implementation of the MOV command, todo: is there a better way to do this?
mov :: Register -> Source -> String -> Register
mov (Register acc bak dat input out) (SourceReg reg) "DAT"
        | reg == "IN" = Register acc bak input input out
        | reg == "ACC" = Register acc bak acc input out
mov (Register acc bak dat input out) (SourceReg reg) "ACC"
        | reg == "DAT" = Register dat bak dat input out
mov (Register acc bak dat input out) (Value val) "ACC" = Register val bak dat input out