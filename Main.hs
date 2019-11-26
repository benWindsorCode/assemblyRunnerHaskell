module Main where

data Register = Register {
    acc :: Int,
    bak :: Int,
    dat :: Int,
    input :: Int,
    out :: Int
} deriving (Show)

type Label = String 
type Instruction = String
type Source = String
type Destination = String
data Command = Label Label| TwoParam Instruction Source Destination | OneParam Instruction Source
-- define function that takes instruction and current state and returns next state

-- Split line into words then pass into parseWords
-- parseLine :: String -> Command
-- parseLine = "Not implemented"

parseWords :: [String] -> Command
parseWords (a:b:[]) = OneParam a b
parseWords (a:b:c:[]) = TwoParam a b c


executeCommand :: Register -> Command -> Register
executeCommand reg (Label label) = reg
executeCommand reg (TwoParam instruction source destination) = mov reg source destination
executeCommand reg (OneParam instruction source) 
    | instruction == "ADD" = add reg (read source :: Int)

add :: Register -> Int -> Register
add (Register acc bak dat input out) val = Register (acc + val) bak dat input out

accessRegister :: Register -> String -> Int
accessRegister (Register acc bak dat input out) "ACC" = acc
accessRegister (Register acc bak dat input out) "BAK" = bak
accessRegister (Register acc bak dat input out) "DAT" = dat
accessRegister (Register acc bak dat input out) "INPUT" = input
accessRegister (Register acc bak dat input out) "OUT" = out

mov :: Register -> String -> String -> Register
mov (Register acc bak dat input out) "IN" "DAT" = Register acc bak input input out

main :: IO ()
main = do
    putStrLn $ show $ executeCommand reg (OneParam "ADD" "5")
    putStrLn $ show $ add reg 5
    where 
        reg = Register 0 0 0 0 0
