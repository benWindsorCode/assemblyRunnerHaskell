module Main where

data Register = Register {
    acc :: Int,
    bak :: Int,
    dat :: Int,
    input :: Int,
    out :: Int
} deriving (Show)

-- define data type to represent the registers
-- define function that takes instruction and current state and returns next state

executeCommand :: Register -> String -> Register
executeCommand reg command = reg

main :: IO ()
main = do
    putStrLn $ show $ executeCommand reg "ga"
    where 
        reg = Register 1 1 1 1 1
