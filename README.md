# assemblyRunnerHaskell
An attempt at writing a basic assembly interpreter in Haskell. The dialect is based on the language from the ['TIS 100'](http://www.zachtronics.com/tis-100/) Zachtronics in-game language. I implemented this in phthon in a separate github project ['assemblyRunner'](https://github.com/benWindsorCode/assemblyRunner) where there is more information on the language itself.

# Running
After installing the haskell stack, run 'cabal v2-run' inside the project directory.
Programs can be run by altering the program variable in the Main function of the Main.hs file.

# Example programs
You have access to IN, ACC, DAT, OUT, BAK registers and MOV, ADD, SUB commands.
```
MOV IN ACC
ADD 5
MOV ACC DAT
```
This will take the value in the IN register, move it into the ACC register, add 5 to this register then save the restult to the DAT register.

# Code commentary
I first define data types to represent a value, register and command. Then provided a new line separated file, I split it into lines and process each line into a command, this happens using the 'parseFile', 'parseLine' and 'parseWords' functions mapped and composed together. 

Given this list of Commands, the program then runs by taking an initial register, and running the commands one by one on this register, feeding the output to each subsequent command. This continues until there are no commands left (unless there is an infinite loop of course). The 'executeCommand' function is the most important in this regard, as that is the function that takes a Register and a single Command and works out what the next state should be.

One problem I had was accessing the members in the Register nicely. If I was programming in Java I would use reflection to go from a string to the value of a member variable of a class, however I couldn't figure out a way to do this in Haskell so resulted to case matching which seems like an inefficient approach.

# Languate features
For details on the assembly like language implemented see the sister project in python ['assemblyRunner'](https://github.com/benWindsorCode/assemblyRunner).

# Missing features/todo
I have yet to implement the JMP, JEZ, JLZ and label functionality of the language.
I have yet to implement th SWP and SAV functionality of the language where you can save to the BAK register and swap it into use when needed.
