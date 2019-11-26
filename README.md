# assemblyRunnerHaskell
An attempt at writing a basic assembly interpreter in Haskell. The dialect is based on the language from the ['TIS 100'](http://www.zachtronics.com/tis-100/) Zachtronics in-game language. I implemented this in phthon in a separate github project ['assemblyRunner'](https://github.com/benWindsorCode/assemblyRunner) where there is more information on the language itself.

# Running
After installing the haskell stack, run 'cabal v2-run' inside the project directory.

# Test programs
Programs can be run by altering the program variable in the Main function of the Main.hs file.

# Languate features
For details on the assembly like language implemented see the sister project in python ['assemblyRunner'](https://github.com/benWindsorCode/assemblyRunner).

# Missing features/todo
I have yet to implement the JMP, JEZ, JLZ and label functionality of the language.
