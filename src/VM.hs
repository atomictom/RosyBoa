module VM where

type Stack = [Int]
type Registers = [Int]

data Instruction = LIT Int Int
                 | LOD Int Int
                 | STO Int Int
                 | ADD Int Int
                 | SUB Int Int
                 | MUL Int Int
                 | DIV Int Int
                 | CMP Int Int
                 | JMP Int
                 | JZE Int
                 | JLE Int
                 | JLT Int
                 | SYS Int Int
                 deriving (Show)

type Program = [Instruction]

data VM = VM { getProgram :: Program
             , getStack :: Stack
             , getRegisters :: Registers }

-- Instruction Pointer
IP :: Int
IP = 0

newVM :: Program -> VM
newVM p = VM p [] (replicate 32 0)

execVM :: Program -> IO ()
execVM program = do
    let vm = newVM program
        codeLength = length program

exec :: (MonadState m, MonadIO m) => Instruction -> m
exec
