import System.IO
import System.Environment

import Control.Monad.State

import ParGrammar
import PrintGrammar
import ErrM
import AbsGrammar

data SVar = VInt Int | VBool Bool | VString String
type Loc = Int
type Fun = ([SVar] -> SVar)

type VEnv = [(String, Loc)]
type Store = [(Loc, String)]


data ExecState = ExecState {
    vEnv :: VEnv,
    store :: Store,
    output :: String
}

type Interp = (StateT ExecState Err) ()

main :: IO ()
main = do
    args <- getArgs
    case args of
        [program] -> do
            content <- readFile program
            execute content
        _ -> usage

execute :: String -> IO ()
execute code = case tryExecute code of
    Ok s  -> do putStrLn s
    Bad s -> do putStrLn $ "Error: " ++ s

tryExecute :: String -> Err String
tryExecute s = do
    ast <- parse s
    runInterp $ interpretProgram ast

    

parse :: String -> Err Program
parse str = pProgram $ myLexer str

interpretProgram :: Program -> Interp
interpretProgram (Program topDefs) = forM_ topDefs interpretTopDef

interpretTopDef :: TopDef -> Interp
interpretTopDef f @ (TypeFnDef funType (Ident funName) args (Block cmds)) = if funName == "main" then
    do
        forM_ cmds interpretCmd
    else
        --TODO add to FEnv
        return ()

interpretCmd :: Cmd -> Interp
interpretCmd (BCmd (Block cmds)) = forM_ cmds interpretCmd
interpretCmd (ValCmd (EApp (Ident funName) (arg:[]))) = if funName == "print"
    then printToOutput "print"
    else return ()


printToOutput :: String -> Interp
printToOutput str = modify $ \s -> s {output = (output s) ++ str ++ "\n"}
    
runInterp :: Interp -> Err String
runInterp m = fmap (output . snd) $ runStateT m emptyExecState

emptyExecState :: ExecState
emptyExecState = ExecState {vEnv = [], store = [], output = ""}

usage :: IO ()
usage = putStrLn "usage: ./interpreter program"
