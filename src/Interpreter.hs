import System.IO
import System.Exit
import System.Environment

import Control.Monad.State

import ParGrammar
import PrintGrammar
import ErrM
import AbsGrammar

data SVar = VInt Int | VBool Bool | VString String | VVoid
type Loc = Int
type Fun = ([SVar] -> SVar)

type VEnv = [(String, Loc)]
type Store = [(Loc, String)]


data ExecState = ExecState {
    vEnv :: VEnv,
    store :: Store,
    output :: String
}

type Interp a = (StateT ExecState Err) a

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
    Ok s  -> putStrLn s
    Bad s -> do
        putStrLn $ "Error: " ++ s
        exitWith $ ExitFailure 1

tryExecute :: String -> Err String
tryExecute s = do
    ast <- parse s
    runInterp $ interpretProgram ast

parse :: String -> Err Program
parse str = pProgram $ myLexer str


-- Interpreting top-level structures

interpretProgram :: Program -> Interp ()
interpretProgram (Program topDefs) = forM_ topDefs interpretTopDef

interpretTopDef :: TopDef -> Interp ()
interpretTopDef (TypeFnDef funType (Ident "main") args (Block cmds)) = forM_ cmds interpretStmt
interpretTopDef (TypeFnDef funType (Ident funName) args (Block cmds)) = return () --TODO


-- Interpreting statements

interpretStmt :: Stmt -> Interp ()
interpretStmt (BStmt (Block cmds)) = forM_ cmds interpretStmt
interpretStmt (ValStmt val) = interpretVal val >> return ()


-- Interpreting values

interpretVal :: Val -> Interp SVar
interpretVal (EApp (Ident "print") args) = case args of
    (arg:[]) -> do
        printToOutput "print"
        return VVoid
    _ -> reportError "print() call should have exactly one argument"

-- Helper functions

printToOutput :: String -> Interp ()
printToOutput str = modify $ \s -> s {output = (output s) ++ str ++ "\n"}

reportError :: String -> Interp a
reportError msg = StateT { runStateT = \s -> Bad msg }
    
runInterp :: Interp () -> Err String
runInterp m = fmap (output . snd) $ runStateT m emptyExecState

emptyExecState :: ExecState
emptyExecState = ExecState {vEnv = [], store = [], output = ""}

usage :: IO ()
usage = putStrLn "usage: ./interpreter program"
