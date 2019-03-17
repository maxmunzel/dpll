module DpllCli where
  import Parser
  import IntDpll
  import System.Environment (getArgs)
  import System.IO
  
  main = do
    args <- getArgs
    if length args == 0
    then do
      putStrLn "Usage: dpll [filename]"
      return 1
    else do
      file <- openFile (head args) ReadMode
      contents <- hGetContents file
      let problem = parseDpll contents
      do 
        let Just clauses = problem
        let (satisfiable, solution) = dpll clauses
        if satisfiable
        then do 
          putStrLn "Satisfiable:"
          putStrLn (show solution)
          return 0
        else do
          putStrLn "Unsatisfiable"
          return 2

  
