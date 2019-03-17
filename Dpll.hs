module Dpll where
  data Literal = Pos Int | Neg Int deriving (Eq, Show)
  type Clause = [Literal]
  
  dpll :: [Clause] -> (Bool, [Literal])
  dpll [] = (True, [])
  dpll clauses = dpll' [] clauses 
  
  dpll' :: [Literal] -> [Clause] -> (Bool, [Literal])
  dpll' solution [] = (True, solution)
  dpll' solution clauses 
            | [] `elem` clauses = (False, solution)
            | containsAtomic clauses = dpll' (atom:solution) (reduceClauses atom clauses)
            | otherwise =      if (fst redPos) then redPos
                          else (if (fst redNeg) then redNeg
                          else (False, solution))
              where atom = head (getAtomicClauses clauses)
                    lit    = (head . head) clauses
                    notLit = negateLiteral lit
                    redPos :: (Bool, [Literal])
                    redPos = (dpll' (lit   :solution) (reduceClauses    lit  clauses)) 
                    redNeg = (dpll' (notLit:solution) (reduceClauses notLit  clauses))

  negateLiteral :: Literal -> Literal
  negateLiteral (Pos x) = Neg x
  negateLiteral (Neg x) = Pos x
  
  removeLiteral :: Literal -> [Literal] -> [Literal]
  removeLiteral _ [] = []
  removeLiteral l (x:xs)
    | x == l    =     removeLiteral l xs
    | otherwise = x : removeLiteral l xs
  
  getAtomicClauses :: [Clause] -> [Literal]
  getAtomicClauses clauses = [head clause | clause <- clauses, length clause == 1]
  
  containsAtomic :: [Clause] -> Bool
  containsAtomic = (\x -> 0 /= length x) . getAtomicClauses
  


  reduceClauses :: Literal -> [Clause] -> [Clause]
  reduceClauses l = (reduceUnSat l) . (reduceSat l)
   where reduceSat _ [] = []
         reduceSat l (x:xs) 
          | elem l x = reduceSat l xs
          | otherwise = x : reduceSat l xs
         reduceUnSat l = map (removeLiteral (negateLiteral l))
  
