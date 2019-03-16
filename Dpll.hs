module Dpll where
  data Literal = Pos Int | Neg Int deriving (Eq, Show)
  type Clause = [Literal]
  
  dpll :: [Clause] -> Bool
  dpll [] = True
  dpll clauses 
    | containsEmptyClause clauses = False
    | containsAtomic clauses= dpll (reduceClauses atom clauses)
    | otherwise = (dpll (reduceClauses first clauses)) || 
                  (dpll (reduceClauses firstNeg clauses))
                  
      where atom = head (getAtomicClauses clauses)
            first = (head . head) clauses
            firstNeg = negateLiteral first

  negateLiteral :: Literal -> Literal
  negateLiteral (Pos x) = Neg x
  negateLiteral (Neg x) = Pos x
  
  removeLiteral :: Literal -> [Literal] -> [Literal]
  removeLiteral _ [] = []
  removeLiteral l (x:xs)
    | x == l    =     removeLiteral l xs
    | otherwise = x : removeLiteral l xs
      
  containsEmptyClause :: [Clause] -> Bool
  containsEmptyClause [] = False
  containsEmptyClause (x:xs) = (length x == 0) || containsEmptyClause xs
  
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