module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend st key value input
  | input == key = value
  | otherwise = st input

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE state exp = case exp of
  Var var -> state var
  Val i -> i
  Op e0 bop e1 -> bopf (evalE state e0) (evalE state e1)
    where
      bopf = case bop of
        Plus -> (+)
        Minus -> (-)
        Times -> (*)
        Divide -> div
        Gt -> b2i (>)
        Ge -> b2i (>=)
        Lt -> b2i (<)
        Le -> b2i (<=)
        Eql -> b2i (==)
      where
        b2i f x y
          | (f x y) == True = 1
          | otherwise = 0

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar statement = case statement of
  Incr string -> DAssign string (Op (Var string) Plus (Val 1))
  For init condition update stmt -> DSequence (desugar init) (DWhile condition (DSequence (desugar stmt) (desugar update)))
  Assign string exp -> DAssign string exp
  If condition st0 st1 -> DIf condition (desugar st0) (desugar st1)
  While exp stmt -> DWhile exp (desugar stmt)
  Sequence st0 st1 -> DSequence (desugar st0) (desugar st1)
  Skip -> DSkip


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state stmt = case stmt of
  DAssign string exp -> extend state string (evalE state exp)
  DIf cond stmt0 stmt1 -> if (evalE state cond) > 0 then (evalSimple state stmt0) else (evalSimple state stmt1)
  DWhile exp stmt0 -> if (evalE state exp) > 0 then (evalSimple (evalSimple state stmt0) stmt) else state
  DSequence stmt0 stmt1 -> evalSimple (evalSimple state stmt0) stmt1
  DSkip -> state

run :: State -> Statement -> State
run state stmt = evalSimple state (desugar stmt)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
