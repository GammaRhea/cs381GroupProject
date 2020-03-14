-- | AuthLang: a imperative programming language
module AuthLang where  

-- | Includes, and assertions
import Prelude hiding (lookup) -- Removed ambi. Def.
import System.IO
--import Data.List     -- Removed ambi. Def.

import Data.Map (Map,fromList,lookup,insert)

-- | Predefined users
type UserEnv listOfUsers = [User] 

-- | Variables.
type Var = String

-- | List of Types, asembled for the data below.
type Name         = String
type Password     = String
type Lit          = Int
type LoggedIn     = Bool

-- | Data for the User, and Permissions for the users. Data for AuthBool as well for access.
data User         =  Info (Name, Password)
 deriving (Eq, Show)

data Permission   = Admin | Regular | Banned
 deriving (Eq, Show)

data AuthBool     = Granted | Denied
  deriving (Eq,Show)

-- | Start of 1st Static tuple Examples, for testing.
connor::User
connor            = Info ("Connor G", "Hunter2")

bob::User
bob               = Info ("Bob Smith", "Hunter1")

tim::User
tim               = Info ("Tim Timmerson", "password")

listOfUsers = [(connor), (bob), (tim)] 

getName :: User -> Name
getName (Info(name, _)) = name

getPass :: User -> Password
getPass (Info(_,pass)) = pass

getNameEx = getName connor
getPassEx = getPass bob

addUser :: User -> (UserEnv listOfUsers) -> (UserEnv listOfUsers)
addUser u (listOfUsers) = listOfUsers ++ [u]


-- Refering to Variables
-- Give the name return the User's information
getUser :: String -> (UserEnv listOfUsers) -> User
getUser name(Info(n,p):xs) = if name == n then (Info(n,p)) else getUser name xs


{-
data Types 
  = Litx Int
  | Textx String   -- 
  | Bx AuthBool
  | Errorx
  deriving (Eq,Show)
-}

-- | Abstract syntax of Expressions(Expr).
data Expr
  = Not         Expr
  | Add         Expr Expr  
  | Sub         Expr Expr
  | Mul         Expr Expr
  | If          Expr Expr Expr
  | Begin       [Expr]
  | Tuple       Expr Expr 
 | Lit         Int           -- Types
 | Text        String        -- Types
 | B           AuthBool      -- Types
 | Error
  | Ref Var     -- Variable reference for AuthLang
-- | Let Var Expr Expr   -- For Functional lambda calc. *Old  core-feature -prototype
-- | App Expr Expr   -- -- For Functional lambda calc. *Old  core-feature -prototype
-- | Fun Var Expr    -- -- For Functional lambda calc. *Old  core-feature -prototype
  | LTE_ Expr Expr  -- Feature
  | LT_ Expr Expr  
  | GT_ Expr Expr
  | GTE_ Expr Expr
 deriving (Eq,Show)

-- | Abstract syntax of statements(Stmt).
data Stmt = Bind Var Expr
          | Ifx Expr Stmt Stmt
          | While Expr Stmt
          | Block [Stmt]
  deriving (Eq,Show)
  
data Types = TyInt | TyBool
  deriving (Eq,Show)

type Def = (Var,Expr) --Should be Types

data Prog = P [Def]Stmt
  deriving(Eq,Show)
  
type Def2 = (Var,Types) --Should be Types

data Prog2 = P2 [Def2]Stmt
  deriving(Eq,Show)  
  
  
  
{-
type Stack  = [Type]
type Prog = [Cmd]
-}

-- | Functions 
login :: User -> Password -> Expr

login (Info (a, pass)) enteredPass = if getPass (Info (a,pass)) == enteredPass
                                    then (B Granted)
                                    else (B Denied)

ifStmt :: Expr -> Expr
ifStmt (If (B Granted) (y) (z))  = y
ifStmt (If (B Denied) (y) (z))  = z
ifStmt (If (_) (y) (z))  = Error
ifStmt _ = Error

add :: Expr -> Expr
add (Add (Lit x) (Lit y)) = Lit (x + y)
--add (Add (Tuple x1 y1) (Tuple x2 y2)) = Tuple (add(x1)( x2)) (y1 + y2)
add (Add (_) (_)) = Error
add _ = Error


sub :: Expr -> Expr
sub (Sub (Lit x) (Lit y)) = Lit (x - y)
sub (Sub (_) (_)) = Error
sub _ = Error

mul :: Expr -> Expr
mul (Mul (Lit x) (Lit y)) = Lit (x * y)
mul (Mul (_) (_)) = Error
mul _ = Error

-- Tuple Creation
tuple :: Expr -> (Expr, Expr)
tuple (Tuple x y) = (x,y)
tupple (_) = Error

-- Tuple Operations (Invertibility)
getFirstVal :: (Expr, Expr) -> Expr
getFirstVal    (x,y) = x

getSecondVal :: (Expr, Expr) -> Expr
getSecondVal    (x,y) = y

-- List Operations
append :: Int -> [Int] -> [Int]
append i [] = [i]
append i (x:xs) = (x:xs) ++ [i]

prepend :: Int -> [Int] -> [Int]
prepend i [] = [i]
prepend i (x:xs) = [i] ++ (x:xs)

-- Adds a constant value to every number in a list
addToAll :: Int -> [Int] -> [Int]
addToAll i [] = []
addToAll i (x:xs) = [(x+i)] ++ addToAll i xs

-- Add two lists together
addLists :: [Int] -> [Int] -> [Int]
addLists [] [] = []
addLists [] (x:xs) = (x:xs)
addLists (x:xs) (y:ys) = [x+y] ++ addLists xs ys

-- Syntactic Sugar!
inc :: Expr -> Expr
inc (Lit x) = add(Add (Lit x) (Lit 1))
inc _ = Error

dec :: Expr -> Expr
dec (Lit x) = sub(Sub (Lit x) (Lit 1))
dec _ = Error

-- Boolean Operations
and' :: Expr -> Expr -> Expr
and' ( B Granted) (B Granted) = (B Granted)
and' (B _ )        (B _)      = (B Denied)
and'   _             _        = Error

or' :: Expr -> Expr -> Expr
or' (B Granted) (B _) = (B Granted)
or' (B _) (B Granted) = (B Granted)
or' (B _)    (B _)    = (B Denied )
or' _ _               = Error

not' :: Expr -> Expr
not' (B Granted) = (B Denied)
not' (B Denied)  = (B Granted)
not' _           = Error




-- EXAMPLES: Operations
addEx = add(Add(Lit 7) ((Lit 8)))
addEx2 = add(Add(addEx) ((Lit 8)))
incEx1 = (inc (Lit 5))
decEx1 = (dec (Lit 70))

-- If Examples: 
ifEx1 = ifStmt (If(B Granted) (Text "This should print") (Text "this should not print"))
ifEx2 = ifStmt (If(B Denied)  (Text "This shouldn't print") (Text "this FALSE text should  print"))
ifEx3 = ifStmt (If(B Granted) (add (Add(Lit 5)(Lit 10))) (Text "This shouldn't print"))
ifEx4 = ifStmt (If(B Denied)  (Text "This shouldn't print") (sub (Sub(Lit 100)(Lit 100)))) 
ifExErr = ifStmt (If(Text "this isn't a bool") (Text "QWERTY") (Text "ASDFG"))

-- Boolean Operations
andEx1 = and' (B Granted) (B Granted)
andEx2 = and' (B Granted) (B Denied)
andEx3 = and' (Text "test") (B Granted)

orEx1 = or' (B Granted) (B Denied)
orEx2 = or' (B Granted) (B Granted)
orEx3 = or' (B Granted) (Text "asdf")

notEx1 = not' (B Granted)
notEx2 = not' (B Denied)
notEx3 = not' (Lit 7)

-- Lists + Operations:
appendEx = append 7 [1,2,3,4]
prependEx = prepend 100 [10,20,30,90]
addToAllEx = addToAll 5 [1,2,3,4,5]
addListsEx = addLists [1,2,3] [10,11,12]

-- Tuples + Operations
tupEx1 = tuple(Tuple (Text "a") (Lit 7))
invertEx1 = getFirstVal tupEx1
invertEx2 = getSecondVal tupEx1

-- defining Variables (Creating new users):
varEx1 = listOfUsers
varEx2 = addUser (Info("Test User", "Password123")) listOfUsers

-- Referencing Variables:
refEx1 = getUser "Connor G" listOfUsers
refEx2 = getUser "Test User" varEx2
refEx3 = getPass (getUser "Connor G" listOfUsers)

-- Login example:
loginEx1 = login connor "Hunter2"
loginEx2 = login connor "ASDFASDf"

loginEx3 = login tim "password"
loginEx4 = login tim "asdfasdf"


--Login chained with other operations:
loginIfEx1 = (ifStmt (If(login connor "Hunter2")(Text "You are logged in")(Text "You are not logged in :(")))
loginIfEx2 = (ifStmt (If(login connor "Hunter2")(add (Add (Lit 5)(Lit 7)))(Text "You are not logged in :(")))


-- | Type System

type Env a = Map Var a

-- | Type relations for AuthLang expressions.
typeExpr :: Expr -> Env Types -> Maybe Types
typeExpr = undefined


typeStmt :: Stmt -> Env Types -> Bool
typeStmt (Bind v e)   m = case (lookup v m, typeExpr e m) of
                            (Just tv, Just te) -> tv == te
--                            _ -> False
-- typeStmt (If c st se) m = case typeExpr c m of
--                             Just TyBool -> typeStmt st m && typeStmt se m
                            _ -> False
typeStmt (While c sb) m = case typeExpr c m of
                            Just TyBool -> typeStmt sb m
                            _ -> False
typeStmt (Block ss)   m = all (\s -> typeStmt s m) ss


-- Type checking with "fromList" function
-- Used with Data.Map module building a map from a list of pairs.
-- Initializing our typing environment
typeProg :: Prog -> Bool
typeProg = undefined
-- typeProg (P ds s ) = typeStmt s (fromList ds)   -- need typstmt function

-- Semantics
type Val = Either Int Bool

evalExpr :: Expr -> Env Val -> Val
evalExpr (Lit i)   _ = Left i
evalExpr (Add l r) m = Left (evalInt l m + evalInt r m)
evalExpr (LTE_ l r) m = Right (evalInt l m <= evalInt r m)
evalExpr (Not e)   m = Right (not (evalBool e m))
evalExpr (Ref x)   m = case lookup x m of
                         Just v  -> v
                         Nothing -> error "internal error: undefined variable"

evalInt :: Expr -> Env Val -> Int
evalInt e m = case evalExpr e m of
                Left i  -> i
                Right _ -> error "internal error: expected Int got Bool"

evalBool :: Expr -> Env Val -> Bool
evalBool e m = case evalExpr e m of
                 Right b -> b
                 Left _  -> error "internal error: expected Bool got Int"


evalStmt :: Stmt -> Env Val -> Env Val
evalStmt (Bind x e)   m = insert x (evalExpr e m) m
-- evalStmt (If c st se) m = if evalBool c m
--                           then evalStmt st m
--                           else evalStmt se m
evalStmt (While c sb) m = if evalBool c m
                          then evalStmt (While c sb) (evalStmt sb m)
                          else m
--evalStmt (Block ss)   m = evalStmts ss m


-- Semantics of programs. Runs a program with an initial env.
-- 
evalProg :: Prog -> Env Val
evalProg = undefined
{- Expr with Type mismatch error
evalProg (P ds s) = evalStmt s m
  where
    m = fromList (map (\(x,t) -> (x, init t)) ds)
    init TyInt  = Left 0
    init TyBool = Right False
-}

-- | Type check and then run a program
liveProg :: Prog -> Maybe (Env Val)
liveProg p = if typeProg p then Just (evalProg p)
                       else Nothing







{-
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen m)(_,p) = ((m,p),Nothing)
cmd (Move x y)(Up, p) =  ((Up,(x,y)), Nothing)
cmd (Move x y)(Down,p) = ((Down,(x,y)),Just ( p,(x,y))) 
--cmd [] x = (x, [])
-}

{-
prog :: Prog -> State -> (State, [Line])
prog [] s = (s , [])
prog (p:ps) s = case (cmd p s) of
                (st,Nothing) -> prog ps st 
                (st,Just li) -> case prog ps st of
                                (sta, lis) -> (sta, li:lis) 

-}

















