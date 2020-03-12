module AuthLang where  -- | Our project's custom language module here, and where below.

-- | Includes, and assertions
import Prelude
import System.IO
import Data.List

-- | This is  where we will look up, and add a user when creating a user
type UserEnv listOfUsers = [User] --- | [AuthBool , User]

-- | List of Types, assembled for the data below.
type Name         = String
type Password     = String
type Lit          = Int
type LoggedIn     = Bool

-- | Data for the User, and Permissions for the users. Data for AuthBool as well for access.
data User         =  Info (Name, Password, Permission, LoggedIn)
 deriving (Eq, Show)

data Permission   = Admin | Regular | Banned
 deriving (Eq, Show)

data AuthBool     = Granted | Denied
  deriving (Eq,Show)

-- | Start of 1st Static tuple Examples, for testing.
connor::User
connor            = Info ("Connor G", "Hunter2", Admin, False)

bob::User
bob               = Info ("Bob Smith", "Hunter1", Regular, False)

tim::User
tim               = Info ("Tim Timmerson", "password", Banned, False)
-- | End of 1st Static Examples, for testing.

-- | Dynamic, allow[Let] a user at run time to create a user, add the user to the list, and more.

-- | Our list of Users, "CreateUser" will create a user to create, and add an user to this list. To use accordingly.
listOfUsers = [(connor), (bob), (tim)] -- | A.H. Comment, should this be a list of tuples? for example, [(user1,AuthBool),(user1,AuthBool)]?
-- after the user defines the type they are the bool access will always need to be checked before any other opperations.

-- | Functions, "GET *This" functions
-- | Get User's Name
getName :: User -> Name
getName (Info(name, _,_,_)) = name

-- | Get User's Password
getPass :: User -> Password
getPass (Info(_,pass,_,_)) = pass

-- | Get User's Permission
getPerm :: User -> Permission
getPerm (Info(_,_, perm,_)) = perm

-- | Functions, "SET *This" functions -- maybe used in the "CreateUser" function
-- | Set User's Name
{-
setName :: Name -> User  -> User
setName name (u)= u(Info(name, _,_,_))

-- | Set User's Password
setPass :: Password -> User  -> User
setPass pass (u)= u(Info(_,pass,_,_))

-- | Set User's Permission
setPerm :: Permission -> User -> User
setPerm perm (u) = u(Info(_,_, perm,_))
-}

-- | Add a user to the global list, "listOfUsers"
addUser :: User -> (UserEnv listOfUsers) -> (UserEnv listOfUsers)
addUser u (listOfUsers) = listOfUsers ++ [u]

-- Adding Variables example
varEx1 = listOfUsers
varEx2 = addUser (Info("Test User", "Password123",Admin, False)) listOfUsers

-- Refering to Variables
-- Give the name return the User's information
getUser :: String -> (UserEnv listOfUsers) -> User
getUser name(Info(n,p,perm,logged):xs) = if name == n then (Info(n,p,perm,logged)) else getUser name xs

-- Referencing Variables:
refEx1 = getUser "Connor G" listOfUsers
refEx2 = getUser "Test User" varEx2
refEx3 = getPass (getUser "Connor G" listOfUsers)

-- let x = 2+3 in (let y = x+4 in x+y)  ==>  14
ex69 = Let "x" (Add (Lit 2) (Lit 3))
              (Let "x" (Add (Ref "x") (Lit 4))
                       (Add (Ref "x") (Ref "x")))

-- * Environments
-- probably will be a list of actions or users???? something like that.

type Env = String -> Maybe Int

empty :: Env
empty = \_ -> Nothing

get :: String -> Env -> Maybe Int
get x m = m x

set :: String -> Int -> Env -> Env
set x i m = \y -> if y == x then Just i else m y

exEnv :: Env
exEnv = (set "a" 3 . set "b" 4 . set "c" 5) empty

-- * Denotational semantics
sem :: Expr -> Env -> Maybe Int
sem (Lit i)     m = Just i
sem (Add l r)   m = case (sem l m, sem r m) of
                      (Just i, Just j) -> Just (i+j)
                      _ -> Nothing
sem (Let x b e) m = case sem b m of
                      Just i -> sem e (set x i m)
                      Nothing -> Nothing
sem (Ref x)     m = get x m

-- This function takes in runtime user input, creates a new user, and adds them to the list.
-- CreateUser :: Name -> Password -> Permission -> User
-- CreateUser n p per u = u(n,p,per)

-- CreateUser, should be chained with add for createAndAdd  or CreateUser(addUser())

-- Login function to tell if a guest is a user,
-- this will be called after a "CreateUser" and "addUser"
login :: User -> Password -> Expr
login user enteredPass = if getPass user == enteredPass
                         then B Granted
                         else B Denied
						
-- | Start of 2nd Static Examples, for testing.
ex1 = login connor "Hunter2"
ex2 = login connor "ASDFASDf"

ex3 = login tim "password"
ex4 = login tim "asdfasdf"
-- | End of 2nd Static Examples, for testing.

-- 3/9/2020 - office hours notes.
-- createUser Jeff, in login user jeff in do stuff
-- param.hs  in psem, and dsem, Instead it will be for a user and have two strings "Create-User string passString Exp Exp"
-- write loops, "let loop = ..." // lambdaCalc. "Let" defines a function that is or can be a loop. "while loop" syntax-sugar with Let!
-- write loops, "let loop = ..." // lambdaCalc. "Let" defines a function that is or can be a loop. "while loop" syntax-sugar with Let!

data Expr
  = Add         Expr Expr   -- x - Should this be changed? -- No, I believe this should not change, if we want to compute expressions dynamically
  | Sub         Expr Expr
  | Mul         Expr Expr
  | If          Expr Expr Expr  -- ??
  | Get
  | Set         Expr  -- x - This should be compartmentalized, and with two arguments atleast.
  | Let         String Expr Expr
  | Ref         String    -- This is for variable binding, and allowing our users to create a function -- Changed "def" to "let"  ??????????
  | Func        Lit Expr      -- This anonymous function with arguments "Lit" and "Expr"  ??????????
  | App         Expr Expr      -- This is the function application
  | While       Test Expr
  | Begin       [Expr]
  | Tuple       Expr Expr  -- x - This is the tuple implementation for AuthLang, even though the users are defined as a Tuple above(ex. line 30) in Haskell
  | Lit         Int
  | Text        String
  | B           AuthBool
  | Error
  deriving (Eq,Show)

-- | Swapped oderd
-- | CreateUser  Name Password Permission
-- | Login       User Password

-- | Below is redundant with the type "Lit", It maybe separated if it is of syntaxical/semantical importance
type Reg = Int
-- | Below the "Reg" will be replaced with "Lit"  --3/9/2020 - Is this too confusing for our program understanding?

p :: Expr
p = Begin
      [ Set (Lit 1)
      , While (LT_ Get (Lit 5))
          (Set (Add Get (Lit 1)))
      ]

-- | Valuation function for statements. --Changed REG to lit
stmt :: Expr -> Reg -> Int
stmt (Set e) r = expr e r
stmt (Sub l r) r1 = expr l r1 - expr r r1
stmt (While c b)  r = if test c r  then stmt (While c b) (stmt b r) else r
stmt (Begin ss)  r = stmts ss r
  where
    stmts []     r = r
    stmts (s:ss)  r= stmts ss (stmt s r)

-- Should Core features all be in Expr

-- IfStmt :: Expr -- -- -> -- Nothing ???
-- If ( (B Granted) tc fc) = tc


-- sem :: Expr -> Value
-- sem (Add (Lit x) (Lit y)) = (Lit (x + y))
-- sem (Add (_) (_)) = Error

-- sem Login (Info (name,password) permlevel) givenPassword  = Error
expr :: Expr -> Reg -> Int
expr (Get)     s = s
expr (Lit i)   s = i
expr (Add l r) s = expr l s  + expr r s

data Test
   = LTE_ Expr Expr
   | LT_ Expr Expr
   | GT_ Expr Expr
   | GTE_ Expr Expr
  deriving (Eq,Show)

test :: Test -> Reg -> Bool
test (LTE_ l r) s = expr l s <= expr r s
test (LT_ l r) s = expr l  s < expr r s
test (GT_ l r) s = expr l s > expr r s
test (GTE_ l r) s = expr l s >= expr r s

add :: Expr -> Expr
add (Add (Lit x) (Lit y)) = Lit (x + y)
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

ifStmt :: Expr -> Expr
ifStmt (If (B Granted) (y) (z))  = y
ifStmt (If (B Denied) (y) (z))  = z
ifStmt (If (_) (y) (z))  = Error
ifStmt _ = Error

ifEx1 = ifStmt (If(B Granted) (Text "This should print") (Text "this should not print"))
ifEx2 = ifStmt (If(B Denied)  (Text "This shouldn't print") (Text "this FALSE text should  print"))
ifEx3 = ifStmt (If(B Granted) (add (Add(Lit 5)(Lit 10))) (Text "This shouldn't print"))  -- Lit 15 should display
ifEx4 = ifStmt (If(B Denied)  (Text "This shouldn't print") (sub (Sub(Lit 100)(Lit 100))))  -- Lit 0 should display
ifExErr = ifStmt (If(Text "this isn't a bool") (Text "QWERTY") (Text "ASDFG"))

-- Tuple Creation
tuple :: Expr -> (Expr, Expr)
tuple (Tuple x y) = (x,y)
tupple (_) = Error

tupEx1 = tuple(Tuple (Text "a") (Lit 7))

-- Tuple Operations (Invertibility)
getFirstVal :: (Expr, Expr) -> Expr
getFirstVal    (x,y) = x

getSecondVal :: (Expr, Expr) -> Expr
getSecondVal    (x,y) = y

invertEx1 = getFirstVal tupEx1
invertEx2 = getSecondVal tupEx1

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

incEx1 = (inc (Lit 5))
decEx1 = (dec (Lit 70))



-- Boolean Operations
and' :: Expr -> Expr -> Expr
and' ( B Granted) (B Granted) = (B Granted)
and' (B _ )        (B _)      = (B Denied)
and'   _             _        = Error

andEx1 = and' (B Granted) (B Granted)
andEx2 = and' (B Granted) (B Denied)
andEx3 = and' (Text "test") (B Granted)



or' :: Expr -> Expr -> Expr
or' (B Granted) (B _) = (B Granted)
or' (B _) (B Granted) = (B Granted)
or' (B _)    (B _)    = (B Denied )    
or' _ _               = Error

orEx1 = or' (B Granted) (B Denied)
orEx2 = or' (B Granted) (B Granted)
orEx3 = or' (B Granted) (Text "asdf")

not' :: Expr -> Expr 
not' (B Granted) = (B Denied)
not' (B Denied)  = (B Granted)
not' _           = Error

notEx1 = not' (B Granted)
notEx2 = not' (B Denied)
notEx3 = not' (Lit 7)



-- type funcEnv m = [ [Expr] ] 
-- m = [ [] ] 
 -- defFunc :: String -> [Expr]






