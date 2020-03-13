module AuthLang where  

-- | Includes, and assertions
import Prelude
import System.IO
import Data.List

-- | Predefined users
type UserEnv listOfUsers = [User] 

-- | List of Types, assembled for the data below.
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


data Expr
  = Add         Expr Expr  
  | Sub         Expr Expr
  | Mul         Expr Expr
  | If          Expr Expr Expr
  | Begin       [Expr]
  | Tuple       Expr Expr 
  | Lit         Int
  | Text        String
  | B           AuthBool
  | Error
  deriving (Eq,Show)

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

















