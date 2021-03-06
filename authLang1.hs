import Prelude
import System.IO
import Data.List

type Name   = String
type Lit    = Int

data User   = Info (Name, Permission)
 deriving (Show)

data AuthBool   = Granted | Denied
 deriving (Show)

data Permission = Admin | Regular | Banned
 deriving (Show)

connor::User
connor = Info ("Connor", Admin)

bob::User
bob = Info ("Bob Smith", Regular)

tim::User
tim = Info ("Tim Timmerson", Banned)

data Expr
  = Lit Int
  | Text String
  | B AuthBool
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | If Expr Expr Expr
  | Login User
  | While Expr Expr
  | Error
 deriving (Show)

data Var
 = UserVar String Name Permission
 | UserInt String Int

 -- Not complete: creates var(?) but can't be used.
defUser :: Var -> User
defUser (UserVar nameOfVar name perm) = let nameOfVar = Info (name, perm) in nameOfVar
-- defUser (UserVar nameOfVar name perm) = nameOfUser where nameOfUser = Info(name, perm)


sem:: Expr -> Expr
-- Math Expressions
sem (Add (Lit x) (Lit y)) = Lit (x + y)
sem (Sub (Lit x) (Lit y)) = Lit (x - y)
sem (Mul (Lit x) (Lit y)) = Lit (x * y)
sem (Add _ _) = Error
sem (Sub _ _)= Error
sem (Mul _ _) = Error


-- Login Expressions
sem (Login (Info (_, Admin))) = B Granted
sem (Login (Info (_, Regular))) = B Granted
sem (Login (Info (_, _))) =  B Denied

-- If statment Expressions
sem (If (B Granted) tc fc) = tc
sem (If (B Denied) tc fc) = fc
sem (If _ tc fc) = Error

-- While Expression
-- COME BACK TOO

listOfUsers::[User] 
listOfUsers = [connor, bob, tim]

addNewUser :: [User] -> User -> [User]
addNewUser [] newUser = addToList newUser 
addNewUser (x:xs) newUser = (addToList x) ++ addNewUser xs newUser

addToList :: User -> [User] 
addToList u = [u]

getName :: User -> Name
getName (Info (un,_)) = un

userExists :: Name -> [User] -> Maybe User
userExists un (x:xs) = if un == getName(x) then Just x
                       else userExists un xs
userExists un []     = Nothing

getPerm :: User -> Permission
getPerm (Info(_,perm)) = perm

login = do
      putStrLn "Enter your username: "
      line <- getLine
      case userExists line listOfUsers of
        Just u  -> putStrLn $ "Greetings " ++ getName u
        Nothing -> do putStrLn "Invalid Username"
                      login

--shows tuple of different users
ex1 = connor 
ex2 = tim

--Shows behavior of logging in with users of different permissions
ex3 = sem (Login connor)
ex4 = sem (Login tim)

-- Shows math operations +  error handling
ex5 = sem (Add (Lit 7) (Lit 8))
ex5a = sem (Add (Text "This throws an Error") (Lit 8)) -- Demonstrates Error

-- Shows If statement functionality 
ex6 =  sem (If (B Granted) (Lit 1) (Lit 0))
ex7 =  sem (If (sem (Login connor)) (B Granted) (B Denied) )
ex8 =  sem (If (sem (Login tim)) (B Granted) (B Denied) )
ex9 =  sem (If (sem (Login connor)) (sem (Add (Lit 5) (Lit 5))) (B Denied) )

-- Defines a userVariable; only defition functionality currently, Referencing to come
ex11 = defUser (UserVar "eric" "Eric Walkingshaw" Admin)

-- Performs the getName function, which returns the Name of a User
ex12 = getName (Info ("TestUser", Admin))

-- Checks to see if a user exists, and if they do, then itoutputs their user information
ex13 = userExists ("Connor") (listOfUsers)

--Failure case of userExists, this is what happens when you try and invalid username, returns Nothing
ex14 = userExists ("Conner") (listOfUsers)

-- Performs the getPerm function, which returns the Permissions of a User
ex15 = getPerm connor
ex16 =  getPerm bob

ex100 = listOfUsers -- Prints current Users
ex110 = addNewUser newListOfUsers (Info ("First Last", Admin)) -- Prints list with a new run time created user
newListOfUsers = addNewUser listOfUsers (Info ("John Doe",Banned)) 











