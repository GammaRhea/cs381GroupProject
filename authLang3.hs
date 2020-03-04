module AuthLang where

import Prelude
import System.IO
import Data.List

type UserEnv listOfUsers = [User]

type Name         = String
type Password     = String
type Lit          = Int
type LoggedIn     = Bool

data User         =  Info (Name, Password, Permission, LoggedIn)
 deriving (Eq, Show)

data AuthBool     = Granted | Denied
 deriving (Show)

data Permission   = Admin | Regular | Banned
 deriving (Eq, Show)

connor::User
connor            = Info ("Connor G", "Hunter2", Admin, False)

bob::User
bob               = Info ("Bob Smith", "Hunter1", Regular, False)

tim::User
tim               = Info ("Tim Timmerson", "password", Banned, False)

listOfUsers = [(connor), (bob), (tim)]

getName :: User -> Name
getName (Info(name, _,_,_)) = name

getPass :: User -> Password
getPass (Info(_,pass,_,_)) = pass

getPerm :: User -> Permission
getPerm (Info(_,_, perm,_)) = perm


login :: User -> Password -> String
login user enteredPass = if getPass user == enteredPass
						then "You are logged in"
						else "Incorrect Password"
						
ex1 = login connor "Hunter2"
ex2 = login connor "ASDFASDf"

ex3 = login tim "password"
ex4 = login tim "asdfasdf"


-- idk how to make it so the global listOfUsers stays updated...
addUser :: User -> (UserEnv listOfUsers) -> (UserEnv listOfUsers)
addUser u (listOfUsers)  = listOfUsers ++ [u]



data Value
  = Lit         Int     
  | Text        String  
  | B           AuthBool
  |	Error
 deriving (Show)

data Expr
  = Add         Value Value   --Should this be changed?
  | Sub         Value Value
  | Mul         Value Value
  | If          Value Expr Expr  -- ??
  | Def  --     ??????????
  | Ref  --     ??????????
  | Func --     ??????????
  | While       Expr Expr
--  | 
-- | Login       User Password
--  | CreateUser  Name Password Permission





-- Should Core features all be in Expr

-- IfStmt :: Expr -- -- -> -- Nothing ???  
-- If ( (B Granted) tc fc) = tc


-- sem :: Expr -> Value
-- sem (Add (Lit x) (Lit y)) = (Lit (x + y)) 
-- sem (Add (_) (_)) = Error

-- sem Login (Info (name,password) permlevel) givenPassword  = Error






