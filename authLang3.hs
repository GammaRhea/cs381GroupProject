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
  deriving (Eq,Show)

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



data Expr
  = Add         Expr Expr   --Should this be changed?
  | Sub         Expr Expr
  | Mul         Expr Expr
  | If          Expr Expr Expr  -- ??
  | Get
  | Set         Expr
  | Def  --     ??????????
  | Ref  --     ??????????
  | Func --     ??????????
  | While       Test Expr
  | Begin [Expr]
  | Lit         Int
  | Text        String
  | B           AuthBool
  |	Error
  deriving (Eq,Show)


--  |
-- | Login       User Password
--  | CreateUser  Name Password Permission

type Reg = Int
p :: Expr
p = Begin
      [ Set (Lit 1)
      , While (LT_ Get (Lit 5))
          (Set (Add Get (Lit 1)))
      ]

-- | Valuation function for statements.
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
