import Prelude

-- Homework 2 Tree

type Name   = String

data User   = Info Name Permission
 deriving (Show)

data Action = Login | Read_Tree
-- Can later add arguments to the actions so they actually do something ex: Login | Read_Tree Tree_Name | Make_Tree (Tree)

data Math = Add Int Int
 |   Sub Int Int
 |   Mul Int Int
 |   Div Int Int



data Auth   = Granted | Denied
 deriving (Show)

data Permission = Admin | Regular | Banned
 deriving (Show)



connor::User
connor = Info "Connor" Admin

bob::User
bob = Info "Bob Smith" Regular

tim::User
tim = Info "Tim Timmerson" Banned

calc :: Math -> Maybe Int
calc (Add x y) = Just (x + y)
calc (Sub x y) = Just (x - y)
calc (Mul x y) = Just (x * y)
calc (Div x y) = case y of
 0 -> Nothing
 _ -> Just (x `div` y)

program :: Action -> User -> Auth
program Login (Info _ Admin) = Granted
program Login (Info _ Regular) = Granted
program Login (Info _ Banned) = Denied


program Read_Tree (Info _ Admin) = Granted -- later can make it show it actually displays Tree if granted (could use case of)
program Read_Tree (Info _ _) = Denied


ex1 = program Login connor
ex2 = program Login bob
ex3 = program Login tim
ex4 = program Read_Tree connor
ex5 = program Read_Tree bob
ex6 = program Read_Tree tim
