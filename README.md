# CS 381 Group Project
## The Team
| Name  | ID |
| ------------- | ------------- |
| Connor G | greenwac  |
|Conner M | maddaloc |
| Conner R | rheac  |
| Arman H | hastinar |

## Introduction
Hello! And welcome to AuthLang, our language for the cs381 group project. AuthLang is a language that allows for the creation and authentication of users with a password. Our language contains many different features which utilize whether or not the user has logged in. Once a user is in the system with permissions they may use this functional program to create variables to add and use together in various expressions.

## Instructions 
Our program is intended to run and compile within ghci. 
In order to run it, simply go into ghci and :load authLang4.hs to get started with our language. We have written a many examples that demonstrate single operations as well as examples which combine several different operations. 

## Example command list, and expected output: 
Look below for a list of examples of the different features our language has. Test them by running the example names in ghci:
Bad examples are anything that results in an Error.
```haskell
addEx: Lit 15
addEx2: Lit 23
incEx1: Lit 6
decEx1: Lit 69 
ifEx1: Text "This should print"
ifEx2: Text “this FALSE text should print”
ifEx3: Lit 15
ifEx4: Lit 0 
ifExErr: Error
andEx1: B Granted
andEx2: B Denied
andEx3: Error
orEx1: B Granted 
orEx2: B Granted
orEx3: Error 
notEx1: B Denied 
notEx2: B Granted
notEx3: Error
appendEx: [1,2,3,4,7]
prependEx: [100,10,20,30,90]
addToAllEx: [6,7,8,910]
addListsEx:  [11,13,15]
tupEx1: (Text "a",Lit 7)
invertEx1: Text “a”
invertEx2: Lit 7 
varEx1: [Info ("Connor G","Hunter2"),Info ("Bob Smith","Hunter1"),Info ("Tim Timmerson","password")]
varEx2: [Info ("Connor G","Hunter2"),Info ("Bob Smith","Hunter1"),Info ("Tim Timmerson","password"),Info ("Test User","Password123")]
refEx1: Info ("Connor G","Hunter2")
refEx2: Info ("Test User","Password123") 
refEx3 “Hunter2”
loginEx1: B Granted
loginEx2: B Denied
loginEx3: B Granted 
loginEx4: B Denied
loginIfEx1: Text "You are logged in"
loginIfEx2: Lit 12 
```
