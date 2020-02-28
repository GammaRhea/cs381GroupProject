# cs381GroupProject
Group of   
(Connor G: greenwac)  
(Conner M: maddaloc)  
(Conner R: rheac)  
(Arman H: hastinar)  

Hello! And welcome to AuthLang, our language for the cs381 group project.
AuthLang is a language specifically designed to emulate a login based system, where different users have
different levels of permissions which determines which tasks they are allowed to run. Once completed, users will be
able to login, run commands, and then logout once their session is done.

Instructions:
Our program is inteded to run and compile withing ghci. In order to run it, simply go into ghci and
:load authLang1.hs to get started with our language. We have written a number of examples detailing some of
the basic functionality of our language. You can run them by typing ex1, ex2, ex11, ex100, etc... you can find a full list
of commented examples within the code of the file authLang1.hs

Examples 1-9 are simply basic functionality examples with the types we have encoded for our language.
These make use of our expr data types as well as our semantics.

Examples 11-16 are cases we devised specifically to test the functionality of our User data type, and accessing
individual fields of the User tuples. They include things such as checking if a user exists, or getting their name and permissions.

Examples 100 and 110 are cases relating to our list of users, which is still in the process of development, but specifically
relate to the manipulation of our list of valid users.

Command List with expected output:  
ex1  : Info ("Connor",Admin)  
ex2  : Info ("Tim Timmerson",Banned)  
ex3  : B Granted  
ex4  : B Denied  
ex5  : Lit 15  
ex5a : Error  
ex6  : Lit 1  
ex7  : B Granted  
ex8  : B Denied  
ex9  : Lit 10  
ex11 : Info ("Eric Walkingshaw",Admin)  
ex12 : "TestUser"  
ex13 : Just (Info ("Connor",Admin))  
ex14 : Nothing  
ex15 : Admin  
ex16 : Regular  
ex100 : [Info ("Connor",Admin),Info ("Bob Smith",Regular),Info ("Tim Timmerson",Banned)]  
ex110 : [Info ("Connor",Admin),Info ("Bob Smith",Regular),Info ("Tim Timmerson",Banned),Info ("John Doe",Banned),Info ("First Last",Admin)]  
