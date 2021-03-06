-- | Team members:
-- | Benjamin Geyer (geyerb)
-- | Mateo Rey-Rosa (reyrosam)
-- | Ryan Chesla (cheslar)
module StackyStack where

import Prelude hiding (Num, not, and, or, reverse, drop, div, round, mod)
import Data.Char

-- | Abstract syntax
type Prog = [Cmd]
type Function = String

data Expr = Num Float
          | Func Function
          | Stack
          | Insert
          | Extract
          | Add
          | Mul
          | Pow
          | Equ
          | Lt
          | Swap
          | Dup
          | Over
          | Drop
          deriving(Eq, Show)

data Cmd = Push Expr
         | While Prog
         | Declare Function Prog
         | Call Function
         | Exec
         | RunInside Prog
         | IfElse Prog Prog
         deriving(Eq, Show)


-- | Semantic domain
data StackItem = Primitive Float
               | Object Stack
               | FunctionName Function
               deriving(Eq, Show)

type Stack = [StackItem]
type Env = Function -> Maybe Prog
type Domain = (Stack, Env) -> Maybe (Stack, Env)


-- | Valuation function
emptyEnv :: Env
emptyEnv _ = Nothing

getEnv :: Function -> Env -> Maybe Prog
getEnv f e = e f

setEnv :: Function -> Prog -> Env -> Env
setEnv f p e = \x -> if x == f then Just p else e x

expr :: Expr -> Stack -> Maybe Stack
expr (Num x) s = Just (Primitive x : s)
expr (Func x) s = Just (FunctionName x : s)
expr (Stack) s = Just (Object [] : s)
expr (Insert) (x : Object y : s) = Just (Object (x : y) : s)
expr (Extract) (Object (x : y) : s) = Just (x : Object y : s)
expr (Add) (Primitive x : Primitive y : s) = Just (Primitive (x + y) : s)
expr (Add) _ = Nothing
expr (Mul) (Primitive x : Primitive y : s) = Just (Primitive (x * y) : s)
expr (Mul) _ = Nothing
expr (Pow) (Primitive x : Primitive y : s) = Just (Primitive (y ** x) : s)
expr (Pow) _ = Nothing
expr (Equ) (x : y : s) = Just (Primitive (if x == y then 1 else 0) : s)
expr (Equ) _ = Nothing
expr (Lt) (Primitive x : Primitive y : s) = Just (Primitive (if x < y then 1 else 0) : s)
expr (Lt) _ = Nothing
expr (Swap) (x:y:s) = Just (y:x:s)
expr (Dup) (x:s) = Just (x:x:s)
expr (Over) (x:y:s) = Just (y:x:y:s)
expr (Drop) (x:s) = Just s
expr _ _ = Nothing

cmd :: Cmd -> Domain
cmd (Push exp) (s, e) = case expr exp s of
                          Just s' -> Just (s', e)
                          _ -> Nothing
cmd (While _) (((Primitive 0):s), e) = Just ((Primitive 0 : s), e)
cmd (While _) (((Object []):s), e) = Just ((Object [] : s), e)
cmd (While p) ((x:s), e) = case prog p ((x:s), e) of
                                         Just d' -> cmd (While p) d'
                                         _ -> Nothing
cmd (Declare f p) (s, e) = Just (s, setEnv f p e)
cmd (Call f) (s, e) = case getEnv f e of
                        Just p -> prog p (s, e)
                        _ -> Nothing
cmd (Exec) ((FunctionName f):s, e) = case getEnv f e of
                                     Just p -> prog p (s, e)
                                     _ -> Nothing
cmd (RunInside p) (((Object x):s), e) = case prog p (x, e) of
                                          Just (x', e') -> Just (((Object x'):s), e')
                                          _ -> Nothing
cmd (IfElse l r) (((Primitive 0):s), e) = prog r (((Primitive 0):s), e)
cmd (IfElse l r) (((Object []):s), e) = prog r (((Object []):s), e)
cmd (IfElse l r) ((Primitive x:s), e) = prog l ((Primitive x:s), e)
cmd (IfElse l r) ((Object x:s), e) = prog l ((Object x:s), e)
cmd (IfElse l r) ([], e) = prog r ([], e)
cmd _ _ = Nothing

prog :: Prog -> Domain
prog [] d = Just d
prog (c:p) d = case cmd c d of
                 Just d' -> prog p d'
                 _ -> Nothing

run :: Prog -> Maybe Stack
run p = case prog p ([], emptyEnv) of
          Just (s', e') -> Just s'
          _ -> Nothing


-- | String helper method -- build string from human readable characters rather than numbers
-- | (technically this is at the language core as it adds the ability to convert from chars to
-- | numbers, however its primary purpose is really just to make testing easier)
stringBuilder :: String -> Prog
stringBuilder [] = [newStack]
stringBuilder (x:s) = (stringBuilder s) ++ [newNum (fromIntegral (ord x)), insert]


-- | Syntactic sugar
-- | Boolean values
true :: Expr
true = Num 1

false :: Expr
false = Num 0

newTrue :: Cmd
newTrue = Push true

newFalse :: Cmd
newFalse = Push false

-- | Integers, strings and functions
newNum :: Float -> Cmd
newNum = Push . Num

newString :: String -> Cmd
newString = block . stringBuilder

newFunc :: Function -> Cmd
newFunc = Push . Func


-- | Convert a program into a single command -- block like { Prog }
block :: Prog -> Cmd
block p = IfElse p p


-- | Basic stack operations
newStack :: Cmd
newStack = Push Stack

insert :: Cmd
insert = Push Insert

extract :: Cmd
extract = Push Extract

swap :: Cmd
swap = Push Swap

dup :: Cmd
dup = Push Dup

over :: Cmd
over = Push Over

drop :: Cmd
drop = Push Drop

swap2 :: Cmd
swap2 = block [newStack, swap, insert, swap, insert, swap, insert, extend]

overn :: Cmd
overn = block [newStack, swap,
              for [
                swap2, insert, swap
              ],
              reverse, over, insert, extend]

swapn :: Cmd
swapn = block [newStack, swap,
              for [
                swap2, insert, swap
              ],
              reverse, extract, swap2, insert, extend]

dup2 :: Cmd
dup2 = block [over, over]

over2 :: Cmd
over2 = block [newNum 2, overn]

drop2 :: Cmd
drop2 = block [drop, drop]

extend :: Cmd
extend = block [reverse,
               While [
                 extract, swap
               ],
               drop]

compress :: Cmd
compress = block [newStack, swap, insert]


-- | Math and logic operations
equ :: Cmd
equ = Push Equ

add :: Cmd
add = Push Add

addone :: Cmd
addone = block [newNum 1, add]

subone :: Cmd
subone = block [newNum 1, sub]

mul :: Cmd
mul = Push Mul

pow :: Cmd
pow = Push Pow

neg :: Cmd
neg = block [newNum (-1), mul]

roundpos :: Cmd
roundpos = block [newNum 0, swap,
                  While [
                    dup, newNum 0.5, gt,
                    IfElse [
                      drop, newFalse
                    ] [
                      drop, subone, swap, addone, swap
                    ]
                  ],
                  drop2]

round :: Cmd
round = block [dup, newNum 0, gt,
              IfElse [
                drop, neg, roundpos, neg
              ] [
                drop, roundpos
              ]]

sub :: Cmd
sub = block [neg, add]

div :: Cmd
div = block [newNum 1, neg, pow, mul]

divint :: Cmd
divint = block [div, round]

modpos :: Cmd
modpos = block [While [
                 dup2, lte,
                 IfElse [
                   drop, swap, over, sub, swap
                 ] []
               ],
               drop2]

mod :: Cmd
mod = block [dup, newNum 0, lt,
            IfElse [
              drop, over, newNum 0, lt,
              IfElse [
                drop, modpos
              ] [
                drop, dup, swap2, neg, swap,
                modpos, sub
              ]
            ] [
              drop, over, newNum 0, lt,
              IfElse [
                drop, neg, dup, swap2, swap,
                modpos, sub, neg
              ] [
                drop, swap, neg, swap, neg,
                modpos, neg
              ]
            ]]

not :: Cmd
not = block [newFalse, equ]

and :: Cmd
and = block [IfElse [
              drop, newTrue, equ
            ] [
              drop, drop, newFalse
            ]]

or :: Cmd
or = block [newTrue, equ,
           IfElse [
             drop, drop, newTrue
           ] [
             drop, newTrue, equ
           ]]

lt :: Cmd
lt = Push Lt

gt :: Cmd
gt = block [over, over, lt,
           IfElse [
             drop, drop, drop, newFalse
           ] [
             drop, equ, not
           ]]

lte :: Cmd
lte = block [gt, not]

gte :: Cmd
gte = block [lt, not]


-- | For loops
for :: Prog -> Cmd
for p = block [While [
                subone, block p
              ],
              drop]

reverse :: Cmd
reverse = block [newStack, swap,
                While [
                  insert,
                  RunInside [
                    extract, swap
                  ],
                  extract
                ],
                drop]

foreach :: Prog -> Cmd
foreach p = block [newStack, swap,
                  While [
                    insert,
                    RunInside [
                      extract
                    ],
                    extract, block p, insert,
                    RunInside [
                      swap
                    ],
                    extract
                  ],
                  drop, reverse]


-- | Library level features
-- | Syntactic sugar for including libraries
include :: Prog -> Cmd
include = block


-- | Array operations
newArray :: Cmd
newArray = Declare "newArray" [
             Call "range",
             foreach [
               drop, newNum 0
              ]
            ]

get :: Cmd
get = Declare "get" [
        insert,
        RunInside [
          overn
        ],
        extract
      ]

set :: Cmd
set = Declare "set" [
        swap2, swap, insert,
        swap, addone, insert,
        RunInside [
          swapn, drop
        ]
      ]

cat :: Cmd
cat = Declare "cat" [
        swap, insert,
        RunInside [
          extend
        ]
      ]

len :: Cmd
len = Declare "len" [
        dup, newNum 0, swap,
        foreach [
          drop, swap, addone, swap, newNum 0
        ],
        drop
      ]

range :: Cmd
range = Declare "range" [
          newStack, swap,
          for [
            swap, over, insert, swap
          ]
        ]

shiftl :: Cmd
shiftl = Declare "shiftl" [
           extract, swap, reverse, swap, insert, reverse
         ]

shiftr :: Cmd
shiftr = Declare "shiftr" [
           reverse, extract, swap, reverse, swap, insert
         ]

shiftnl :: Cmd
shiftnl = Declare "shiftnl" [
            for [
              swap, Call "shiftl", swap
            ]
          ]

shiftnr :: Cmd
shiftnr = Declare "shiftnr" [
            for [
              swap, Call "shiftr", swap
            ]
          ]

arraylib :: Prog
arraylib = [newArray, get, set, cat, len, range, shiftl, shiftr, shiftnl, shiftnr]


-- | Tuple operations
newTuple :: Cmd
newTuple = Declare "newTuple" [
             newNum 2, Call "newArray"
           ]

getfirst :: Cmd
getfirst = Declare "getfirst" [
             newNum 0, Call "get"
          ]

getsecond :: Cmd
getsecond = Declare "getsecond" [
              newNum 1, Call "get"
            ]

setfirst :: Cmd
setfirst = Declare "setfirst" [
             newNum 0, Call "set"
           ]

setsecond :: Cmd
setsecond = Declare "setsecond" [
              newNum 1, Call "set"
            ]

tuplelib :: Prog
tuplelib = [include arraylib, newTuple, getfirst, getsecond, setfirst, setsecond]


-- | Full standard library
stdlib :: Prog
stdlib = [include arraylib, include tuplelib]


-- | Example programs:

-- | Generate array with the first n fibonacci numbers:

-- | With recursion
fiboRecursive = [Declare "fibo" [
                  dup, newNum 3, gt,
                  IfElse [
                    drop, drop,
                    newStack,
                    newNum 1, insert,
                    newNum 1, insert
                  ] [
                    drop,
                    subone,
                    Call "fibo",
                    RunInside [
                      dup2, add
                    ]
                  ]
                ]]

-- | With while loop
fiboWhile = [include stdlib,
            Declare "fibo" [
              newNum 20, dup,
              Call "newArray",
              newNum 1, Call "setfirst",
              Call "shiftl",
              newNum 1, Call "setfirst",
              Call "shiftl",
              swap, subone, subone,
              While [
                swap, over, Call "get",
                swap, over2, addone, Call "get",
                swap, swap2, add,
                over2, subone, Call "set",
                swap, subone
              ],
              drop
            ]]

-- | With for loop
fiboFor = [Declare "fibo" [
            newStack,
            RunInside [
              newNum 1, newNum 1
            ],
            swap, subone, subone,
            for [
              swap,
              RunInside [
                dup2, add
              ],
              swap
            ]
          ]]


----------------------------------------------------------------------------------------
-- | The below examples include good and bad examples of programs. These programs are
-- | simpler and less practical, but show a more complete view of the features and
-- | potential errors.
----------------------------------------------------------------------------------------

-- | Basic syntax, logic, and arithmetic
-- | Good examples:

-- | Perform 5 + 5
syntaxGoodEx1 = [newNum 5, dup, add]

-- | Perform (3 * (4 + 5))
syntaxGoodEx2 = [newNum 3, newNum 4, newNum 5, add, mul]

-- | Perform true or false
syntaxGoodEx3 = [newTrue, newFalse, or]

-- | Perform (not 0) and true
-- | (booleans are really integers so they can be used interchangeably)
syntaxGoodEx4 = [newNum 0, not, newTrue, and]

-- | Bad examples:
-- | Performing operation on empty stack
syntaxBadEx1 = [neg]

-- | Some operations need two arguments
syntaxBadEx2 = [newNum 1, add]


-- | Basic stack operations
-- | Good examples:

-- | Duplicate second item, add new item, duplicate new item
stackGoodEx1 = [newNum 1, newNum 2, over, newNum 3, dup]

-- | Duplicate last two items, duplicate third item, get rid of last item
stackGoodEx2 = [newNum 1, newNum 2, dup2, over2, drop]

-- | Create stack inside stack, then add integer to it -- two ways
stackGoodEx3 = [newStack,
                RunInside [
                  newNum 1
                ]]
stackGoodEx4 = [newStack, newNum 1, insert]

-- | Remove integer from stack
stackGoodEx5 = [newStack, newNum 1, insert, extract]

-- | Bad examples:
-- | Trying to insert with stack in front
stackBadEx1 = [newNum 1, newStack, insert]

-- | Using operations without enough arguments
stackBadEx2 = [dup]
stackBadEx3 = [newNum 1, over]


-- | Conditionals
-- | Good examples:

-- | If 5 < 3 then push 1 to the stack else push 0
condGoodEx1 = [newNum 3, newNum 5, lt, IfElse [drop, newNum 1] [drop, newNum 0]]

-- | If stack within stack is empty then insert 1 to it
condGoodEx2 = [newStack,
              IfElse [
              ] [
                newNum 1, insert
              ]]

-- | If stack is empty perform default behavior in else block -- add 1 to stack
condGoodEx3 = [IfElse [
              ] [
                newNum 1
              ]]

-- | Bad examples:

-- | Calling IfElse on a function variable
condBadEx1 = [newFunc "foo",
             IfElse [
               newNum 0
             ] [
               newNum 1
             ]]


-- | Functions
-- | Good examples:

-- | Declare function to square top number on stack and add one
funcGoodEx1 = [Declare "foo" [
                dup, mul, addone
              ]]

-- | Call "foo" function on number 5
funcGoodEx2 = [include funcGoodEx1,
              newNum 5, Call "foo"]

-- | First order functions -- store function as variable and call it
funcGoodEx3 = [include funcGoodEx1,
              newNum 5, newFunc "foo", Exec]

-- | First order functions -- use function as argument
funcGoodEx4 = [include funcGoodEx1,
              Declare "bar" [
                Exec, subone
              ],
              newNum 5, newFunc "foo", Call "bar"]

-- | Bad examples:
-- | Calling a function that doesn't exist (or using Exec on one)
funcBadEx1 = [Call "baz"]

-- | Using Exec on something that is not a function
funcBadEx2 = [newNum 1, Exec]


-- | Recursion and loops
-- | Good examples:

-- | Recursively count down from five
loopGoodEx1 = [Declare "foo" [
                dup, newNum 0, lt,
                IfElse [
                  drop, subone, Call "foo"
                ] [
                  drop
                ]
              ],
              newNum 5, Call "foo"]

-- | Count down from five in while loop
loopGoodEx2 = [newNum 5,
              While [
                subone
              ]]

-- | Make numbers one through five in for loop
loopGoodEx3 = [newNum 5,
              for [
                dup, addone, swap
              ]]

-- | Square each number in array with foreach
loopGoodEx4 = [include stdlib,
              newNum 5, Call "range",
              foreach [
                dup, mul
              ]]

-- | Bad examples:

-- | Missing parameters
loopBadEx1 = [While []]
loopBadEx2 = [for []]

-- | For each on non-stack or other parameter type mismatch
loopBadEx3 = [newNum 3,
             foreach []]

-- | Infinite while loop due to never making top item on stack false
loopBadEx4 = [newTrue,
             While []]

-- | Infinite for loop if value chosen is less than 0
loopBadEx5 = [newNum 1, neg,
             for []]


-- | Arrays
-- | Good examples:

-- | Create new array of size 10
arrayGoodEx1 = [include stdlib,
               newNum 10, Call "newArray"]

-- | Make array of numbers 0-9
arrayGoodEx2 = [include stdlib,
               newNum 10, Call "range"]

-- | Access 3rd element of array
arrayGoodEx3 = [include stdlib,
               newNum 10, Call "range",
               newNum 2, Call "get"]

-- | Set 4th element of array to 100
arrayGoodEx4 = [include stdlib,
               newNum 10, Call "newArray",
               newNum 100, newNum 3, Call "set"]

-- | Make 10 * 10 2d array
arrayGoodEx5 = [include stdlib,
               newNum 10, Call "newArray",
               foreach [
                 drop, newNum 10, Call "newArray"
               ]]

-- | Concatenate two arrays
arrayGoodEx6 = [include stdlib,
               newNum 10, Call "newArray",
               newNum 10, Call "range",
               Call "cat"]

-- | Get length of array
arrayGoodEx7 = [include stdlib,
               newNum 10, Call "newArray",
               Call "len"]

-- | Left shift array elements by 5, then shift right by 1
arrayGoodEx8 = [include stdlib,
               newNum 10, Call "range",
               newNum 5, Call "shiftnl",
               Call "shiftr"]

-- | Bad examples:

-- | Trying to access an element outside of the array
arrayBadEx1 = [include stdlib,
              newNum 10, Call "range",
              newNum 10, Call "get"]

-- | Trying to concatenate with a non array
arrayBadEx2 = [include stdlib,
              newNum 10, Call "range",
              newNum 10,
              Call "cat"]

-- | Using newArray or range with a negative value (causes infinite loop due to for loop used)
arrayBadEx3 = [include stdlib,
              newNum (-1), Call "range"]

-- | Too few arguments
arrayBadEx4 = [include stdlib,
              newNum 10, Call "range",
              newNum 10, Call "set"]


-- | Tuples
-- | Good examples:

-- | Creating a tuple:
tupleGoodEx1 = [include stdlib,
               Call "newTuple"]

-- | Set the first element of the tuple to be one
tupleGoodEx2 = [include stdlib,
               Call "newTuple",
               newNum 1, Call "setfirst"]

-- | Set the second element of the tuple to be another tuple
tupleGoodEx3 = [include stdlib,
               Call "newTuple",
               Call "newTuple", Call "setsecond"]

-- | Get the first element of a tuple
tupleGoodEx4 = [include stdlib,
               Call "newTuple",
               newNum 1, Call "setfirst",
               Call "getfirst"]

-- | Bad examples:

-- | Calling get/set on non tuple
tupleBadEx1 = [include stdlib,
              newNum 10,
              Call "getfirst"]

-- | Too few arguments
tupleBadEx2 = [include stdlib,
              Call "newTuple",
              Call "setfirst"]
