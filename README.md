# Team Members:

Ben Geyer (geyerb)
Mateo Rey-Rosa (reyrosam)
Ryan Chesla (cheslar)

# Introduction:

The language that we created is a stack-based language called StackyStack. An interesting feature that we decided to include was stacks within stacks. We chose to implement this because it allowed the language to perform subsets of operations within the main stack and complete subtasks that can be used in equations regarding the entire stack.

# Instructions:

Run ghci
Load the module “StackyStack.hs”
Run the input of the Good Examples found below to see examples of our language performing its expected actions. The Example Description will state what the example is and what features are being displayed with it.
Run the input of the Bad Examples found below to see the parts of our language that can lead to errors. The Error Description will state what the example is and what error is being displayed with it.


# Good Examples:

**Example Name:** fiboRecursive

**Example Description:** This example generates the first n fibonacci numbers in an array, where n is the number at the top of the stack. It does this using a recursive function. The example below shows how it can be used to generate the first 10 fibonacci numbers.

**Input:** “run [include fiboRecursive, newInt 10, Call “fibo”]”

**Output:** “Just [Object [Primitive 55,Primitive 34,Primitive 21,Primitive 13,Primitive 8,Primitive 5,Primitive 3,Primitive 2,Primitive 1,Primitive 1]]”

**Example Name:** fiboWhile

**Example Description:** This example generates the first n fibonacci numbers in an array, where n is the number at the top of the stack. It does this using a while loop, which uses a variety of stack manipulation operations and function operations to calculate the sequence. The while loop declares “fibo” which consists of calls to newArray, and shift left twice. The example below shows how it can be used to generate the first 10 fibonacci numbers.

**Input:** “run [include fiboWhile, newInt 10, Call “fibo”]”

**Output:** “Just [Object [Primitive 55,Primitive 34,Primitive 21,Primitive 13,Primitive 8,Primitive 5,Primitive 3,Primitive 2,Primitive 1,Primitive 1]]”

**Example Name:** fiboFor

**Example Description:** This example generates the first n fibonacci numbers in an array, where n is the number at the top of the stack. It does this using a for loop, which uses swap, dup2, add and swap, the latter of which are called from within Run Inside. The example declares “fibo” which consists of using RunInside to set the newInt values and then uses swap and subtract one. The example below shows how it can be used to generate the first 10 fibonacci numbers.

**Input:** “run [include fiboFor, newInt 10, Call “fibo”]”

**Output:** “Just [Object [Primitive 55,Primitive 34,Primitive 21,Primitive 13,Primitive 8,Primitive 5,Primitive 3,Primitive 2,Primitive 1,Primitive 1]]”

**Example Name:** syntaxGoodEx1-4, stackGoodEx1-5, condGoodEx1-3, funcGoodEx1-4, loopGoodEx1-4, arrayGoodEx1-8, and tupleGoodEx1-4

**Example Description:** These examples are less practical, but each displays a different feature of the language. They are just a complement to the previous examples, provided to show off every feature of the language.

**Input:** “run exampleName” (for instance, “run syntaxGoodEx1”)

**Output:** Varies

# Bad Examples:

**Example Name:** syntaxBadEx1-2, stackBadEx1-3, condBadEx1, funcBadEx1-2, loopBadEx1-3, arrayBadEx1-2, arrayBadEx4, tupleBadEx1-2

**Error Description:** These examples include errors with type mismatch, stack underflow, and calling functions that do not exist. All of these examples cause an error in the program, making it output the error value of “Nothing”.

**Input:** “run exampleName” (for instance, “run syntaxBadEx1”)

**Output:** “Nothing”

**Example Name:** loopBadEx4-5, arrayBadEx3

**Error Description:** These examples include any error with loops that cause the program to enter an infinite loop. These examples will all have no output, as the program will continue to loop until it is terminated by a signal such as SIGINT.

**Input:** “run exampleName” (for instance, “run loopBadEx4”)

**Output:** “” (infinite loop, no output or program termination until SIGINT signal)

