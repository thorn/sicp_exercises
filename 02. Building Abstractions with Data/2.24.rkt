#lang racket
#|
Suppose we evaluate the expression (list 1 (list 2 (list 3 4))). Give the result printed by the interpreter, the corresponding box-and-pointer structure, and the interpretation of this as a tree (as in figure 2.6).
|#

#|
Tree:
  (1 (2 (3 4)))
      ^
    /   \
   1     ^ (2 (3 4))
       /   \
      2     ^ (3 4)
          /   \
         3     4

                 
 (1 (2 (3 4)))  ((2 (3 4)))
   +---+---+    +---+---+
   | * | *-+--->| * | / |
   +-+-+---+    +-+-+---+
     |            |   
     V            V (2 (3 4))   ((3 4))
   +---+        +---+---+      +---+---+
   | 1 |        | * | *-+----->| * | / | 
   +---+        +-+-+---+      +---+---+
                  |              |
                  V              V (3 4)
                +---+          +---+---+    +---+---+
                | 2 |          | * | *-+--->| * | / |
                +---+          +-+-+---+    +-+-+---+
                                 |            |
                                 V            V
                               +---+        +---+
                               | 3 |        | 4 |
                               +---+        +---+
|#