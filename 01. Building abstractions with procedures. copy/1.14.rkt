#lang racket
#|
Draw the tree illustrating the process gen- erated by the count-change procedure of Section 1.2.2 in making change for 11 cents. What are the orders of growth of the space and number of steps used by this process as the amount to be changed increases?
|#


#|
1. The tree:
count-change(11, 5)
├── count-change(11, 4)
│   ├── count-change(11, 3)
│   │   ├── count-change(11, 2)
│   │   │   ├── count-change(11, 1)
│   │   │   │   ├── count-change(11, 0)
│   │   │   │   │   └── 0
│   │   │   │   └── count-change(10, 1)
│   │   │   │       ├── count-change(10, 0)
│   │   │   │       │   └── 0
│   │   │   │       └── count-change(9, 1)
│   │   │   │           ├── count-change(9, 0)
│   │   │   │           │   └── 0
│   │   │   │           └── count-change(8, 1)
│   │   │   │               ├── ...
│   │   │   └── count-change(6, 2)
│   │   │       ├── count-change(6, 1)
│   │   │       │   ├── ...
│   │   │       └── count-change(1, 2)
│   │   │           ├── ...
│   │   └── count-change(1, 3)
│   │       ├── ...
│   └── count-change(1, 4)
│       ├── ...
└── count-change(6, 5)
    ├── count-change(6, 4)
    │   ├── count-change(6, 3)
    │   │   ├── ...
    │   └── count-change(1, 4)
    │       ├── ...
    └── count-change(1, 5)
        ├── ...

2. The order of growth in number of steps is exponential - O(2^n). In worst case, every new call to count-change will spawn 2 new calls.
3. Order of growth in space is linear (O(k)) because the stack stores the number of calls.
|#