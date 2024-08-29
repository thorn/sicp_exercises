#lang racket
#|
Explain, in general, why equivalent alge- braic expressions may lead to different answers. Can you devise an interval-arithmetic package that does not have this shortcoming, or is this task impossible? (Warning: This problem is very difficult.)
|#

#|
From https://github.com/xxyzz/SICP/blob/master/2_Building_Abstractions_with_Data/2.1_Introduction_to_Data_Abstraction/README.md#exercise-216

This is called [dependency problem](https://en.wikipedia.org/wiki/Interval_arithmetic#Dependency_problem) of interval arithmetic.

If an interval occurs several times in a calculation using parameters, and each occurrence is taken independently then this can lead to an unwanted expansion of the resulting intervals.
|#