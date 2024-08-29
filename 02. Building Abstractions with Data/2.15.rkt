#lang racket
#|
Eva Lu Ator, another user, has also noticed the different intervals computed by different but algebraically equivalent expressions. She says that a formula to compute with intervals using Alyssa’s system will produce tighter error bounds if it can be written in such a form that no variable that represents an uncertain number is repeated. Thus, she says, par2 is a “better” program for parallel resistances than par1. Is she right? Why?
|#

#|
Answer from the [wiki](http://community.schemewiki.org/?sicp-ex-2.14-2.15-2.16):

We can see it as a problem of finding extrema of n-variate functions. for R1, R2, R_, ... these are variables, The interval of R1, R2, R_, ... are the definition fields. The operations on variables are the function. And we want to find the maximum and minimum of the function in the definition fields.

For the general questions, It's very clear that for most functions, their extrema won't be found in the border, so we can't simple get the result by operating on lower and upper bound.

For the specific question there, the problem is that the functions Lem wrote assume the variables are independent. If the variables are dependent, the result is wrong.

For example, R1 = [1, 2]; R2 = R1 + 1=[2, 3], then R1 and R2 are dependent, so R1 - R2 = 1, and (sub-interval R1 R2) = [0, 2] is wrong.

Here, for par1, R1*R2 and R1+R2 are dependent, so (div-interval R1*R2 R1+R2) got wrong answer.

But, for pa2, [1,1] and R1; [1,1] and R2; 1/R1 and 1/R2; [1,1] and 1/R1 + 1/R2; they are all independent, so the result is right.

If we want let the result be correct, either we change the code fundamentally, or we need to ensure for each operation, their operands should be independent.

To say more detailedly:

Here since we have division, we assume R1 and R2 are both positive intervals (definition see Exercise 2.11). Then R1R2 take the minimum when both R1 and R2 take minimum. But 1/(R1+R2) takes minimum when both R1 and R2 take maximum. So the minimum of the 1st method result by multiplying these 2 minimums is wrong and can't be reached.

So here the book says

You will get the most insight by using intervals whose width is a ''small percentage of the center value''.
since this can make R1R2 and 1/(R1+R2) almost one number. Then these 2 methods have almost same center value. Otherwise, 1/(R1_max+R2_max) (R1_max means the upper bound of R1. Similar for others) and 1/(R1_min+R2_min) will differ a lot, which will cause R1R2/(R1+R2) result far from the right result. This is implied by https://web.archive.org/web/20141122102312/http://wqzhang.wordpress.com/2009/06/18/sicp-exercise-2-14/ and https://github.com/xxyzz/SICP/blob/2decd1017f898529340dd07e73224f2e750b65b8/2_Building_Abstractions_with_Data/2.1_Introduction_to_Data_Abstraction/Exercise_2_14.rkt#L67-L74.


|#