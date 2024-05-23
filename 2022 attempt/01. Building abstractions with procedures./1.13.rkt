#lang racket

; Let's show that
; 1. Fib(n) = (φ^n-ψ^n)/√5, where ψ=(1-√5)/2;
; 2. (φ^n-ψ^n)/√5 is the closest to φ^n/√5.


; φ² = ((1+√5)/2)² = (1+2√5+5)/4 = (3+√5)/2 = 1+(1+√5)/2 = 1+φ,
; ψ² = ((1-√5)/2)² = (1-2√5+5)/4 = (3-√5)/2 = 1+(1-√5)/2 = 1+ψ.


; Fib(0) = 0 = (1-1)/√5 = (φ^0-ψ^0)/√5,
; Fib(1) = 1 = √5/√5 = ((1+√5)/2-(1-√5)/2)/√5 = (φ¹-ψ¹)/√5.

; Now suppose that Fib(k) = (φ^k-ψ^k)/√5 for k = 0 to n and let's proove for k + 1
; Fib(n+1) = Fib(n) + Fib(n-1) = (φ^n-ψ^n)/√5 + (φ^(n-1) - ψ^(n-1))/√5 = [φ^(n-1)(φ+1) – ψ^(n-1)(ψ+1) ]/√5 = (φ^(n-1)φ² – ψ^(n-1)ψ²)/√5 = (φ^(n+1) – ψ^(n+1))/√5.

; (φ^n-ψ^n)/√5 is an integer due to above; for φ^n/√5 to be the closest integer to Fib(n), ψ^n/√5 has to be less than 1/2.
; ψ=(1-√5)/2;
; 2 < √5 and ψ < 1 as (1-√5) < 1;
; QED