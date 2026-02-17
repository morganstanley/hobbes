# Rosetta Code Tasks for Hobbes

Tracking file for Hobbes examples corresponding to Haskell tasks on Rosetta Code.

## Infrastructure
- Test runner: `examples/rosetta/run_tests.sh`
- File format: `.hob` files with `// @test: <expression>` directive on line 1
- Expected output: `.expected` files with matching names
- Run: `bash examples/rosetta/run_tests.sh ./build/hi`
- Update expected output: `bash examples/rosetta/run_tests.sh ./build/hi --update`

## DONE: Hello World (1 task)
- [x] Hello world/Text

## Batch 1: Fundamentals (15 tasks)
- [x] FizzBuzz
- [x] Fibonacci sequence
- [x] Factorial
- [x] Greatest common divisor
- [x] Towers of Hanoi
- [x] Reverse a string
- [x] Palindrome detection
- [x] Even or odd
- [x] Primality by trial division
- [x] 99 bottles of beer
- [x] Sum of a series
- [x] 100 doors
- [x] Ackermann function
- [x] Dot product
- [x] Empty program

## Batch 2: Number Theory (15 tasks)
- [x] Abundant, deficient and perfect number classifications
- [x] Additive primes
- [x] Amicable pairs
- [x] Attractive numbers
- [x] Catalan numbers
- [x] Chowla numbers
- [x] Circular primes
- [x] Cousin primes
- [x] Digital root
- [x] Emirp primes
- [x] Factors of an integer
- [x] Happy numbers
- [x] Harshad or Niven series
- [x] Least common multiple
- [x] Perfect numbers

## Batch 3: More Number Theory (15 tasks)
- [x] Pernicious numbers
- [x] Population count
- [x] Prime decomposition
- [x] Proper divisors
- [x] Totient function
- [x] Sieve of Eratosthenes
- [x] Self numbers
- [x] Semiprime
- [x] Smith numbers
- [x] Strong and weak primes
- [x] Sexy primes
- [x] Kaprekar numbers
- [x] Hailstone sequence
- [x] Hamming numbers
- [x] Humble numbers

## Batch 4: String Processing (15 tasks)
- [x] Caesar cipher
- [x] Balanced brackets
- [x] Comma quibbling
- [x] Pangram checker
- [x] Rot-13
- [x] Run-length encoding
- [x] Strip whitespace from a string/Top and tail
- [x] Reverse words in a string
- [x] Remove duplicate elements
- [x] Determine if a string has all unique characters
- [x] Determine if a string has all the same characters
- [x] Count how many vowels and consonants occur in a string
- [x] Count occurrences of a substring
- [x] Repeat a string
- [x] Roman numerals/Encode

## Batch 5: Algorithms & Data Structures (15 tasks)
- [x] Binary search
- [x] Sorting algorithms/Quicksort
- [x] Sorting algorithms/Merge sort
- [x] Sorting algorithms/Insertion sort
- [x] Sorting algorithms/Bubble sort
- [x] Sorting algorithms/Selection sort
- [x] Greatest subsequential sum
- [x] Josephus problem
- [x] Longest common subsequence
- [x] N-queens problem
- [x] Knapsack problem/0-1
- [x] Levenshtein distance
- [x] Look-and-say sequence
- [x] Ethiopian multiplication
- [x] Horner's rule for polynomial evaluation

## Batch 6: Math & Geometry (15 tasks)
- [x] Arithmetic-geometric mean
- [x] Calculating the value of e
- [x] Evaluate binomial coefficients
- [x] Roots of a quadratic function
- [x] Shoelace formula for polygonal area
- [x] Vector products
- [x] Sum and product of an array
- [x] Sum of squares
- [x] Sum multiples of 3 and 5
- [x] Sum digits of an integer
- [x] Pi
- [x] Temperature conversion
- [x] Leap year
- [x] Averages/Arithmetic mean
- [x] Averages/Root mean square

## Batch 7: Higher-Order Functions & Functional (15 tasks)
- [x] Apply a callback to an array
- [x] Catamorphism
- [x] Filter
- [x] First-class functions
- [x] Function composition
- [x] Higher-order functions
- [x] List comprehensions
- [x] Cartesian product of two or more lists
- [x] Combinations
- [x] Power set
- [x] Symmetric difference
- [x] Flatten a list
- [x] Combinations with repetitions
- [x] Mutual recursion
- [x] Pascal's triangle

## Batch 8: Remaining Feasible (11 tasks)
- [x] Roman numerals/Decode
- [x] Babbage problem
- [x] Convex hull
- [x] Luhn test of credit card numbers
- [x] Fibonacci word
- [x] Convert seconds to compound duration
- [x] Vigenere cipher
- [x] Truth table
- [x] Show ASCII table
- [x] The Twelve Days of Christmas
- [x] Old lady swallowed a fly

## Batch 9: Stretch Goals (11 tasks)
- [ ] Stern-Brocot sequence
- [ ] Thue-Morse
- [ ] Van Eck sequence
- [ ] Yellowstone sequence
- [ ] Zeckendorf number representation
- [ ] Recaman's sequence
- [ ] Kolakoski sequence
- [ ] Floyd's triangle
- [ ] Narcissistic decimal number
- [ ] Farey sequence
- [ ] Forward difference

## Not Feasible in Hobbes
- Accumulator factory (no general-purpose mutable local variables)
- Y combinator (closure types don't unify with recursive types)
- Haversine formula (no sin/cos/asin)
- Trigonometric functions (no sin/cos/tan)
- Bernoulli numbers (no memoization, exponential recursion)
- Euler's sum of powers conjecture (no pow, brute force too slow)
