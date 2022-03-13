# Rosy Boa Language

A toy language vaguely inspired by Python and Go.

Supposedly Rosy Boas are one of the slowest snake, so take what you will from
that. :)

## Usage

I somehow got this to run using Stack. Something like:

```
stack run source.boa
```

Should do the trick once it has downloaded all dependencies. An EBNF-like
grammar is specified in spec.txt.

## Demo

```
$ cat fizzbuzz.boa
-- Lazy language designer didn't add a modulo operator!
def mod(a, b) {
  n = a / b
  return (a - (n * b))
}

def fizzbuzz(n) {
  i = 1
  while i <= n {
    if mod(i, 15) == 0 {
      println "Fizzbuzz"
    } elif mod(i, 5) == 0 {
      println "Buzz"
    } elif mod(i, 3) == 0 {
      println "Fizz"
    } else {
      println i
    }
    i = i + 1
  }
}

fizzbuzz(30)
```

```
$ stack run fizzbuzz.boa
--------------------
Running program...
1
2
Fizz
4
Buzz
Fizz
7
8
Fizz
Buzz
11
Fizz
13
14
Fizzbuzz
16
17
Fizz
19
Buzz
Fizz
22
23
Fizz
Buzz
26
Fizz
28
29
Fizzbuzz

--------------------
Stats:
Number of ticks: 486

--------------------
```


## History

This started when I was on an airplane and wanted to program something in
Haskell for fun. I figured I'd just make a language (easier thought than done...).
Since I did not have internet to download the Parsec library, I thought I'd
start with just writing my own parser combinator library (easier thought than
done...). By the end of the trip, I believe I could parse digits or something.
After that, I worked on it here and there over a few months and eventually
finished the parser combinator library.

Perhaps on another trip, or maybe while looking for a project, I eventually
ended up writing a basic grammar (see spec.txt), a parser, and an interpreter
for the language. Just for fun, I made fizzbuzz in it (who doesn't want to claim
they wrote fizzbuzz in their own language? :D).

## Next Steps

I'd like to make a small VM and compile to bytecode for it. I just need to fly
somewhere far away to have time...
