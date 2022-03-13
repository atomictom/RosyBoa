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
