Parsifal
========

Parsifal is a library for parser combinators in (chicken) Scheme. The inspiration comes from Parsec, the famous Haskell library.

Features
--------

- Purely functional implementation;
- Implements most of the combinators in Parsec;
- Easily creates lexers for programming languages;
- Can report user defined errors;
- Nice syntax, based on Haskell's DO notation, but also support desugared
  expressions, like `>>=`.

This library is intended for parsing all kinds of text, but the main support is
for programming languages and data formats with programming-like syntax, such
as JSON.

Sample usage & tests
--------------------
Examples are in the `examples/` folder, while test are in `src/test`.

Documentation
-------------
You can find the whole Parsifal documentation in the [Wiki](https://github.com/mbal/parsifal/wiki/First-steps).

License
-------
MIT License

