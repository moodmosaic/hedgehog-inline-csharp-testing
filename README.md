# hedgehog-inline-csharp-testing

_Example of using
[haskell-hedgehog](https://github.com/hedgehogqa/haskell-hedgehog)'s
model-based state machine testing together with
[clr-inline](http://hackage.haskell.org/package/clr-inline) (C#)._

## Points of Interest

* The code under test, a circular buffer implemented in C#:
[CircularBuffer.hs](src/CircularBuffer.hs#L15-L59) (clr-inline provides a
quasiquoter to inline F# and C# code in Haskell modules)
* The code wrapping our circular buffer class, providing a Haskell API:
[CircularBuffer.hs](src/CircularBuffer.hs#L78-L110)
* The tests, [test.hs](test/test.hs), including:
  - A simplified model of a circular buffer (the `Model` data type)
  - The commands of the API (`New`, `Get`, `Put`, `Size`), with generators, pre
  and post-conditions, and model update functions
  - A Hedgehog property running sequential generative tests on the model and
  the C#-implemented `CircularBuffer` to verify that they behave the same

## References

Translated from [Oskar Wickström](https://github.com/owickstrom)'s
[example](https://github.com/owickstrom/hedgehog-inline-java-testing):

>The example used, the circular buffer and its model, is based on the paper
>[Experiences with QuickCheck: Testing the Hard Stuff and Staying
>Sane](http://publications.lib.chalmers.se/records/fulltext/232550/local_232550.pdf),
>by John Hughes. In the original paper the code under test in written C, and the
>tests in Erlang and Quviq QuickCheck; in this example testing a Java
>implementation using Haskell and the Hedgehog library.

That example is licensed under BSD3, and so is this code.

## Build Requirements

This has only been tested and hacked on using Stack and .NET Framework. Adding
support for .NET Core in `clr-inline` is something I consider working on.

Make sure `csc.exe` and `fsc.exe` are in PATH; for the Community edition of
Microsoft Visual Studio this means:

* C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\MSBuild\15.0\bin\Roslyn
* C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\Common7\IDE\CommonExtensions\Microsoft\FSharp

You should be able to run the following using a recent Stack version:

```
$ stack build --test
hedgehog-inline-csharp-testing-0: unregistering (local file changes: README.md)
hedgehog-inline-csharp-testing-0: configure (lib + test)
Configuring hedgehog-inline-csharp-testing-0...
hedgehog-inline-csharp-testing-0: build (lib + test)
Preprocessing library for hedgehog-inline-csharp-testing-0..
Building library for hedgehog-inline-csharp-testing-0..
[1 of 1] Compiling CircularBuffer <...>
Microsoft (R) Visual C# Compiler version 2.9.0.63208 (958f2354)
Copyright (C) Microsoft Corporation. All rights reserved.

Preprocessing test suite 'test' for hedgehog-inline-csharp-testing-0..
Building test suite 'test' for hedgehog-inline-csharp-testing-0..
[1 of 1] Compiling Main <...>
Linking .stack-work\dist\5c8418a7\build\test\test.exe ...
hedgehog-inline-csharp-testing-0: copy/register
Installing library in <...>
Registering library for hedgehog-inline-csharp-testing-0..
hedgehog-inline-csharp-testing-0: test (suite: test)

━━━ Main ━━━
  ✓ prop_circular_buffer_sequential passed 25 tests.
  ✓ 1 succeeded.

hedgehog-inline-csharp-testing-0: Test suite test passed
Completed 2 action(s).
```
