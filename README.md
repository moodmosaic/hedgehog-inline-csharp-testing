# hedgehog-inline-java-testing

_Example of using
[haskell-hedgehog](https://github.com/hedgehogqa/haskell-hedgehog)'s
model-based state machine testing together with
[inline-java](https://github.com/tweag/inline-java)._

## Points of Interest

* The code under test, a circular buffer implemented in Java:
[CircularBuffer.java](src/main/java/example/CircularBuffer.java)
* The code wrapping our circular buffer class, providing a Haskell API:
[CircularBuffer.hs](src/main/haskell/CircularBuffer.hs)
* The tests, [Main.hs](src/test/haskell/Main.hs), including:
  - A simplified model of a circular buffer (the `Model` data type)
  - The commands of the API (`New`, `Get`, `Put`, `Size`), with generators, pre
  and post-conditions, and model update functions
  - A Hedgehog property running sequential generative tests on the model and
  the Java-implemented `CircularBuffer` to verify that they behave the same

## References

The example used, the circular buffer and its model, is based on the paper
[Experiences with QuickCheck: Testing the Hard Stuff and Staying
Sane](http://publications.lib.chalmers.se/records/fulltext/232550/local_232550.pdf),
by John Hughes. In the original paper the code under test in written C, and the
tests in Erlang and Quviq QuickCheck; in this example testing a Java
implementation using Haskell and the Hedgehog library.

The build-related code is based on the examples from `inline-java`:

* https://github.com/tweag/inline-java/tree/master/examples/hello
* https://github.com/tweag/inline-java/tree/master/examples/classpath

Those examples are licensed under BSD3, and so is this code.

## Build Requirements

This has only been tested and hacked on using Nix. You should be able to run
the following using a recent nixpkgs revision:

```sh
$ nix-shell
$ cabal configure --enable-tests
$ cabal test
...

Running 1 test suites...
Test suite hedgehog-inline-java-testing: RUNNING...
Test suite hedgehog-inline-java-testing: PASS
Test suite logged to:
dist/test/hedgehog-inline-java-testing-0.1-hedgehog-inline-java-testing.log
1 of 1 test suites (1 of 1 test cases) passed.
```

You may be able to build and run without Nix, using a new version of Cabal or
Stack, but may also have troubles with native dependencies (`libjvm` in
particular). In any case, make sure you have OpenJDK and Gradle installed.
Good luck!
