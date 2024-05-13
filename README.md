# craptography

A very silly program for obscuring messages inspired by [Caesar cipher](https://en.wikipedia.org/wiki/Caesar_cipher) and [bqn](https://www.aplwiki.com/wiki/BQN). It works by rotating alphanumeric characters according to the [Fibonacci sequence](https://en.wikipedia.org/wiki/Fibonacci_sequence).

```
apl -> bqn
```

## Development

### Requirements

- [`stack`](https://docs.haskellstack.org/en/stable/) version `>=2.13`

### installation

The best way to install and manage Haskell tooling is probably [`ghcup`](https://www.haskell.org/ghcup/). Although `stack` will install it's own version of `ghc` you may want to also install a system wide version. This project is using `ghc 9.4.8`. The `haskell-language-server` can also be installed the same way.

Then clone this repo `cd` into it and run one of the following commands:

- `stack run` to run the project (dependencies will be installed automatically)
- `stack run --` if you need to pass command line options to the project not `stack`
- `stack test` to run the tests in `test/Spec.hs`
- `stack build` to create a binary

The project haskell code is formatted using  [`ormolu`](https://hackage.haskell.org/package/ormolu) with default settings.

## Usage

Currently the project does not provide binary releases so it will need to be downloaded and built or run with `stack` as per development.

```sh
$ stack run -- --help

 Usage: craptography-exe <options>


 Options:
   --decrapt | -d        - Runs the encraption in reverse
   --input   | -i        - Path to file you would like to encrapt
             :             if not specified reads from stdin
   --output  | -o        - Path to file you would like to the write result
             :             if not specified writes to stdout
   --help    | -h        - Show this help text


 Examples:
   $ craptography-exe -i diaryEntry.txt -o secretMessage.txt
   $ craptography-exe -d -i secretMessage.txt -o message.txt
   $ cat diaryEntry.txt | craptography-exe -o secretMessage.txt
   $ craptography-exe -d -i secretMessage.txt
```
