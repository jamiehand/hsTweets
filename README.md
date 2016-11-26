# hsTweets

## To run

In command line:

```
$ cd hsTweets
$ cabal sandbox init
$ cabal install spock twitter-conduit
$ cabal exec ghci
```

## Basic functionality of each module:

### Main

```
Prelude> :l Main.hs
*Main> main
```

In a browser:
`localhost:8080/search/hello%20there`

### TwSearch

```
Prelude> :l TwSearch
*TwSearch> searchContent 10 "obama"
```

## TODO

- Add search box
