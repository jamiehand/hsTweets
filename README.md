# hsTweets

## To run

In command line:

```
$ cd hsTweets
$ cabal sandbox init
$ cabal install happstack-lite twitter-conduit
$ cabal exec ghci
```

## Basic functionality of each module:

### Main

```
Prelude> :l Main.hs
*Main> main
```

In a browser:  
[localhost:8080/index](localhost:8080/index)

[localhost:8080/search?term=hello%20there](localhost:8080/search?term=hello%20there)

[localhost:8080/about](localhost:8080/about)

[localhost:8080/badlink](localhost:8080/badlink)


## To test:

* On [localhost:8080/search](localhost:8080/search), submit the following queries into the form,
  and make sure they produce pages:
  * These pages should have a `search` form at top and results underneath:
    * basic search: `hello`
    * search with space: `hello world`
  * This page should (likely) have a `search` form at top and "There are no
    recent results..." underneath:
    * search with no results: `as;dlkfjaselrjkas;dlfkjas;dlfkj`
  * This page should have a `search` form at top and "Please enter a
    search term." underneath:
    * search the empty string: ""

* Visit the following URLs, which test the same things as above using
  query params:
  * basic: [localhost:8080/search?term=hello](localhost:8080/search?term=hello)
  * with a space: [localhost:8080/search?term=hello%20there](localhost:8080/search?term=hello%20there)
  * no results: [localhost:8080/search?term=as;dlkfjaselrjkas;dlfkjas;dlfkj](localhost:8080/search?term=as;dlkfjaselrjkas;dlfkjas;dlfkj)
  * empty query: [localhost:8080/search?term=](localhost:8080/search?term=)

* [localhost:8080/badlink](localhost:8080/badlink) should lead to a Not Found page.
* [localhost:8080/about](localhost:8080/about) should lead to our About page.
