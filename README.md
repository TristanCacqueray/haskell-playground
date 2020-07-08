# Haskell playground

This project contains some standalone haskell source code file to demonstrate basic usage.

Install the dependencies and start the REPL like so:

```ShellSession
$ stack install .
$ stack ghci
Prelude> :set prompt "λ> "
λ> :load ZuulStatus
λ> :t uuid
uuid :: Job -> Maybe Text
```

## hcurl

```ShellSession
./hcurl.hs --help
HCurl - Network.HTTP.Req playground

Usage: hcurl.hs [--insecure] --url TEXT

Available options:
  -h,--help                Show this help text
```

## zuul-status

Demonstrate aeson generic fromJSON decoder to process zuul-ci status data


## bugzilla-demo

Use hsbugzilla to predict bug creation time