# Refraction
## Decentralized Bitcoin Mixing Service

Refraction is sybil-resistant mixing for Bitcoin.

The goal is to fully implement the Xim protocol explained in [0] which depends on the FairExchange protocol [1].

There is a JavaScript prototype implementation linked in the references below. [2]


## User Guide

```
> refraction
Enter source private key:
Enter destination address:
info: source private key valid
info: destination address valid
info: mixing X BTC with Xim delta=Y
info: starting Xim(delta)
info: starting Discover()
info: Advertising with|Responding to onion address:
info: FairExchange()
info: done! Mixed Z BTC now deposited in address:
>
```

## Developer Guide

### Building

    > stack build

### Testing

    > hlint .
    > stack test

### Running

    > stack exec refraction-exe

### Releasing

### Developer Resources

- Haskell: https://haskell-lang.org/get-started
- Bitcoin Wiki: https://en.bitcoin.it/wiki/Main_Page


## References


[0] http://forensics.umass.edu/pubs/bissias.wpes.2014.pdf

[1] http://elaineshi.com/docs/bitcoin.pdf

[2] https://github.com/hudon/refraction
