# Refraction

[![Build Status](https://travis-ci.org/hudon/refraction-hs.svg?branch=hudon-travis)](https://travis-ci.org/hudon/refraction-hs)

## Decentralized Bitcoin Mixing Service

Refraction is an application that allows you to have complete financial privacy with your Bitcoin. It is decentralized and peer-to-peer. All it relies on is the Bitcoin network and the users running Refraction. It is resistant to Sybil and timing attacks, with configurable parameters to opt for more or less security depending on your time constraints.

It is an implementation of the Xim protocol explained in [0] which depends on the FairExchange protocol [1].


## User Guide

Before running refraction, you need a running instance of Tor with version >= 0.2.7.1-alpha.

Then, refraction will ask you for a prvkey it should sweep funds from, and an output
address it should direct mixed funds to.

```
> refraction
Enter source private key:
Enter destination address:

info: source private key valid
info: destination address valid
info: mixing X BTC
info: starting round: 1
info: finding exchange peers, please wait...
info: peer found!
info: mixing, please wait...
info: done! Mixed Z BTC now deposited in address: <address>
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
    > stack exec refraction-exe -- -b  # run as bob (fairexchange server)
    > stack-exec refraction-exe -- -i  # ignore input key/address validation

### Releasing

### Developer Resources

- Haskell: https://haskell-lang.org/get-started
- Bitcoin Wiki: https://en.bitcoin.it/wiki/Main_Page


### Roadmap

1. Create Xim MVP over Tor
2. Move to using a trusted full node rather than a third party block explorer

## References


[0] http://forensics.umass.edu/pubs/bissias.wpes.2014.pdf (xim)

[1] http://elaineshi.com/docs/bitcoin.pdf (section 7 is the relevant part -- FairExchange)

[2] https://people.xiph.org/~greg/confidential_values.txt
