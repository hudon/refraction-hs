# Refraction

## Why do we need better privacy in bitcoin?

Greg Maxwell explains it well in his proposal for Confidential Transactions [3]:

> Insufficient financial privacy can have serious security and privacy
> implications for both commercial and personal transactions. Without
> adequate protection, thieves and scammers can focus their efforts on
> known high-value targets, competitors can learn business details, and
> negotiating positions can be undermined. Since publishing often requires
> spending money, lack of privacy can chill free speech.  Insufficient
> privacy can also result in a loss of fungibility--where some coins are
> treated as more acceptable than others--which would further undermine
> Bitcoin's utility as money.
> 
> Bitcoin partially addresses the privacy problem by using pseudonymous
> addresses. If someone does not know which users own which addresses,
> the privacy impact is reduced. But any time you transact with someone
> you learn at least one of their addresses. From there, you can trace out
> other connected addresses and estimate the values of their transactions
> and holdings. For example, suppose your employer pays you with Bitcoin
> and you later spend those coins on your rent and groceries. Your landlord
> and the supermarket will both learn your income, and could charge you
> higher prices as your income changes or target you for theft.


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
