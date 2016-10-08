var bitcore = require('bitcore');
console.log('hello');


// use bx ec-new -c data/bx-testnet.cfg
// bx ec-to-wif <ec key>
var privateKey = new bitcore.PrivateKey('cQrgind4kVZbZpfAVfZq8Nw6HcPBZyT2pktrn2t5dSS7H9aeNFmx');
// bx ec-to-public <ec key>
// bx ec-to-address <ec public key>
// moCvBdctTGGBwquWx647GvQWAsr4XBQBXh

// use haskoin faucet to get some coins http://faucet.xeno-genesis.com/
// use https://testnet.blockexplorer.com/ to find the faucet transactions
var utxo = {
  "txId" : "1c7a7f9488d330c60363c2ceed8417c8b44bdcf131a9e53a798d59adaa0f1e44",
  "outputIndex" : 1,
  "address" : "moCvBdctTGGBwquWx647GvQWAsr4XBQBXh",
// get the script from going to https://testnet.blockexplorer.com/tx/<tx hash> and looking for
// the script in the right output. It's probably not the first output
  "script" : "76a9145457c1cbd45710c749b7aba24f9d9e97382893d588ac",
// haskoin faucet gives 0.001 btc (100000 satoshis)
  "satoshis" : 100000
};


var transaction = new bitcore.Transaction()
      .from(utxo)
  // this is the message refraction currently looks for
      .addData("RFRCTN")
      .change("moCvBdctTGGBwquWx647GvQWAsr4XBQBXh")
      .sign([privateKey])

var serialized = transaction.toString();
// use https://testnet.blockexplorer.com/tx/send to broadcast
console.log(serialized);
