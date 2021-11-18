# utxosAt PAB testing

We implemented this smal contract to test and show what we think is a wrong
behavior with the `utxoAt` function. The function is returning sometimes the
complete list of “utxos” for a given address, including the utxos that were
already spent.

Ideally this contract is designed to show a strange behavior with the utxosAt
function, so we want always to have a single utxo. For that we should change the
integer on the validator each time we activate the contract; other way we could
endup with more than one utxo for a given address, and that will break the spend
and spend-pay actions.

# Other documents

- public testnet log:
- private testnet log:

# Building and running

`$> cabal build contract`
`$> cabal exec -- contract --config pab-config.yml migrate`
`$> cabal exec -- contract --config pab-config.yml webserver --passphrase [PASS]`

# Actions

- utxos: List all the utxos of the started contract.
- spend: Consumes the only utxo.
- spend-pay: Consumes the only utxo and replicate it.
