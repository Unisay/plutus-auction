# Auction Validator example written in PlutusTx

The command

```
cabal run blueprint
```

produces two files:

- `validator/validator.uplc` containing validator's UPLC code.
- `validator/validator.json` containing the CIP-57 Blueprint.

Alternatively, 

```
nix run ".#blueprint"
```
