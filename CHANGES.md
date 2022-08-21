## Changes
- del ./plutus-contract/test
- remove all PlutusTx dependencies
- everything under `Plutus.Contract.StateMachine`
- everything under `Plutus.Contract.Test`
- remove interpretations for Effects in `Plutus.Contract.Types`
- everything under `Plutus.Contract.Trace`
- everything under `Plutus.Contract.Wallet` 
  (FIXME: remove imports in `Plutus.Trace.Emulator.Extract`/ `Plutus.Emulator.Wallet`)
- everything in `Plutus.Trace.*` (remove imports from   `Wallet.Emulator.{Stream|Wallet|MultiAgent|Folds}`)
- Wallet.API
