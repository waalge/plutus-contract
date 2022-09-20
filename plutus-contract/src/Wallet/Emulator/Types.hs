--FIXME: this can go module reexports that didn't go
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Wallet.Emulator.Types(
    -- * Wallets
    Wallet.Emulator.Wallet.Wallet(..),
    Wallet.Emulator.Wallet.WalletId(..),
    Crypto.XPrv,
    Crypto.XPub,
    Wallet.Emulator.Wallet.mockWalletPaymentPubKey,
    Wallet.Emulator.Wallet.mockWalletPaymentPubKeyHash,
    Wallet.Emulator.Wallet.knownWallets,
    Wallet.Emulator.Wallet.knownWallet,
    Ledger.CardanoWallet.WalletNumber(..),
    Ledger.CardanoWallet.toWalletNumber,
    Wallet.Emulator.Wallet.fromWalletNumber,
    Ledger.CardanoWallet.MockWallet(..),
    Wallet.Emulator.Chain.TxPool,
    -- * Emulator
    Wallet.Emulator.MultiAgent.Assertion(OwnFundsEqual, IsValidated),
    Wallet.Emulator.MultiAgent.assert,
    Wallet.Emulator.MultiAgent.assertIsValidated,
    Plutus.Contract.Error.AssertionError(..),
    Plutus.Contract.Error.AsAssertionError(..),
    Wallet.Emulator.NodeClient.ChainClientNotification(..),
    Wallet.Emulator.MultiAgent.EmulatorEvent,
    Wallet.Emulator.MultiAgent.EmulatorEvent',
    Wallet.Emulator.MultiAgent.EmulatorTimeEvent(..),
    -- ** Wallet state
    Wallet.Emulator.Wallet.WalletState(..),
    Wallet.Emulator.Wallet.emptyWalletState,
    Wallet.Emulator.Wallet.ownPaymentPrivateKey,
    Wallet.Emulator.Wallet.ownAddress,
    -- ** Traces
    Wallet.Emulator.MultiAgent.walletAction,
    Wallet.Emulator.MultiAgent.assertion,
    Wallet.Emulator.MultiAgent.assertOwnFundsEq,
    Wallet.Emulator.MultiAgent.ownFundsEqual,
    -- * Emulator internals
    Wallet.Emulator.MultiAgent.EmulatorState(..),
    Wallet.Emulator.MultiAgent.emptyEmulatorState,
    Wallet.Emulator.MultiAgent.emulatorState,
    Wallet.Emulator.MultiAgent.emulatorStatePool,
    Wallet.Emulator.MultiAgent.emulatorStateInitialDist,
    Wallet.Emulator.Chain.txPool,
    Wallet.Emulator.MultiAgent.walletStates,
    Wallet.Emulator.Chain.index,
    Wallet.Emulator.MultiAgent.chainState,
    Wallet.Emulator.Chain.currentSlot,
    Wallet.Emulator.MultiAgent.fundsDistribution,
    Wallet.Emulator.MultiAgent.emLog,
    Wallet.Emulator.Wallet.selectCoin
    ) where

import Cardano.Crypto.Wallet qualified as Crypto

import Ledger.CardanoWallet qualified
import Plutus.Contract.Error (AssertionError)
import Plutus.Contract.Error qualified
import Wallet.Emulator.Chain qualified
import Wallet.Emulator.MultiAgent (EmulatorEvent', EmulatorState, chainState)
import Wallet.Emulator.MultiAgent qualified
import Wallet.Emulator.NodeClient qualified
import Wallet.Emulator.Wallet qualified
