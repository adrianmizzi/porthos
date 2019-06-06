# Porthos: A Macroprogramming and Interoperability Framework for Blockchains

Porthos is a macroprogramming language and framework for interoperability of Blockchain systems.  This repository can be used to write a single-contract in the Porthos language to define a multi-chain dapp.  The blockchain systems which are supported in this version include:

* Ethereum
* Hyperledger

## Getting Started

### Prerequisites

* Docker
* Ganache

## Porthos Smart Contracts

Porthos is a domain specific language embedded in Haskell.  Porthos smart contracts look like Haskell programs which use a set of domain specific operators.  Porthos smart contracts are first compiled into smart contracts which can be loaded on the target blockchain systems (e.g. Ethereum, Hyperledger).  These smart contracts are then compiled and deployed using the Porthos framework.

### Writing and Compiling a Porthos Smart Contract

The following example is a swap contract written in Porthos where two participants swap assets which reside on different blockchain systems.

```
mem :: ChainToLang
mem = update (update (update initMem ("Ethereum_1", Solidity)) ("Ethereum_2", Solidity)) ("Hyperledger", Chaincode)


data MyAssetType = Apple | Orange
  deriving (Show, Eq)

instance AssetType MyAssetType where
  chainOf Apple = "Ethereum_1"
  chainOf Orange = "Hyperledger"

-- Atomic Swap
swap :: (Participant, Asset MyAssetType) -> (Participant, Asset MyAssetType) -> Contract
swap (p1, a1) (p2, a2) = onUserCommit "p1Commit" (Apple, (isCommitTo p2 .&. isAsset a1))
                           doP2Commit 
                           (onTimeout 10 end)
  where
    doP2Commit = onUserCommit "p2Commit" (Orange, (isCommitTo p1 .&. isAsset a2 .&. isCommitBy p2))
                    (releaseAll end)
                    (onTimeout 20 (cancelAll end))
```


The first stage compilation is made using Haskell (e.g. ghci) and this generates smart contracts which will then be loaded on the target blockchain systems.  In the above example, Apples are located on Ethereum_1 and Oranges on Hyperledger - so this contract, when compiled will generate two smart contracts: one for Ethereum_1 in Solidity and one for Hyperledger in GoLang.

To generate the smart contracts, in the Haskell environment execute:

```
toScreen mem (swap (alice, asset(Apple, 1)) (bob, asset (Orange, 2)))
```

alternatively 

```
toFiles mem (swap (alice, asset(Apple, 1)) (bob, asset (Orange, 2)))
```

### Compile and deploy Solidity-on-Ethereum smart contracts

Solidity smart contracts need to be compiled before being deployed on an Ethereum blockchain system.  In this project, we use Web4j to connect our framework (written in Java) to the Ethereum platform.  Smart contracts are first compiled (Java code is generated) and then deployed programmatically via the Java application.  

In the scripts folder, you can find a tool that takes the ".sol" files and generates Java code.

```
ethereum/buildContracts.sh
```

We can now write a Java application that deploys the smart contract onto the Ethereum blockchain and uses the on-chain framework to manage communication with the contracts.

### Compile and deploy GoLang-on-Hyperledger smart contracts

The automatically generated GoLang chaincode can be deployed directly to the Hyperledger blockchain instance.  

Deploy a new instance of Hyperledger using 

```
./teardown.sh
./build.sh
```

Then from the Java code, run one after the other:

* CreateChannel.java - to create a channel in the hyperledger instance
* DeployInstantiateChaincode.java - to deploy the automatically generated chaincode.  Update ConfigPorthos.java to ensure the correct details for the chaincode being deployed.
* InvokeQuery.java - customise and execute InvokeQuery.java according to the contract methods available
* HyperledgerEventListener.java - listens for chain events to react accordingly

## Deployment

The scripts provided launch two Ethereum instances (Ethereum_1 on port 7545 and Ethereum_2 on port 6545) and a Hyperledger instance.

