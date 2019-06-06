# Porthos: Blockchain Scripts

These scripts can be used to launch a number of blockchain systems for Porthos applications to run on.  The included scripts can load Ethereum and Hyperledger blockchain instances.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

### Prerequisites

* Ganache
* Docker

### Launching Ethereum

In the Ethereum folder, 2 scripts are available to launch two different instances of the Ethereum blockchain system.  The systems are loaded using Ganache.

```
./runChain1.sh
./runChain2.sh
```

Chain1 can be accessed on port 7545, and Chain2 can be accessed on port 6545.

## Launching Hyperledger

In the Hyperledger folder, a build script launches a Hyperledger instance made up of 2 organisations, and 2 peers in every organisation.


```
./build.sh
```

To stop and teardown the hyperledger instance, use ./stop.sh and ./teardown.sh respectively.


## Author

* **Adrian Mizzi** - *Initial work* - [Porthos](https://github.com/adrianmizzi/porthos-1)

