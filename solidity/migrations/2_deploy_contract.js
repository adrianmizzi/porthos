const FungibleAsset = artifacts.require("./FungibleAsset.sol")
const PorthosContract = artifacts.require("./PorthosContract.sol")
const T2 = artifacts.require("./T2.sol")
const AssetRegisterInterface = artifacts.require("./AssetRegisterInterface.sol")
const NotaryConnector = artifacts.require("./NotaryConnector.sol")

module.exports = function(deployer, entwork, accounts) {
	deployer.deploy(NotaryConnector).then(function() {
	  return deployer.deploy(T2, accounts[1], accounts[2], accounts[3], NotaryConnector.address);
	});
}



