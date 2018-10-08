const FungibleAsset = artifacts.require("./FungibleAsset.sol")
const PorthosContract = artifacts.require("./PorthosContract.sol")
const T2 = artifacts.require("./T2.sol")

module.exports = function(deployer) {
  // deployer.deploy(FungibleAsset);
  // deployer.deploy(PorthosContract);
	deployer.deploy(T2);
};
