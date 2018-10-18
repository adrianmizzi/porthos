pragma solidity ^0.4.24;

import "../framework/Gateway.sol";

contract T1 {

	Gateway private gateway;

	constructor (address gatewayAddress) public {
		gateway = Gateway(gatewayAddress);
	}

	function start() public {
		gateway.fireMessage("Starting T1 - sending a call back request");
		gateway.requestCallback(this, "resume()", block.number + 2);
	}

	function initiateCrossChainCall(string system, address c) public {
		gateway.initiateCCC(system, c, "resume2()");
	}

	function resume() public {
		gateway.fireMessage("Resume() function called");
	}

	function resume2() public {
		gateway.fireMessage("Resume2() function called");
	}

	function ping() public {
		gateway.fireMessage("Ping");
	}
}