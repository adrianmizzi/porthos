pragma solidity ^0.4.24;

contract NotaryConnector {
  event LogEvent(string message, uint blockNumber);

  function fireTestMessage() public {
  	emit LogEvent("Hello World", block.number);
  }

  function fireMessage(string _message) public {
  	emit LogEvent(_message, block.number);
  }
}