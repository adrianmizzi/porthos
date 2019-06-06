// pragma solidity ^0.4.24;
pragma solidity ^0.5.4;

import "../framework/PorthosContract.sol";

contract Ethereum_2 is PorthosContract {
  address alice;
  address bob;
  address charlie;

  address dave;
  address erin;

  constructor(address _alice, address _bob, address _charlie, address _gateway, string _blockchainName)
    PorthosContract (_blockchainName) public
  {
    alice   = _alice;
    bob     = _bob;
    charlie = _charlie;
    gateway = Gateway(_gateway);
  }

  function votebob_commit(string _assetType, uint _quantity, address _recipient) public
  {
    if(msg.sender != bob)
      return;
    if(!isGateOpen("votebob"))
      return;
    addCommitment(Commitment({tagId: "", sender: msg.sender, recipient: _recipient, assetType: _assetType, quantity: _quantity, status: 0}));
    closeGate("Vote Ethereum_2", "votebob", false);
  }

  function votebob_timeout() public
  {
    if(block.number < 200)
      return;
    if(!isGateOpen("votebob"))
      return;
    closeGate("Vote Ethereum_2", "votebob", true);
  }

  function paybob_commit(string _assetType, uint _quantity, address _recipient) public
  {
    if(msg.sender != bob)
      return;
    if(_recipient != dao)
      return;
    if(!isGateOpen("paybob"))
      return;
    addCommitment(Commitment({tagId: "", sender: msg.sender, recipient: _recipient, assetType: _assetType, quantity: _quantity, status: 0}));
    closeGate("Currency GBP", "paybob", false);
  }

  function paybob_timeout() public
  {
    if(block.number < 200)
      return;
    if(!isGateOpen("paybob"))
      return;
    closeGate("Currency GBP", "paybob", true);
  }

}