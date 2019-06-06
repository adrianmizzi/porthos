// pragma solidity ^0.4.24;
pragma solidity ^0.5.4;

import "framework/PorthosContract.sol";

contract Ethereum_1 is PorthosContract {
  address alice;
  address bob;
  address charlie;

  address dave;
  address erin;

  constructor(address _alice, address _bob, address _charlie, address _gateway, string memory _blockchainName)
    PorthosContract (_blockchainName) public
  {
    alice   = _alice;
    bob     = _bob;
    charlie = _charlie;
    gateway = Gateway(_gateway);
  }

  function continue_1() private
  {
    if(!semaphore["semLeft1"])
      return;
    if(!semaphore["semRight1"])
      return;
    releaseAllCommitments();
    sendAssets(maxOf(groupBySender(filterCommitmentsByAsset("Vote Ethereum_1", getAllCommitments()))));
  }

  function continue_2() private
  {
    if(!semaphore["semLeft2"])
      return;
    if(!semaphore["semRight2"])
      return;
    semaphore["semLeft1"] = false;
    semaphore["semRight1"] = false;
    openGate("Vote Ethereum_1", "votealice");
    openGate("Vote Ethereum_2", "votebob");
  }

  function votealice_commit(string memory _assetType, uint _quantity, address _recipient) public
  {
    if(msg.sender != alice)
      return;
    if(!isGateOpen("votealice"))
      return;
    addCommitment(Commitment({tagId: "", sender: msg.sender, recipient: _recipient, assetType: _assetType, quantity: _quantity, status: 0}));
    closeGate("Vote Ethereum_1", "votealice", false);
    semaphore["semLeft1"] = true;
    continue_1();
  }

  function votealice_timeout() public
  {
    if(block.number < 200)
      return;
    if(!isGateOpen("votealice"))
      return;
    closeGate("Vote Ethereum_1", "votealice", true);
    cancelAllCommitments();
    semaphore["semLeft1"] = true;
    continue_1();
  }

  function votebob_complete_onuseraction() public
  {
    if(tx.origin != owner)
      return;
    semaphore["semRight1"] = true;
    continue_1();
  }

  function votebob_complete_ontimeout() public
  {
    if(tx.origin != owner)
      return;
    semaphore["semRight1"] = true;
    continue_1();
  }

  function start() public
  {
    semaphore["semLeft2"] = false;
    semaphore["semRight2"] = false;
    openGate("Currency EUR", "payalice");
    openGate("Currency GBP", "paybob");
  }

  function payalice_commit(string memory _assetType, uint _quantity, address _recipient) public
  {
    if(msg.sender != alice)
      return;
    if(_recipient != dao)
      return;
    if(!isGateOpen("payalice"))
      return;
    addCommitment(Commitment({tagId: "", sender: msg.sender, recipient: _recipient, assetType: _assetType, quantity: _quantity, status: 0}));
    closeGate("Currency EUR", "payalice", false);
    semaphore["semLeft2"] = true;
    continue_2();
  }

  function payalice_timeout() public
  {
    if(block.number < 200)
      return;
    if(!isGateOpen("payalice"))
      return;
    closeGate("Currency EUR", "payalice", true);
    cancelAllCommitments();
    semaphore["semLeft2"] = true;
    continue_2();
  }

  function paybob_complete_onuseraction() public
  {
    if(tx.origin != owner)
      return;
    semaphore["semRight2"] = true;
    continue_2();
  }

  function paybob_complete_ontimeout() public
  {
    if(tx.origin != owner)
      return;
    semaphore["semRight2"] = true;
    continue_2();
  }

}