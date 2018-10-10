pragma solidity ^0.4.24;

import "./PorthosContract.sol";

contract T2 is PorthosContract {

  address alice = 0xf1c13a88cf28c4d06269a150dd8cdb2e3061d44f;
  address bob = 0x4603bd7000aba82eab4aaea605df43a1e37ef2bb;
  address charlie = 0xb9f62ffe791ff9fa9c51722e3b833bf51db290de;

function continue_1() private
{
  if(!semaphore["semLeft1"])
    return;
  if(!semaphore["semRight1"])
    return;
  setContractStatus(2);
  emit LogEvent("ready");
  openGate("c3");
}

function c3_commit(string _assetType, uint _quantity, address _recipient) public
{
  if(msg.sender != charlie)
    return;
  if(_recipient != bob)
    return;
  if(!isGateOpen("c3"))
    return;
  addCommitment(Commitment({tagId: "", sender: msg.sender, recipient: _recipient, assetType: _assetType, quantity: _quantity, status: 0}));
  setContractStatus(3);
  closeGate("c3");
  releaseCommitments(getAllCommitments());
}

function c3_timeout() public
{
  if(block.number < 2)
    return;
  if(!isGateOpen("c3"))
    return;
  setContractStatus(4);
  closeGate("c3");
  cancelCommitments(getAllCommitments());
}

constructor() public
{
  setContractStatus(5);
  semaphore["semLeft1"] = false;
  semaphore["semRight1"] = false;
  emit LogEvent("starting c1");
  openGate("c1");
  emit LogEvent("starting c2");
  openGate("c2");
}

function c1_commit(string _assetType, uint _quantity, address _recipient) public
{
  if(msg.sender != bob)
    return;
  if(_recipient != alice)
    return;
  if(!isGateOpen("c1"))
    return;
  addCommitment(Commitment({tagId: "", sender: msg.sender, recipient: _recipient, assetType: _assetType, quantity: _quantity, status: 0}));
  setContractStatus(6);
  closeGate("c1");
  semaphore["semLeft1"] = true;
  continue_1();
}

function c1_timeout() public
{
  if(block.number < 200)
    return;
  if(!isGateOpen("c1"))
    return;
  setContractStatus(7);
  closeGate("c1");
  semaphore["semLeft1"] = true;
  continue_1();
}

function c2_commit(string _assetType, uint _quantity, address _recipient) public
{
  if(msg.sender != alice)
    return;
  if(_recipient != bob)
    return;
  if(!isGateOpen("c2"))
    return;
  addCommitment(Commitment({tagId: "", sender: msg.sender, recipient: _recipient, assetType: _assetType, quantity: _quantity, status: 0}));
  setContractStatus(8);
  closeGate("c2");
  semaphore["semRight1"] = true;
  continue_1();
}

function c2_timeout() public
{
  if(block.number < 200)
    return;
  if(!isGateOpen("c2"))
    return;
  setContractStatus(9);
  closeGate("c2");
  semaphore["semRight1"] = true;
  continue_1();
}

}
