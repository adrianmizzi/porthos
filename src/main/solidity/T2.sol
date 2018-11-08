pragma solidity ^0.4.24;

import "./PorthosContract.sol";

contract T2Chain1Eur is PorthosContract {
  address alice;
  address bob;
  address charlie;

constructor(address _alice, address _bob, address _charlie, address _notary) public
{
  alice = _alice;
  bob   = _bob;
  charlie = _charlie;
  notary = NotaryConnector(_notary);

  setContractStatus(5);
  semaphore["semLeft1"] = false;
  semaphore["semRight1"] = false;
  fireEvent("starting c1");
  openGate("c1");
  fireEvent("starting c2");
  openGate("c2");
}


function continue_1() private
{
  if(!semaphore["semLeft1"])
    return;
  if(!semaphore["semRight1"])
    return;
  setContractStatus(2);
  fireEvent("ready");
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


function c1_commit(string _assetType, uint _quantity, address _recipient) public
{
  if(msg.sender != bob)
    return;
  if(_recipient != alice)
    return;
  if(!isGateOpen("c1"))
    return;
  fireEvent("C1 Commit in progress");
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
