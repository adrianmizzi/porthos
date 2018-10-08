pragma solidity ^0.4.24;

import "./PorthosContract.sol";

contract T2 is PorthosContract {

    address alice = 0xe9ff96cd80f97a9a27ca7b8a19296b8c55885dd7;
    address bob = 0xf927acf6405b0a5153a064f6933971320f234da0;
    address charlie = 0xaa4b4a309b7c50eae56b61835cd7ecaa21b3ea17;

    constructor() public
    {
        emit LogEvent("starting c2");
        emit LogEvent("starting c1");
        semaphore["semRight1"] = false;
        semaphore["semLeft1"] = false;
        openGate("c1");
        openGate("c2");
        setContractStatus(5);
        eurAssetRegistry.issueAssets(alice, 100);
        eurAssetRegistry.issueAssets(bob, 100);
        eurAssetRegistry.issueAssets(charlie, 100);
    }

    function continue_1() private
    {
      if(!semaphore["semLeft1"])
        return;
      if(!semaphore["semRight1"])
        return;
      openGate("c3");
      emit LogEvent("ready");
      setContractStatus(2);
    }

    function c3_commit(string _assetType, uint _quantity, address _recipient) public
    {
        require(contractStatus == 2);
        require(msg.sender == charlie); //charlie
        addCommitment(Commitment({tagId: "",
                                  sender: msg.sender,
                                  recipient: _recipient,
                                  assetType: _assetType,
                                  quantity: _quantity,
                                  status: 0}));
        setContractStatus(3);
    }

    function c3_timeout() public
    {
        require(contractStatus == 2);
        require(now > 200);
        setContractStatus(4);
    }

    function c1_commit(string _assetType, uint _quantity, address _recipient) public
    {
        if (!isGateOpen("c1"))
          return;

        require(contractStatus == 5 || (contractStatus == 8 && !semaphore["semLeft1"]));
        require(msg.sender == bob); //bob
        setContractStatus(6);
        semaphore["semLeft1"] = true;
        addCommitment(Commitment({tagId: "",
                                  sender: msg.sender,
                                  recipient: _recipient,
                                  assetType: _assetType,
                                  quantity: _quantity,
                                  status: 0}));
        continue_1();
    }

    function c1_timeout() public
    {
        require(contractStatus == 5);
        require(now > 200);
        semaphore["semLeft1"] = true;
        setContractStatus(7);
        continue_1();
    }

    function c2_commit(string _assetType, uint _quantity, address _recipient) public
    {
        require(contractStatus == 5 || (contractStatus == 6 && !semaphore["semRight1"]));
        require(msg.sender == alice); //alice
        setContractStatus(8);
        semaphore["semRight1"] = true;
        addCommitment(Commitment({tagId: "",
                                  sender: msg.sender,
                                  recipient: _recipient,
                                  assetType: _assetType,
                                  quantity: _quantity,
                                  status: 0}));
        continue_1();
    }

    function c2_timeout() public
    {
        require(contractStatus == 5);
        require(now > 200);
        semaphore["semRight1"] = true;
        setContractStatus(9);
        continue_1();
    }
}
