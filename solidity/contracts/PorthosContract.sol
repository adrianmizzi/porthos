pragma solidity ^0.4.24;

import "./FungibleAsset.sol";
import "./strings.sol";

contract PorthosContract {
  using strings for *;

  struct Commitment {
      string tagId;
      address sender;
      address recipient;
      string assetType;
      uint quantity;
      uint status; // 0 = open; 1 = released; 2 = cancelled
  }

  event LogEvent(string message);
  mapping (string => bool) internal semaphore;
  mapping (string => uint8) private gateStatus;
  uint internal contractStatus = 0;

  Commitment[] commitments;

  mapping (string => address) private assetRegisters;

  FungibleAsset eurAssetRegistry;

  address owner;

  constructor () public {
    owner = msg.sender;
    eurAssetRegistry = new FungibleAsset(this);
  }

  function issueAsset(string assetType, uint quantity, address recipient) public {
    if (owner != msg.sender) {
        revert("Only the owner can issue assets");
    }

    require(assetRegisters[assetType].call(bytes4(keccak256("issueAssets(address, uint)")), recipient, quantity));
  }

  function addAssetRegister (string assetType, address register) internal {
    assetRegisters[assetType] = register;
  }

  function addCommitment(Commitment c) internal {
    // transfer the ownership of the asset to this contract
    eurAssetRegistry.transfer(c.sender, this, c.quantity);

    // add to commitments
    commitments.push(c);
  }

  function releaseCommitment(uint _commitmentId) internal {
    // fetch the commitment
    Commitment storage c = commitments[_commitmentId];

    // check that it is still open
    require(c.status == 0, "Commitment status must be Open (0) to allow release");

    // transfer the ownership of the asset (in the commitment) to the recipient
    require(assetRegisters[c.assetType].call(bytes4(keccak256("transfer(address, address, uint)")), this, c.recipient, c.quantity));

    // mark commitment as released
    commitments[_commitmentId].status = 1;
  }

  function cancelCommitment(uint _commitmentId) internal {
    // fetch the commitment
    Commitment storage c = commitments[_commitmentId];

    // check that it is still open
    require(c.status == 0, "Commitment status must be Open (0) to allow cancel");


    // return the ownership of the asset (in the commitment) to the sender
    require(assetRegisters[c.assetType].call(bytes4(keccak256("transfer(address, address, uint)")), this, c.sender, c.quantity));

    // mark commitment as cancelled
    commitments[_commitmentId].status = 2;
  }

  function fetchCommitment(uint _commitmentId) public view returns (string, uint, address, address, uint) {
    return (commitments[_commitmentId].assetType,
            commitments[_commitmentId].quantity,
            commitments[_commitmentId].sender,
            commitments[_commitmentId].recipient,
            commitments[_commitmentId].status);
  }

  function getContractStatus() public view returns (uint) {
    return contractStatus;
  }

  function setContractStatus(uint _status) internal {
    contractStatus = _status;
  }

  function openGate(string _gateName) internal {
    gateStatus[_gateName] = 1;
  }

  function closeGate(string _gateName) internal {
    gateStatus[_gateName] = 2;
  }

  function isGateOpen(string _gateName) internal view returns (bool) {
    return gateStatus[_gateName] == 1;
  }

  function isGateClosed(string _gateName) internal view returns (bool) {
    return gateStatus[_gateName] == 2;
  }

  function releaseCommitments(string _commitments) internal {
    var s = _commitments.toSlice();
    var delim = ",".toSlice();
    uint[] memory parts = new uint[](s.count(delim) + 1);

    for (uint i = 0; i < parts.length; i++) {
      parts[i] = stringToUint(s.split(delim).toString());
    }

    uint arrayLength = parts.length;

    for (uint j = 0; j < arrayLength; j++) {
      if (commitments[parts[j]].status == 0) {
        releaseCommitment(parts[j]);
      }
    }
  }

  function cancelCommitments(string _commitments) internal {
    var s = _commitments.toSlice();
    var delim = ",".toSlice();
    uint[] memory parts = new uint[](s.count(delim) + 1);

    for (uint i = 0; i < parts.length; i++) {
      parts[i] = stringToUint(s.split(delim).toString());
    }

    uint arrayLength = parts.length;

    for (uint j = 0; j < arrayLength; j++) {
      if (commitments[parts[j]].status == 0) {
        cancelCommitment(parts[j]);
      }
    }
  }

  function stringToUint(string s) internal pure returns (uint result) {
      bytes memory b = bytes(s);
      uint i;
      result = 0;
      for (i = 0; i < b.length; i++) {
          uint c = uint(b[i]);
          if (c >= 48 && c <= 57) {
              result = result * 10 + (c - 48);
          }
      }
  }

  function getAllCommitments() internal view returns (string) {
    uint arrayLength = commitments.length;

    string memory result = "";
    for (uint i = 0; i < arrayLength; i++) {
      result = string(abi.encodePacked(result, ",", uintToString(i)));
    }

    return result;
  }

  function uintToString(uint v) internal pure returns (string str) {
    uint maxlength = 100;
    bytes memory reversed = new bytes(maxlength);
    uint i = 0;
    while (v != 0) {
        uint remainder = v % 10;
        v = v / 10;
        reversed[i++] = byte(48 + remainder);
    }
    bytes memory s = new bytes(i);
    for (uint j = 0; j < i; j++) {
        s[j] = reversed[i - j - 1];
    }
    str = string(s);
  }


}
