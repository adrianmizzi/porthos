pragma solidity ^0.4.24;

contract FungibleAsset {
    address registryOwner;

    mapping (address => uint) balances;

    constructor(address _owner) public {
        registryOwner = _owner;
    }

    function issueAssets(address recipient, uint amount) public {
        if (msg.sender != registryOwner) {
            revert("Assets can only be issued by the registry owner");
        }

        balances[recipient] += amount;
    }

    function transfer(address _sender, address _recipient, uint amount) public {
        if (msg.sender != registryOwner) {
            revert("Assets can only be transferred by the registry owner");
        }

        if (balances[_sender] < amount) {
            revert("Not enough balance");
        }

        balances[_sender] -= amount;
        balances[_recipient] += amount;
    }

    function getBalance(address _user) public view returns (uint){
      return balances[_user];
    }
}
