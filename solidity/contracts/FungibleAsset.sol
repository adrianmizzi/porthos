pragma solidity ^0.4.24;

contract FungibleAsset {
    address registryOwner;

    mapping (address => uint) balances;

    event AssetRegisterEvent(address sender, address recipient, uint amount, uint balance_sender, uint balance_recipient);
  

    constructor(address _owner) public {
        registryOwner = _owner;
    }

    function issueAssets(address _recipient, uint _amount) public {
        if (msg.sender != registryOwner) {
            revert("Assets can only be issued by the registry owner");
        }

        balances[_recipient] += _amount;

        emit AssetRegisterEvent(0x00, _recipient, _amount, 0, balances[_recipient]);
    }

    function transfer(address _sender, address _recipient, uint _amount) public {
        if (msg.sender != registryOwner) {
            revert("Assets can only be transferred by the registry owner");
        }

        if (balances[_sender] < _amount) {
            revert("Not enough balance");
        }

        balances[_sender] -= _amount;
        balances[_recipient] += _amount;

        emit AssetRegisterEvent(_sender, _recipient, _amount,balances[_sender],balances[_recipient]);
    }

    function getBalance(address _user) public view returns (uint){
      return balances[_user];
    }
}
