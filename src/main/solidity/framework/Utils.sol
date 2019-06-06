// pragma solidity ^0.4.24;
pragma solidity ^0.5.4;

library Utils {

	/**
	 * Concatentation of up to 5 strings.  Utility methods for 2,3,4 further down
	 */
	function strConcat(string memory _a, string memory _b, string memory _c, string memory _d, string memory _e) internal pure returns (string memory){
	    bytes memory _ba = bytes(_a);
	    bytes memory _bb = bytes(_b);
	    bytes memory _bc = bytes(_c);
	    bytes memory _bd = bytes(_d);
	    bytes memory _be = bytes(_e);
	    string memory abcde = new string(_ba.length + _bb.length + _bc.length + _bd.length + _be.length);
	    bytes memory babcde = bytes(abcde);
	    uint k = 0;
	    for (uint i = 0; i < _ba.length; i++) babcde[k++] = _ba[i];
	    for (uint i = 0; i < _bb.length; i++) babcde[k++] = _bb[i];
	    for (uint i = 0; i < _bc.length; i++) babcde[k++] = _bc[i];
	    for (uint i = 0; i < _bd.length; i++) babcde[k++] = _bd[i];
	    for (uint i = 0; i < _be.length; i++) babcde[k++] = _be[i];
	    return string(babcde);
	}

	function strConcat(string memory _a, string memory _b, string memory _c, string memory _d) internal pure returns (string memory) {
	    return strConcat(_a, _b, _c, _d, "");
	}

	function strConcat(string memory _a, string memory _b, string memory _c) internal pure returns (string memory) {
	    return strConcat(_a, _b, _c, "", "");
	}

	function strConcat(string memory _a, string memory _b) internal pure returns (string memory) {
	    return strConcat(_a, _b, "", "", "");
	}

	/**
	 * Compares 2 strings
	 */
	function compare(string memory a, string memory b) internal pure returns (bool){
		return keccak256(abi.encodePacked(a)) == keccak256(abi.encodePacked(b));
	}



  function uintToString(uint _i) internal pure returns (string memory _uintAsString) {
    if (_i == 0) {
        return "0";
    }
    uint j = _i;
    uint len;
    while (j != 0) {
        len++;
        j /= 10;
    }
    bytes memory bstr = new bytes(len);
    uint k = len - 1;
    while (_i != 0) {
        bstr[k--] = byte(uint8(48 + _i % 10));
        _i /= 10;
    }
    return string(bstr);
  }

	// function uintToString(uint v) internal pure returns (string memory str) {
	// 	uint maxlength = 100;
	// 	bytes memory reversed = new bytes(maxlength);
	// 	uint i = 0;
	// 	while (v != 0) {
	// 	    uint remainder = v % 10;
	// 	    v = v / 10;
	// 	    reversed[i++] = byte(48 + remainder);
	// 	}
	// 	bytes memory s = new bytes(i);
	// 	for (uint j = 0; j < i; j++) {
	// 	    s[j] = reversed[i - j - 1];
	// 	}
	// 	str = string(s);
	// }

	function stringToUint(string memory s) internal pure returns (uint result) {
		bytes memory b = bytes(s);
		uint i;
		result = 0;
		for (i = 0; i < b.length; i++) {
			uint8 c = uint8(b[i]);
			if (c >= 48 && c <= 57) {
				result = result * 10 + (c - 48);
			}
		}
	}

	/**
	 * Converts an address to string
	 */
	// function addressToString(address _addr) public pure returns(string memory) {
	//     bytes32 value = bytes32(uint256(_addr));
	//     bytes memory alphabet = "0123456789abcdef";

	//     bytes memory str = new bytes(51);
	//     str[0] = '0';
	//     str[1] = 'x';
	//     for (uint i = 0; i < 20; i++) {
	//         str[2+i*2] = alphabet[uint(value[i + 12] >> 4)];
	//         str[3+i*2] = alphabet[uint(value[i + 12] & 0x0f)];
	//     }
	//     return string(str);
	// }

}
