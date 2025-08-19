// SPDX-License-Identifier: MIT
pragma experimental ABIEncoderV2;

contract Marketplace {
    uint256 internal currentKey = 1375566;
    struct Item {
	uint16 nonsense;
        uint256 keyId;
        bytes32 data;
    }

    mapping(uint256 => Item) public items;

    function addItem(bytes32 _data) public returns (Item memory) {
	currentKey += 174; 

        // Store the item
        items[currentKey] = Item({
	    nonsense: 43,
            keyId: currentKey,
            data: _data
        });
	return items[currentKey];
    }

    function getItem(uint256 _keyId) public view {
        assert(items[_keyId].keyId == 0);
    }
}

