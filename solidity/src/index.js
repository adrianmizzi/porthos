var Web3 = require('web3');
var web3 = new Web3(new Web3.providers.HttpProvider("http://localhost:7545"));

// replace with the correct address for the contract and the caller
var contract = "0xdc950b39bdd7aff729ae15defec67d108cedffcf";
var caller   = "0x1598eecbc9c273875e976f8c85cf78397a9aa089";

// replace with the ABI (taken from REMIX)
var abi = [
	{
		"constant": false,
		"inputs": [
			{
				"name": "recipient",
				"type": "address"
			},
			{
				"name": "amount",
				"type": "uint256"
			}
		],
		"name": "issueAssets",
		"outputs": [],
		"payable": false,
		"stateMutability": "nonpayable",
		"type": "function"
	},
	{
		"constant": false,
		"inputs": [
			{
				"name": "_sender",
				"type": "address"
			},
			{
				"name": "_recipient",
				"type": "address"
			},
			{
				"name": "amount",
				"type": "uint256"
			}
		],
		"name": "transfer",
		"outputs": [],
		"payable": false,
		"stateMutability": "nonpayable",
		"type": "function"
	},
	{
		"inputs": [],
		"payable": false,
		"stateMutability": "nonpayable",
		"type": "constructor"
	}
];


var MyContract = new web3.eth.Contract(abi, contract,{from: caller});



// callback function for events
// var events = MyContract.events.allEvents({fromBlock: 0}, function(error, event){ console.log(event); });
// var events = MyContract.events.SimpleEvent().on("data", (function(event){ console.log(event); })).on("error", console.error);
//
// listenForEvents();
//
// async function listenForEvents() {
//   var from = await web3.eth.getBlockNumber();
//   console.log('Latest block number is ' + from);
//
//   // START
//   setInterval( function() {
//     MyContract.getPastEvents('allEvents', {
//       fromBlock: from,
//       toBlock: 'latest'
//     }, function(error, events){
//       for (i=0; i<events.length; i++) {
//         var eventObj = events[i];
//         from = eventObj.blockNumber + 1;
//
//         // console.log(eventObj);
//         console.log('Block Number: ' + eventObj.blockNumber);
//         console.log('Event Type: ' + eventObj.event);
//         console.log(eventObj.returnValues);
//       }
//     })
//   }, 100);
// }
