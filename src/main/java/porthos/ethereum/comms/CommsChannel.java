package porthos.ethereum.comms;

import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.web3j.abi.EventEncoder;
import org.web3j.crypto.Credentials;
import org.web3j.protocol.Web3j;
import org.web3j.protocol.core.DefaultBlockParameterName;
import org.web3j.protocol.core.methods.request.EthFilter;
import org.web3j.tx.Contract;
import org.web3j.tx.ManagedTransaction;

import porthos.ethereum.Web3jManager;
import porthos.ethereum.Web3jManager.Blockchain;
import porthos.ethereum.contracts.generated.Gateway;

public class CommsChannel {
	private static final Logger log = LoggerFactory.getLogger(CommsChannel.class);

	private Map<Blockchain, Gateway> gateways;

	public CommsChannel() {
		gateways = new HashMap<Blockchain, Gateway>();
	}

	public void registerGateway(Blockchain system, String gatewayAddress) throws Exception {
		log.info("Registering a new gateway on system {}", system);
		Web3j web3j = Web3jManager.getWeb3jInstance(system).getWeb3j();
		Credentials credentials = Web3jManager.getWeb3jInstance(system).getCredentials();

		// Connect to a previously deployed contract - CallbackManager
		log.info("Connecting to previously deployed smart contract");
		@SuppressWarnings("deprecation")
		Gateway gateway = Gateway.load(gatewayAddress,
				web3j, credentials,
				ManagedTransaction.GAS_PRICE, Contract.GAS_LIMIT);

		EthFilter filter = new EthFilter(DefaultBlockParameterName.LATEST, DefaultBlockParameterName.LATEST, gateway.getContractAddress().substring(2));
        filter.addSingleTopic(EventEncoder.encode(Gateway.CROSSCHAIN_EVENT));

		// add to our map
		gateways.put(system, gateway);
		
		gateway.crossChainEventObservable(filter)  
		.subscribe(event -> {
			log.info("Received Cross Chain Call {} / {}", event.blockchainSystem, event.methodName);
			try {
				crosschainCall(Blockchain.valueOf(event.blockchainSystem), event.contractAddress, event.methodName);
			} catch (Exception e) {
				log.error("Unable to execute cross chain call", e);
			}
		});
	}
	
	private void crosschainCall(Blockchain system, String contractAddress, String methodName) throws Exception {
		Gateway gateway = gateways.get(system);
		log.info("Initiating cross chain call {} {}", contractAddress, methodName);
		gateway.call(contractAddress, methodName).send();
	}
}
