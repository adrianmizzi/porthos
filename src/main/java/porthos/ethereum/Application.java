package porthos.ethereum;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.web3j.crypto.Credentials;
import org.web3j.protocol.Web3j;
import org.web3j.tx.Contract;
import org.web3j.tx.ManagedTransaction;

import porthos.ethereum.Web3jManager.Blockchain;
import porthos.ethereum.callback.CallbackHandler;
import porthos.ethereum.comms.CommsChannel;
import porthos.ethereum.contracts.generated.Gateway;
import porthos.ethereum.contracts.generated.T1;

public class Application {

    private static final Logger log = LoggerFactory.getLogger(Application.class);

	public static void main(String[] args) {
        try {
        	// Start by deploying a new instance of the default contracts on Ethereum 1: EventGenerator and CallbackManager
        	String gateway1 = deployGatewayContract(Blockchain.ETHEREUM_1);
        	log.info("Gateway framwork contract deployed on Ethereum 1");
        	
        	// Start by deploying a new instance of the default contracts on Etheruem 2: EventGenerator and CallbackManager
        	String gateway2 = deployGatewayContract(Blockchain.ETHEREUM_2);
        	log.info("Gateway framwork contract deployed on Ethereum 2");

        	// initialise the callback handler on Ethereum 1
        	log.info("Initialising Callback Handler on the public blockchain");
			new CallbackHandler(Blockchain.ETHEREUM_1, gateway1);

			// initialise the callback handler on Ethereum 2
        	log.info("Initialising Callback Handler on the private blockchain");
			new CallbackHandler(Blockchain.ETHEREUM_2, gateway2);

			// initialise the comms channel
			CommsChannel comms = new CommsChannel();
			comms.registerGateway(Blockchain.ETHEREUM_1, gateway1);
			comms.registerGateway(Blockchain.ETHEREUM_2, gateway2);
			
			runClient(gateway1, gateway2);
			
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	private static void runClient(String g1, String g2) throws Exception {
		// deploy contract on Ethereum 1
	    log.info("Deploying T1 contract on Ethereum 1");
	    @SuppressWarnings("deprecation")
		T1 t1Contract1 = T1.deploy(
	              Web3jManager.getWeb3jInstance(Blockchain.ETHEREUM_1).getWeb3j(), 
	              Web3jManager.getWeb3jInstance(Blockchain.ETHEREUM_1).getCredentials(),
	              ManagedTransaction.GAS_PRICE, Contract.GAS_LIMIT, 
	              g1).send();

	    // deploy contract on Ethereum 2
	    log.info("Deploying T1 contract on Ethereum 2");
	    @SuppressWarnings("deprecation")
		T1 t1Contract2 = T1.deploy(
	              Web3jManager.getWeb3jInstance(Blockchain.ETHEREUM_2).getWeb3j(), 
	              Web3jManager.getWeb3jInstance(Blockchain.ETHEREUM_2).getCredentials(),
	              ManagedTransaction.GAS_PRICE, Contract.GAS_LIMIT, 
	              g2).send();		
	    
	    log.info("Sending a cross chain call from Ethereum_1 to Ethereum_2");
	    t1Contract1.initiateCrossChainCall(Blockchain.ETHEREUM_2.toString(), t1Contract2.getContractAddress()).send();
	}

	
	private static String deployGatewayContract(Blockchain bcSystem) throws Exception {
        Web3j web3j = Web3jManager.getWeb3jInstance(bcSystem).getWeb3j(); 
        log.info("Connected to Ethereum client version: "
                + web3j.web3ClientVersion().send().getWeb3ClientVersion());

        // We provide a private key to create credentials
        Credentials credentials = Web3jManager.getWeb3jInstance(bcSystem).getCredentials();
        
        // Now lets deploy the EventGenerator smart contract
        log.info("Deploying smart contract: EventGenerator");
        @SuppressWarnings("deprecation")
		Gateway contract = Gateway.deploy(
                web3j, credentials,
                ManagedTransaction.GAS_PRICE, Contract.GAS_LIMIT).send();
        
        log.info("Smart contract (Gateway) deployed at address " + contract.getContractAddress());
        
        return contract.getContractAddress();
	}
}
