package porthos.ethereum;

public class Deployer {
//    private static final Logger log = LoggerFactory.getLogger(Deployer.class);
//
//    public static void main(String[] args) throws Exception {
//        new Deployer().run();
//    }
//
//    private void run() throws Exception {
//        Web3j web3j = Web3jManager.getWeb3jInstance(Blockchain.ETHEREUM_1).getWeb3j(); 
//        log.info("Connected to Ethereum client version: "
//                + web3j.web3ClientVersion().send().getWeb3ClientVersion());
//
//        // We provide a private key to create credentials
//        Credentials credentials = Web3jManager.getWeb3jInstance(Blockchain.ETHEREUM_1).getCredentials();
//        log.info("Credentials loaded");
//        
//        // Now lets deploy the EventGenerator smart contract
//        log.info("Deploying smart contract");
//        @SuppressWarnings("deprecation")
//		EventGenerator contract = EventGenerator.deploy(
//                web3j, credentials,
//                ManagedTransaction.GAS_PRICE, Contract.GAS_LIMIT).send();
//
//        String contractAddress = contract.getContractAddress();
//        log.info("Smart contract (EventGenerator) deployed at address " + contractAddress);
//
//        // Now lets deploy the CallbackManager smart contract
//        log.info("Deploying smart contract");
//        @SuppressWarnings("deprecation")
//		CallbackManager contract2 = CallbackManager.deploy(
//                web3j, credentials,
//                ManagedTransaction.GAS_PRICE, Contract.GAS_LIMIT).send();
//
//        String contractAddress2 = contract2.getContractAddress();
//        log.info("Smart contract (CallbackManager) deployed at address " + contractAddress2);
//
//        log.info("Deployment complete");
//    }

}
