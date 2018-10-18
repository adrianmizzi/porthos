package porthos.ethereum.comms;

public class LogEventListener {
//    private static final Logger log = LoggerFactory.getLogger(LogEventListener.class);
//    private static CallbackHandler callbackManager;
//
//    
//    public static void main(String[] args) throws Exception {
//    	callbackManager = new CallbackHandler();
//        new LogEventListener().run();
//    }
//
//    private void run() throws Exception {
//        // We start by creating a new web3j instance to connect to remote nodes on the network.
//        // Note: if using web3j Android, use Web3jFactory.build(...
//        Web3j web3j = Web3jManager.getWeb3jInstance(Blockchain.ETHEREUM_PUBLIC).getWeb3j();
//        log.info("Connected to Ethereum client version: "
//                + web3j.web3ClientVersion().send().getWeb3ClientVersion());
//
//        // We provide a private key to create credentials
//        Credentials credentials = Web3jManager.getCredentials();
//        log.info("Credentials loaded");
//        
//
//        // Let's connect to a previously deployed contract
//        log.info("Connecting to previously deployed smart contract");
//        EventGenerator contract = EventGenerator.load("0x87f47521b1cad0c6bc7e8c99317de44c4e107505",
//                web3j, credentials,
//                ManagedTransaction.GAS_PRICE, Contract.GAS_LIMIT);
//        
//        String contractAddress = contract.getContractAddress();
//        log.info("Smart contract deployed to address " + contractAddress);
//
//        
////        EthFilter filter = new EthFilter(DefaultBlockParameterName.LATEST, DefaultBlockParameterName.LATEST, contractAddress.substring(2));
////        
////        String encodedEventSignature = EventEncoder.encode(EventGenerator.LOG_EVENT);
////
////        filter.addSingleTopic(encodedEventSignature);
////        
////        log.info("subscribing to event with filter");
////        
////        web3j.ethLogObservable(filter).subscribe(eventString -> log.info("event string={}", eventString.toString()));
//        
//        log.info("Start Observer");
//        EthFilter filter2 = new EthFilter(DefaultBlockParameterName.LATEST, DefaultBlockParameterName.LATEST, contractAddress.substring(2));
//
//        contract.logEventObservable(filter2)  
//        		.subscribe(event -> {
//        			log.info("Received Log Event [{}]: {}", event.blockNumber, event.message);
//        		});
//        
//        CallbackHandler cbHandler = new CallbackHandler();
//        
//        contract.callbackRequestEventObservable(filter2)
//        		.subscribe(event -> {
//        			log.info("Received callback request {}, {} at time {}", event.contractAddress, event.methodName, event.timeRequested);
//        			cbHandler.addCallback(new CallbackInfo(event.contractAddress, event.methodName, event.timeRequested));
//        		});
//    }
}


