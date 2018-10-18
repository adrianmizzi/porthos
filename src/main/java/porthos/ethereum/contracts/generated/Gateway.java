package porthos.ethereum.contracts.generated;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import org.web3j.abi.EventEncoder;
import org.web3j.abi.TypeReference;
import org.web3j.abi.datatypes.Address;
import org.web3j.abi.datatypes.Event;
import org.web3j.abi.datatypes.Function;
import org.web3j.abi.datatypes.Type;
import org.web3j.abi.datatypes.Utf8String;
import org.web3j.abi.datatypes.generated.Uint256;
import org.web3j.crypto.Credentials;
import org.web3j.protocol.Web3j;
import org.web3j.protocol.core.DefaultBlockParameter;
import org.web3j.protocol.core.RemoteCall;
import org.web3j.protocol.core.methods.request.EthFilter;
import org.web3j.protocol.core.methods.response.Log;
import org.web3j.protocol.core.methods.response.TransactionReceipt;
import org.web3j.tx.Contract;
import org.web3j.tx.TransactionManager;
import org.web3j.tx.gas.ContractGasProvider;
import rx.Observable;
import rx.functions.Func1;

/**
 * <p>Auto generated code.
 * <p><strong>Do not modify!</strong>
 * <p>Please use the <a href="https://docs.web3j.io/command_line.html">web3j command line tools</a>,
 * or the org.web3j.codegen.SolidityFunctionWrapperGenerator in the 
 * <a href="https://github.com/web3j/web3j/tree/master/codegen">codegen module</a> to update.
 *
 * <p>Generated with web3j version 3.6.0.
 */
public class Gateway extends Contract {
    private static final String BINARY = "608060405234801561001057600080fd5b50610786806100206000396000f30060806040526004361061005e5763ffffffff60e060020a60003504166327524b99811461006357806344e1ce0d1461007a5780634a8b3917146100e15780636b46e3bc1461014a578063c06e8f94146101a3578063e2a6083a14610249575b600080fd5b34801561006f57600080fd5b506100786102b2565b005b34801561008657600080fd5b5060408051602060046024803582810135601f8101859004850286018501909652858552610078958335600160a060020a03169536956044949193909101919081908401838280828437509497506103199650505050505050565b3480156100ed57600080fd5b5060408051602060046024803582810135601f8101859004850286018501909652858552610078958335600160a060020a031695369560449491939091019190819084018382808284375094975050933594506103b29350505050565b34801561015657600080fd5b506040805160206004803580820135601f81018490048402850184019095528484526100789436949293602493928401919081908401838280828437509497506104d79650505050505050565b3480156101af57600080fd5b506040805160206004803580820135601f810184900484028501840190955284845261007894369492936024939284019190819084018382808284375050604080516020601f818a01358b0180359182018390048302840183018552818452989b600160a060020a038b35169b909a90999401975091955091820193509150819084018382808284375094975061057b9650505050505050565b34801561025557600080fd5b5060408051602060046024803582810135601f8101859004850286018501909652858552610078958335600160a060020a0316953695604494919390910191908190840183828082843750949750509335945061069a9350505050565b60408051436020820152818152600b818301527f48656c6c6f20576f726c64000000000000000000000000000000000000000000606082015290517fdd970dd9b5bfe707922155b058a407655cb18288b807e2216442bca8ad83d6b59181900360800190a1565b81600160a060020a0316816040518082805190602001908083835b602083106103535780518252601f199092019160209182019101610334565b6001836020036101000a038019825116818451168082178552505050505050905001915050604051809103902060e060020a90046040518163ffffffff1660e060020a0281526004016000604051808303816000875af1505050505050565b7ffbae48b012d13856174b7aee7b0f404f7ac7638a85d03bb2f18eb075f5ec6e348383836040518084600160a060020a0316600160a060020a0316815260200180602001838152602001828103825284818151815260200191508051906020019080838360005b83811015610431578181015183820152602001610419565b50505050905090810190601f16801561045e5780820380516001836020036101000a031916815260200191505b5094505050505060405180910390a1604080514360208201528181526010818301527f526571756573742043616c6c6261636b00000000000000000000000000000000606082015290517fdd970dd9b5bfe707922155b058a407655cb18288b807e2216442bca8ad83d6b59181900360800190a1505050565b7fdd970dd9b5bfe707922155b058a407655cb18288b807e2216442bca8ad83d6b581436040518080602001838152602001828103825284818151815260200191508051906020019080838360005b8381101561053d578181015183820152602001610525565b50505050905090810190601f16801561056a5780820380516001836020036101000a031916815260200191505b50935050505060405180910390a150565b7f789cb267811e16019b70baf9e73ebe13bc3e8f598f2af80fa56e8c01d8b1d5f3838383604051808060200184600160a060020a0316600160a060020a0316815260200180602001838103835286818151815260200191508051906020019080838360005b838110156105f85781810151838201526020016105e0565b50505050905090810190601f1680156106255780820380516001836020036101000a031916815260200191505b50838103825284518152845160209182019186019080838360005b83811015610658578181015183820152602001610640565b50505050905090810190601f1680156106855780820380516001836020036101000a031916815260200191505b509550505050505060405180910390a1505050565b7f64feac9a4ecd5322419d9cd1ea242c183b3bc7694e5c5a8be25a099da5828fc48383836040518084600160a060020a0316600160a060020a0316815260200180602001838152602001828103825284818151815260200191508051906020019080838360005b83811015610719578181015183820152602001610701565b50505050905090810190601f1680156107465780820380516001836020036101000a031916815260200191505b5094505050505060405180910390a15050505600a165627a7a7230582027ab9ce10931d5e8ee36d1bf0d89908ec9a17a4ebf46235a36514d5decef498d0029";

    public static final String FUNC_FIRETESTMESSAGE = "fireTestMessage";

    public static final String FUNC_CALL = "call";

    public static final String FUNC_REQUESTCALLBACK = "requestCallback";

    public static final String FUNC_FIREMESSAGE = "fireMessage";

    public static final String FUNC_INITIATECCC = "initiateCCC";

    public static final String FUNC_CANCELCALLBACK = "cancelCallback";

    public static final Event LOG_EVENT = new Event("Log", 
            Arrays.<TypeReference<?>>asList(new TypeReference<Utf8String>() {}, new TypeReference<Uint256>() {}));
    ;

    public static final Event CALLBACKREQUEST_EVENT = new Event("CallbackRequest", 
            Arrays.<TypeReference<?>>asList(new TypeReference<Address>() {}, new TypeReference<Utf8String>() {}, new TypeReference<Uint256>() {}));
    ;

    public static final Event CALLBACKCANCEL_EVENT = new Event("CallbackCancel", 
            Arrays.<TypeReference<?>>asList(new TypeReference<Address>() {}, new TypeReference<Utf8String>() {}, new TypeReference<Uint256>() {}));
    ;

    public static final Event CROSSCHAIN_EVENT = new Event("CrossChain", 
            Arrays.<TypeReference<?>>asList(new TypeReference<Utf8String>() {}, new TypeReference<Address>() {}, new TypeReference<Utf8String>() {}));
    ;

    @Deprecated
    protected Gateway(String contractAddress, Web3j web3j, Credentials credentials, BigInteger gasPrice, BigInteger gasLimit) {
        super(BINARY, contractAddress, web3j, credentials, gasPrice, gasLimit);
    }

    protected Gateway(String contractAddress, Web3j web3j, Credentials credentials, ContractGasProvider contractGasProvider) {
        super(BINARY, contractAddress, web3j, credentials, contractGasProvider);
    }

    @Deprecated
    protected Gateway(String contractAddress, Web3j web3j, TransactionManager transactionManager, BigInteger gasPrice, BigInteger gasLimit) {
        super(BINARY, contractAddress, web3j, transactionManager, gasPrice, gasLimit);
    }

    protected Gateway(String contractAddress, Web3j web3j, TransactionManager transactionManager, ContractGasProvider contractGasProvider) {
        super(BINARY, contractAddress, web3j, transactionManager, contractGasProvider);
    }

    public RemoteCall<TransactionReceipt> fireTestMessage() {
        final Function function = new Function(
                FUNC_FIRETESTMESSAGE, 
                Arrays.<Type>asList(), 
                Collections.<TypeReference<?>>emptyList());
        return executeRemoteCallTransaction(function);
    }

    public RemoteCall<TransactionReceipt> call(String contractAddress, String method) {
        final Function function = new Function(
                FUNC_CALL, 
                Arrays.<Type>asList(new org.web3j.abi.datatypes.Address(contractAddress), 
                new org.web3j.abi.datatypes.Utf8String(method)), 
                Collections.<TypeReference<?>>emptyList());
        return executeRemoteCallTransaction(function);
    }

    public RemoteCall<TransactionReceipt> requestCallback(String _contractAddress, String _methodName, BigInteger _timeRequested) {
        final Function function = new Function(
                FUNC_REQUESTCALLBACK, 
                Arrays.<Type>asList(new org.web3j.abi.datatypes.Address(_contractAddress), 
                new org.web3j.abi.datatypes.Utf8String(_methodName), 
                new org.web3j.abi.datatypes.generated.Uint256(_timeRequested)), 
                Collections.<TypeReference<?>>emptyList());
        return executeRemoteCallTransaction(function);
    }

    public RemoteCall<TransactionReceipt> fireMessage(String _message) {
        final Function function = new Function(
                FUNC_FIREMESSAGE, 
                Arrays.<Type>asList(new org.web3j.abi.datatypes.Utf8String(_message)), 
                Collections.<TypeReference<?>>emptyList());
        return executeRemoteCallTransaction(function);
    }

    public RemoteCall<TransactionReceipt> initiateCCC(String _bcSystem, String _contractAddress, String _methodName) {
        final Function function = new Function(
                FUNC_INITIATECCC, 
                Arrays.<Type>asList(new org.web3j.abi.datatypes.Utf8String(_bcSystem), 
                new org.web3j.abi.datatypes.Address(_contractAddress), 
                new org.web3j.abi.datatypes.Utf8String(_methodName)), 
                Collections.<TypeReference<?>>emptyList());
        return executeRemoteCallTransaction(function);
    }

    public RemoteCall<TransactionReceipt> cancelCallback(String _contractAddress, String _methodName, BigInteger _timeRequested) {
        final Function function = new Function(
                FUNC_CANCELCALLBACK, 
                Arrays.<Type>asList(new org.web3j.abi.datatypes.Address(_contractAddress), 
                new org.web3j.abi.datatypes.Utf8String(_methodName), 
                new org.web3j.abi.datatypes.generated.Uint256(_timeRequested)), 
                Collections.<TypeReference<?>>emptyList());
        return executeRemoteCallTransaction(function);
    }

    public List<LogEventResponse> getLogEvents(TransactionReceipt transactionReceipt) {
        List<Contract.EventValuesWithLog> valueList = extractEventParametersWithLog(LOG_EVENT, transactionReceipt);
        ArrayList<LogEventResponse> responses = new ArrayList<LogEventResponse>(valueList.size());
        for (Contract.EventValuesWithLog eventValues : valueList) {
            LogEventResponse typedResponse = new LogEventResponse();
            typedResponse.log = eventValues.getLog();
            typedResponse.message = (String) eventValues.getNonIndexedValues().get(0).getValue();
            typedResponse.blockNumber = (BigInteger) eventValues.getNonIndexedValues().get(1).getValue();
            responses.add(typedResponse);
        }
        return responses;
    }

    public Observable<LogEventResponse> logEventObservable(EthFilter filter) {
        return web3j.ethLogObservable(filter).map(new Func1<Log, LogEventResponse>() {
            @Override
            public LogEventResponse call(Log log) {
                Contract.EventValuesWithLog eventValues = extractEventParametersWithLog(LOG_EVENT, log);
                LogEventResponse typedResponse = new LogEventResponse();
                typedResponse.log = log;
                typedResponse.message = (String) eventValues.getNonIndexedValues().get(0).getValue();
                typedResponse.blockNumber = (BigInteger) eventValues.getNonIndexedValues().get(1).getValue();
                return typedResponse;
            }
        });
    }

    public Observable<LogEventResponse> logEventObservable(DefaultBlockParameter startBlock, DefaultBlockParameter endBlock) {
        EthFilter filter = new EthFilter(startBlock, endBlock, getContractAddress());
        filter.addSingleTopic(EventEncoder.encode(LOG_EVENT));
        return logEventObservable(filter);
    }

    public List<CallbackRequestEventResponse> getCallbackRequestEvents(TransactionReceipt transactionReceipt) {
        List<Contract.EventValuesWithLog> valueList = extractEventParametersWithLog(CALLBACKREQUEST_EVENT, transactionReceipt);
        ArrayList<CallbackRequestEventResponse> responses = new ArrayList<CallbackRequestEventResponse>(valueList.size());
        for (Contract.EventValuesWithLog eventValues : valueList) {
            CallbackRequestEventResponse typedResponse = new CallbackRequestEventResponse();
            typedResponse.log = eventValues.getLog();
            typedResponse.contractAddress = (String) eventValues.getNonIndexedValues().get(0).getValue();
            typedResponse.methodName = (String) eventValues.getNonIndexedValues().get(1).getValue();
            typedResponse.timeRequested = (BigInteger) eventValues.getNonIndexedValues().get(2).getValue();
            responses.add(typedResponse);
        }
        return responses;
    }

    public Observable<CallbackRequestEventResponse> callbackRequestEventObservable(EthFilter filter) {
        return web3j.ethLogObservable(filter).map(new Func1<Log, CallbackRequestEventResponse>() {
            @Override
            public CallbackRequestEventResponse call(Log log) {
                Contract.EventValuesWithLog eventValues = extractEventParametersWithLog(CALLBACKREQUEST_EVENT, log);
                CallbackRequestEventResponse typedResponse = new CallbackRequestEventResponse();
                typedResponse.log = log;
                typedResponse.contractAddress = (String) eventValues.getNonIndexedValues().get(0).getValue();
                typedResponse.methodName = (String) eventValues.getNonIndexedValues().get(1).getValue();
                typedResponse.timeRequested = (BigInteger) eventValues.getNonIndexedValues().get(2).getValue();
                return typedResponse;
            }
        });
    }

    public Observable<CallbackRequestEventResponse> callbackRequestEventObservable(DefaultBlockParameter startBlock, DefaultBlockParameter endBlock) {
        EthFilter filter = new EthFilter(startBlock, endBlock, getContractAddress());
        filter.addSingleTopic(EventEncoder.encode(CALLBACKREQUEST_EVENT));
        return callbackRequestEventObservable(filter);
    }

    public List<CallbackCancelEventResponse> getCallbackCancelEvents(TransactionReceipt transactionReceipt) {
        List<Contract.EventValuesWithLog> valueList = extractEventParametersWithLog(CALLBACKCANCEL_EVENT, transactionReceipt);
        ArrayList<CallbackCancelEventResponse> responses = new ArrayList<CallbackCancelEventResponse>(valueList.size());
        for (Contract.EventValuesWithLog eventValues : valueList) {
            CallbackCancelEventResponse typedResponse = new CallbackCancelEventResponse();
            typedResponse.log = eventValues.getLog();
            typedResponse.contractAddress = (String) eventValues.getNonIndexedValues().get(0).getValue();
            typedResponse.methodName = (String) eventValues.getNonIndexedValues().get(1).getValue();
            typedResponse.timeRequested = (BigInteger) eventValues.getNonIndexedValues().get(2).getValue();
            responses.add(typedResponse);
        }
        return responses;
    }

    public Observable<CallbackCancelEventResponse> callbackCancelEventObservable(EthFilter filter) {
        return web3j.ethLogObservable(filter).map(new Func1<Log, CallbackCancelEventResponse>() {
            @Override
            public CallbackCancelEventResponse call(Log log) {
                Contract.EventValuesWithLog eventValues = extractEventParametersWithLog(CALLBACKCANCEL_EVENT, log);
                CallbackCancelEventResponse typedResponse = new CallbackCancelEventResponse();
                typedResponse.log = log;
                typedResponse.contractAddress = (String) eventValues.getNonIndexedValues().get(0).getValue();
                typedResponse.methodName = (String) eventValues.getNonIndexedValues().get(1).getValue();
                typedResponse.timeRequested = (BigInteger) eventValues.getNonIndexedValues().get(2).getValue();
                return typedResponse;
            }
        });
    }

    public Observable<CallbackCancelEventResponse> callbackCancelEventObservable(DefaultBlockParameter startBlock, DefaultBlockParameter endBlock) {
        EthFilter filter = new EthFilter(startBlock, endBlock, getContractAddress());
        filter.addSingleTopic(EventEncoder.encode(CALLBACKCANCEL_EVENT));
        return callbackCancelEventObservable(filter);
    }

    public List<CrossChainEventResponse> getCrossChainEvents(TransactionReceipt transactionReceipt) {
        List<Contract.EventValuesWithLog> valueList = extractEventParametersWithLog(CROSSCHAIN_EVENT, transactionReceipt);
        ArrayList<CrossChainEventResponse> responses = new ArrayList<CrossChainEventResponse>(valueList.size());
        for (Contract.EventValuesWithLog eventValues : valueList) {
            CrossChainEventResponse typedResponse = new CrossChainEventResponse();
            typedResponse.log = eventValues.getLog();
            typedResponse.blockchainSystem = (String) eventValues.getNonIndexedValues().get(0).getValue();
            typedResponse.contractAddress = (String) eventValues.getNonIndexedValues().get(1).getValue();
            typedResponse.methodName = (String) eventValues.getNonIndexedValues().get(2).getValue();
            responses.add(typedResponse);
        }
        return responses;
    }

    public Observable<CrossChainEventResponse> crossChainEventObservable(EthFilter filter) {
        return web3j.ethLogObservable(filter).map(new Func1<Log, CrossChainEventResponse>() {
            @Override
            public CrossChainEventResponse call(Log log) {
                Contract.EventValuesWithLog eventValues = extractEventParametersWithLog(CROSSCHAIN_EVENT, log);
                CrossChainEventResponse typedResponse = new CrossChainEventResponse();
                typedResponse.log = log;
                typedResponse.blockchainSystem = (String) eventValues.getNonIndexedValues().get(0).getValue();
                typedResponse.contractAddress = (String) eventValues.getNonIndexedValues().get(1).getValue();
                typedResponse.methodName = (String) eventValues.getNonIndexedValues().get(2).getValue();
                return typedResponse;
            }
        });
    }

    public Observable<CrossChainEventResponse> crossChainEventObservable(DefaultBlockParameter startBlock, DefaultBlockParameter endBlock) {
        EthFilter filter = new EthFilter(startBlock, endBlock, getContractAddress());
        filter.addSingleTopic(EventEncoder.encode(CROSSCHAIN_EVENT));
        return crossChainEventObservable(filter);
    }

    public static RemoteCall<Gateway> deploy(Web3j web3j, Credentials credentials, ContractGasProvider contractGasProvider) {
        return deployRemoteCall(Gateway.class, web3j, credentials, contractGasProvider, BINARY, "");
    }

    @Deprecated
    public static RemoteCall<Gateway> deploy(Web3j web3j, Credentials credentials, BigInteger gasPrice, BigInteger gasLimit) {
        return deployRemoteCall(Gateway.class, web3j, credentials, gasPrice, gasLimit, BINARY, "");
    }

    public static RemoteCall<Gateway> deploy(Web3j web3j, TransactionManager transactionManager, ContractGasProvider contractGasProvider) {
        return deployRemoteCall(Gateway.class, web3j, transactionManager, contractGasProvider, BINARY, "");
    }

    @Deprecated
    public static RemoteCall<Gateway> deploy(Web3j web3j, TransactionManager transactionManager, BigInteger gasPrice, BigInteger gasLimit) {
        return deployRemoteCall(Gateway.class, web3j, transactionManager, gasPrice, gasLimit, BINARY, "");
    }

    @Deprecated
    public static Gateway load(String contractAddress, Web3j web3j, Credentials credentials, BigInteger gasPrice, BigInteger gasLimit) {
        return new Gateway(contractAddress, web3j, credentials, gasPrice, gasLimit);
    }

    @Deprecated
    public static Gateway load(String contractAddress, Web3j web3j, TransactionManager transactionManager, BigInteger gasPrice, BigInteger gasLimit) {
        return new Gateway(contractAddress, web3j, transactionManager, gasPrice, gasLimit);
    }

    public static Gateway load(String contractAddress, Web3j web3j, Credentials credentials, ContractGasProvider contractGasProvider) {
        return new Gateway(contractAddress, web3j, credentials, contractGasProvider);
    }

    public static Gateway load(String contractAddress, Web3j web3j, TransactionManager transactionManager, ContractGasProvider contractGasProvider) {
        return new Gateway(contractAddress, web3j, transactionManager, contractGasProvider);
    }

    public static class LogEventResponse {
        public Log log;

        public String message;

        public BigInteger blockNumber;
    }

    public static class CallbackRequestEventResponse {
        public Log log;

        public String contractAddress;

        public String methodName;

        public BigInteger timeRequested;
    }

    public static class CallbackCancelEventResponse {
        public Log log;

        public String contractAddress;

        public String methodName;

        public BigInteger timeRequested;
    }

    public static class CrossChainEventResponse {
        public Log log;

        public String blockchainSystem;

        public String contractAddress;

        public String methodName;
    }
}
