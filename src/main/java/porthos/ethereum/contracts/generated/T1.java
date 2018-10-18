package porthos.ethereum.contracts.generated;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collections;
import org.web3j.abi.FunctionEncoder;
import org.web3j.abi.TypeReference;
import org.web3j.abi.datatypes.Function;
import org.web3j.abi.datatypes.Type;
import org.web3j.crypto.Credentials;
import org.web3j.protocol.Web3j;
import org.web3j.protocol.core.RemoteCall;
import org.web3j.protocol.core.methods.response.TransactionReceipt;
import org.web3j.tx.Contract;
import org.web3j.tx.TransactionManager;
import org.web3j.tx.gas.ContractGasProvider;

/**
 * <p>Auto generated code.
 * <p><strong>Do not modify!</strong>
 * <p>Please use the <a href="https://docs.web3j.io/command_line.html">web3j command line tools</a>,
 * or the org.web3j.codegen.SolidityFunctionWrapperGenerator in the 
 * <a href="https://github.com/web3j/web3j/tree/master/codegen">codegen module</a> to update.
 *
 * <p>Generated with web3j version 3.6.0.
 */
public class T1 extends Contract {
    private static final String BINARY = "608060405234801561001057600080fd5b506040516020806105dd833981016040525160008054600160a060020a03909216600160a060020a031990921691909117905561058b806100526000396000f30060806040526004361061006c5763ffffffff7c010000000000000000000000000000000000000000000000000000000060003504166302906ab18114610071578063046f7da2146100885780635c36b1861461009d57806376f8223c146100b2578063be9a655514610116575b600080fd5b34801561007d57600080fd5b5061008661012b565b005b34801561009457600080fd5b506100866101c1565b3480156100a957600080fd5b5061008661023d565b3480156100be57600080fd5b506040805160206004803580820135601f810184900484028501840190955284845261008694369492936024939284019190819084018382808284375094975050509235600160a060020a031693506102bb92505050565b34801561012257600080fd5b506100866103e6565b6000805460408051600080516020610540833981519152815260206004820152601960248201527f526573756d653228292066756e6374696f6e2063616c6c65640000000000000060448201529051600160a060020a0390921692636b46e3bc9260648084019382900301818387803b1580156101a757600080fd5b505af11580156101bb573d6000803e3d6000fd5b50505050565b6000805460408051600080516020610540833981519152815260206004820152601860248201527f526573756d6528292066756e6374696f6e2063616c6c6564000000000000000060448201529051600160a060020a0390921692636b46e3bc9260648084019382900301818387803b1580156101a757600080fd5b60008054604080516000805160206105408339815191528152602060048083019190915260248201527f50696e670000000000000000000000000000000000000000000000000000000060448201529051600160a060020a0390921692636b46e3bc9260648084019382900301818387803b1580156101a757600080fd5b600080546040517fc06e8f94000000000000000000000000000000000000000000000000000000008152600160a060020a038481166024830152606060048301908152865160648401528651919093169363c06e8f949387938793919283926044830192608401916020880191908190849084905b83811015610348578181015183820152602001610330565b50505050905090810190601f1680156103755780820380516001836020036101000a031916815260200191505b50838103825260098152602001807f726573756d653228290000000000000000000000000000000000000000000000815250602001945050505050600060405180830381600087803b1580156103ca57600080fd5b505af11580156103de573d6000803e3d6000fd5b505050505050565b6000805460408051600080516020610540833981519152815260206004820152602960248201527f5374617274696e67205431202d2073656e64696e6720612063616c6c2062616360448201527f6b2072657175657374000000000000000000000000000000000000000000000060648201529051600160a060020a0390921692636b46e3bc9260848084019382900301818387803b15801561048857600080fd5b505af115801561049c573d6000803e3d6000fd5b505060008054604080517f4a8b391700000000000000000000000000000000000000000000000000000000815230600482015260024301604482015260606024820152600860648201527f726573756d65282900000000000000000000000000000000000000000000000060848201529051600160a060020a039092169450634a8b3917935060a480820193929182900301818387803b1580156101a757600080fd006b46e3bc00000000000000000000000000000000000000000000000000000000a165627a7a72305820f010d9b42fe8523aaaed6e2ec89ade46c26d3d511d14207f036f532752f970a60029";

    public static final String FUNC_RESUME2 = "resume2";

    public static final String FUNC_RESUME = "resume";

    public static final String FUNC_PING = "ping";

    public static final String FUNC_INITIATECROSSCHAINCALL = "initiateCrossChainCall";

    public static final String FUNC_START = "start";

    @Deprecated
    protected T1(String contractAddress, Web3j web3j, Credentials credentials, BigInteger gasPrice, BigInteger gasLimit) {
        super(BINARY, contractAddress, web3j, credentials, gasPrice, gasLimit);
    }

    protected T1(String contractAddress, Web3j web3j, Credentials credentials, ContractGasProvider contractGasProvider) {
        super(BINARY, contractAddress, web3j, credentials, contractGasProvider);
    }

    @Deprecated
    protected T1(String contractAddress, Web3j web3j, TransactionManager transactionManager, BigInteger gasPrice, BigInteger gasLimit) {
        super(BINARY, contractAddress, web3j, transactionManager, gasPrice, gasLimit);
    }

    protected T1(String contractAddress, Web3j web3j, TransactionManager transactionManager, ContractGasProvider contractGasProvider) {
        super(BINARY, contractAddress, web3j, transactionManager, contractGasProvider);
    }

    public RemoteCall<TransactionReceipt> resume2() {
        final Function function = new Function(
                FUNC_RESUME2, 
                Arrays.<Type>asList(), 
                Collections.<TypeReference<?>>emptyList());
        return executeRemoteCallTransaction(function);
    }

    public RemoteCall<TransactionReceipt> resume() {
        final Function function = new Function(
                FUNC_RESUME, 
                Arrays.<Type>asList(), 
                Collections.<TypeReference<?>>emptyList());
        return executeRemoteCallTransaction(function);
    }

    public RemoteCall<TransactionReceipt> ping() {
        final Function function = new Function(
                FUNC_PING, 
                Arrays.<Type>asList(), 
                Collections.<TypeReference<?>>emptyList());
        return executeRemoteCallTransaction(function);
    }

    public RemoteCall<TransactionReceipt> initiateCrossChainCall(String system, String c) {
        final Function function = new Function(
                FUNC_INITIATECROSSCHAINCALL, 
                Arrays.<Type>asList(new org.web3j.abi.datatypes.Utf8String(system), 
                new org.web3j.abi.datatypes.Address(c)), 
                Collections.<TypeReference<?>>emptyList());
        return executeRemoteCallTransaction(function);
    }

    public RemoteCall<TransactionReceipt> start() {
        final Function function = new Function(
                FUNC_START, 
                Arrays.<Type>asList(), 
                Collections.<TypeReference<?>>emptyList());
        return executeRemoteCallTransaction(function);
    }

    public static RemoteCall<T1> deploy(Web3j web3j, Credentials credentials, ContractGasProvider contractGasProvider, String gatewayAddress) {
        String encodedConstructor = FunctionEncoder.encodeConstructor(Arrays.<Type>asList(new org.web3j.abi.datatypes.Address(gatewayAddress)));
        return deployRemoteCall(T1.class, web3j, credentials, contractGasProvider, BINARY, encodedConstructor);
    }

    public static RemoteCall<T1> deploy(Web3j web3j, TransactionManager transactionManager, ContractGasProvider contractGasProvider, String gatewayAddress) {
        String encodedConstructor = FunctionEncoder.encodeConstructor(Arrays.<Type>asList(new org.web3j.abi.datatypes.Address(gatewayAddress)));
        return deployRemoteCall(T1.class, web3j, transactionManager, contractGasProvider, BINARY, encodedConstructor);
    }

    @Deprecated
    public static RemoteCall<T1> deploy(Web3j web3j, Credentials credentials, BigInteger gasPrice, BigInteger gasLimit, String gatewayAddress) {
        String encodedConstructor = FunctionEncoder.encodeConstructor(Arrays.<Type>asList(new org.web3j.abi.datatypes.Address(gatewayAddress)));
        return deployRemoteCall(T1.class, web3j, credentials, gasPrice, gasLimit, BINARY, encodedConstructor);
    }

    @Deprecated
    public static RemoteCall<T1> deploy(Web3j web3j, TransactionManager transactionManager, BigInteger gasPrice, BigInteger gasLimit, String gatewayAddress) {
        String encodedConstructor = FunctionEncoder.encodeConstructor(Arrays.<Type>asList(new org.web3j.abi.datatypes.Address(gatewayAddress)));
        return deployRemoteCall(T1.class, web3j, transactionManager, gasPrice, gasLimit, BINARY, encodedConstructor);
    }

    @Deprecated
    public static T1 load(String contractAddress, Web3j web3j, Credentials credentials, BigInteger gasPrice, BigInteger gasLimit) {
        return new T1(contractAddress, web3j, credentials, gasPrice, gasLimit);
    }

    @Deprecated
    public static T1 load(String contractAddress, Web3j web3j, TransactionManager transactionManager, BigInteger gasPrice, BigInteger gasLimit) {
        return new T1(contractAddress, web3j, transactionManager, gasPrice, gasLimit);
    }

    public static T1 load(String contractAddress, Web3j web3j, Credentials credentials, ContractGasProvider contractGasProvider) {
        return new T1(contractAddress, web3j, credentials, contractGasProvider);
    }

    public static T1 load(String contractAddress, Web3j web3j, TransactionManager transactionManager, ContractGasProvider contractGasProvider) {
        return new T1(contractAddress, web3j, transactionManager, contractGasProvider);
    }
}
