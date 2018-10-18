package porthos.ethereum;

import java.util.HashMap;
import java.util.Map;


public class Web3jManager {


	public enum Blockchain {ETHEREUM_1, ETHEREUM_2};

	private static Map<Blockchain, Web3jInstance> systems = new HashMap<Blockchain, Web3jInstance>();


	public static Web3jInstance getWeb3jInstance(Blockchain bcSystem) throws Exception {
		if (systems.get(bcSystem) == null) {
			Web3jInstance i;
			switch (bcSystem) {
			case ETHEREUM_1 : 
				i = new Web3jInstance("http://localhost:7545/","d474253311b4bb8b6b67375a4cd5539063f6346f63d900eb97b8f57e139d2dd8");
				break;
			case ETHEREUM_2 : 
				i = new Web3jInstance("http://localhost:6545/","dd4802ccb9ba68077d5c59abfa7530f5e8d842bcf02fe939335ad1d81827c76d");
				break;
			default :
				throw new Exception("Blockchain system not supported: " + bcSystem);

			}

			// add the blockchain system to the Map
			systems.put(bcSystem, i);

			return i;
		} else {
			return systems.get(bcSystem);
		}
	}

}
