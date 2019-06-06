module ExampleDao where

import           General
import           Lang
import           Porthos
-- import           Commitments

alice, bob, charlie, dave, erin, this :: Participant
alice = Participant "alice" "0xf1c13a88cf28c4d06269a150dd8cdb2e3061d44f"
bob = Participant "bob" "0x4603bd7000aba82eab4aaea605df43a1e37ef2bb"
charlie = Participant "charlie" "0xb9f62ffe791ff9fa9c51722e3b833bf51db290de"
dave = Participant "dave" ""
erin = Participant "erin" ""
this = Participant "dao" ""

mem :: ChainToLang
mem = update (update (update initMem (Ethereum_1, Solidity)) (Ethereum_2, Solidity)) (Hyperledger, Chaincode)


data Curr = USD | EUR | GBP
  deriving (Show)

data Currency = Currency Curr
  deriving (Show)

instance AssetType Currency where
  chainOf (Currency USD) = Ethereum_1
  chainOf (Currency EUR) = Ethereum_1
  chainOf (Currency GBP) = Ethereum_2
  typeOf = show 
  -- valueOf = show

fee1, fee2 :: Asset Currency
fee1     = asset(Currency EUR, 1)
fee2     = asset(Currency GBP, 1)

data DaoVote = Vote Blockchain
  deriving (Show)

instance AssetType DaoVote where
  chainOf (Vote x) = x
  typeOf = show

vote1, vote2 :: Asset DaoVote
vote1 = asset(Vote Ethereum_1, 1)
vote2 = asset(Vote Ethereum_2, 1)

vote :: DaoVote
vote = Vote Ethereum_1

dao :: [(Participant, Currency, Asset DaoVote)] -> Contract
dao pp =
  (foldl1 (.&&.) (map (\(x, y, _) -> doPayment x y) pp)) .>>>. 
  (foldl1 (.&&.) (map (\(x, _, z) -> doVote x z) pp)) .>>>. 
  releaseAll (sendAssets winner end)
  where
    doPayment x y = onUserCommit ("pay" ++ show x) (y, (isCommitBy x .&. isCommitTo this))
                      end
                      (onTimeout 200 (cancelAll end))
    doVote x y = onUserCommit ("vote" ++ show x) (assetType y, (isCommitBy x))
                      end
                      (onTimeout 200 (cancelAll end))
    winner = maxOf (groupBySender (whereAssetTypeIs allCommitments (vote)))

-- Executables
d1 :: Contract
d1 = dao [(alice, Currency EUR, vote1), (bob,Currency GBP,vote2)]

-- just an example on how to instantiate contracts
runDao :: IO[()]
runDao = toFiles mem d1


liftN :: Int -> N
liftN x = I (toInteger x)


-- tmp section - FEEL FREE TO DELETE ANYTIME
-- gee :: CommitmentSet
gee = maxOf (groupBySender (whereSenderIs allCommitments alice))



