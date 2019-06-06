module Examples where


import           General
import           Lang
import           Porthos


alice, bob, charlie, dave, erin, this :: Participant
alice = Participant {name="alice", address="0xf1c13a88cf28c4d06269a150dd8cdb2e3061d44f"}
bob = Participant {name="bob", address="0x4603bd7000aba82eab4aaea605df43a1e37ef2bb"}
charlie = Participant {name="charlie", address="0xb9f62ffe791ff9fa9c51722e3b833bf51db290de"}
dave = Participant {name="dave", address=""}
erin = Participant {name="erin", address=""}
this = Participant {name="dao", address=""}

mem :: ChainToLang
mem = update (update (update initMem (Ethereum_1, Solidity)) (Ethereum_2, Solidity)) (Hyperledger, Chaincode)

data MyAssetType = Apple | Orange
  deriving (Show, Eq)

instance AssetType MyAssetType where
  chainOf Apple = Ethereum_1
  chainOf Orange = Hyperledger
  -- valueOf = show
  typeOf  = show
-- Atomic Swap
swap :: (Participant, Asset MyAssetType) -> (Participant, Asset MyAssetType) -> Contract
swap (p1, a1) (p2, a2) = onUserCommit "p1Commit" (Apple, (isCommitTo p2 .&. isAsset a1))
                           doP2Commit 
                           (onTimeout 10 end)
  where
    doP2Commit = onUserCommit "p2Commit" (Orange, (isCommitTo p1 .&. isAsset a2 .&. isCommitBy p2))
                    (releaseAll end)
                    (onTimeout 20 (cancelAll end))

-- Crowdfunding
crowdFunding :: Participant -> Asset Currency -> Contract
crowdFunding recipient target
          = repeatCommit "fund" (EUR, isCommitTo recipient)
              (onTimeout 100 closeCampaign)
  where
    aType = assetType target
    closeCampaign = ifThenElse (sumCommit .>. target)
                      (releaseAll (fireEvent "Campaign Successful" end),
                       cancelAll (fireEvent "Campagin Failed" end))
    sumCommit = sumC (aType,allCommitments)

-- Group Pay
groupPay :: [(Participant, Asset Currency)] -> Participant -> Contract
groupPay yy recipient = allOf (userCommits yy) `followedBy`
                          ifThenElse (countC(allCommitments) .==. liftN (length yy))
                            (releaseAll end,
                            cancelAll end)
  where
    userCommits = map (\x -> onUserCommit (name (fst x)) (EUR, txFilter x) end (onTimeout 100 end))
    txFilter (a, b) = isCommitTo recipient .&. isCommitBy a .&. isAsset b

allOf :: [Contract] -> Contract
allOf []     = Null
allOf [c]    = c
allOf (c:cc) = both (c, allOf cc)

-- Time-locked piggy bank
piggy :: Participant -> Time -> Contract
piggy recipient expiryTime = repeatCommit "save" (Apple, isCommitTo recipient)
                               (onTimeout expiryTime (releaseAll end))

-- Crowdfunding on multiple assets
crowdFunding2 :: Participant -> (Currency, Currency, Float) -> Asset Currency -> Contract
crowdFunding2 recipient (x, y, f) targetY = both (campaignX, campaignY) `followedBy` closeCampaign
  where
    campaignX = repeatCommit "fundX" (x, isCommitTo recipient)
                  (onTimeout 100 end)
    campaignY = repeatCommit "fundY" (y, isCommitTo recipient)
                  (onTimeout 100 end)
    closeCampaign = ifThenElse (totalY .>. targetY)
                      (releaseAll (fireEvent "Campaign Successful" end),
                       cancelAll (fireEvent "Campaign Failed" end))
    sumCommitX = sumC (x, allCommitments)
    sumCommitY = sumC (y, allCommitments)

    totalY = sumCommitY .+. exchange (x, y, f) sumCommitX

data Property = Property -- {ref :: String}
  deriving (Show, Eq)

instance AssetType Property where
  chainOf _ = Ethereum_1
  typeOf Property = "Property"
  -- valueOf = show

data Currency = USD | EUR | GBP
  deriving (Show)

instance AssetType Currency where
  chainOf USD = Ethereum_1
  chainOf EUR = Ethereum_1
  chainOf GBP = Ethereum_2
  typeOf = show 
  -- valueOf = show

data Vote = ApprovedByNotary | RejectedByNotary
  deriving (Show)

instance AssetType Vote where
  chainOf _ = Ethereum_1
  typeOf = show
  -- valueOf = show

-- Property Sale
propSale :: (Participant, Asset Property)
              -> Participant -> Asset Currency -> Asset Currency
              -> Participant -> Contract
propSale (seller, property) buyer deposit balance notary =
            onUserCommit "commitProperty" (getAssetType property, isCommitTo buyer .&. isCommitBy seller .&. isAsset property)
              doBuyerCommit
              (onTimeout 10 end)
  where
    doBuyerCommit   = onUserCommit "payDeposit" (EUR, isCommitBy buyer .&. isCommitTo seller .&. isAsset deposit)
                        doBalanceCommit
                        (onTimeout 20 (cancelAll end))
    doBalanceCommit = onUserCommit "payBalance" (EUR, isCommitTo seller .&. isAsset balance) -- buyer or bank submits balance
                        (oneOf (notaryApproval, notaryRejection))
                        (onTimeout 100 sellerTakesAll)
    sellerTakesAll  = autoRelease (whereRecipientIs(seller, allCommitments))
                        (autoCancel (whereCommitterIs(seller, allCommitments))
                          end)
    notaryApproval  = onUserCommit "approved" (ApprovedByNotary, isCommitBy notary .&. isCommitTo notary)
                       (releaseAll end)
                       (onTimeout 200 (cancelAll end))
    notaryRejection = onUserCommit "rejected" (RejectedByNotary, isCommitBy notary .&. isCommitTo notary)
                        (cancelAll end)
                        (onTimeout 200 (cancelAll end))

notaryApprove :: Asset Vote
notaryApprove = asset(ApprovedByNotary, 1)

notaryReject :: Asset Vote
notaryReject = asset(RejectedByNotary, 1)

hamrunFlat :: Asset Property
hamrunFlat = asset(Property, 1) -- {ref="1,High Street, Hamrun"}

deposit, balance, fee1, fee2 :: Asset Currency
deposit = asset(EUR, 10000)
balance = asset(EUR, 90000)
fee1     = asset(EUR, 1)
fee2     = asset(GBP, 1)

dv1, dv2 :: Asset DaoVote
dv1 = asset(VoteDaveE1, 1)
dv2 = asset(VoteDaveE2, 1)

-- DAO - a Decentralised Autonomous Organisation
-- A DAO has 2 parts.  
-- (1) Collect funds from participants 
-- (2) Disburse funds according to votes from participants
data DaoVote = VoteDaveE1 | VoteDaveE2 | VoteErinE1 | VoteErinE2 
  deriving (Show)

instance AssetType DaoVote where
  chainOf VoteDaveE1 = Ethereum_1
  chainOf VoteDaveE2 = Ethereum_2
  chainOf VoteErinE1 = Ethereum_1
  chainOf VoteErinE2 = Ethereum_2
  typeOf = show
  -- valueOf  VoteDaveE1 = "dave"
  -- valueOf  VoteDaveE2 = "dave"
  -- valueOf  VoteErinE1 = "erin"
  -- valueOf  VoteErinE2 = "erin"

data DaoVote2 = VoteE1 | VoteE2
  deriving (Show)

instance AssetType DaoVote2 where
  chainOf VoteE1 = Ethereum_1
  chainOf VoteE2 = Ethereum_2
  typeOf = show
  -- valueOf (VoteE1 x) = name x
  -- valueOf (VoteE2 x) = name x


dao :: ((Participant, Asset Currency, Asset DaoVote), (Participant, Asset Currency, Asset DaoVote), (Participant, Asset Currency, Asset DaoVote)) -> 
        (Participant, Participant) -> Contract
dao ((voter1, v1Currency, v1Vote), (voter2, v2Currency, v2Vote), (voter3, v3Currency, v3Vote)) (candidate1, candidate2) = 
        (doPayment voter1 v1Currency .&&. doPayment voter2 v2Currency .&&. doPayment voter3 v3Currency) .>>>.
        (doVote voter1 v1Vote .&&. doVote voter2 v2Vote .&&. doVote voter3 v3Vote) .>>>. 
        ifThenElse (cand1Count .>. cand2Count) 
          (releaseAll (sendAssets candidate1 end),
          releaseAll (sendAssets candidate2 end))
  where
    doPayment x y = onUserCommit ("pay" ++ name x) (assetType y, (isCommitBy x .&. isCommitTo this))
                      end
                      (onTimeout 200 (cancelAll end))
    doVote x y = onUserCommit ("vote" ++ name x) (assetType y, (isCommitBy x .&. isCommitTo x))
                      end
                      (onTimeout 200 (cancelAll end))
    
    cand1Count = countC (whereAssetValueIs (name candidate1, allCommitments))
    cand2Count = countC (whereAssetValueIs (name candidate2, allCommitments))

vote1, vote2 :: Asset DaoVote2
vote1 = asset(VoteE1, 1)
vote2 = asset(VoteE2, 1)

dao2 :: [(Participant, Currency, Asset DaoVote2)] -> Contract
dao2 pp =
  (foldl1 (.&&.) (map (\(x, y, _) -> doPayment x y) pp)) .>>>. 
  (foldl1 (.&&.) (map (\(x, _, z) -> doVote x z) pp)) .>>>. 
  autoRelease (payments) (sendAssets this (cancelAll end))
  -- cancelAll (committedVotes) (releaseAll (sendAssets this end))
  where
    doPayment x y = onUserCommit ("pay" ++ name x) (y, (isCommitBy x .&. isCommitTo this))
                      end
                      (onTimeout 200 (cancelAll end))
    doVote x y = onUserCommit ("vote" ++ name x) (assetType y, (isCommitBy x))
                      end
                      (onTimeout 200 (cancelAll end))
    payments = whereAssetTypeIs (EUR, allCommitments)

-- Executables
t1 :: Contract
t1 = both (fireEvent "event 1" end, fireEvent "event 2" end)

t2 :: Contract
t2 = both (c1, c2) .>>>. fireEvent "ready" c3
  where
    c1 = fireEvent "starting c1"
           (onUserCommit "c1" (EUR, isCommitBy bob .&. isCommitTo alice) end
             (onTimeout 200 end))
    c2 = fireEvent "starting c2"
           (onUserCommit "c2" (GBP, isCommitBy alice .&. isCommitTo bob) end
             (onTimeout 200 end))
    c3 = onUserCommit "c3" (EUR, isCommitBy charlie .&. isCommitTo bob) (releaseAll end)
          (onTimeout 200 (cancelAll end))

t3 :: Contract
t3 = both (c1, c2) .>>>. releaseAll end
  where
    c1 = onUserCommit "c1" (EUR, isCommitBy bob .&. isCommitTo alice) end
             (onTimeout 200 end)
    c2 = onUserCommit "c2" (GBP, isCommitBy alice .&. isCommitTo bob) end
             (onTimeout 200 end)

t4 :: Contract
t4 = ifThenElse (sumC(USD, allCommitments) .>. asset(USD, 100)) (c1, c2)
    where
      c1 = fireEvent "c1" end
      c2 = fireEvent "c2" end

t5 :: Contract
t5 = ifThenElse (sumC(USD, commitments) .<=. asset (USD, 100)) (c1, c2)
    where
      c1 = fireEvent "c1" end
      c2 = fireEvent "c2" end

      commitments = whereCommitterIs(alice, allCommitments)

d1 :: Contract
d1 = dao ((alice, fee1, dv1), (bob, fee1, dv2), (charlie, fee2, dv1)) (dave, erin)

d2 :: Contract
d2 = dao2 [(alice, EUR, vote1), (bob,GBP,vote2)]

-- just an example on how to instantiate contracts
runExample :: IO[()]
runExample = toFiles mem (swap (alice, asset(Apple, 1)) (bob, asset (Orange, 2)))

runDao :: IO[()]
runDao = toFiles mem d1

runDao2 :: IO[()]
runDao2 = toFiles mem d2


liftN :: Int -> N
liftN x = I (toInteger x)


