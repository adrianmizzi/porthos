module Examples where

import           Codegen.Solidity
import           Lang
import           Porthos
import           StatementGenerator

data MyAssetType = Apple | Orange
  deriving (Show, Eq)

instance AssetType MyAssetType where

-- Atomic Swap
swap :: (Participant, Asset MyAssetType) -> (Participant, Asset MyAssetType) -> Contract
swap (p1, a1) (p2, a2) = onUserCommit "p1Commit" (isCommitTo p2 .&. isAsset a1)
                           doP2Commit 
                           (onTimeout 10 end)
  where
    doP2Commit = onUserCommit "p2Commit" (isCommitTo p1 .&. isAsset a2 .&. isCommitBy p2)
                    (releaseAll end)
                    (onTimeout 20 (cancelAll end))

-- Crowdfunding
crowdFunding :: Participant -> Asset MyAssetType -> Contract
crowdFunding recipient target
          = repeatCommit "fund" (isCommitTo recipient .&. isAssetType aType)
              (onTimeout 100 closeCampaign)
  where
    aType = assetType target
    closeCampaign = ifThenElse (sumCommit .>. target)
                      (releaseAll (fireEvent "Campaign Successful" end),
                       cancelAll (fireEvent "Campagin Failed" end))
    sumCommit = sumC aType allCommitments

-- Group Pay
groupPay :: [(Participant, Asset MyAssetType)] -> Participant -> Contract
groupPay yy recipient = allOf (userCommits yy) `followedBy`
                          ifThenElse (countC allCommitments .==. length yy)
                            (releaseAll end,
                            cancelAll end)
  where
    userCommits = map (\x -> onUserCommit "pay" (txFilter x) end (onTimeout 100 end))
    txFilter (a, b) = isCommitTo recipient .&. isCommitBy a .&. isAsset b

allOf :: [Contract] -> Contract
allOf []     = Null
allOf [c]    = c
allOf (c:cc) = Both c (allOf cc)

-- Time-locked piggy bank
piggy :: Participant -> Time -> Contract
piggy recipient expiryTime = repeatCommit "save" (isCommitTo recipient .&. isAssetType Apple)
                               (onTimeout expiryTime (releaseAll end))

-- Crowdfunding on multiple assets
crowdFunding2 :: Participant -> (Asset MyAssetType, Asset MyAssetType) -> Asset MyAssetType -> Contract
crowdFunding2 recipient (x, y) target = both (campaignX, campaignY) `followedBy` closeCampaign
  where
    campaignX = repeatCommit "fundX" (isCommitTo recipient .&. isAssetType xType)
                  (onTimeout 100 end)
    campaignY = repeatCommit "fundY" (isCommitTo recipient .&. isAssetType yType)
                  (onTimeout 100 end)
    closeCampaign = ifThenElse (totalX .>. target)
                      (releaseAll (fireEvent "Campaign Successful" end),
                       cancelAll (fireEvent "Campaign Failed" end))
    xType = assetType x
    yType = assetType y
    sumCommitX = sumC xType allCommitments
    sumCommitY = sumC yType allCommitments

    totalX = sumCommitX .+. exchange (x, y) sumCommitY

data Property = Property {ref :: String}
  deriving (Show, Eq)

instance AssetType Property where

data Currency = USD | EUR | GBP
  deriving (Show)

instance AssetType Currency where

data Vote = ApprovedByNotary | RejectedByNotary
  deriving (Show)

instance AssetType Vote where

-- Property Sale
propSale :: (Participant, Asset Property)
              -> Participant -> Asset Currency -> Asset Currency
              -> Participant -> Contract
propSale (seller, property) buyer deposit balance notary =
            onUserCommit "commitProperty" (isCommitTo buyer .&. isCommitBy seller .&. isAsset property)
              doBuyerCommit
              (onTimeout 10 end)
  where
    doBuyerCommit   = onUserCommit "payDeposit" (isCommitBy buyer .&. isCommitTo seller .&. isAsset deposit)
                        doBalanceCommit
                        (onTimeout 20 (cancelAll end))
    doBalanceCommit = onUserCommit "payBalance" (isCommitTo seller .&. isAsset balance) -- buyer or bank submits balance
                        (oneOf (notaryApproval, notaryRejection))
                        (onTimeout 100 sellerTakesAll)
    sellerTakesAll  = autoRelease (whereRecipientIs seller allCommitments)
                        (autoCancel (whereCommitterIs seller allCommitments)
                          end)
    notaryApproval  = onUserCommit "approved" (isCommitBy notary .&. isAsset notaryApprove .&. isCommitTo notary)
                       (releaseAll end)
                       (onTimeout 200 (cancelAll end))
    notaryRejection = onUserCommit "rejected" (isCommitBy notary .&. isAsset notaryReject .&. isCommitTo notary)
                        (cancelAll end)
                        (onTimeout 200 (cancelAll end))

notaryApprove :: Asset Vote
notaryApprove = asset(ApprovedByNotary, 1)

notaryReject :: Asset Vote
notaryReject = asset(RejectedByNotary, 1)

t1 :: Contract
t1 = both (fireEvent "event 1" end, fireEvent "event 2" end)

t2 :: Contract
t2 = both (c1, c2) .>>>. fireEvent "ready" c3
  where
    c1 = fireEvent "starting c1"
           (onUserCommit "c1" (isCommitBy bob .&. isCommitTo alice) end
             (onTimeout 200 end))
    c2 = fireEvent "starting c2"
           (onUserCommit "c2" (isCommitBy alice .&. isCommitTo bob) end
             (onTimeout 200 end))
    c3 = onUserCommit "c3" (isCommitBy charlie .&. isCommitTo bob) (releaseAll end)
          (onTimeout 200 (cancelAll end))

t3 :: Contract
t3 = x .>>>. y
  where
    x = fireEvent "Hello" end
    y = fireEvent "World" end


alice, bob, charlie :: Participant
alice = Participant {name="alice", address="0xf1c13a88cf28c4d06269a150dd8cdb2e3061d44f"}
bob = Participant {name="bob", address="0x4603bd7000aba82eab4aaea605df43a1e37ef2bb"}
charlie = Participant {name="charlie", address="0xb9f62ffe791ff9fa9c51722e3b833bf51db290de"}


eurRegister, gbpRegister :: AssetRegister
eurRegister = AssetRegister {regType = "EUR", blockchainSys=Ethereum_1}
gbpRegister = AssetRegister {regType = "GBP", blockchainSys=Ethereum_2}

main :: Contract -> IO()
main = generateSolidity [eurRegister, gbpRegister]

main2 :: Contract -> IO()
main2 = justStatements [eurRegister, gbpRegister]
