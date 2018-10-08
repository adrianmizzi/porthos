module Lang where

import           Porthos

end :: Contract
end = Null

claim :: (ActionName, TxFilterExpr, Commitment, Contract, Timeout, Contract) -> Contract
claim (name, tf, commitment, continueWith, timeout, c') =
  UserAction name tf (Claim commitment) continueWith timeout c'

cancel :: (ActionName, TxFilterExpr, Commitment, Contract, Timeout, Contract) -> Contract
cancel (name, tf, commitment, continueWith, timeout, c') =
  UserAction name tf (CancelCommit commitment) continueWith timeout c'

onUserCommit :: ActionName -> TxFilterExpr -> Contract -> (Timeout, Contract) -> Contract
onUserCommit n x c y = commitWithTagAndId n x c y NoTag NoId

commitWithTag :: ActionName -> TxFilterExpr -> Contract -> (Timeout, Contract) -> TagId -> Contract
commitWithTag n x c y tag = commitWithTagAndId n x c y tag NoId

commitWithId :: ActionName -> TxFilterExpr -> Contract -> (Timeout, Contract) -> CommitmentId -> Contract
commitWithId n x c y = commitWithTagAndId n x c y NoTag

commitWithTagAndId :: ActionName -> TxFilterExpr -> Contract -> (Timeout, Contract) -> TagId -> CommitmentId -> Contract
commitWithTagAndId name tf continueWith (timeout, c) tag cId=
  UserAction name tf (Commit cId tag) continueWith timeout c

repeatCommit :: ActionName -> TxFilterExpr -> (Timeout, Contract) -> Contract
repeatCommit name tf (timeout, c) =
  RepeatUserAction name tf (Commit NoId NoTag) timeout c

autoCancel :: Commitment -> Contract -> Contract
autoCancel commitment = AutoAction (AutoCancelCommit commitment)

autoRelease :: Commitment -> Contract -> Contract
autoRelease commitment = AutoAction (Release commitment)

onTimeout :: Time -> Contract -> (Timeout, Contract)
onTimeout t c = (Timeout t, c)

releaseAll :: Contract -> Contract
releaseAll = AutoAction (Release AllCommitments)

cancelAll :: Contract -> Contract
cancelAll = AutoAction (AutoCancelCommit AllCommitments)

isCommitTo :: Participant -> TxFilterExpr
isCommitTo = Recipient

isCommitBy :: Participant -> TxFilterExpr
isCommitBy = Sender

isAsset :: (AssetType t) => Asset t -> TxFilterExpr
isAsset = AssetIs

isAssetType :: (AssetType t) => t -> TxFilterExpr
isAssetType = AssetTypeIs

ifThenElse :: CBool -> (Contract, Contract) -> Contract
ifThenElse cond (c1, c2) = IfThenElse cond c1 c2

fireEvent :: String -> Contract -> Contract
fireEvent = FireEvent

assetType :: (AssetType t) => Asset t -> t
assetType (Asset t _) = t
assetType (Sum t _)   = t

followedBy :: Contract -> Contract -> Contract
followedBy = FollowedBy

both :: (Contract, Contract) -> Contract
both (c1, c2) = Both c1 c2

oneOf :: (Contract, Contract) -> Contract
oneOf (c1, c2) = OneOf c1 c2

asset :: (AssetType t) => (t, Integer) -> Asset t
asset (t, q) = Asset t q

(.>>>.) :: Contract -> Contract -> Contract
a .>>>. b = a `followedBy` b

-- CBool
true :: CBool
true = CTrue

false :: CBool
false = CFalse

not :: CBool -> CBool
not = CNot

(.&&.) :: CBool -> CBool -> CBool
x .&&. y = CAnd x y

(.||.) :: CBool -> CBool -> CBool
x .||. y = COr x y

(.+.) :: (AssetType t) => Asset t -> Asset t -> Asset t
x .+. y = Add x y

exchange :: (AssetType tx, AssetType ty) => (Asset tx, Asset ty) -> Asset ty -> Asset tx
exchange (Asset a _, Asset _ q2) (Asset _ q) = Asset a (q * q2) -- / q1))

-- Commitments
allCommitments :: Commitment
allCommitments = AllCommitments

whereCommitterIs :: Participant -> Commitment -> Commitment
whereCommitterIs = WhereCommitter

whereRecipientIs :: Participant -> Commitment -> Commitment
whereRecipientIs = WhereRecipient

whereAssetTypeIs :: (AssetType t) => t -> Commitment -> Commitment
whereAssetTypeIs = WhereAssetType

orderByParticipant :: Order -> Commitment -> Commitment
orderByParticipant = OrderCF CParticipantField

asc :: Order
asc = ASC

desc :: Order
desc = DESC

selectAll :: (Commitment -> Commitment) -> Commitment -> Commitment
selectAll f = f

sumC :: (Show t, AssetType t) => t -> Commitment -> Asset t
sumC = Sum

countC :: Commitment -> N
countC = Count
