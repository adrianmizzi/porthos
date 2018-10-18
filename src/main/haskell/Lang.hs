module Lang where

import           Porthos

end :: Contract
end = Null

-- claim :: (ActionName, TxFilterExpr, Commitment, Contract, Timeout, Contract) -> Contract
-- claim (n, tf, commitment, continueWith, timeout, c') =
--   UserAction n tf (Claim commitment) continueWith timeout c'

-- cancel :: (ActionName, TxFilterExpr, Commitment, Contract, Timeout, Contract) -> Contract
-- cancel (n, tf, commitment, continueWith, timeout, c') =
--   UserAction n tf (CancelCommit commitment) continueWith timeout c'

onUserCommit :: (AssetType t) => (ActionName, t, TxFilterExpr) -> Contract -> (Timeout, Contract) -> Contract
onUserCommit (n, t, x) c y = commitWithTagAndId n t x c y NoTag NoId

commitWithTag :: (AssetType t) => ActionName -> t -> TxFilterExpr -> Contract -> (Timeout, Contract) -> TagId -> Contract
commitWithTag n t x c y tag = commitWithTagAndId n t x c y tag NoId

commitWithId :: (AssetType t) => ActionName -> t -> TxFilterExpr -> Contract -> (Timeout, Contract) -> CommitmentId -> Contract
commitWithId n t x c y = commitWithTagAndId n t x c y NoTag

commitWithTagAndId :: (AssetType t) => ActionName -> t -> TxFilterExpr -> Contract -> (Timeout, Contract) -> TagId -> CommitmentId -> Contract
commitWithTagAndId n t tf continueWith (timeout, c) tag cId=
  UserAction n t tf (Commit cId tag) continueWith timeout c

repeatCommit :: (AssetType t) => (ActionName, t, TxFilterExpr) -> (Timeout, Contract) -> Contract
repeatCommit (n, at, tf) (timeout, c) =
  RepeatUserAction n at tf (Commit NoId NoTag) timeout c

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

-- isAssetType :: (AssetType t) => t -> TxFilterExpr
-- isAssetType = AssetTypeIs

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
