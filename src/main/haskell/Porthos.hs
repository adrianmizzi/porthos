{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Porthos where

data Contract where
  Null :: Contract
  UserAction :: (AssetType t) => ActionName -> t -> TxFilterExpr ->  UserTx -> Contract -> Timeout -> Contract -> Contract
  RepeatUserAction :: (AssetType t) => ActionName -> t -> TxFilterExpr -> UserTx -> Timeout -> Contract -> Contract
  AutoAction :: AutoTx -> Contract -> Contract
  FollowedBy :: Contract -> Contract -> Contract
  FireEvent :: EventDetails -> Contract -> Contract
  IfThenElse :: CBool -> Contract -> Contract -> Contract
  OneOf :: Contract -> Contract -> Contract
  Both :: Contract -> Contract -> Contract
  SendAssets :: Participant -> Contract -> Contract

instance Show Contract where
  show = showI ""

showI :: String -> Contract -> String
showI _ Null = "End"
showI indent (UserAction n at f tx c t c') = "\n" ++ indent' ++ "On User Action " ++ show n ++ "[" ++ show at ++ "]: " ++ show tx ++ "\n" ++ indent' ++ "  filter=(" ++ show f ++ ")\n" ++ indent' ++ "then {" ++ showI indent' c ++ "}\n  " ++ indent ++ show t ++ " {" ++ showI indent' c' ++ "}"
  where
    indent' = indent ++ "  "
showI indent (RepeatUserAction n at f tx t c) = "\n" ++ indent' ++ "Repeat User Action " ++ show n ++ "[" ++ show at ++ "]: " ++ show tx ++ " filter=(" ++ show f ++ ")\n  " ++ show t ++ " - [" ++ showI indent' c ++ "]"
  where
    indent' = indent ++ "  "
showI _ (AutoAction tx c) = "Auto Action: " ++ show tx ++ ", then " ++ show c
showI _ (FollowedBy c1 c2) = "{" ++ show c1 ++ "}\nthen\n{" ++ show c2 ++ "}"
showI _ (FireEvent d c) = "Event (" ++ show d ++ ") then " ++ show c
showI _ (IfThenElse b c1 c2) = "If (" ++ show b ++ ") then {" ++ show c1 ++ "} else {" ++ show c2 ++ "}"
showI _ (OneOf c1 c2) = "One of (" ++ show c1 ++ ", " ++ show c2 ++ ")"
showI _ (Both c1 c2) = "Both (" ++ show c1 ++ ", " ++ show c2 ++ ")"
showI indent (SendAssets p c) = "SendAssets to " ++ show p ++ " then {\n" ++ showI indent' c ++ "}"
  where
    indent' = indent ++ "  "

data TxFilterExpr where
  NoTxFilter  :: TxFilterExpr
  AndTF       :: TxFilterExpr -> TxFilterExpr -> TxFilterExpr
  -- OrTF        :: TxFilterExpr -> TxFilterExpr -> TxFilterExpr
  Sender      :: Participant -> TxFilterExpr
  Recipient   :: Participant -> TxFilterExpr
  AssetIs     :: (AssetType t) => Asset t -> TxFilterExpr
  AssetTypeIs :: (AssetType t) => t -> TxFilterExpr

instance Show TxFilterExpr where
  show NoTxFilter      = "No Filter"
  show (AndTF f1 f2)   = "(" ++ show f1 ++ ") and (" ++ show f2 ++ ")"
  -- show (OrTF f1 f2)    = "(" ++ show f1 ++ ") or (" ++ show f2 ++ ")"
  show (Sender p)      = "Sender = " ++ show p
  show (Recipient p)   = "Participant = " ++ show p
  show (AssetIs a)     = "Asset = " ++ show a
  show (AssetTypeIs t) = "Asset Type = " ++ show t

class Filter a where
  and :: a -> a -> a
  -- or  :: a -> a -> a
  (.&.) :: a -> a -> a
  x .&. y = Porthos.and x y
  -- (.|.) :: a -> a -> a
  -- x .|. y = Porthos.or x y

instance Filter TxFilterExpr where
  and = AndTF
  -- or  = OrTF

-- DO NOT DELETE
-- instance Filter ConditionFilter where
--   and = AndCF
--   or  = OrCF

-- data Condition = CTrue |
--                  CFalse |
--                  And Condition Condition |
--                  Or Condition Condition |
--                  Not Condition |
--                  GT_ Asset Asset | LT_ Asset Asset | EQ_ Asset Asset |
--                  GTE_ Asset Asset | LTE_ Asset Asset
--   deriving (Show, Eq)

data UserTx = Commit CommitmentId TagId -- |
              -- Claim Commitment |
              -- CancelCommit Commitment

instance Show UserTx where
  show (Commit cid tag) = "Commit[" ++ show cid ++ ", " ++ show tag ++ "]"
  -- show (Claim c)        = "Claim " ++ show c
  -- show (CancelCommit c) = "CancelCommit " ++ show c

data AutoTx = ReleaseAll 
            | AutoCancelAll
            | Release CommitmentSet
            | Cancel CommitmentSet
  deriving (Show)

data Asset t where
  Asset :: (AssetType t) => t -> Integer -> Asset t
  Sum   :: (AssetType t) => t -> CommitmentSet -> Asset t
  Add   :: (AssetType t) => Asset t -> Asset t -> Asset t
  Convert :: (AssetType t1, AssetType t2) => (t1, t2, Float) -> Asset t1 -> Asset t2

instance (Show t, AssetType t) => Show (Asset t) where
  show (Asset t n) =  show n ++ " " ++ show t
  show (Sum t c)   = "Sum " ++ show t ++ " " ++ show c
  show (Add a b)   = "Add " ++ show a ++ " " ++ show b
  show (Convert (t1, t2, f) at1) = "Convert " ++ show t1 ++ " -> " ++ show t2 ++ "(" ++ show f ++ ")" ++ show at1



-- type Blockchain = String

data Blockchain = Ethereum_1 | Ethereum_2 | Hyperledger
  deriving (Show, Eq)

data Language = Solidity | Chaincode
  deriving (Show, Eq)

type ChainToLang = Blockchain -> Language

initMem :: ChainToLang
initMem _ = undefined

update :: ChainToLang -> (Blockchain, Language) -> ChainToLang
update state (v, n) = \w -> if v==w then n else state w

class (Show a) => AssetType a where
    chainOf :: a -> Blockchain
    typeOf  :: a -> String
    -- valueOf :: a -> String
    -- equals  :: a -> a -> CBool
    -- compare :: a -> a -> CBool 

-- data Blockchain = Ethereum_1 | Ethereum_2 | Hyperledger
--   deriving (Show)

-- class (Show t) => AssetType t where
--   onChain :: t -> Blockchain

data CommitmentId = NoId |
                    Id String
  deriving (Show, Eq)

data TagId = NoTag |
             Tag String
  deriving (Show, Eq)

newtype Timeout   = Timeout Time
  deriving (Show, Eq)

type EventDetails = String
type Time         = Integer
type ActionName   = String

data Participant where
  Participant :: String -> String -> Participant
  MaxOf :: CommitmentGroup -> Participant

instance Show Participant where
  show (Participant name _) = name
  show (MaxOf g) = "maxOf [" ++ show g ++"]"

address :: Participant -> String
address (Participant _ a) = a
address (MaxOf g) = "address maxof [" ++ show g ++ "]"

data CBool where
  CTrue   :: CBool 
  CFalse  :: CBool
  CAnd    :: CBool -> CBool -> CBool
  COr     :: CBool -> CBool -> CBool
  CNot    :: CBool -> CBool
  CGTN    :: N -> N -> CBool
  CEQN    :: N -> N -> CBool
  CLTN    :: N -> N -> CBool
  CGTEN   :: N -> N -> CBool
  CLTEN   :: N -> N -> CBool -- (Show a, Comparable a) => a -> a -> CBool
  CGTA    :: (AssetType t) => Asset t -> Asset t -> CBool
  CEQA    :: (AssetType t) => Asset t -> Asset t -> CBool
  CLTA    :: (AssetType t) => Asset t -> Asset t -> CBool
  CGTEA   :: (AssetType t) => Asset t -> Asset t -> CBool
  CLTEA   :: (AssetType t) => Asset t -> Asset t -> CBool

instance Show CBool where
  show CTrue        = "True"
  show CFalse       = "False"
  show (CAnd c1 c2) = "And " ++ show c1 ++ " " ++ show c2
  show (COr c1 c2)  = "Or " ++ show c1 ++ " " ++ show c2
  show (CNot c)     = "Not " ++ show c
  show (CGTN c1 c2)  = show c1 ++ " > " ++ show c2
  show (CLTN c1 c2)  = show c1 ++ " < " ++ show c2
  show (CEQN c1 c2)  = show c1 ++ " == " ++ show c2
  show (CGTEN c1 c2) = show c1 ++ " >= " ++ show c2
  show (CLTEN c1 c2) = show c1 ++ " <= " ++ show c2
  show (CGTA c1 c2)  = show c1 ++ " > " ++ show c2
  show (CLTA c1 c2)  = show c1 ++ " < " ++ show c2
  show (CEQA c1 c2)  = show c1 ++ " == " ++ show c2
  show (CGTEA c1 c2) = show c1 ++ " >= " ++ show c2
  show (CLTEA c1 c2) = show c1 ++ " <= " ++ show c2

-- instance Eq CBool where
--   CTrue == CTrue = True
--   CFalse == CFalse = True
--   (CAnd c1 c2) == (CAnd c1' c2') = (c1 == c1') && (c2 == c2')
--   (COr c1 c2) == (COr c1' c2') = (c1 == c1') && (c2 == c2')
--   (CNot c) == (CNot c') = c == c'
--   _ == _ = False
  -- (CGT c1 c2) == (CGT c1' c2') = (c1 == c1') && (c2 == c2')
  -- (CLT c1 c2) == (CLT c1' c2') = (c1 == c1') && (c2 == c2')
  -- (CEQ c1 c2) == (CEQ c1' c2') = (c1 == c1') && (c2 == c2')
  -- (CGTE c1 c2) == (CGTE c1' c2') = (c1 == c1') && (c2 == c2')
  -- (CLTE c1 c2) == (CLTE c1' c2') = (c1 == c1') && (c2 == c2')

data N = I Integer |
         Count CommitmentSet |
         AddN N N | 
         NegN N
  deriving (Show)

instance Num N where
  I x + I y     = I (x + y)
  x + y         = AddN x y
  fromInteger x = I x
  negate (I x)  = I (negate x)
  negate x      = NegN x

class Comparable a where
  (.==.) :: a -> a -> CBool
  (.>.) :: a -> a -> CBool
  (.<.) :: a -> a -> CBool
  (.>=.) :: a -> a -> CBool
  (.<=.) :: a -> a -> CBool

instance Comparable N where
  x .==. y = CEQN x y
  x .>.  y = CGTN x y
  x .<.  y = CLTN x y
  x .<=. y = CLTEN x y
  x .>=. y = CGTEN x y

instance (AssetType t) => Comparable (Asset t) where
  a .==. b
    | typeOf (getAssetType a) == typeOf (getAssetType b) = CEQA a b
    | otherwise            = error ("Type mismatch:" ++ typeOf (getAssetType a) ++ " - " ++ typeOf (getAssetType b)) 
  a .>. b
    | typeOf (getAssetType a) == typeOf (getAssetType b) = CGTA a b
    | otherwise            = error ("Type mismatch:" ++ typeOf (getAssetType a) ++ " - " ++ typeOf (getAssetType b)) 
  a .<. b
    | typeOf (getAssetType a) == typeOf (getAssetType b) = CLTA a b
    | otherwise            = error ("Type mismatch:" ++ typeOf (getAssetType a) ++ " - " ++ typeOf (getAssetType b)) 
  a .>=. b
    | typeOf (getAssetType a) == typeOf (getAssetType b) = CGTEA a b
    | otherwise            = error ("Type mismatch:" ++ typeOf (getAssetType a) ++ " - " ++ typeOf (getAssetType b)) 
  a .<=. b
    | typeOf (getAssetType a) == typeOf (getAssetType b) = CLTEA a b
    | otherwise            = error ("Type mismatch:" ++ typeOf (getAssetType a) ++ " - " ++ typeOf (getAssetType b)) 


getAssetType :: (AssetType t) => Asset t -> t
getAssetType (Asset x _) = x
getAssetType (Sum x _)   = x
getAssetType (Add x _)   = getAssetType x
getAssetType (Convert (_, t2, _) _) = t2

getAssetQuantity :: (AssetType t) => Asset t -> Integer
getAssetQuantity (Asset _ q) = q
getAssetQuantity (Convert (_, _, f) at1) = round (f * fromIntegral (getAssetQuantity at1))
getAssetQuantity _ = undefined


-- Commitments

data CommitmentSet where
    AllCommitments   :: CommitmentSet
    FilterBySender   :: CommitmentSet -> Participant -> CommitmentSet
    FilterByReceiver :: CommitmentSet -> Participant -> CommitmentSet
    FilterByAsset    :: (AssetType t) => CommitmentSet -> t -> CommitmentSet
    -- OrderBy :: CommitmentSet -> CommitmentField -> CommitmentSet

data CommitmentGroup where
    GroupBySender :: CommitmentSet -> CommitmentGroup
    GroupByReceiver :: CommitmentSet -> CommitmentGroup

-- data CommitmentMax where
--     MaxOf :: CommitmentGroup -> CommitmentMax

instance Show CommitmentSet where
    show AllCommitments = "AllCommitments"
    show (FilterBySender c p) = "(" ++ show c ++ ") where sender is " ++ show p
    show (FilterByReceiver c p) = "(" ++ show c ++ ") where receiver is " ++ show p
    -- show (GroupByMax c f) = show c ++ " group by max " ++ show f

instance Show CommitmentGroup where
    show (GroupBySender c) = "(" ++ show c ++ ") group by sender"
    show (GroupByReceiver c) = "(" ++ show c ++ ") group by receiver"

-- instance Show CommitmentMax where
--     show (MaxOf g) = "max of [" ++ show g ++ "]"

allCommitments :: CommitmentSet
allCommitments = AllCommitments

whereSenderIs :: CommitmentSet -> Participant -> CommitmentSet
whereSenderIs c f = FilterBySender c f

whereReceiverIs :: CommitmentSet -> Participant -> CommitmentSet
whereReceiverIs c f = FilterByReceiver c f

whereAssetTypeIs :: (AssetType t) => CommitmentSet -> t -> CommitmentSet
whereAssetTypeIs c f = FilterByAsset c f

groupBySender :: CommitmentSet -> CommitmentGroup
groupBySender c = GroupBySender c

maxOf :: CommitmentGroup -> Participant
maxOf g = MaxOf g



