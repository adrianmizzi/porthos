{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Porthos where

data Contract = Null |
                UserAction ActionName TxFilterExpr UserTx Contract Timeout Contract |
                RepeatUserAction ActionName TxFilterExpr UserTx Timeout Contract |
                AutoAction AutoTx Contract |
                FollowedBy Contract Contract |
                FireEvent EventDetails Contract |
                IfThenElse CBool Contract Contract |
                OneOf Contract Contract |
                Both Contract Contract

instance Show Contract where
  show = showI ""

showI :: String -> Contract -> String
showI _ Null = "End"
showI indent (UserAction n f tx c t c') = "\n" ++ indent' ++ "On User Action " ++ show n ++ ": " ++ show tx ++ "\n" ++ indent' ++ "  filter=(" ++ show f ++ ")\n" ++ indent' ++ "then {" ++ showI indent' c ++ "}\n  " ++ indent ++ show t ++ " {" ++ showI indent' c' ++ "}"
  where
    indent' = indent ++ "  "
showI indent (RepeatUserAction n f tx t c) = "\n" ++ indent' ++ "Repeat User Action " ++ show n ++ ": " ++ show tx ++ " filter=(" ++ show f ++ ")\n  " ++ show t ++ " - [" ++ showI indent' c ++ "]"
  where
    indent' = indent ++ "  "
showI _ (AutoAction tx c) = "Auto Action: " ++ show tx ++ ", then " ++ show c
showI _ (FollowedBy c1 c2) = "{" ++ show c1 ++ "}\nthen\n{" ++ show c2 ++ "}"
showI _ (FireEvent d c) = "Event (" ++ show d ++ ") then " ++ show c
showI _ (IfThenElse b c1 c2) = "If (" ++ show b ++ ") then {" ++ show c1 ++ "} else {" ++ show c2 ++ "}"
showI _ (OneOf c1 c2) = "One of (" ++ show c1 ++ ", " ++ show c2 ++ ")"
showI _ (Both c1 c2) = "Both (" ++ show c1 ++ ", " ++ show c2 ++ ")"

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

data UserTx = Commit CommitmentId TagId |
              Claim Commitment |
              CancelCommit Commitment

instance Show UserTx where
  show (Commit cid tag) = "Commit[" ++ show cid ++ ", " ++ show tag ++ "]"
  show (Claim c)        = "Claim " ++ show c
  show (CancelCommit c) = "CancelCommit " ++ show c

data AutoTx = Release Commitment |
              AutoCancelCommit Commitment
  deriving (Show)

data Asset t where
  Asset :: (AssetType t) => t -> Integer -> Asset t
  Sum   :: (AssetType t) => t -> Commitment -> Asset t
  Add   :: (AssetType t) => Asset t -> Asset t -> Asset t

instance (Show t, AssetType t) => Show (Asset t) where
  show (Asset t n) =  show n ++ " " ++ show t
  show (Sum t c)   = "Sum " ++ show t ++ " " ++ show c
  show (Add a b)   = "Add " ++ show a ++ " " ++ show b

data Commitment where
  AllCommitments :: Commitment
  AndCF :: Commitment -> Commitment -> Commitment
  OrCF :: Commitment -> Commitment -> Commitment
  WhereCommitter :: Participant -> Commitment -> Commitment
  WhereRecipient :: Participant -> Commitment -> Commitment
  WhereAssetType :: (AssetType t) => t -> Commitment -> Commitment
  WhereDate :: Time -> Commitment -> Commitment
  WhereDateBefore :: Time -> Commitment -> Commitment
  WhereDateAfter :: Time -> Commitment -> Commitment
  WhereAssetQuantity :: Integer -> Commitment -> Commitment
  WhereAssetQuantityLess :: Integer -> Commitment -> Commitment
  WhereAssetQuantityGreater :: Integer -> Commitment -> Commitment
  WhereId :: CommitmentId -> Commitment -> Commitment
  OrderCF :: CommitmentField -> Order -> Commitment -> Commitment

instance Show Commitment where
  show AllCommitments = "All Commitments"
  show (AndCF c1 c2)  = "(" ++ show c1 ++ ") and (" ++ show c2 ++ ")"
  show (OrCF c1 c2)  = "(" ++ show c1 ++ ") or (" ++ show c2 ++ ")"
  show (WhereCommitter p c) = "Where committer is " ++ show p ++ "(" ++ show c ++ ")"
  show (WhereRecipient p c) = "Where recipient is " ++ show p ++ "(" ++ show c ++ ")"
  show (WhereAssetType p c) = "Where assetType is " ++ show p ++ "(" ++ show c ++ ")"
  show (WhereDate p c) = "Where date is " ++ show p ++ "(" ++ show c ++ ")"
  show (WhereDateBefore p c) = "Where date before " ++ show p ++ "(" ++ show c ++ ")"
  show (WhereDateAfter p c) = "Where date after " ++ show p ++ "(" ++ show c ++ ")"
  show (WhereAssetQuantity p c) = "Where quantity is " ++ show p ++ "(" ++ show c ++ ")"
  show (WhereAssetQuantityLess p c) = "Where quantity less than  " ++ show p ++ "(" ++ show c ++ ")"
  show (WhereAssetQuantityGreater p c) = "Where quantity greater than " ++ show p ++ "(" ++ show c ++ ")"
  show (WhereId p c) = "Where id is " ++ show p ++ "(" ++ show c ++ ")"
  show (OrderCF p o c) = "order by  " ++ show p ++ " " ++ show o ++ "(" ++ show c ++ ")"

data Order = ASC | DESC
  deriving (Show, Eq)

data CommitmentField = CParticipantField |
                       CAssetTypeField |
                       CDateField |
                       CAssetQuantityField |
                       CIdField |
                       CTagField
  deriving (Show, Eq)


class (Show t) => AssetType t where

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

data Participant = Participant {name::String, address::String}

instance Show Participant where
  show = name

data CBool where
  CTrue   :: CBool
  CFalse  :: CBool
  CAnd    :: CBool -> CBool -> CBool
  COr     :: CBool -> CBool -> CBool
  CNot    :: CBool -> CBool
  CGT     :: (CEq a b) => a -> b -> CBool
  CLT     :: (CEq a b) => a -> b -> CBool
  CEQ     :: (CEq a b) => a -> b -> CBool
  CGTE    :: (CEq a b) => a -> b -> CBool
  CLTE    :: (CEq a b) => a -> b -> CBool

instance Show CBool where
  show CTrue        = "True"
  show CFalse       = "False"
  show (CAnd c1 c2) = "And " ++ show c1 ++ " " ++ show c2
  show (COr c1 c2)  = "Or " ++ show c1 ++ " " ++ show c2
  show (CNot c)     = "Not " ++ show c
  show (CGT c1 c2)  = show c1 ++ " > " ++ show c2
  show (CLT c1 c2)  = show c1 ++ " < " ++ show c2
  show (CEQ c1 c2)  = show c1 ++ " == " ++ show c2
  show (CGTE c1 c2) = show c1 ++ " >= " ++ show c2
  show (CLTE c1 c2) = show c1 ++ " <= " ++ show c2

-- instance Eq CBool where
--   CTrue == CTrue = True
--   CFalse == CFalse = True
--   (CAnd c1 c2) == (CAnd c1' c2') = (c1 == c1') && (c2 == c2')
--   (COr c1 c2) == (COr c1' c2') = (c1 == c1') && (c2 == c2')
--   (CNot c) == (CNot c') = c == c'
--   (CGT c1 c2) == (CGT c1' c2') = (c1 == c1') && (c2 == c2')
--   (CLT c1 c2) == (CLT c1' c2') = (c1 == c1') && (c2 == c2')
--   (CEQ c1 c2) == (CEQ c1' c2') = (c1 == c1') && (c2 == c2')
--   (CGTE c1 c2) == (CGTE c1' c2') = (c1 == c1') && (c2 == c2')
--   (CLTE c1 c2) == (CLTE c1' c2') = (c1 == c1') && (c2 == c2')

data N = I Int |
         Count Commitment
  deriving (Show)

class (Show a, Show b) => CEq a b where
  (.==.) :: a -> b -> CBool
  (.>.)  :: a -> b -> CBool

instance CEq N N where
  x .==. y = CEQ x y
  x .>.  y = CGT x y

instance CEq N Int where
  x .==. y = CEQ x (I y)
  x .>.  y = CGT x (I y)

instance CEq Int N where
  x .==. y = CEQ (I x) y
  x .>. y  = CGT (I x) y

instance CEq Int Int where
  x .==. y = CEQ (I x) (I y)
  x .>. y  = CGT (I x) (I y)

instance (AssetType t) => CEq (Asset t) (Asset t) where
  x .==. y = CEQ x y
  x .>.  y = CGT x y

data Blockchain = Ethereum_1 | Ethereum_2 | Hyperledger

data AssetRegister = AssetRegister {regType :: String, blockchainSys :: Blockchain}

