{-# LANGUAGE GADTs                 #-}
module Commitments where

-- data Participant = Participant {name::String, address::String}
-- data Asset = Asset {aName::String, aQuantity::Integer}

-- data Commitment = Commitment{sender::Participant, receiver::Participant, asset::Asset}



-- data Commitment where
--   AllCommitments :: Commitment
--   AndCF :: Commitment -> Commitment -> Commitment
--   OrCF :: Commitment -> Commitment -> Commitment
--   WhereCommitter :: Participant -> Commitment -> Commitment
--   WhereRecipient :: Participant -> Commitment -> Commitment
--   WhereAssetType :: (AssetType t) => t -> Commitment -> Commitment
--   WhereAssetValue :: String -> Commitment -> Commitment
--   WhereDate :: Time -> Commitment -> Commitment
--   WhereDateBefore :: Time -> Commitment -> Commitment
--   WhereDateAfter :: Time -> Commitment -> Commitment
--   WhereAssetQuantity :: Integer -> Commitment -> Commitment
--   WhereAssetQuantityLess :: Integer -> Commitment -> Commitment
--   WhereAssetQuantityGreater :: Integer -> Commitment -> Commitment
--   WhereId :: CommitmentId -> Commitment -> Commitment
--   OrderCF :: CommitmentField -> Order -> Commitment -> Commitment

-- instance Show Commitment where
--   show AllCommitments = "AllCommitments"
--   show (AndCF c1 c2)  = "(" ++ show c1 ++ ") and (" ++ show c2 ++ ")"
--   show (OrCF c1 c2)  = "(" ++ show c1 ++ ") or (" ++ show c2 ++ ")"
--   show (WhereCommitter p c) = "Where committer is " ++ show p ++ "(" ++ show c ++ ")"
--   show (WhereRecipient p c) = "Where recipient is " ++ show p ++ "(" ++ show c ++ ")"
--   show (WhereAssetType p c) = "Where assetType is " ++ show p ++ "(" ++ show c ++ ")"
--   show (WhereAssetValue p c) = "Where assetValue is " ++ show p ++ "(" ++ show c ++ ")"  
--   show (WhereDate p c) = "Where date is " ++ show p ++ "(" ++ show c ++ ")"
--   show (WhereDateBefore p c) = "Where date before " ++ show p ++ "(" ++ show c ++ ")"
--   show (WhereDateAfter p c) = "Where date after " ++ show p ++ "(" ++ show c ++ ")"
--   show (WhereAssetQuantity p c) = "Where quantity is " ++ show p ++ "(" ++ show c ++ ")"
--   show (WhereAssetQuantityLess p c) = "Where quantity less than  " ++ show p ++ "(" ++ show c ++ ")"
--   show (WhereAssetQuantityGreater p c) = "Where quantity greater than " ++ show p ++ "(" ++ show c ++ ")"
--   show (WhereId p c) = "Where id is " ++ show p ++ "(" ++ show c ++ ")"
--   show (OrderCF p o c) = "order by  " ++ show p ++ " " ++ show o ++ "(" ++ show c ++ ")"

-- data Order = ASC | DESC
--   deriving (Show, Eq)

-- data CommitmentField = CParticipantField |
--                        CAssetTypeField |
--                        CDateField |
--                        CAssetQuantityField |
--                        CIdField |
--                        CTagField
--   deriving (Show, Eq)
