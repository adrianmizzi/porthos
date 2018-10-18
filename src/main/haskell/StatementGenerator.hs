{-# LANGUAGE GADTs #-}
module StatementGenerator where

import           Data.IORef
import           Porthos
import           System.IO.Unsafe

{-# NOINLINE idCounter #-}
idCounter :: IORef Integer
idCounter = unsafePerformIO (newIORef 1)

{-# NOINLINE bothCounter #-}
bothCounter :: IORef Integer
bothCounter = unsafePerformIO (newIORef 1)

-- generate a new label every time
nextId :: () -> Integer
nextId () = unsafePerformIO $ do
  p <- readIORef idCounter
  writeIORef idCounter (p Prelude.+ 1)
  return p

nextBothRef :: () -> Integer
nextBothRef () = unsafePerformIO $ do
  p <- readIORef bothCounter
  writeIORef idCounter (p Prelude.+ 1)
  return p

resetCounters :: IO ()
resetCounters = do
  writeIORef idCounter 1
  writeIORef bothCounter 1
  return ()

type State = Integer
type MethodName = String

data AccessModifier = Public | Private
  deriving (Eq)

instance Show AccessModifier where
  show Public  = "public"
  show Private = "private"

data PreCondition where
  PcFilter :: TxFilterExpr -> PreCondition
  PcTimeout :: Timeout -> PreCondition
  PcSemaphore :: String -> PreCondition
  PcGate :: String -> PreCondition

instance Show PreCondition where
  show (PcFilter tf)   = "PcFilter " ++ show tf
  show (PcTimeout t)   = "PcTimeout " ++ show t
  show (PcSemaphore s) = "PcSemaphore " ++ show s
  show (PcGate s)      = "PcGate " ++ show s

data Method = Method {modifier      :: AccessModifier,
                      methodName    :: MethodName,
                      methodType    :: MethodType,
                      preconditions :: [PreCondition],
                      statements    :: [Statement]}
  deriving (Show)

data MethodType = MTCommit |
                 MTCancel |
                 MTClaim |
                 MTAutoCancel |
                 MTRelease |
                 MTCommitTimeout |
                 MTCancelTimeout |
                 MTClaimTimeout |
                 MTConstructor |
                 MTOther
  deriving (Show, Eq)

data Statement where 
  S_AddCommitment :: Statement
  S_ReleaseCommitment :: Commitment -> Statement
  S_AutoCancelCommitment :: Commitment -> Statement
  S_FireEvent :: String -> Statement
  S_IfThenElse :: CBool -> [Statement] -> [Statement] -> Statement
  S_InitSemaphore :: String -> Statement
  S_CompleteSemaphore :: String -> Statement
  S_ContinueWith :: String -> Statement
  S_OpenGate :: (AssetType t) => t -> String -> Statement
  S_CloseGate :: String -> Statement

instance Show Statement where
  show S_AddCommitment = "S_AddCommitment"
  show (S_ReleaseCommitment c) = "S_ReleaseCommitment " ++ show c
  show (S_AutoCancelCommitment c) = "S_AutoCancelCommitment " ++ show c
  show (S_FireEvent s) = "S_FireEvent " ++ show s
  show (S_IfThenElse b ss1 ss2) = "S_IfThenElse (" ++ show b ++ ") then (" ++ show ss1 ++ ") else (" ++ show ss2 ++ ")"
  show (S_InitSemaphore s) = "S_InitSemaphore " ++ show s
  show (S_CompleteSemaphore s)  = "S_CompleteSemaphore " ++ show s
  show (S_ContinueWith s) = "S_ContinueWith " ++ show s
  show (S_OpenGate t g) = "S_OpenGate " ++ show t ++ " " ++ show g
  show (S_CloseGate g)  = "S_CloseGate " ++ show g

generateStatements :: Method -> Contract -> [Method]
generateStatements currentMethod Null = [currentMethod]
generateStatements currentMethod (UserAction actionName assetType txf tx c t tc) =
          currentMethod' : methods1 ++ methods2
  where
    methods1 = generateStatements cMethod c
    methods2 = generateStatements tMethod tc

    currentMethod' = addToMethod currentMethod (S_OpenGate assetType actionName)
    cMethod = Method Public actionName MTCommit
                [PcFilter txf, PcGate actionName]
                [S_AddCommitment, S_CloseGate actionName]
    tMethod = Method Public actionName MTCommitTimeout
                [PcTimeout t, PcGate actionName]
                [S_CloseGate actionName]
generateStatements currentMethod (RepeatUserAction actionName assetType txf tx t tc) =
            currentMethod' : cMethod : methods
  where
    methods = generateStatements tMethod tc

    currentMethod' = addToMethod currentMethod (S_OpenGate assetType actionName)

    cMethod = Method Public actionName MTCommit
                [PcFilter txf, PcGate actionName]
                [S_AddCommitment]
    tMethod = Method Public actionName MTCommitTimeout
                [PcTimeout t, PcGate actionName]
                [S_CloseGate actionName]
generateStatements currentMethod (AutoAction tx c) = methods
  where
    methods = generateStatements currentMethod' c

    currentMethod' = addToMethod currentMethod (getS tx)

    getS :: AutoTx -> Statement
    getS (Release c)          = S_ReleaseCommitment c
    getS (AutoCancelCommit c) = S_AutoCancelCommitment c


generateStatements currentMethod (FollowedBy c1 c2) = m2 ++ tail m1
  where
    m1 = generateStatements currentMethod c1
    m2 = generateStatements (head m1) c2
generateStatements currentMethod (FireEvent e c) = methods
  where
    methods = generateStatements currentMethod' c

    currentMethod' = addToMethod currentMethod (S_FireEvent e)

generateStatements currentMethod (IfThenElse b c1 c2) = [currentMethod']
  where
    -- TODO: Generate unique method names with a counter or something
    newMethod1 = Method Private "ifTrue" MTOther [] []
    newMethod2 = Method Private "ifFalse" MTOther [] []

    ifTrue  = generateStatements newMethod1 c1
    ifFalse = generateStatements newMethod2 c2

    ifTrue' = [x | x <- ifTrue, methodName x /= "ifTrue"]
    ifFalse' = [x | x <- ifFalse, methodName x /= "ifFalse"]

    trueStatements  = head [statements x | x <- ifTrue, methodName x == "ifTrue"]
    falseStatements = head [statements x | x <- ifFalse, methodName x == "ifFalse"]

    currentMethod' = addToMethod currentMethod (S_IfThenElse b trueStatements falseStatements)

generateStatements currentMethod (OneOf c1 c2) = [currentMethod]
generateStatements currentMethod (Both c1 c2) = continueMethod : currentMethod' : leftM'' ++ rightM''
  where
    bothId = nextBothRef ()

    leftName = "bothLeft" ++ show bothId
    rightName = "bothRight" ++ show bothId

    semLeft = "semLeft" ++ show bothId
    semRight = "semRight" ++ show bothId

    left = Method Private leftName MTOther [] []
    right = Method Private rightName MTOther [] []

    leftM = generateStatements left c1
    rightM = generateStatements right c2

    leftM' = [x | x <- leftM, methodName x /= leftName]
    rightM' = [x | x <- rightM, methodName x /= rightName]

    leftStatements  = head [statements x | x <- leftM, methodName x == leftName]
    rightStatements = head [statements x | x <- rightM, methodName x == rightName]

    leftM'' = addToMethods2 leftM' [S_CompleteSemaphore semLeft, S_ContinueWith contWithName]
    rightM'' = addToMethods2 rightM' [S_CompleteSemaphore semRight, S_ContinueWith contWithName]

    currentMethod' = addToMethod2 currentMethod
                       ([S_InitSemaphore semLeft,
                        S_InitSemaphore semRight] ++ leftStatements ++ rightStatements)

    contWithName = "continue_" ++ show bothId
    continueMethod = Method Private contWithName MTOther [PcSemaphore semLeft, PcSemaphore semRight] []


addToMethod :: Method -> Statement -> Method
addToMethod (Method modifier name t preC ss) s = Method modifier name t preC (ss ++ [s])

addToMethod2 :: Method -> [Statement] -> Method
addToMethod2 = foldl addToMethod

addToMethods :: [Method] -> Statement -> [Method]
addToMethods [] _     = []
addToMethods (m:mm) s = addToMethod m s : addToMethods mm s

addToMethods2 :: [Method] -> [Statement] -> [Method]
addToMethods2 [] _      = []
addToMethods2 (m:mm) ss = addToMethod2 m ss : addToMethods2 mm ss

-- filterToPreConditions :: TxFilterExpr -> [PreCondition]
-- filterToPreConditions NoTxFilter = []
-- filterToPreConditions (AndTF t1 t2) = filterToPreConditions t1 ++ filterToPreConditions t2
-- filterToPreConditions (OrTF t1 t2) = filterToPreConditions t1 ++ filterToPreConditions t2
-- filterToPreConditions (Sender p)   =
