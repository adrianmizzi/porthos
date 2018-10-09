module Codegen.Solidity where

import           Porthos
import           StatementGenerator

generateSolidity :: Contract -> IO()
generateSolidity c = do
    resetCounters;
    let m = Method Public "constructor" MTConstructor [] []
    putStrLn (intro ++ generateCode (generateStatements m c) ++ "\n}")

intro :: String
intro = "pragma solidity ^0.4.24;\n\n" ++
        "import \"./PorthosContract.sol\";\n\n" ++
        "contract T2 is PorthosContract {\n\n" ++
        "  address alice = 0xed5356d32d00de3eb32dbafd96a04af60f49fa08;\n" ++
        "  address bob = 0x088f3ea1e40afb3ac90a78738c5ab97071ed8c6c;\n" ++
        "  address charlie = 0xc1a39c99ff252bcea57f0c2b8843a4ab445ab75d;\n"

generateCode :: [Method] -> String
generateCode []     = ""
generateCode (m:mm) = generateMethodCode m ++ generateCode mm

generateMethodCode :: Method -> String
generateMethodCode (Method modifier name t pp ss)
  | t == MTConstructor   = "\n" ++ name ++ prog
  | t == MTCommit        = "\nfunction " ++ name ++ "_commit" ++ prog
  | t == MTCommitTimeout = "\nfunction " ++ name ++ "_timeout" ++ prog
  | otherwise            = "\nfunction " ++ name ++ prog
  where
    prog = "(" ++ params ++ ") " ++ show modifier ++ "\n{" ++
            generatePreConditions pp ++ generateStmtCode ss
            ++ "\n}\n"
    params
      | t == MTCommit = "string _assetType, uint _quantity, address _recipient"
      | otherwise = ""

generatePreConditions :: [PreCondition] -> String
generatePreConditions [] = ""
generatePreConditions (p:pp) = generatePreCondition p ++ generatePreConditions pp

generatePreCondition :: PreCondition -> String
-- generatePreCondition (PcState i)             = "\n  if(contractStatus != " ++ show i ++ ") \n    return;"
generatePreCondition (PcFilter f)            = generateFilterCode f
generatePreCondition (PcTimeout (Timeout t)) = "\n  if(block.number < " ++ show t ++ ") \n    return;"
generatePreCondition (PcSemaphore s)         = "\n  if(!semaphore[" ++ show s ++ "]) \n    return;"
generatePreCondition (PcGate s)              = "\n  if(!isGateOpen(" ++ show s ++ ")) \n    return;"

generateFilterCode :: TxFilterExpr -> String
generateFilterCode NoTxFilter      = ""
generateFilterCode (AndTF f1 f2)   =  generateFilterCode f1 ++ generateFilterCode f2
-- generateFilterCode (OrTF f1 f2)    = "\n  require((" ++ generateFilterCode f1 ++ ") || (" ++ generateFilterCode f2 ++ "));"
generateFilterCode (Sender p)      = "\n  if(msg.sender != " ++ name p ++ ") \n    return; "
generateFilterCode (Recipient p)   = "\n  if(_recipient != " ++ name p ++") \n    return; ";
generateFilterCode (AssetIs a)     = "\n  if(asset != " ++ show a ++ ")\n    return;"
generateFilterCode (AssetTypeIs t) = "\n  if(asset.type == " ++ show t ++ ")\n    return;"

generateStmtCode :: [Statement] -> String
generateStmtCode []     = ""
generateStmtCode (s:ss) = generateStmtCode' s ++ generateStmtCode ss

generateStmtCode' :: Statement -> String
generateStmtCode' S_AddCommitment            = "\n  addCommitment(Commitment({tagId: \"\", sender: msg.sender, recipient: _recipient, assetType: _assetType, quantity: _quantity, status: 0}));"
-- generateStmtCode' (S_UpdateState s)          = "\n  setContractStatus(" ++ show s ++ ");"
generateStmtCode' (S_ReleaseCommitment c)    = "\n  releaseCommitments(" ++ generateCommitmentDSLCode c ++ ");"
generateStmtCode' (S_AutoCancelCommitment c) = "\n  cancelCommitments(" ++ generateCommitmentDSLCode c ++ ");"
generateStmtCode' (S_FireEvent s)            = "\n  emit LogEvent(" ++ show s ++ ");"
generateStmtCode' (S_IfThenElse b ss1 ss2)   = "\n  If (" ++ show b ++ " then \n{" ++ generateStmtCode ss1 ++ "\n} else \n{" ++ generateStmtCode ss2 ++ "}"
generateStmtCode' (S_InitSemaphore s)        = "\n  semaphore[" ++ show s ++ "] = false;"
generateStmtCode' (S_CompleteSemaphore s)    = "\n  semaphore[" ++ show s ++ "] = true;"
generateStmtCode' (S_ContinueWith s)         = "\n  " ++ s ++ "();"
generateStmtCode' (S_OpenGate s)             = "\n  openGate(" ++ show s ++ ");"
generateStmtCode' (S_CloseGate s)            = "\n  closeGate(" ++ show s ++ ");"

generateCommitmentDSLCode :: Commitment -> String
generateCommitmentDSLCode AllCommitments = "getAllCommitments()"
generateCommitmentDSLCode _              = "NOT SUPPORTED YET"
