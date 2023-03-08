module Llvm.Llvm (compileModuleToObj) where
import LLVM.AST as LLAST
    ( defaultModule,
      functionDefaults,
      Definition(GlobalDefinition),
      Module(moduleName, moduleDefinitions, Module),
      BasicBlock(BasicBlock),
      Parameter(Parameter),
      Operand(ConstantOperand, LocalReference), Name (UnName) )
import LLVM.AST.Global as Global
    ( Global(name, returnType, parameters, basicBlocks) )
import LLVM.AST.Type as Type ( i32, i1, double )
import LLVM.AST.Name as Name ( Name(Name) )
import LLVM.AST.Linkage as Linkage (Linkage (External))
import LLVM.AST.Visibility as Visibility ()
import LLVM.AST.CallingConvention as CallingConvention ()
import LLVM.AST.AddrSpace as AddrSpace ()
import LLVM.AST.FloatingPointPredicate as FloatingPointPredicate ()
import LLVM.AST.IntegerPredicate as IntegerPredicate ()
import LLVM.AST.Constant as Constant ( Constant(Int, Float, GlobalReference) )
import LLVM.AST.Float as Float (SomeFloat (Double))
import LLVM.AST.DataLayout as DataLayout ()
import LLVM.AST.Attribute as Attribute ()
import LLVM.AST.ParameterAttribute as ParameterAttribute ()
import LLVM.Target as Target ()
import LLVM.Context as Context (withContext)

import LLVM.Internal.Module as Module (withModuleFromAST, writeLLVMAssemblyToFile, File (File))

import LLVM.Internal.OrcJIT.LinkingLayer as LinkingLayer ()

import qualified Data.ByteString.Char8 as C
import LLVM.Module as LModule ()

import Ast.Ast (Ast(..))
import Data.ByteString.Short (toShort)
import Cpt.Literal ( Literal(Char, Int, Float, String, Bool, Array, Expression) )
import qualified LLVM.AST.Operand as Operand
import LLVM.IRBuilder.Module
import qualified Data.ByteString as B
import LLVM.AST.Instruction
import TcRnTypes (SpliceType(Typed))
import qualified Data.ByteString.Short as Data.ByteString.Short.Internal
import LLVM.IRBuilder (IRBuilderT)
import qualified LLVM.AST as Global
import qualified LLVM.AST.IntegerPredicate as IPred


astToLlvm :: Ast -> Operand.Operand
astToLlvm (Value (Cpt.Literal.Int i)) = ConstantOperand $ Constant.Int 32 $ fromIntegral i
astToLlvm (Value (Char c)) = ConstantOperand $ Constant.Int 8 $ fromIntegral $ fromEnum c
astToLlvm (Value (Cpt.Literal.Float f)) = ConstantOperand $ Constant.Float $ Float.Double f
astToLlvm (Value (Cpt.Literal.Bool b)) = ConstantOperand $ Constant.Int 1 $ fromIntegral $ fromEnum b
astToLlvm (Value (Cpt.Literal.Array a)) = ConstantOperand $ Constant.Int 32 $ fromIntegral $ length a
astToLlvm (Value Cpt.Literal.Expression) = ConstantOperand $ Constant.Int 32 0
astToLlvm _ = ConstantOperand $ Constant.Int 32 0


s2n :: String -> Name.Name
s2n s = Name.Name (toShort $ C.pack s)

-- generateFunction :: Ast -> IRBuilderT (ModuleBuilderT IO) ()
-- generateFunction (Define name ast) = do
--   let params = case ast of
--         Lambda ps _ -> map (\(Name p) -> (Type.i32, B.pack p)) ps
--         _ -> []
--   let retType = case ast of
--         Lambda _ _ -> Type.i32
--         Value (Cpt.Literal.Int _) -> Type.i32
--         Value (Cpt.Literal.Float _) -> Type.double
--         Value (Bool _) -> Type.i1
--         _ -> error "unsupported return type"
--   func <- LLVM.AST.Instruction.function (B.pack name) params retType $ \args -> do
--     defineArgs args
--     generateCode ast
--   return ()
-- generateFunction _ = return ()

-- defineArgs :: [SpliceType] -> m ()
-- defineArgs args = do
--       let names = map (\Typed -> n) args
--       mapM_ (\n -> do
--         val <- alloca Type.i32 Nothing 0
--         _ <- store val 0 (LocalReference Type.i32 (Name n))
--         assign n val) names

-- assign :: Data.ByteString.Short.Internal.ShortByteString -> a -> m ()
-- assign name val = do
--         let n = Name name
--         _ <- global n Type.i32 (Just val) Linkage.External
--         return ()

-- generateCode :: Monad m => Ast -> m Operand
-- generateCode (Value (Cpt.Literal.Int i)) = return $ ConstantOperand $ Constant.Int 32 $ fromIntegral i
-- generateCode (Value (Char c)) = return $ ConstantOperand $ Constant.Int 8 $ fromIntegral $ fromEnum c
-- generateCode (Value (Cpt.Literal.Float f)) = return $ ConstantOperand $ Constant.Float $ Float.Double f
-- generateCode (Value (Cpt.Literal.String s)) = return $ ConstantOperand $ Constant.Int 8 $ fromIntegral $ fromEnum s
-- generateCode (Value (Cpt.Literal.Bool b)) = return $ ConstantOperand $ Constant.Int 1 $ fromIntegral $ fromEnum b
-- generateCode (Value (Cpt.Literal.Array a)) = return $ ConstantOperand $ Constant.Int 32 $ fromIntegral $ length a
-- generateCode (Value Cpt.Literal.Expression) = return $ ConstantOperand $ Constant.Int 32 0

defAdd :: Definition
defAdd =  GlobalDefinition
  functionDefaults {
    Global.name = s2n "add",
    Global.returnType = Type.i32,
    Global.parameters = ([Parameter Type.i32 (s2n "a") [], Parameter Type.i32 (s2n "b") []], False),
    Global.basicBlocks = [
      BasicBlock (s2n "entry") [
        UnName 0 := Add False False (LocalReference Type.i32 (s2n "a")) (LocalReference Type.i32 (s2n "b")) []
      ] (Do $ Ret (Just (LocalReference Type.i32 (UnName 0))) [])
    ]
  }

defSub :: Definition
defSub =  GlobalDefinition
  functionDefaults {
    Global.name = s2n "sub",
    Global.returnType = Type.i32,
    Global.parameters = ([Parameter Type.i32 (s2n "a") [], Parameter Type.i32 (s2n "b") []], False),
    Global.basicBlocks = [
      BasicBlock (s2n "entry") [
        UnName 0 := Sub False False (LocalReference Type.i32 (s2n "a")) (LocalReference Type.i32 (s2n "b")) []
      ] (Do $ Ret (Just (LocalReference Type.i32 (UnName 0))) [])
    ]
  }

defMul :: Definition
defMul =  GlobalDefinition
  functionDefaults {
    Global.name = s2n "mul",
    Global.returnType = Type.i32,
    Global.parameters = ([Parameter Type.i32 (s2n "a") [], Parameter Type.i32 (s2n "b") []], False),
    Global.basicBlocks = [
      BasicBlock (s2n "entry") [
        UnName 0 := Mul False False (LocalReference Type.i32 (s2n "a")) (LocalReference Type.i32 (s2n "b")) []
      ] (Do $ Ret (Just (LocalReference Type.i32 (UnName 0))) [])
    ]
  }

defDiv :: Definition
defDiv =  GlobalDefinition
  functionDefaults {
    Global.name = s2n "div",
    Global.returnType = Type.i32,
    Global.parameters = ([Parameter Type.i32 (s2n "a") [], Parameter Type.i32 (s2n "b") []], False),
    Global.basicBlocks = [
      BasicBlock (s2n "entry") [
        UnName 0 := SDiv False (LocalReference Type.i32 (s2n "a")) (LocalReference Type.i32 (s2n "b")) []
      ] (Do $ Ret (Just (LocalReference Type.i32 (UnName 0))) [])
    ]
  }

defMod :: Definition
defMod =  GlobalDefinition
  functionDefaults {
    Global.name = s2n "mod",
    Global.returnType = Type.i32,
    Global.parameters = ([Parameter Type.i32 (s2n "a") [], Parameter Type.i32 (s2n "b") []], False),
    Global.basicBlocks = [
      BasicBlock (s2n "entry") [
        UnName 0 := SRem (LocalReference Type.i32 (s2n "a")) (LocalReference Type.i32 (s2n "b")) []
      ] (Do $ Ret (Just (LocalReference Type.i32 (UnName 0))) [])
    ]
  }

defIf :: Definition
defIf =  GlobalDefinition
  functionDefaults {
    Global.name = s2n "if",
    Global.returnType = Type.i32,
    Global.parameters = ([Parameter Type.i1 (s2n "cond") [], Parameter Type.i32 (s2n "a") [], Parameter Type.i32 (s2n "b") []], False),
    Global.basicBlocks = [
      BasicBlock (s2n "entry") [
        UnName 0 := ICmp IPred.EQ (LocalReference Type.i1 (s2n "cond")) (ConstantOperand $ Constant.Int 1 1) [],
        UnName 1 := Select (LocalReference Type.i1 (UnName 0)) (LocalReference Type.i32 (s2n "a")) (LocalReference Type.i32 (s2n "b")) []
      ] (Do $ Ret (Just (LocalReference Type.i32 (UnName 1))) [])
    ]
  }

defMain :: Definition
defMain = GlobalDefinition
  functionDefaults {
    Global.name = s2n "main",
    Global.returnType = Type.i32,
    Global.parameters = ([], False),
    Global.basicBlocks = [
      BasicBlock (s2n "entry") [
    ] (Do $ Ret (Just (ConstantOperand $ Constant.Int 32 0)) [])
    ]
  }


generateModule :: [Ast] -> LLAST.Module
generateModule _ = defaultModule {
  moduleName = toShort $ C.pack "glados",
  moduleDefinitions = [defAdd, defSub, defMul, defDiv, defMod, defIf, defMain]}


compileModuleToObj :: [Ast] -> IO ()
compileModuleToObj a = withContext $ \context ->
     withModuleFromAST context (generateModule a)  $ \p ->
        writeLLVMAssemblyToFile (File "glados.ll") p