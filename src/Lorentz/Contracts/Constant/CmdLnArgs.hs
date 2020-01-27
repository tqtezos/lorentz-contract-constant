{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind -Wno-partial-fields -Wno-orphans #-}

module Lorentz.Contracts.Constant.CmdLnArgs where

import Control.Applicative
import Control.Monad
import Text.Show (Show(..))
import Data.List
import Data.Either
import Data.Function (id)
import Data.Functor
import Prelude (FilePath, IO, Ord(..), nonEmpty)
import Data.String (IsString(..), String)
import Data.Maybe
import Data.Typeable
import Text.Read

import Lorentz hiding (get)
import Michelson.Parser
import Michelson.Text
import Michelson.Typed.Annotation
import Michelson.Typed.Arith
import Michelson.Typed.Haskell.Value
import Michelson.Typed.Scope
import Michelson.Typed.Sing
import Michelson.Typed.T
import Michelson.Typed.Value
import Util.IO
import qualified Michelson.Untyped.Type as U

import qualified Options.Applicative as Opt
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Data.Constraint
import Data.Singletons

import Lorentz.Contracts.Util ()
import Lorentz.Contracts.SomeContractParam
import Lorentz.Contracts.Parse
import qualified Lorentz.Contracts.GenericMultisig.Wrapper as G

import qualified Lorentz.Contracts.Constant as Constant

instance IsoCValue (Value ('Tc ct)) where
  type ToCT (Value ('Tc ct)) = ct
  toCVal (VC xs) = xs
  fromCVal = VC

instance SingI ct => Ord (Value ('Tc ct)) where
  compare =
    case sing @ct of
      SCInt -> Prelude.compare
      SCNat -> Prelude.compare
      SCString -> Prelude.compare
      SCBytes -> Prelude.compare
      SCMutez -> Prelude.compare
      SCBool -> Prelude.compare
      SCKeyHash -> Prelude.compare
      SCTimestamp -> Prelude.compare
      SCAddress -> Prelude.compare

instance (Typeable ct, CompareOp ct) => CompareOpHs (Value ('Tc ct)) where

compareOpCT :: forall ct. SingI ct :- CompareOp ct
compareOpCT = Sub $
  case sing @ct of
    SCInt -> Dict
    SCNat -> Dict
    SCString -> Dict
    SCBytes -> Dict
    SCMutez -> Dict
    SCBool -> Dict
    SCKeyHash -> Dict
    SCTimestamp -> Dict
    SCAddress -> Dict

assertOpAbsense :: forall (t :: T) a. SingI t => (HasNoOp t => a) -> a
assertOpAbsense f =
  case opAbsense (sing @t) of
    Nothing -> error "assertOpAbsense"
    Just Dict -> forbiddenOp @t f

assertContractAbsense :: forall (t :: T) a. SingI t => (HasNoContract t => a) -> a
assertContractAbsense f =
  case contractTypeAbsense  (sing @t) of
    Nothing -> error "assertContractAbsense"
    Just Dict -> forbiddenContractType @t f

assertBigMapAbsense :: forall (t :: T) a. SingI t => (HasNoBigMap t => a) -> a
assertBigMapAbsense f =
  case bigMapAbsense (sing @t) of
    Nothing -> error "assertBigMapAbsense"
    Just Dict -> forbiddenBigMap @t f

assertNestedBigMapsAbsense :: forall (t :: T) a. SingI t => (HasNoNestedBigMaps t => a) -> a
assertNestedBigMapsAbsense f =
  case nestedBigMapsAbsense (sing @t) of
    Nothing -> error "assertNestedBigMapsAbsense"
    Just Dict -> forbiddenNestedBigMaps @t f

-- type IsComparable c = ToT c ~ 'Tc (ToCT c)
assertIsComparable ::
     forall (t :: T) a. SingI t
  => (( IsComparable (Value t)
      , SingI (ToCT (Value t))
      , Typeable (ToCT (Value t))
      ) =>
        a)
  -> a
assertIsComparable f =
  case sing @t of
    STc _ -> f
    _ -> error "assertIsComparable"

singTypeableCT :: forall (t :: CT). Sing t -> Dict (Typeable t)
singTypeableCT SCInt = Dict
singTypeableCT SCNat = Dict
singTypeableCT SCString = Dict
singTypeableCT SCBytes = Dict
singTypeableCT SCMutez = Dict
singTypeableCT SCBool = Dict
singTypeableCT SCKeyHash = Dict
singTypeableCT SCTimestamp = Dict
singTypeableCT SCAddress = Dict

singTypeableT :: forall (t :: T). Sing t -> Dict (Typeable t)
singTypeableT (STc ct) =
  withDict (singTypeableCT ct) $
  Dict
singTypeableT STKey = Dict
singTypeableT STUnit = Dict
singTypeableT STSignature = Dict
singTypeableT STChainId = Dict
singTypeableT (STOption st) =
  withDict (singTypeableT st) $
  Dict
singTypeableT (STList st) =
  withDict (singTypeableT st) $
  Dict
singTypeableT (STSet st) =
  withDict (singTypeableCT st) $
  Dict
singTypeableT STOperation  = Dict
singTypeableT (STContract st) =
  withDict (singTypeableT st) $
  Dict
singTypeableT (STPair st su) =
  withDict (singTypeableT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STOr st su) =
  withDict (singTypeableT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STLambda st su) =
  withDict (singTypeableT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STMap st su) =
  withDict (singTypeableCT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STBigMap st su) =
  withDict (singTypeableCT st) $
  withDict (singTypeableT su) $
  Dict

singICT :: forall (t :: CT). Sing t -> Dict (SingI t)
singICT SCInt = Dict
singICT SCNat = Dict
singICT SCString = Dict
singICT SCBytes = Dict
singICT SCMutez = Dict
singICT SCBool = Dict
singICT SCKeyHash = Dict
singICT SCTimestamp = Dict
singICT SCAddress = Dict

singIT :: forall (t :: T). Sing t -> Dict (SingI t)
singIT (STc ct) =
  withDict (singICT ct) $
  Dict
singIT STKey = Dict
singIT STUnit = Dict
singIT STSignature = Dict
singIT STChainId = Dict
singIT (STOption st) =
  withDict (singIT st) $
  Dict
singIT (STList st) =
  withDict (singIT st) $
  Dict
singIT (STSet st) =
  withDict (singICT st) $
  Dict
singIT STOperation  = Dict
singIT (STContract st) =
  withDict (singIT st) $
  Dict
singIT (STPair st su) =
  withDict (singIT st) $
  withDict (singIT su) $
  Dict
singIT (STOr st su) =
  withDict (singIT st) $
  withDict (singIT su) $
  Dict
singIT (STLambda st su) =
  withDict (singIT st) $
  withDict (singIT su) $
  Dict
singIT (STMap st su) =
  withDict (singICT st) $
  withDict (singIT su) $
  Dict
singIT (STBigMap st su) =
  withDict (singICT st) $
  withDict (singIT su) $
  Dict

data CmdLnArgs
  = Print
      { givenConstant :: SomeContractParam
      , mOutput :: Maybe FilePath
      , forceOneLine :: Bool
      }
  | PrintMetadata
      { metadata :: [String]
      , mOutput :: Maybe FilePath
      , forceOneLine :: Bool
      }
  | GetConstant
      { viewAdmin :: Address
      }

-- | Make a type non-explicit
unExplicitType :: U.Type -> U.T
unExplicitType =
  \case
    U.Type t _ -> t

-- | Convert a `U.Comparable` to `CT`
fromUntypedComparable :: U.Comparable -> CT
fromUntypedComparable (U.Comparable ct _) = ct

-- | Convert a `U.Type` to `T`
fromUntypedT' :: U.Type -> T
fromUntypedT' = fromUntypedT . unExplicitType

-- | Convert a `U.T` to `T`
fromUntypedT :: U.T -> T
fromUntypedT (U.Tc ct) = Tc ct
fromUntypedT U.TKey = TKey
fromUntypedT U.TUnit = TUnit
fromUntypedT U.TChainId = TChainId
fromUntypedT U.TSignature = TSignature
fromUntypedT (U.TOption x) = TOption $ fromUntypedT' x
fromUntypedT (U.TList x) = TList $ fromUntypedT' x
fromUntypedT (U.TSet ct) = TSet $ fromUntypedComparable ct
fromUntypedT U.TOperation = TOperation
fromUntypedT (U.TContract x) = TContract $ fromUntypedT' x
fromUntypedT (U.TPair _ _ x y) = TPair (fromUntypedT' x) (fromUntypedT' y)
fromUntypedT (U.TOr _ _ x y) = TOr (fromUntypedT' x) (fromUntypedT' y)
fromUntypedT (U.TLambda x y) = TLambda (fromUntypedT' x) (fromUntypedT' y)
fromUntypedT (U.TMap ct x) = TMap (fromUntypedComparable ct) $ fromUntypedT' x
fromUntypedT (U.TBigMap ct x) = TBigMap (fromUntypedComparable ct) $ fromUntypedT' x

-- | Parse some `T`
parseSomeT :: String -> Opt.Parser (SomeSing T)
parseSomeT name =
  (\typeStr ->
    let parsedType = parseNoEnv
          type_
          name
          typeStr
     in let type' = either (error . T.pack . show) unExplicitType parsedType
     in withSomeSingT (fromUntypedT type') SomeSing
  ) <$>
  Opt.strOption @Text
    (mconcat
      [ Opt.long $ name ++ "Type"
      , Opt.metavar "Michelson Type"
      , Opt.help $ "The Michelson Type of " ++ name
      ])

parseSomeContractParam :: String -> Opt.Parser SomeContractParam
parseSomeContractParam name =
  (\(SomeSing (st :: Sing t)) paramStr ->
    withDict (singIT st) $
    withDict (singTypeableT st) $
    assertOpAbsense @t $
    assertBigMapAbsense @t $
    let parsedParam = parseNoEnv
          (G.parseTypeCheckValue @t)
          name
          paramStr
     in let param = either (error . T.pack . show) id parsedParam
     in SomeContractParam param (st, starNotes) (Dict, Dict)
  ) <$>
  parseSomeT name <*>
  Opt.strOption @Text
    (mconcat
      [ Opt.long name
      , Opt.metavar "Michelson Value"
      , Opt.help $ "The Michelson Value: " ++ name
      ])

parseView_ :: NiceParameter r => Opt.Parser (View () r)
parseView_ = parseView $ pure ()

argParser :: Opt.Parser CmdLnArgs
argParser = Opt.hsubparser $ mconcat
  [ printSubCmd
  , printMetadataSubCmd
  , getConstantSubCmd
  ]
  where
    mkCommandParser commandName parser desc =
      Opt.command commandName $
      Opt.info (Opt.helper <*> parser) $
      Opt.progDesc desc

    printSubCmd =
      mkCommandParser "print"
      (Print <$> parseSomeContractParam "constant-value" <*> outputOptions <*> onelineOption)
      "Dump the Constant contract in form of Michelson code"

    printMetadataSubCmd =
      mkCommandParser "print-metadata"
      (PrintMetadata <$>
        (Opt.option Opt.auto $ mconcat
          [ Opt.long "metadata"
          , Opt.metavar "[STRING]"
          , Opt.help "List of strings representing metadata values to store"
          ]
        ) <*>
        outputOptions <*>
        onelineOption
      )
      "Dump the Constant contract, specialized to a tuple of the given MText's, in form of Michelson code"

    getConstantSubCmd =
      mkCommandParser "GetConstant"
      (GetConstant <$> parseAddress "callback")
      "Generate the parameter for the Constant contract: GetConstant"

infoMod :: Opt.InfoMod CmdLnArgs
infoMod = mconcat
  [ Opt.fullDesc
  , Opt.progDesc "Constant contract CLI interface"
  ]

runCmdLnArgs :: CmdLnArgs -> IO ()
runCmdLnArgs = \case
  Print {..} ->
    fromSomeContractParam givenConstant $ \(givenConstant' :: Value t) ->
      let st = sing @t in
      withDict (singIT st) $
      withDict (singTypeableT st) $
      assertOpAbsense @t $
      assertContractAbsense @t $
      assertBigMapAbsense @t $
      assertNestedBigMapsAbsense @t $
      maybe TL.putStrLn writeFileUtf8 mOutput $
      printLorentzContract forceOneLine (Constant.constantContract @(Value t) givenConstant')
  PrintMetadata {..} ->
    Constant.printMetadataContract
      (either (error . ("Invalid Michelson text: " <>)) id .
        mapM (mkMText . T.pack) .
        fromMaybe (error "empty list of metadata") .
        nonEmpty $
        metadata
      )
      mOutput
      forceOneLine
  GetConstant {..} ->
    let forceSingleLine = True in
    TL.putStrLn . printLorentzValue @(Constant.Parameter ()) forceSingleLine $
    Constant.GetContstant $
    mkView () viewAdmin

