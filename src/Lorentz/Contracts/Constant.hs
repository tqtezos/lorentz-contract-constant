{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind -Wno-partial-fields #-}

module Lorentz.Contracts.Constant where

import Prelude hiding ((>>), drop, swap, get)
import GHC.Generics (Generic(..))
import Text.Show (Show(..))
import Text.Read (Read(..))

import Data.Singletons (SingI)
import qualified Data.Text.Lazy.IO as TL
import Util.IO

import Lorentz
import Michelson.Typed.Scope

import Lorentz.Contracts.Util ()

-- | The only entrypoint allows you to view the constant
data Parameter a
  = GetContstant
      { getConstantParams :: !(View () a)
      }
  deriving  (Generic)

instance (NiceParameter a) => ParameterEntryPoints (Parameter a) where
  parameterEntryPoints = pepNone

deriving instance (NiceParameter a, Read a) => Read (Parameter a)

deriving instance Show a => Show (Parameter a)

deriving instance IsoValue a => IsoValue (Parameter a)

-- | There is no storage
type Storage = ()

-- | This contract serves a contstant value
constantContract ::
     forall a.
     ( IsoValue a
     , Typeable (ToT a)
     , SingI (ToT a)
     , ForbidOp (ToT a)
     , ForbidBigMap (ToT a)
     , ForbidNestedBigMaps (ToT a)
     , ForbidContract (ToT a)
     )
  => a
  -> Contract (Parameter a) Storage
constantContract constantValue = do
  car
  coerce_ @(Parameter a) @((), ContractRef a)
  cdr
  push $ toEnum @Mutez 0
  push constantValue
  transferTokens
  dip $ do
    unit
    nil
  cons
  pair

data SomeConstant where
  SomeConstant :: forall a.
     ( IsoValue a
     , Typeable (ToT a)
     , SingI (ToT a)
     , ForbidOp (ToT a)
     , ForbidBigMap (ToT a)
     , ForbidNestedBigMaps (ToT a)
     , ForbidContract (ToT a)
     )
    => Proxy a
    -> Value (ToT a)
    -> SomeConstant

printSomeConstantContract :: SomeConstant -> Maybe FilePath -> Bool -> IO ()
printSomeConstantContract (SomeConstant _ xs) mOutput forceSingleLine =
  maybe TL.putStrLn writeFileUtf8 mOutput $
  printLorentzContract forceSingleLine $ constantContract xs

someConstantMText :: MText -> SomeConstant
someConstantMText xs = SomeConstant (Proxy @MText) $ toVal xs

instance Semigroup SomeConstant where
  (<>) (SomeConstant (_ :: Proxy a) xs) (SomeConstant (_ :: Proxy b) ys) =
    forbiddenOp @(ToT a) $
    forbiddenContractType @(ToT a) $
    forbiddenBigMap @(ToT a) $
    forbiddenNestedBigMaps @(ToT a) $
    SomeConstant (Proxy @(a, b)) $ toVal (xs, ys)

printMetadataContract :: NonEmpty MText -> Maybe FilePath -> Bool -> IO ()
printMetadataContract = printSomeConstantContract . sconcat . fmap someConstantMText

