{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, GeneralizedNewtypeDeriving, ScopedTypeVariables, TemplateHaskell, TypeFamilies, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fglasgow-exts -fallow-undecidable-instances -fallow-incoherent-instances -fno-warn-unused-imports #-}
module HSP.Formlets.Generics
    ( allDefaultValues
    , hidden'
    , radioButtons
    , FormletOf(formletOf)
    , FormletOfD(formletOfD)
    , formletOfProxy
    , formlet
    ) where

import Control.Applicative -- (Applicative, (<$>), (<*>))
import Control.Applicative.Error (Failing(..), maybeRead')
import Control.Arrow (first)
import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Instances ()
import Data.List (elemIndex)
import Data.Maybe (catMaybes, maybeToList, listToMaybe)
import qualified Data.Map as M
import Data.Traversable
import qualified Data.Set as S
import HAppS.Data (Default(..), DefaultD(..), defaultProxy, deriveNewData)
import HSP (cdata)
import HSP.Formlets (FormHSXT, input, inputInteger, hidden, radio, radio', select, checkbox, span, div)
import qualified HSX.XMLGenerator as HSX
import Prelude hiding (div, span)
import Text.Formlets

-- | Given a value of an algebraic type, return a list of the
-- defaultValue for each of the type's constructors.  This could
-- move into HAppS.Extra, or HAppS.Data, or Data.Generics.SYB.
allDefaultValues :: (Data DefaultD a,Default a) => a -> [a]
allDefaultValues x = res
    where res = case dataTypeRep (dataTypeOf defaultProxy x) of
                    AlgRep cs ->
                        map (fromConstrB defaultProxy (defaultValueD dict)) cs
                    r ->
                        error ("allDefaultValues: Bad DataRep: " ++ show r)

-- |Hide a value of any instance of Show and Read.
hidden' :: (Show a, Read a, Monad v, HSX.XMLGenerator x) => a -> FormHSXT x v a
hidden' x = hidden (Just (show x)) `check` (`maybeRead'` ("Failure in hidden: " ++ show x))

-- |Generate radio buttons selecting the different constructors of an
-- algebraic type, but ignoring any fields that might be associated
-- with the different constructors.
radioButtons :: forall a. forall x. forall v. (Show a, Default a, Ord a, Data FormletOfD a, HSX.XMLGenerator x, Monad v, Applicative v) =>
                         (a -> String) -> ([(a, String)] -> Maybe a -> FormHSXT x v a) -> Maybe a -> FormHSXT x v a
radioButtons labelFn selectFn x =
    selectFn pairs x
    where
      pairs :: [(a, String)]
      pairs = map (\ x' -> (replace x x', labelFn x')) (allDefaultValues (defaultValue :: a))
      -- toString = showConstr . toConstr formletOfProxy
      replace (Just x) x' = if toConstr formletOfProxy x' == toConstr formletOfProxy x then x else x'
      replace Nothing x' = x'

class (Show a, Default a, Ord a, Data FormletOfD a) => FormletOf a where
    formletOf :: (HSX.XMLGenerator x, Monad v, Applicative v) =>
                 Bool		-- ^ If false make a form that only returns values with the same constructor as x
              -> a		-- ^ The type the form returns, and the value it is initialized with
              -> FormHSXT x v a

data FormletOfD a =
    FormletOfD { formletOfD :: (HSX.XMLGenerator x, Monad v, Applicative v) => Bool -> a -> FormHSXT x v a }

formletOfProxy :: Proxy FormletOfD
formletOfProxy = error "urk"

instance FormletOf t => Sat (FormletOfD t) where
    dict = FormletOfD { formletOfD = formletOf }

-- |We need to wrap the formlet method in a function because there are
-- some cases we know how to handle that can't be distinguished by
-- specifying the type of an instance.  Specifically, if none of the
-- constructors of an algebraic type have fields we can generate the
-- formlet automatically, otherwise we need to call the method and
-- enter a specialized instance.
formlet :: forall a. forall x. forall v. (Read a, Show a, Default a, Ord a, Data FormletOfD a, HSX.XMLGenerator x, Monad v, Applicative v) =>
                  a -> FormHSXT x v a
formlet x =
    case dataTypeRep (dataTypeOf formletOfProxy x) of
      AlgRep constrs -> formletOfD dict (length constrs > 1) x
      _ -> formletOfD dict False x

instance (Read a, Show a, Default a, Ord a, Data FormletOfD a) => FormletOf a where
    formletOf union x =
        case dataTypeRep t of
          AlgRep constrs
              -- Special case for types that only have constructors of
              -- arity zero, we don't have to write special instances
              -- for these.
              | all (== []) (map constrFields constrs) ->
                  if union
                  then div (dataTypeName t) (radioButtons (showConstr . toConstr formletOfProxy) radio' (Just x))
                  else hidden' x
              | otherwise -> error $ "Missing formletOf instance for type " ++ show (dataTypeName (dataTypeOf formletOfProxy x)) ++ " (" ++ show x ++ ")"
          _ -> error $ "Missing formletOf instance for type " ++ show (dataTypeName (dataTypeOf formletOfProxy x)) ++ " (" ++ show x ++ ")"
        where
          t = dataTypeOf formletOfProxy x

$(deriveNewData [''S.Set])

instance Ord Constr where
    compare a b = compare (showConstr a) (showConstr b)

instance (Read a, Show a, Default a, Ord a, Data FormletOfD a) => FormletOf (S.Set a) where
    formletOf union xs =
        -- This code puts all the checkboxes at the top and the fields
        -- associated with each checkbox below.  This may not always be
        -- the desired arrangement.
        -- Put the fields after all the checkboxes
        -- ((,) <$> (sequenceA checks) <*> (sequenceA fields)) `check` makeSet
        -- Put the fields directly after the associated checkbox
        sequenceA (map (\ (check, field) -> (,) <$> check <*> field) (zip checks fields)) `check` makeSet'
        where
          -- The checkboxes
          -- checks :: [FormHSXT x v Bool]
          checks = map (\ c -> checkbox (c `S.member` cs) (Just (showConstr c))) constrs
              where
                cs = S.map (toConstr formletOfProxy) xs
          -- The data fields for each of the type's constructors
          -- fields :: [FormHSXT x v a]
          fields =
              map (formletOfD dict False) initialMap
              where
                -- The initial value for each constructor
                initialMap :: [a]
                initialMap = map (\ x -> M.findWithDefault x (toConstr formletOfProxy x) currentMap) allDefaults
                -- The values in the initial set
                currentMap :: M.Map Constr a    -- Map of the initial set elements
                currentMap = M.fromList (map (\ x -> (toConstr formletOfProxy x, x)) (S.toList xs))
                allDefaults = allDefaultValues (defaultValue :: a)
          -- Construct the new set
          makeSet :: ([Bool], [a]) -> Failing (S.Set a)
          makeSet (checks, values) = Success . S.fromList . mask $ zip checks values
          makeSet' :: [(Bool, a)] -> Failing (S.Set a)
          makeSet' pairs = Success . S.fromList . mask $ pairs
          mask :: [(Bool, a)] -> [a]
          mask pairs = catMaybes $ map (\ (b, x) -> if b then Just x else Nothing) pairs
          constrs = dataTypeConstrs (dataTypeOf formletOfProxy (defaultValue :: a))

-- |Treat a maybe as a "all that apply" set with a single element.
instance (Read a, Show a, Default a, Ord a, Data FormletOfD a) => FormletOf (Maybe a) where
    formletOf _ mx = formlet (S.fromList (maybeToList mx)) `check` (Success . listToMaybe . S.elems)

-- |Display the elements of a list with an extra defaultValue added.
-- This extra value is removed if it is still equal to defaultValue.
instance (Read a, Show a, Default a, Ord a, Data FormletOfD a) => FormletOf [a] where
    formletOf _ xs = 
        frm `check` stripExtra
        where
          frm = sequenceA . map formlet $ xs ++ [defaultValue :: a]
          stripExtra xs' = Success $ if last xs' == defaultValue then init xs' else xs'

-- Primitive types
instance FormletOf Integer where formletOf _ n = div "Integer" $ inputInteger (Just n)
instance FormletOf String where formletOf _ s = div "String" $ input (Just s)
