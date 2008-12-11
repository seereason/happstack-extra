{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, GeneralizedNewtypeDeriving, ScopedTypeVariables, TemplateHaskell, TypeFamilies, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fglasgow-exts -fallow-undecidable-instances -fallow-incoherent-instances #-}
module HSP.Formlets.Generics
    ( FormletOf(formletOf)
    , FormletOfD(formletOfD)
    , formletOfUnion
    , formletOfConstructors
    ) where

import Control.Applicative -- (Applicative, (<$>), (<*>))
import Control.Applicative.Error (Failing(..))
import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Instances ()
import Data.Maybe (catMaybes, maybeToList, listToMaybe)
import qualified Data.Map as M
import Data.Traversable
import qualified Data.Set as S
import HAppS.Data (Default(..), DefaultD(..), defaultProxy, deriveNewData)
import HSP.Formlets (FormHSXT, input, inputInteger, radio', select, checkbox, span, div)
import qualified HSX.XMLGenerator as HSX
import Prelude hiding (div)
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

class (Show a, Default a, Ord a, Data FormletOfD a) => FormletOf a where
    formletOf :: (HSX.XMLGenerator x, Monad v, Applicative v) => a -> FormHSXT x v a

data FormletOfD a =
    FormletOfD { formletOfD :: (HSX.XMLGenerator x, Monad v, Applicative v) => a -> FormHSXT x v a }

formletOfProxy :: Proxy FormletOfD
formletOfProxy = error "urk"

instance FormletOf t => Sat (FormletOfD t) where
    dict = FormletOfD { formletOfD = formletOf }

-- |Convert a value of an arbitrary type to a formlet.  This method
-- needs to be overridden with specialized instances.
instance (Show a, Default a, Ord a, Data FormletOfD a) => FormletOf a where
    formletOf x =
        case dataTypeRep (dataTypeOf defaultProxy x) of
          -- AlgRep [_] ->
          AlgRep constrs
              | all (== []) (map constrFields constrs) -> formletOfConstructors radio' (Just x)
              -- The other cases we don't know how to handle generically, where
              -- we want to call formletOfUnion on each argument to the Constructor.
          _ -> error $ "Missing formletOf instance for type " ++
                                show (dataTypeName (dataTypeOf formletOfProxy x)) ++ " (" ++ show x ++ ")"

-- |Display the elements of a list with an extra defaultValue added.
-- This extra value is removed if it is still equal to defaultValue.
instance (Show a, Default a, Ord a, Data FormletOfD a) => FormletOf [a] where
    formletOf xs = 
        frm `check` stripExtra
        where
          frm = sequenceA . map formletOfUnion $ xs ++ [defaultValue :: a]
          stripExtra xs' = Success $ if last xs' == defaultValue then init xs' else xs'

-- |A helper function for algebraic types with more than one
-- constructor, i.e. unions.  Note that you will get into an
-- infinite recursion if a formletOf method calls this with its
-- value argument.  You need to call it with a subtype, such as
-- an element of a list you are converting to a formlet.
formletOfUnion :: forall a. forall x. forall v. (Show a, Default a, Ord a, Data FormletOfD a, HSX.XMLGenerator x, Monad v, Applicative v) =>
                  a -> FormHSXT x v a
formletOfUnion x =
    case dataTypeRep t of
      AlgRep [_] -> div "union" $ formletOfD dict x
      AlgRep constrs
          | (any (/= []) . map constrFields $ constrs) ->
              div "record" $ sequenceA (formletOfConstructors radio' (Just x) : formletsOfFields f) `check` chooseConstructor
      _ -> div "prim" $ formletOfD dict x
    where
      f :: Constr -> a
      f c = if c == toConstr formletOfProxy x then x else defaultValue
      chooseConstructor :: [a] -> Failing a
      chooseConstructor (index : elems) = Success (elems !! (constrIndex (toConstr formletOfProxy index)))
      chooseConstructor [] = error "Internal error"
      t = dataTypeOf formletOfProxy x

-- |Generate radio buttons selecting the different constructors of an
-- algebraic type, but ignoring any fields that might be associated
-- with the different constructors.
formletOfConstructors :: forall a. forall x. forall v. (Show a, Default a, Ord a, Data FormletOfD a, HSX.XMLGenerator x, Monad v, Applicative v) =>
                         ([(a, String)] -> Maybe a -> FormHSXT x v a) -> Maybe a -> FormHSXT x v a
formletOfConstructors selectFn x =
    selectFn pairs x
    where
      pairs :: [(a, String)]
      pairs = map (\ x' -> (replace x x', toString x')) (allDefaultValues (defaultValue :: a))
      toString = showConstr . toConstr formletOfProxy
      replace (Just x) x' = if toString x' == toString x then x else x'
      replace Nothing x' = x'

-- |Generate a formlet for the field data of each of the constructors
-- in the type.
formletsOfFields :: forall a. forall x. forall v. (Show a, Default a, Ord a, Data FormletOfD a, HSX.XMLGenerator x, Monad v, Applicative v) =>
                    (Constr -> a)	-- ^ Given a constructor representation, return the value to use to initialize the form
                 -> [FormHSXT x v a]
formletsOfFields f = 
    map (formletOfD dict) (map f constrs)
    where
      constrs = dataTypeConstrs (dataTypeOf formletOfProxy (defaultValue :: a))

$(deriveNewData [''S.Set])

-- |Convert a set to a formlet.  By convention, a Set of an algebraic
-- type is considered to be a "check all that apply" situation, even
-- if some of the constructors have arguments which would allow you to
-- legitimately construct several set elements with the same
-- constructor.
instance (Show a, Default a, Ord a, Data FormletOfD a) => FormletOf (S.Set a) where
    formletOf xs =
        sequenceA cbs `check` (Success . makeSet)
        where
          -- Turn the values returned by the checkbox formlets into a list.
          makeSet = S.fromList . catMaybes . map (\ (flag, x) -> if flag then Just x else Nothing)
          -- List of CheckBox formlets
          cbs = map (\ (chk, fld) -> (,) <$> chk <*> fld) (zip checks fields)
          checks = map (\ c -> checkbox (c `S.member` cs) (Just "xxx")) (dataTypeConstrs t)
          t = dataTypeOf formletOfProxy (defaultValue :: a)
          fields = formletsOfFields f
          cs = S.map (toConstr formletOfProxy) xs
          -- The initial values for the sub-formlets.  Each is either
          -- a value from xs if available, or a defaultValue.
          f :: Constr -> a
          f c = M.findWithDefault defaultValue c (M.fromList (map (\ x -> (toConstr formletOfProxy x, x)) (S.toList xs)))

instance Ord Constr where
    compare a b = compare (showConstr a) (showConstr b)

-- |Treat a maybe as a "all that apply" set with a single element.
instance (Show a, Default a, Ord a, Data FormletOfD a) => FormletOf (Maybe a) where
    formletOf mx = formletOf (S.fromList (maybeToList mx)) `check` (Success . listToMaybe . S.elems)

-- Primitive types
instance FormletOf Integer where formletOf n = inputInteger (Just n)
instance FormletOf String where formletOf s = input (Just s)
instance FormletOf Bool where formletOf x = formletOfConstructors radio' (Just x)
