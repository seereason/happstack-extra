{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, GeneralizedNewtypeDeriving, ScopedTypeVariables, TemplateHaskell, TypeFamilies, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fglasgow-exts -fallow-undecidable-instances -fallow-incoherent-instances -fno-warn-unused-imports -fno-warn-missing-signatures -fno-warn-name-shadowing #-}
-- |Use Scrap Your Boilerplate with Class to convert arbitrary values
-- to formlets, forms that can be initialized with a value of the type
-- and can, after being suitably filled out, return any other value of
-- that type.
module HSP.Formlets.Generics
    ( FormletOf(formletOf)
    , FormletOfD(formletOfD)
    , formletOfProxy
    -- * Wrapper functions - used to implement @FormletOf@ instances
    , formlet
    , formletOfSet
    , formletOfMaybe
    , formletOfList
    , formlet1
    , formlet2
    , formlet3
    , formlet4
    , formlet5
    , formlet6
    , formlet7
    , formlet8
    , formlet9
    , formlet10
    , formlet11
    , formlet12
    , formlet13
    , formlet14
    , formlet15
    , formlet16
    -- * Helper functions
    , radioButtons
    ) where

import Control.Applicative -- (Applicative, (<$>), (<*>))
import Control.Applicative.Error (Failing(..), maybeRead')
import Control.Arrow (first)
import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Instances ()
import Data.List (elemIndex)
import Data.Maybe (catMaybes, maybeToList, listToMaybe, isJust)
import qualified Data.Map as M
import Data.Traversable
import qualified Data.Set as S
import HAppS.Data (Default(..), DefaultD(..), defaultProxy, deriveNewData)
import HSP (cdata)
import HSP.Formlets (FormHSXT, input, labelledinput, inputInteger, hidden', radio, radio', select, checkbox, span, div)
import qualified HSX.XMLGenerator as HSX
import Prelude hiding (div, span)
import qualified HSP.Formlets.Util as X
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

data FormStatus = Enabled | Disabled deriving Eq

-- |A class of values that can be converted into formlets.
class (Read a, Show a, Default a, Ord a, Data FormletOfD a) => FormletOf a where
    formletOf :: (HSX.XMLGenerator x, Monad v, Applicative v) =>
                 FormStatus     -- ^ Should the form be initially enabled?
              -> Bool           -- ^ Controls treatment of union types.  If false, the form will
                                -- only generate values with the same constructor as @x@, if true
                                -- a choice form will be generated which allows values using any
                                -- of the constructors in the union.
              -> a              -- ^ The type the form returns, and the value it is initialized with
              -> FormHSXT x v a

data FormletOfD a =
    FormletOfD { formletOfD :: (HSX.XMLGenerator x, Monad v, Applicative v) => FormStatus -> Bool -> a -> FormHSXT x v a }

formletOfProxy :: Proxy FormletOfD
formletOfProxy = error "urk"

instance FormletOf t => Sat (FormletOfD t) where
    dict = FormletOfD { formletOfD = formletOf }

-- |Call 'formlet' and wrap the result in a div with a class name
-- constructed from the constructor and field names of the containing
-- algebraic type.
formlet' :: forall a. forall x. forall v. (FormletOf a, HSX.XMLGenerator x, Monad v, Applicative v) =>
            FormStatus -> Constr -> String -> a -> FormHSXT x v a
formlet' e constr field x = X.field (showConstr constr) field `plug` formlet e x

-- |This function calls the 'formletOf' method to convert any value
-- into a formlet.  The only difference between this wrapper function
-- and calling the 'formletOf' method directly is the computation of
-- the union flag.  Here the flag gets set to true whenever there are
-- multiple constructors, while in certain places, such as in
-- formletOfSet, we want to call formletOf directly with the flag set
-- to @False@ to get a form that only returns a particular
-- constructor.
formlet :: forall a. forall x. forall v. (FormletOf a, HSX.XMLGenerator x, Monad v, Applicative v) =>
                  FormStatus -> a -> FormHSXT x v a
formlet e x =
    case dataTypeRep t of
      AlgRep constrs ->
          let f = formletOfD dict e (length constrs > 1) x in
          case dataTypeName t of
            "Prelude.[]" -> f
            "Prelude.Bool" -> f
            "Set" -> f
            _ -> X.typename (dataTypeName t) `plug` f
      _ -> formletOfD dict e False x
    where
      t = dataTypeOf formletOfProxy x

formletOfAny e union x =
    case dataTypeRep t of
      AlgRep constrs
          -- Special case for types that only have constructors of
          -- arity zero, we don't have to write special instances
          -- for these.
        | all (== []) (map constrFields constrs) ->
            if union
            then radioButtons (showConstr . toConstr formletOfProxy) (radio' (e == Enabled)) (Just x)
            else hidden' x
        | otherwise -> error $ "Missing formletOf instance for type " ++ show (dataTypeName (dataTypeOf formletOfProxy x)) ++ " (" ++ show x ++ ")"
      _ -> error $ "Missing formletOf instance for type " ++ show (dataTypeName (dataTypeOf formletOfProxy x)) ++ " (" ++ show x ++ ")"
    where
      t = dataTypeOf formletOfProxy x

-- |Generate radio buttons selecting the different constructors of an
-- algebraic type, but ignoring any fields that might be associated
-- with the different constructors.
radioButtons :: forall a. forall x. forall v. (Read a, Show a, Default a, Ord a, Data FormletOfD a, HSX.XMLGenerator x, Monad v, Applicative v) =>
                         (a -> String) -> ([(a, String)] -> Maybe a -> FormHSXT x v a) -> Maybe a -> FormHSXT x v a
radioButtons labelFn selectFn x =
    selectFn pairs x
    where
      pairs :: [(a, String)]
      pairs = map (\ x' -> (replace x x', labelFn x')) (allDefaultValues (defaultValue :: a))
      -- toString = showConstr . toConstr formletOfProxy
      replace (Just x) x' = if toConstr formletOfProxy x' == toConstr formletOfProxy x then x else x'
      replace Nothing x' = x'

instance Ord Constr where compare a b = compare (showConstr a) (showConstr b)
$(deriveNewData [''S.Set])

-- |Create a formlet for a set of elements of an algebraic type.  It
-- is assumed that only one element with a particular constructor can
-- be in the set.
formletOfSet :: forall a. forall x. forall v. (FormletOf a, HSX.XMLGenerator x, Monad v, Applicative v) =>
                FormStatus -> S.Set a -> FormHSXT x v (S.Set a)
formletOfSet e xs =
        ((,) <$> (X.someof `plug` sequenceA checks) <*> (X.forms `plug` sequenceA fields)) `check` makeSet
        --X.someof `plug` sequenceA (map (\ (check, field) -> X.choice `plug` ((,) <$> check <*> field)) (zip checks fields)) `check` makeSet
        where
          -- The checkboxes
          -- checks :: [FormHSXT x v Bool]
          checks = map (\ c -> checkbox (e == Enabled) (c `S.member` cs) (Just (showConstr c))) constrs
          -- The data fields for each of the type's constructors
          fields :: [FormHSXT x v a]
          fields =
              -- Mark each formlet with the associated constructor
              map (\ c -> X.choice `plug` frm (initial c)) constrs
              where
                frm :: a -> FormHSXT x v a
                frm x = formletOfD dict (if enabled x then e else Disabled) False x
                -- If x is in the set, enable the corresponding form.
                enabled :: a -> Bool
                enabled x = (toConstr formletOfProxy x) `S.member` cs
          -- Construct the new set
          --makeSet :: [(Bool, a)] -> Failing (S.Set a)
          --makeSet pairs = Success . S.fromList . mask $ pairs
          makeSet :: ([Bool], [a]) -> Failing (S.Set a)
          makeSet (flags, values) = Success . S.fromList . mask $ (zip flags values)
              where
                mask :: [(Bool, a)] -> [a]
                mask pairs = catMaybes $ map (\ (b, x) -> if b then Just x else Nothing) pairs
          -- The initial value for each constructor - either a member
          -- of xs or one of the default values.
          initial :: Constr -> a
          initial c = M.findWithDefault (error $ "Unexpected constructor: " ++ showConstr c) c initialMap
              where initialMap = M.fromList (map (\ (c, x) -> (c, M.findWithDefault x c m)) (zip constrs defaults))
                    m = M.fromList (map (\ x -> (toConstr formletOfProxy x, x)) (S.toList xs))
          defaults = allDefaultValues (defaultValue :: a)
          cs = S.map (toConstr formletOfProxy) xs
          constrs = dataTypeConstrs (dataTypeOf formletOfProxy (undefined :: a))

-- |The treatment of the type (Maybe a) depends on a.  If a is a union
-- it adds the "None Chosen" choice to the selections.  If it is a set
-- it is ignored (or treated as an error?)  Otherwise it is treated as
-- a set with either one element or none.
formletOfMaybe :: forall a. forall x. forall v. (FormletOf a, HSX.XMLGenerator x, Monad v, Applicative v) =>
                  FormStatus -> Maybe a -> FormHSXT x v (Maybe a)
formletOfMaybe e mx = formlet e (S.fromList (maybeToList mx)) `check` (Success . listToMaybe . S.elems)

-- |Display the elements of a list with an extra defaultValue added.
-- This extra value is removed if it is still equal to defaultValue.
formletOfList :: forall a. forall x. forall v. (FormletOf a, HSX.XMLGenerator x, Monad v, Applicative v) =>
                 FormStatus -> [a] -> FormHSXT x v [a]
formletOfList e xs =
        frm `check` stripExtra
        where
          frm = sequenceA . map (formlet e) $ xs ++ [defaultValue :: a]
          -- It is assumed that defaultValue is not an important
          -- element of the list, either a form element that was never
          -- filled in our one whose content was erased, so here we
          -- filter it out.
          stripExtra xs' = Success $ filter (/= defaultValue) xs'

-- |Convert a generic value into a formlet.  This code only handles
-- the case of a union type whose constructors all have arity zero.
-- Other cases must be handled in specialized instances.
instance (Read a, Show a, Default a, Ord a, Data FormletOfD a) => FormletOf a where formletOf e union x = formletOfAny e union x
instance FormletOf a => FormletOf (S.Set a) where formletOf e _ xs = formletOfSet e xs
instance FormletOf a => FormletOf (Maybe a) where formletOf e _ mx = formletOfMaybe e mx
instance FormletOf a => FormletOf [a] where formletOf e _ xs = formletOfList e xs
instance FormletOf Integer where formletOf e _ n = inputInteger (e == Enabled) (Just n)
instance FormletOf String where formletOf e _ s = input (e == Enabled) (Just s)
instance FormletOf Bool where formletOf e _ b = checkbox (e == Enabled) b Nothing

-- |Convert a record with one argument.  We need to pass the
-- constructor and field name so they can be included in the class of
-- the div which is wrapped around the resulting xml.
formlet1 :: forall t. forall a. forall x. forall v.
            (Read t, Show t, Ord t, Default t, Data FormletOfD t,
             Read a, Show a, Ord a, Default a, Data FormletOfD a,
             HSX.XMLGenerator x, Applicative v, Monad v) => FormStatus -> (a -> t) -> a -> FormHSXT x v t
formlet1  e con a =
    case constrFields constr of
      [a'] -> con <$> formlet' e constr a' a
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error $ "formlet1: " ++ show x ++ "(constrFields -> " ++ show (constrFields constr) ++ ", maxConstrIndex -> " ++ show (maxConstrIndex (dataTypeOf formletOfProxy x)) ++ ")"
    where
      constr = toConstr formletOfProxy x
      x = con a

-- |Convert a record with two arguments.
formlet2 :: forall t. forall a. forall b. forall x. forall v.
            (Read t, Show t, Ord t, Default t, Data FormletOfD t,
             Read a, Show a, Ord a, Default a, Data FormletOfD a,
             Read b, Show b, Ord b, Default b, Data FormletOfD b,
             HSX.XMLGenerator x, Applicative v, Monad v) => FormStatus -> (a -> b -> t) -> a -> b -> FormHSXT x v t
formlet2  e con a b =
    case constrFields constr of
      [a', b'] -> con <$> formlet' e constr a' a <*> formlet' e constr b' b
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error $ "formlet2: " ++ show x
    where
      constr = toConstr formletOfProxy x
      x = con a b

-- |Convert a record with three arguments.
formlet3 :: forall t. forall a. forall b. forall c. forall x. forall v.
            (Read t, Show t, Ord t, Default t, Data FormletOfD t,
             Read a, Show a, Ord a, Default a, Data FormletOfD a,
             Read b, Show b, Ord b, Default b, Data FormletOfD b,
             Read c, Show c, Ord c, Default c, Data FormletOfD c,
             HSX.XMLGenerator x, Applicative v, Monad v) => FormStatus -> (a -> b -> c -> t) -> a -> b -> c -> FormHSXT x v t
formlet3  e con a b c =
    case constrFields constr of
      [a', b', c'] -> con <$> formlet' e constr a' a <*> formlet' e constr b' b <*> formlet' e constr c' c
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet3: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c

formlet4  en con a b c d =
    case constrFields constr of
      [a', b', c', d'] -> con <$> formlet' en constr a' a <*> formlet' en constr b' b <*> formlet' en constr c' c <*> formlet' en constr d' d
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet4: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c d

formlet5  en con a b c d e =
    case constrFields constr of
      [a', b', c', d', e'] -> con <$> formlet' en constr a' a <*> formlet' en constr b' b <*> formlet' en constr c' c <*> formlet' en constr d' d <*> formlet' en constr e' e
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet5: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c d e

formlet6  en con a b c d e f =
    case constrFields constr of
      [a', b', c', d', e', f'] -> con <$> formlet' en constr a' a <*> formlet' en constr b' b <*> formlet' en constr c' c <*> formlet' en constr d' d <*> formlet' en constr e' e <*> formlet' en constr f' f
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet6: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c d e f

formlet7  en con a b c d e f g =
    case constrFields constr of
      [a', b', c', d', e', f', g'] -> con <$> formlet' en constr a' a <*> formlet' en constr b' b <*> formlet' en constr c' c <*> formlet' en constr d' d <*> formlet' en constr e' e <*> formlet' en constr f' f <*> formlet' en constr g' g
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet7: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c d e f g

formlet8  en con a b c d e f g h =
    case constrFields constr of
      [a', b', c', d', e', f', g', h'] -> con <$> formlet' en constr a' a <*> formlet' en constr b' b <*> formlet' en constr c' c <*> formlet' en constr d' d <*> formlet' en constr e' e <*> formlet' en constr f' f <*> formlet' en constr g' g <*> formlet' en constr h' h
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet8: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c d e f g h

formlet9  en con a b c d e f g h i =
    case constrFields constr of
      [a', b', c', d', e', f', g', h', i'] -> con <$> formlet' en constr a' a <*> formlet' en constr b' b <*> formlet' en constr c' c <*> formlet' en constr d' d <*> formlet' en constr e' e <*> formlet' en constr f' f <*> formlet' en constr g' g <*> formlet' en constr h' h <*> formlet' en constr i' i
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet9: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c d e f g h i

formlet10 en con a b c d e f g h i j =
    case constrFields constr of
      [a', b', c', d', e', f', g', h', i', j'] -> con <$> formlet' en constr a' a <*> formlet' en constr b' b <*> formlet' en constr c' c <*> formlet' en constr d' d <*> formlet' en constr e' e <*> formlet' en constr f' f <*> formlet' en constr g' g <*> formlet' en constr h' h <*> formlet' en constr i' i <*> formlet' en constr j' j
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet10: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c d e f g h i j

formlet11 en con a b c d e f g h i j k =
    case constrFields constr of
      [a', b', c', d', e', f', g', h', i', j', k'] -> con <$> formlet' en constr a' a <*> formlet' en constr b' b <*> formlet' en constr c' c <*> formlet' en constr d' d <*> formlet' en constr e' e <*> formlet' en constr f' f <*> formlet' en constr g' g <*> formlet' en constr h' h <*> formlet' en constr i' i <*> formlet' en constr j' j <*> formlet' en constr k' k
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet11: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c d e f g h i j k

formlet12 en con a b c d e f g h i j k l =
    case constrFields constr of
      [a', b', c', d', e', f', g', h', i', j', k', l'] -> con <$> formlet' en constr a' a <*> formlet' en constr b' b <*> formlet' en constr c' c <*> formlet' en constr d' d <*> formlet' en constr e' e <*> formlet' en constr f' f <*> formlet' en constr g' g <*> formlet' en constr h' h <*> formlet' en constr i' i <*> formlet' en constr j' j <*> formlet' en constr k' k <*> formlet' en constr l' l
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet12: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c d e f g h i j k l

formlet13 en con a b c d e f g h i j k l m =
    case constrFields constr of
      [a', b', c', d', e', f', g', h', i', j', k', l', m'] -> con <$> formlet' en constr a' a <*> formlet' en constr b' b <*> formlet' en constr c' c <*> formlet' en constr d' d <*> formlet' en constr e' e <*> formlet' en constr f' f <*> formlet' en constr g' g <*> formlet' en constr h' h <*> formlet' en constr i' i <*> formlet' en constr j' j <*> formlet' en constr k' k <*> formlet' en constr l' l <*> formlet' en constr m' m
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet13: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c d e f g h i j k l m

formlet14 en con a b c d e f g h i j k l m n =
    case constrFields constr of
      [a', b', c', d', e', f', g', h', i', j', k', l', m', n'] -> con <$> formlet' en constr a' a <*> formlet' en constr b' b <*> formlet' en constr c' c <*> formlet' en constr d' d <*> formlet' en constr e' e <*> formlet' en constr f' f <*> formlet' en constr g' g <*> formlet' en constr h' h <*> formlet' en constr i' i <*> formlet' en constr j' j <*> formlet' en constr k' k <*> formlet' en constr l' l <*> formlet' en constr m' m <*> formlet' en constr n' n
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet14: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c d e f g h i j k l m n

formlet15 en con a b c d e f g h i j k l m n o =
    case constrFields constr of
      [a', b', c', d', e', f', g', h', i', j', k', l', m', n', o'] -> con <$> formlet' en constr a' a <*> formlet' en constr b' b <*> formlet' en constr c' c <*> formlet' en constr d' d <*> formlet' en constr e' e <*> formlet' en constr f' f <*> formlet' en constr g' g <*> formlet' en constr h' h <*> formlet' en constr i' i <*> formlet' en constr j' j <*> formlet' en constr k' k <*> formlet' en constr l' l <*> formlet' en constr m' m <*> formlet' en constr n' n <*> formlet' en constr o' o
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet15: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c d e f g h i j k l m n o

formlet16 en con a b c d e f g h i j k l m n o p =
    case constrFields constr of
      [a', b', c', d', e', f', g', h', i', j', k', l', m', n', o', p'] -> con <$> formlet' en constr a' a <*> formlet' en constr b' b <*> formlet' en constr c' c <*> formlet' en constr d' d <*> formlet' en constr e' e <*> formlet' en constr f' f <*> formlet' en constr g' g <*> formlet' en constr h' h <*> formlet' en constr i' i <*> formlet' en constr j' j <*> formlet' en constr k' k <*> formlet' en constr l' l <*> formlet' en constr m' m <*> formlet' en constr n' n <*> formlet' en constr o' o <*> formlet' en constr p' p
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet16: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c d e f g h i j k l m n o p
