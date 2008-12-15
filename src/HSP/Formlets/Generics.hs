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
import Data.Maybe (catMaybes, maybeToList, listToMaybe)
import qualified Data.Map as M
import Data.Traversable
import qualified Data.Set as S
import HAppS.Data (Default(..), DefaultD(..), defaultProxy, deriveNewData)
import HSP (cdata)
import HSP.Formlets (FormHSXT, input, labelledinput, inputInteger, hidden', radio, radio', select, checkbox, span, div)
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

-- |A class of values that can be converted into formlets.
class (Read a, Show a, Default a, Ord a, Data FormletOfD a) => FormletOf a where
    formletOf :: (HSX.XMLGenerator x, Monad v, Applicative v) =>
                 Bool           -- ^ If false make a form that only returns values with the same constructor as @x@,
                                -- if true checkboxes and other form elements are included to return any values of
                                -- type @a@.
              -> a              -- ^ The type the form returns, and the value it is initialized with
              -> FormHSXT x v a

data FormletOfD a =
    FormletOfD { formletOfD :: (HSX.XMLGenerator x, Monad v, Applicative v) => Bool -> a -> FormHSXT x v a }

formletOfProxy :: Proxy FormletOfD
formletOfProxy = error "urk"

instance FormletOf t => Sat (FormletOfD t) where
    dict = FormletOfD { formletOfD = formletOf }

-- |Call 'formlet' and wrap the result in a div with a class name
-- constructed from the constructor and field names of the containing
-- algebraic type.
formlet' :: forall a. forall x. forall v. (Read a, Show a, Default a, Ord a, Data FormletOfD a, HSX.XMLGenerator x, Monad v, Applicative v) =>
            (a, Constr, String) -> FormHSXT x v a
formlet' (x, constr, field) = div (showConstr constr ++ "_" ++ field) $ formlet x

-- |This function calls the 'formletOf' method to convert any value
-- into a formlet.  The only difference between this wrapper function
-- and calling the 'formletOf' method directly is the computation of
-- the union flag.  Here the flag gets set to true whenever there are
-- multiple constructors, while in certain places, such as in
-- formletOfSet, we want to call formletOf directly with the flag set
-- to @False@ to get a form that only returns a particular
-- constructor.
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
    formletOf _ xs = formletOfSet xs

instance (Read a, Show a, Default a, Ord a, Data FormletOfD a) => FormletOf (Maybe a) where
    formletOf _ mx = formletOfMaybe mx

instance (Read a, Show a, Default a, Ord a, Data FormletOfD a) => FormletOf [a] where
    formletOf _ xs = formletOfList xs

-- Primitive types
instance FormletOf Integer where formletOf _ n = inputInteger (Just n)
instance FormletOf String where formletOf _ s = labelledinput (Just s)

-- |Create a formlet for a set of elements of an algebraic type.  It
-- is assumed that only one element with a particular constructor can
-- be in the set.
formletOfSet :: forall a. forall x. forall v. (Read a, Show a, Default a, Ord a, Data FormletOfD a, HSX.XMLGenerator x, Monad v, Applicative v) =>
                S.Set a -> FormHSXT x v (S.Set a)
formletOfSet xs =
        sequenceA (map (\ (check, field) -> (,) <$> check <*> field) (zip checks fields)) `check` makeSet
        where
          -- The checkboxes
          -- checks :: [FormHSXT x v Bool]
          checks = map (\ c -> {- label (showConstr c) -} (checkbox (c `S.member` cs) (Just (showConstr c)))) constrs
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
          makeSet :: [(Bool, a)] -> Failing (S.Set a)
          makeSet pairs = Success . S.fromList . mask $ pairs
          mask :: [(Bool, a)] -> [a]
          mask pairs = catMaybes $ map (\ (b, x) -> if b then Just x else Nothing) pairs
          constrs = dataTypeConstrs (dataTypeOf formletOfProxy (defaultValue :: a))

-- |Treat a maybe as a "all that apply" set with a single element.
formletOfMaybe :: forall a. forall x. forall v. (Read a, Show a, Default a, Ord a, Data FormletOfD a, HSX.XMLGenerator x, Monad v, Applicative v) =>
                  Maybe a -> FormHSXT x v (Maybe a)
formletOfMaybe mx = formlet (S.fromList (maybeToList mx)) `check` (Success . listToMaybe . S.elems)

-- |Display the elements of a list with an extra defaultValue added.
-- This extra value is removed if it is still equal to defaultValue.
formletOfList :: forall a. forall x. forall v. (Read a, Show a, Default a, Ord a, Data FormletOfD a, HSX.XMLGenerator x, Monad v, Applicative v) =>
                 [a] -> FormHSXT x v [a]
formletOfList xs =
        frm `check` stripExtra
        where
          frm = sequenceA . map formlet $ xs ++ [defaultValue :: a]
          stripExtra xs' = Success $ if last xs' == defaultValue then init xs' else xs'

-- |Convert a record with one argument.  We need to pass the
-- constructor and field name so they can be included in the class of
-- the div which is wrapped around the resulting xml.
formlet1 :: forall t. forall a. forall x. forall v.
            (Read t, Show t, Ord t, Default t, Data FormletOfD t,
             Read a, Show a, Ord a, Default a, Data FormletOfD a,
             HSX.XMLGenerator x, Applicative v, Monad v) => (a -> t) -> a -> FormHSXT x v t
formlet1  con a =
    case constrFields constr of
      [a'] -> con <$> formlet' (a, constr, a')
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
             HSX.XMLGenerator x, Applicative v, Monad v) => (a -> b -> t) -> a -> b -> FormHSXT x v t
formlet2 con a b =
    case constrFields constr of
      [a', b'] -> con <$> formlet' (a, constr, a') <*> formlet' (b, constr, b')
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
             HSX.XMLGenerator x, Applicative v, Monad v) => (a -> b -> c -> t) -> a -> b -> c -> FormHSXT x v t
formlet3  con a b c =
    case constrFields constr of
      [a', b', c'] -> con <$> formlet' (a, constr, a') <*> formlet' (b, constr, b') <*> formlet' (c, constr, c')
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet3: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c

formlet4  con a b c d =
    case constrFields constr of
      [a', b', c', d'] -> con <$> formlet' (a, constr, a') <*> formlet' (b, constr, b') <*> formlet' (c, constr, c') <*> formlet' (d, constr, d')
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet4: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c d

formlet5  con a b c d e =
    case constrFields constr of
      [a', b', c', d', e'] -> con <$> formlet' (a, constr, a') <*> formlet' (b, constr, b') <*> formlet' (c, constr, c') <*> formlet' (d, constr, d') <*> formlet' (e, constr, e')
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet5: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c d e

formlet6  con a b c d e f =
    case constrFields constr of
      [a', b', c', d', e', f'] -> con <$> formlet' (a, constr, a') <*> formlet' (b, constr, b') <*> formlet' (c, constr, c') <*> formlet' (d, constr, d') <*> formlet' (e, constr, e') <*> formlet' (f, constr, f')
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet6: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c d e f

formlet7  con a b c d e f g =
    case constrFields constr of
      [a', b', c', d', e', f', g'] -> con <$> formlet' (a, constr, a') <*> formlet' (b, constr, b') <*> formlet' (c, constr, c') <*> formlet' (d, constr, d') <*> formlet' (e, constr, e') <*> formlet' (f, constr, f') <*> formlet' (g, constr, g')
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet7: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c d e f g

formlet8  con a b c d e f g h =
    case constrFields constr of
      [a', b', c', d', e', f', g', h'] -> con <$> formlet' (a, constr, a') <*> formlet' (b, constr, b') <*> formlet' (c, constr, c') <*> formlet' (d, constr, d') <*> formlet' (e, constr, e') <*> formlet' (f, constr, f') <*> formlet' (g, constr, g') <*> formlet' (h, constr, h')
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet8: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c d e f g h

formlet9  con a b c d e f g h i =
    case constrFields constr of
      [a', b', c', d', e', f', g', h', i'] -> con <$> formlet' (a, constr, a') <*> formlet' (b, constr, b') <*> formlet' (c, constr, c') <*> formlet' (d, constr, d') <*> formlet' (e, constr, e') <*> formlet' (f, constr, f') <*> formlet' (g, constr, g') <*> formlet' (h, constr, h') <*> formlet' (i, constr, i')
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet9: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c d e f g h i

formlet10 con a b c d e f g h i j =
    case constrFields constr of
      [a', b', c', d', e', f', g', h', i', j'] -> con <$> formlet' (a, constr, a') <*> formlet' (b, constr, b') <*> formlet' (c, constr, c') <*> formlet' (d, constr, d') <*> formlet' (e, constr, e') <*> formlet' (f, constr, f') <*> formlet' (g, constr, g') <*> formlet' (h, constr, h') <*> formlet' (i, constr, i') <*> formlet' (j, constr, j')
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet10: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c d e f g h i j

formlet11 con a b c d e f g h i j k =
    case constrFields constr of
      [a', b', c', d', e', f', g', h', i', j', k'] -> con <$> formlet' (a, constr, a') <*> formlet' (b, constr, b') <*> formlet' (c, constr, c') <*> formlet' (d, constr, d') <*> formlet' (e, constr, e') <*> formlet' (f, constr, f') <*> formlet' (g, constr, g') <*> formlet' (h, constr, h') <*> formlet' (i, constr, i') <*> formlet' (j, constr, j') <*> formlet' (k, constr, k')
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet11: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c d e f g h i j k

formlet12 con a b c d e f g h i j k l =
    case constrFields constr of
      [a', b', c', d', e', f', g', h', i', j', k', l'] -> con <$> formlet' (a, constr, a') <*> formlet' (b, constr, b') <*> formlet' (c, constr, c') <*> formlet' (d, constr, d') <*> formlet' (e, constr, e') <*> formlet' (f, constr, f') <*> formlet' (g, constr, g') <*> formlet' (h, constr, h') <*> formlet' (i, constr, i') <*> formlet' (j, constr, j') <*> formlet' (k, constr, k') <*> formlet' (l, constr, l')
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet12: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c d e f g h i j k l

formlet13 con a b c d e f g h i j k l m =
    case constrFields constr of
      [a', b', c', d', e', f', g', h', i', j', k', l', m'] -> con <$> formlet' (a, constr, a') <*> formlet' (b, constr, b') <*> formlet' (c, constr, c') <*> formlet' (d, constr, d') <*> formlet' (e, constr, e') <*> formlet' (f, constr, f') <*> formlet' (g, constr, g') <*> formlet' (h, constr, h') <*> formlet' (i, constr, i') <*> formlet' (j, constr, j') <*> formlet' (k, constr, k') <*> formlet' (l, constr, l') <*> formlet' (m, constr, m')
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet13: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c d e f g h i j k l m

formlet14 con a b c d e f g h i j k l m n =
    case constrFields constr of
      [a', b', c', d', e', f', g', h', i', j', k', l', m', n'] -> con <$> formlet' (a, constr, a') <*> formlet' (b, constr, b') <*> formlet' (c, constr, c') <*> formlet' (d, constr, d') <*> formlet' (e, constr, e') <*> formlet' (f, constr, f') <*> formlet' (g, constr, g') <*> formlet' (h, constr, h') <*> formlet' (i, constr, i') <*> formlet' (j, constr, j') <*> formlet' (k, constr, k') <*> formlet' (l, constr, l') <*> formlet' (m, constr, m') <*> formlet' (n, constr, n')
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet14: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c d e f g h i j k l m n

formlet15 con a b c d e f g h i j k l m n o =
    case constrFields constr of
      [a', b', c', d', e', f', g', h', i', j', k', l', m', n', o'] -> con <$> formlet' (a, constr, a') <*> formlet' (b, constr, b') <*> formlet' (c, constr, c') <*> formlet' (d, constr, d') <*> formlet' (e, constr, e') <*> formlet' (f, constr, f') <*> formlet' (g, constr, g') <*> formlet' (h, constr, h') <*> formlet' (i, constr, i') <*> formlet' (j, constr, j') <*> formlet' (k, constr, k') <*> formlet' (l, constr, l') <*> formlet' (m, constr, m') <*> formlet' (n, constr, n') <*> formlet' (o, constr, o')
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet15: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c d e f g h i j k l m n o

formlet16 con a b c d e f g h i j k l m n o p =
    case constrFields constr of
      [a', b', c', d', e', f', g', h', i', j', k', l', m', n', o', p'] -> con <$> formlet' (a, constr, a') <*> formlet' (b, constr, b') <*> formlet' (c, constr, c') <*> formlet' (d, constr, d') <*> formlet' (e, constr, e') <*> formlet' (f, constr, f') <*> formlet' (g, constr, g') <*> formlet' (h, constr, h') <*> formlet' (i, constr, i') <*> formlet' (j, constr, j') <*> formlet' (k, constr, k') <*> formlet' (l, constr, l') <*> formlet' (m, constr, m') <*> formlet' (n, constr, n') <*> formlet' (o, constr, o') <*> formlet' (p, constr, p')
      [] -> error $ "Records without field names not supported: " ++ show x
      _ -> error ("formlet16: " ++ show x)
    where
      constr = toConstr formletOfProxy x
      x = con a b c d e f g h i j k l m n o p
