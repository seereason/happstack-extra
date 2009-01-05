{-# LANGUAGE FlexibleContexts, RankNTypes, PatternGuards, PatternSignatures #-}
{-# OPTIONS_GHC -F -pgmFtrhsx -Wall -Wwarn -fno-warn-name-shadowing #-}
module HSP.Formlets.Transform (redesign) where

import Control.Monad.Identity
import Control.Monad.Trans
import Data.Char (toUpper)
import Data.List (lookup)
import Data.Maybe
import HSP
import HSP.Formlets.CamelCase
import qualified HSX.XMLGenerator as HSX
import Text.Formlets

-- Note that we can't do complicated nested patterns like this:
--
--redesign <field constr=(c :: String) name=(n :: String)><typename name=(n :: String)><input type="hidden" attrs /></typename></field> = ...
--
-- we get the error message "Conflicting definitions for `hsx_attrs0'"

--redesign' :: (XMLGenerator x, EmbedAsChild x String, EmbedAsChild x XML, (EmbedAsAttr x Attribute)) => XML -> XMLGenT x (HSX.XML x)
--redesign' xml = <% redesign xml %><% xml %></div>

redesign :: (XMLGenerator x, EmbedAsChild x String, EmbedAsChild x XML, (EmbedAsAttr x Attribute)) =>
            (String -> String -> Maybe String) -> XML -> XMLGenT x (HSX.XML x)
-- When we see the outer form element, append add the original code
-- for debugging purposes.
redesign xlate <form attrs><[ content* ]></form> =
    <div>
      <form attrs><% map (redesign xlate) content %></form>
      <h1>Un-transformed XML</h1>
      <pre><% map renderAsHTML content %></pre>
    </div>
-- Add a label to the right of a radio button.  This code may not get
-- called because the following case overrides it.
redesign _ <input value=(v :: String) id=(ident :: String) type="radio" attrs /> = 
    <span><input value="" id=ident type="radio" attrs /><label for=ident>ee <% camelToText v %></label></span>
-- The HSP.Formlets.radio function outputs a span with the radio
-- button and the label.  This wouldn't be necessary if the radio
-- buttons were inside a oneof tag.
redesign _ <span><input type="radio" attrs1 /><label attrs2><[ label* ]></label></span> =
    case label of
      [CDATA _ s] -> <span><input type="radio" attrs1 /><label attrs2><% camelToText s %></label></span>
      _ -> <span><input type="radio" attrs1 /><label attrs2>gg <% label %></label></span>
-- Add labels to the right of a checkbox
{-
redesign <input value=(v :: String) id=(ident :: String) type="checkbox" attrs /> =
    <span><input value=v id=ident type="checkbox" attrs /><label for=ident><% camelToText v %></label></span>
-}
-- Redesign a single field of a record type.
redesign xlate <field constr=(c :: String) name=(n :: String)><[ wiggles* ]></field> =
    case wiggles of
      -- If we see a typename we look at what is inside,
      -- perhaps using the typename somewhere (though it is
      -- not used at this time.)
      [<typename name=(t :: String)><[ content* ]></typename>] ->
          case content of
            -- Leave hidden elements alone
            [<input type="hidden" attrs />] ->
                <input type="hidden" attrs />
            -- Add a header label on a selection, and recurse on the
            -- options.
            [<select attrs><[ options* ]></select>] ->
                <span><% makeBoldLabel c n %><select attrs><% map (redesign xlate) options %></select></span>
            -- Anything else
            _ -> redesignField xlate c n wiggles
      -- Here we have a list of checkboxes and a list of associated forms
      [<someof><[ choices* ]></someof>, <forms><[ forms* ]></forms>] ->
          <div class=(c ++ "_" ++ n)><% makeBoldLabel c n %><% enableDisableCheckboxes choices forms %><% map (redesign xlate) forms %></div>
{-
      [<input type="checkbox" attrs />] ->
          <div class=(c ++ "_" ++ n)><input type="checkbox" attrs /><% makeLabel c n %></div>
-}
      _ -> redesignField xlate c n wiggles
    where
      redesignField xlate c n wiggles =
          <div class=(c ++ "_" ++ n)><% makeBoldLabel c n %><% map (redesign xlate) wiggles %></div>
      makeLabel c n =
          case xlate c n of
            Just s -> <label><% s %></label>
            Nothing -> <label><% fieldName n %></label>
      makeBoldLabel c n =
          case xlate c n of
            -- Why is one <b> and one <h3>?
            Just _ -> <h3><% makeLabel c n %></h3>
            Nothing -> <b><% makeLabel c n %></b>
-- Reformat the data associated with a constructor
redesign xlate <constr name=(n :: String)><[ children* ]></constr> =
    case children of
      [] -> <span></span>
      -- [x] -> <% map redesign [x] %>
      xs -> <div><% map (redesign xlate) xs %></div>
redesign xlate <typename name=(n :: String)><[ wiggles* ]></typename> =
    <div class=n><% map (redesign xlate) wiggles %></div>
-- There are still a few leftover divs in the XML
redesign xlate <div class=(c :: String) attrs><[ wiggles* ]></div> =
    let children = map (redesign xlate) wiggles in
    case c of
      "form" -> <div class=c attrs><% children %></div>
      "submit" -> <div class=c attrs><% children %></div>
      -- Generated in createForm upon failure
      "fault" -> <div class=c attrs><% children %></div>
      _ -> <div class=("unexpected div: " ++ c) attrs><% children %></div>
      -- Hidden fields - change div to span
      -- The USState dropdown shouldn't be on its own line, replace div with span
      --"USState" -> <span class=c attrs><% children %></span>
      --otherwise -> <div class=c attrs><% children %></div>
redesign _ (Element name attrs [cd@(CDATA _ _)]) = genElement name ([] ++ map asAttr attrs) [asChild cd]
redesign xlate (Element name attrs children) = genElement name ([] ++ map asAttr attrs) [asChild (asChild (map (redesign xlate) children))]
redesign _ cd@(CDATA f v) = genElement (Nothing, "span") [] [asChild cd]
-- error $ "Found CDATA " ++ show f ++ " " ++ show v

fieldName = (\ (c : cs) -> toUpper c : cs) . camelToText

-- Each checkbox enables or disables the inputs of the associated form.
enableDisableCheckboxes choices forms =
    <% map (uncurry enableDisableCheckbox) (zip classLists choices) %>
    where
      classLists = map findClasses forms

-- A radio button enables its own form and disables all the others.
enableDisableRadios choices forms =
    <% map (uncurry enableDisableRadio) (zip classPairs choices) %>
    where
      classPairs = map (\ (a, b) -> (a, concat b)) (pairs classLists)
      classLists = map findClasses forms

-- Return a list of ids to refer to when enabling or disabling a form.
findClasses :: XML -> [String]
findClasses <input class=(c :: String) attrs /> = [c]
findClasses (Element name attrs [cd@(CDATA _ _)]) = []
findClasses (Element name attrs children) = concatMap findClasses children
findClasses cd@(CDATA f v) = []

enableDisableCheckbox :: (XMLGenerator x, EmbedAsChild x String, EmbedAsChild x XML, (EmbedAsAttr x Attribute)) => [String] -> XML -> XMLGenT x (HSX.XML x)
enableDisableCheckbox classes (<choice name=(n :: String)><[ children* ]></choice>) =
    case children of
      [child] -> handleChoice n (camelToText n) child
      _ -> error "Unexpected inputs inside choice"
    where
      handleChoice n label (<input type=(t :: String) id=(ident :: String) attrs />) =
          case classes of
            [] ->
                <span class=n>
                 <input type=t id=(ident :: String) attrs />
                 <label for=ident><% label %></label>
                </span>
            _ ->
                <span class=n>
                 <input type=t id=(ident :: String) onchange=(concat (map enableDisable classes)) attrs />
                 <label for=ident><% label %></label>
                </span>

enableDisableRadio :: (XMLGenerator x, EmbedAsChild x String, EmbedAsChild x XML, (EmbedAsAttr x Attribute)) => ([String], [String]) -> XML -> XMLGenT x (HSX.XML x)
enableDisableRadio (toEnable, toDisable) (<input type="radio" value=(v :: String) id=(ident :: String) attrs />) =
    case (toEnable, toDisable) of
      ([], []) ->
          <span>
            <input type="radio" value=v id=ident attrs />
            <label for=ident>cc <% camelToText v %></label>
          </span>
      _ -> 
          <span>
            <input type="radio" value=v id=ident onchange=(concat (map (enable True) toEnable ++ map (enable False) toDisable)) attrs />
            <label for=ident>dd <% camelToText v %></label>
          </span>

enable f s = "enable(" ++ (if f then "1" else "0") ++ ", '." ++ s ++ "');"
enableDisable s = "enable(this.checked, '." ++ s ++ "');"

-- [1, 2, 3] -> [(1, [2, 3]), (2, [1, 3]), (3, [1, 2])]
pairs l =
    map (\ n -> (l !! n, drop1 n l)) [0 .. (length l - 1)]
    where drop1 n l = let (a, b) = splitAt n l in a ++ drop 1 b
