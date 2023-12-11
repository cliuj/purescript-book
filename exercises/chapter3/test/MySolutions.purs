module Test.MySolutions where

import Prelude

import Data.AddressBook (Entry, AddressBook, findEntry)
import Data.List (filter, head, nubByEq)
import Data.Maybe (Maybe(..))

-- Note to reader: Add your solutions to this file

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.address.street == street


isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName book = Nothing /= findEntry firstName lastName book

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates book = nubByEq fn book
  where
    fn :: Entry -> Entry -> Boolean
    fn a b = a.firstName == b.firstName && a.lastName == b.lastName
