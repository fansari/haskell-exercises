#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}

type Person = String
type Book   = String

type Database = [ (Person, Book) ]

exampleBase :: Database
exampleBase = [ ("Alice", "Tintin"), ("Anna", "Little Woman"), ("Alice", "Asterix"), ("Rory", "Tintin") ]

books :: Database -> Person -> [Book]
books dBase findPerson = [ book | (person,book) <- dBase, person==findPerson ]

makeLoan :: Database -> Person -> Book -> Database
makeLoan dBase pers bk = [ (pers,bk) ] ++ dBase

returnLoan :: Database -> Person -> Book -> Database
returnLoan dBase pers bk = [ pair | pair <- dBase, pair /= (pers,bk) ]

borrowed :: Database -> Book -> Bool
borrowed dBase bk
  | [ book | (person,book) <- dBase, book==bk ] /= [] = True
  | otherwise = False

numBorrowed :: Database -> Person -> Int
numBorrowed dBase person = length (books dBase person)

test1 :: Bool
test1 = borrowed exampleBase "Asterix"

test2 :: Database
test2 =  makeLoan exampleBase "Alice" "Rotten Romans"

test3 :: Database
test3 = returnLoan test2 "Alice" "Tintin"

main = do
  print(books exampleBase "Alice")
  print(test1)
  print(test2)
  print(books test2 "Alice")
  print(test3)
  print(numBorrowed test3 "Alice")
