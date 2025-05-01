#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}

{---------------------------------------------------------------------------
File    : ackermann.hs
Date    : 2024-03-08
Author  : Frank Ansari
Purpose : ackermann function

(ack 4 1)
real    34m19.715s
user    34m14.147s
sys     0m1.132s
---------------------------------------------------------------------------}

ack 0 m = m+1
ack n 0 = ack (n-1) 1
ack n m = ack (n-1) (ack n (m-1))

main = do
  print(ack 4 1)
