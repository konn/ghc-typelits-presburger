{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module ErrorsNoPlugin where

import Shared

zipMVec :: Vec n a -> Vec n b -> Vec n (a, b)
zipMVec Nil Nil = Nil
zipMVec zs@(a :- as) (b :- bs) = (a, b) :- zipMVec zs bs

spin :: Vec n a -> Vec n a -> ()
spin _ _ = ()

unSpin :: Vec n a -> ()
unSpin Nil = ()
unSpin zs@(_ :- ws) = spin zs ws
