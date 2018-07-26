{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fplugin=Language.Java.Inline.Plugin #-}
module CircularBuffer where

import           Data.Int
import           Foreign.JNI.Types          (J, JObject, JVM, jnull)
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Java              as Java
import           Language.Java.Inline

newtype CircularBuffer a =
  CircularBuffer (J (Java.Generic (Java.Class "example.CircularBuffer") '[ Java.Interp a]))
  deriving (Eq)

new :: Int -> IO (CircularBuffer a)
new capacity = do
  obj :: J (Java.Class "example.CircularBuffer") <- Java.new
    [Java.coerce (fromIntegral capacity :: Int32)]
  return (CircularBuffer (Java.unsafeCast (Java.generic obj)))

put :: Java.Reflect a => CircularBuffer a -> a -> IO ()
put (CircularBuffer obj) x = do
  value <- Java.reflect x
  Java.call (Java.unsafeUngeneric obj) "put" [Java.coerce (Java.upcast value)]

get :: Java.Reify a => CircularBuffer a -> IO (Maybe a)
get (CircularBuffer obj) = do
  x :: JObject <- Java.call (Java.unsafeUngeneric obj) "get" []
  if x == jnull
    then return Nothing
    else Just <$> Java.reify (Java.unsafeCast x)

size :: CircularBuffer a -> IO Int
size (CircularBuffer obj) = do
  s :: Int32 <- Java.call (Java.unsafeUngeneric obj) "size" []
  return (fromIntegral s)
