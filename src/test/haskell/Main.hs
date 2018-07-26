{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Int
import           Data.Maybe
import           Data.String                (fromString)
import           Data.Text                  (Text)
import           Foreign.JNI                (newJVM, withJVM)
import           Foreign.JNI.Types          (J, JObject, JVM)
import qualified Language.Haskell.TH.Syntax as TH
import           Language.Java.Inline
import           System.Environment         (lookupEnv)

import           Hedgehog                   hiding (Size)
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Internal.State    as Gen
import qualified Hedgehog.Range             as Range

import           CircularBuffer             (CircularBuffer)
import qualified CircularBuffer             as CircularBuffer

data Model = Model
  { contents :: [Int32]
  , capacity :: Int
  }

data State (v :: * -> *) = State
  { models   :: [(Var (Opaque (CircularBuffer Int32)) v, Model)]
  }

addOrReplace :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
addOrReplace key value assoc = (key, value) : filter ((key /=) . fst) assoc

initialState :: State v
initialState = State []

-- * new

data New (v :: * -> *) = New Int
  deriving (Show)

instance HTraversable New where
  htraverse _ (New c) = pure (New c)

new :: (Monad n, MonadGen n, MonadIO m) => Command n m State
new =
  let gen _ = Just (New <$> Gen.int (Range.linear 0 100))
      execute (New c) = do
        buf <- liftIO (CircularBuffer.new c)
        return (Opaque buf)
  in Command
       gen
       execute
       [ Update $ \s (New capacity) buf ->
           s {models = addOrReplace buf (Model [] capacity) (models s)}
       , Ensure $ \before after input (output :: Opaque (CircularBuffer Int32)) ->
           pure ()
       ]

-- * get

data Get (v :: * -> *) =
  Get (Var (Opaque (CircularBuffer Int32)) v)
  deriving (Show)

instance HTraversable Get where
  htraverse f (Get buf) = Get <$> htraverse f buf

get :: (MonadGen n, Monad n, MonadIO m) => Command n m State
get =
  let gen State {..}
        | null models = Nothing
        | otherwise = pure (Get <$> Gen.element (map fst models))
      execute (Get ref) = liftIO (CircularBuffer.get (opaque ref))
  in Command
       gen
       execute
       [ Require $ \State {..} (Get ref) ->
           isJust $ do
             Model {..} <- lookup ref models
             guard (not (null contents))
       , Update $ \s@State {..} (Get ref) _ ->
           case lookup ref models of
             Just model ->
               s
               { models =
                   addOrReplace
                     ref
                     model {contents = tail (contents model)}
                     models
               }
             Nothing -> s
       , Ensure $ \State {..} _ (Get ref) output ->
           output === (listToMaybe . contents =<< lookup ref models)
       ]

-- * put

data Put (v :: * -> *) = Put (Var (Opaque (CircularBuffer Int32)) v) Int32
  deriving (Show)

instance HTraversable Put where
  htraverse f (Put buf n) = Put <$> htraverse f buf <*> pure n

put :: (MonadGen n, Monad n, MonadIO m) => Command n m State
put =
  let gen State {..}
        | null models = Nothing
        | otherwise =
          pure
            (Put <$> Gen.element (map fst models) <*>
             Gen.int32 Range.linearBounded)
      execute (Put ref n) = liftIO (CircularBuffer.put (opaque ref) n)
  in Command
       gen
       execute
       [ Require $ \State {..} (Put ref _) ->
           isJust $ do
             Model {..} <- lookup ref models
             guard (length contents < capacity)
       , Update $ \s@State {..} (Put ref x) _ ->
           case lookup ref models of
             Just model ->
               s
               { models =
                   addOrReplace
                     ref
                     (model {contents = contents model ++ [x]})
                     models
               }
             Nothing -> s
       , Ensure $ \State {} _ (Put _ n) () -> pure ()
       ]

-- * size

data Size (v :: * -> *) = Size (Var (Opaque (CircularBuffer Int32)) v)
  deriving (Show)

instance HTraversable Size where
  htraverse f (Size buf) = Size <$> htraverse f buf

size :: (MonadGen n, Monad n, MonadIO m) => Command n m State
size =
  let gen State{..}
        | null models = Nothing
        | otherwise = pure (Size <$> Gen.element (map fst models))
      execute (Size ref) = liftIO (CircularBuffer.size (opaque ref))
  in Command
       gen
       execute
       [ Require $ \State {..} (Size ref) -> isJust (lookup ref models)
       , Ensure $ \State {..} _ (Size ref) s -> do
           let Just model = lookup ref models
           assert (s <= capacity model)
           s === length (contents model)
       ]

prop_circular_buffer_sequential :: Property
prop_circular_buffer_sequential = property $ do
  actions <- forAll $
    Gen.sequential (Range.linear 1 100) initialState [new, get, put, size]
  Gen.executeSequential initialState actions

tests :: IO Bool
tests = checkSequential $$(discover)

main :: IO ()
main = do
  let -- We use the classpath provided at build time.
      jvmArgs = case $(TH.lift =<< TH.runIO (lookupEnv "CLASSPATH")) of
        Nothing -> []
        Just cp -> [fromString ("-Djava.class.path=" ++ cp)]
  withJVM jvmArgs $ do
    succeeded <- tests
    unless succeeded (error "There were test failures.")
