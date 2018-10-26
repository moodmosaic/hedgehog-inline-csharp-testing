--
-- Based on https://github.com/owickstrom/hedgehog-inline-java-testing/blob/d0e51bd6712ce548b175262718d25649c5280933/src/test/haskell/Main.hs
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

import           Clr.Inline (Clr)
import qualified Clr.Inline as Clr

import qualified CircularBuffer

import           Control.Monad (guard, unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Maybe (isJust, listToMaybe)

import           Hedgehog hiding (Size)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


------------------------------------------------------------------------
-- State

newtype State (v :: * -> *) =
  State {
      models :: [(Var (Opaque (Clr "CircularBuffer<int>")) v, Model)]
  }

initialState :: State v
initialState =
  State []

data Model =
  Model {
      contents :: [Int]
    , capacity :: Int
  }

addOrReplace :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
addOrReplace key value assoc =
  (key, value) : filter ((key /=) . fst) assoc

------------------------------------------------------------------------
-- New

newtype New (v :: * -> *) =
  New Int
  deriving (Show)

instance HTraversable New where
  htraverse _ (New c) =
    pure (New c)

new :: (Monad n, MonadGen n, MonadIO m) => Command n m State
new =
  let
    gen _ =
      Just $
        New <$> Gen.int (Range.linear 0 100)

    execute (New c) =
      fmap Opaque . liftIO $ CircularBuffer.new c
  in
    Command gen execute [
        Update $ \s (New capacity) buf ->
          s {
            models =
              addOrReplace buf (Model [] capacity) (models s)
          }
      ]

------------------------------------------------------------------------
-- Get

newtype Get (v :: * -> *) =
  Get (Var (Opaque (Clr "CircularBuffer<int>")) v)
  deriving (Show)

instance HTraversable Get where
  htraverse f (Get buf) =
    Get <$> htraverse f buf

get :: (MonadGen n, Monad n, MonadIO m) => Command n m State
get =
  let
    gen State {..}
      | null models =
          Nothing
      | otherwise =
          pure (Get <$> Gen.element (map fst models))
    execute (Get ref) =
      liftIO (CircularBuffer.get (opaque ref))
  in
    Command gen execute [
        Require $ \State {..} (Get ref) ->
          isJust $ do
            Model {..} <- lookup ref models
            guard (not (null contents))

      , Update $ \s@State {..} (Get ref) _ ->
          case lookup ref models of
            Just model ->
              s {
                models =
                  addOrReplace
                    ref
                    model {
                      contents =
                        tail (contents model)
                    }
                    models
              }
            Nothing -> s

      , Ensure $ \State {..} _ (Get ref) output ->
          output === (listToJust . contents =<< lookup ref models)
      ]
  where
    listToJust xs =
      let
        m =
          listToMaybe xs
      in
        if isJust m then
          m
        else
          -- T in CircularBuffer<T> is a Value Type (CLR's System.Int32)
          -- and Value Types can't be null so simulate it via 'Just 0'.
          Just 0

------------------------------------------------------------------------
-- Put

data Put (v :: * -> *) =
  Put (Var (Opaque (Clr "CircularBuffer<int>")) v) Int
  deriving (Show)

instance HTraversable Put where
  htraverse f (Put buf n) =
    Put <$> htraverse f buf <*> pure n

put :: (MonadGen n, Monad n, MonadIO m) => Command n m State
put =
  let
    gen State {..}
      | null models =
          Nothing
      | otherwise =
          pure
            (Put <$> Gen.element (map fst models) <*> Gen.int (Range.linear 0 100))
    execute (Put ref n) =
      liftIO (CircularBuffer.put (opaque ref) n)
  in
    Command gen execute [
        Require $ \State {..} (Put ref _) ->
          isJust $ do
            Model {..} <- lookup ref models
            guard (length contents < capacity)

      , Update $ \s@State {..} (Put ref x) _ ->
          case lookup ref models of
            Just model ->
              s {
                models =
                  addOrReplace
                    ref
                    model {
                      contents =
                        contents model ++ [x]
                    }
                    models
              }
            Nothing ->
              s
      ]

------------------------------------------------------------------------
-- Size

newtype Size (v :: * -> *) =
  Size (Var (Opaque (Clr "CircularBuffer<int>")) v)
  deriving (Show)

instance HTraversable Size where
  htraverse f (Size buf) =
    Size <$> htraverse f buf

size :: (MonadGen n, Monad n, MonadIO m) => Command n m State
size =
  let
    gen State{..}
      | null models =
          Nothing
      | otherwise =
          pure (Size <$> Gen.element (map fst models))
    execute (Size ref) =
      liftIO (CircularBuffer.size (opaque ref))
  in
    Command gen execute [
        Require $ \State {..} (Size ref) ->
          isJust (lookup ref models)

      , Ensure $ \State {..} _ (Size ref) s -> do
          let
            Just model =
              lookup ref models
          assert $ s <= capacity model
          s === length (contents model)
      ]

------------------------------------------------------------------------

prop_circular_buffer_sequential :: Property
prop_circular_buffer_sequential =
  withTests 25 . property $ do
    actions <- forAll $
      Gen.sequential
        (Range.linear 1 100)
        initialState
        [new, get, put, size]

    executeSequential initialState actions

------------------------------------------------------------------------

main :: IO ()
main = do
  Clr.startClr
  succeeded <- checkSequential $$discover
  unless succeeded (error "There were test failures.")
