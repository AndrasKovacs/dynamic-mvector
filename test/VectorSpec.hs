{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VectorSpec (spec) where

import Prelude hiding (read, length, replicate, null)
import qualified Prelude as P

import Data.Vector.Mutable.Dynamic
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.QuickCheck
import Control.Monad (forM_)

spec :: Spec
spec = do
  describe "Dynamic MVector" $ do

    describe "Initialization" $ do
      it "new creates a vector with correct initial length" $ do
        v <- new 10 :: IO (IOVector Int)
        len <- length v
        len `shouldBe` 10

      it "new reserves extra capacity (newReserve = 5)" $ do
        v <- new 10 :: IO (IOVector Int)
        cap <- capacity v
        cap `shouldBe` 15  -- 10 + 5

      it "replicate creates a vector with repeated values" $ do
        v <- replicate 5 42 :: IO (IOVector Int)
        len <- length v
        len `shouldBe` 5
        vals <- mapM (read v) [0..4]
        vals `shouldBe` [42, 42, 42, 42, 42]

    describe "Length and Capacity" $ do
      it "length returns the number of elements" $ do
        v <- new 10 :: IO (IOVector Int)
        len <- length v
        len `shouldBe` 10

      it "capacity returns reserved space" $ do
        v <- new 10 :: IO (IOVector Int)
        cap <- capacity v
        cap `shouldBe` 15

      it "null detects empty vectors" $ do
        v <- new 0 :: IO (IOVector Int)
        isEmpty <- null v
        isEmpty `shouldBe` True

      it "null detects non-empty vectors" $ do
        v <- new 5 :: IO (IOVector Int)
        isEmpty <- null v
        isEmpty `shouldBe` False

    describe "Reading and Writing" $ do
      it "write and read work correctly" $ do
        v <- new 5 :: IO (IOVector Int)
        write v 0 10
        write v 1 20
        write v 2 30
        val0 <- read v 0
        val1 <- read v 1
        val2 <- read v 2
        [val0, val1, val2] `shouldBe` [10, 20, 30]

      it "unsafeRead matches read" $ do
        v <- replicate 3 99 :: IO (IOVector Int)
        val1 <- read v 1
        val2 <- unsafeRead v 1
        val1 `shouldBe` val2

      it "readFront reads the first element" $ do
        v <- replicate 3 42 :: IO (IOVector Int)
        write v 0 100
        val <- readFront v
        val `shouldBe` 100

      it "readBack reads the last element" $ do
        v <- replicate 3 42 :: IO (IOVector Int)
        write v 2 200
        val <- readBack v
        val `shouldBe` 200

      it "set changes all elements" $ do
        v <- new 3 :: IO (IOVector Int)
        set v 77
        vals <- mapM (read v) [0..2]
        vals `shouldBe` [77, 77, 77]

    describe "Clear and Reserve" $ do
      it "clear sets length to 0" $ do
        v <- replicate 5 42 :: IO (IOVector Int)
        clear v
        len <- length v
        len `shouldBe` 0

      it "reserve increases capacity" $ do
        v <- new 5 :: IO (IOVector Int)
        reserve v 20
        cap <- capacity v
        cap `shouldSatisfy` (>= 20)

      it "reserve is a no-op if capacity sufficient" $ do
        v <- new 20 :: IO (IOVector Int)
        cap1 <- capacity v
        reserve v 10
        cap2 <- capacity v
        cap2 `shouldBe` cap1

    describe "Push and Pop Operations" $ do
      it "pushBack increases length and adds element starting from empty" $ do
        -- Start with length 0
        v <- new 0 :: IO (IOVector Int)
        pushBack v 10
        pushBack v 20
        pushBack v 30
        len <- length v
        len `shouldBe` 3
        vals <- mapM (read v) [0..2]
        vals `shouldBe` [10, 20, 30]

      it "pushBack works within reserved capacity" $ do
        v <- new 0 :: IO (IOVector Int)
        initialCap <- capacity v
        -- Push within the reserved capacity
        forM_ [0..pred initialCap] $ \i -> do
          pushBack v i
          cap <- capacity v
          cap `shouldBe` initialCap  -- Should not reallocate

      it "pushBack triggers reallocation when exceeding capacity" $ do
        v <- new 0 :: IO (IOVector Int)
        clear v
        initialCap <- capacity v  -- Should be 5 (0 + 5)
        initialCap `shouldBe` 5

        -- Fill to capacity
        forM_ [0..4] $ \i -> pushBack v i
        cap1 <- capacity v
        cap1 `shouldBe` 5

        -- Push one more, should trigger reallocation
        -- grow by (s + 1) = (5 + 1) = 6
        pushBack v 5
        cap2 <- capacity v
        cap2 `shouldBe` 11  -- 5 + 6

      it "popBack removes and returns last element" $ do
        v <- new 0 :: IO (IOVector Int)
        pushBack v 10
        pushBack v 20
        pushBack v 30
        val <- popBack v
        val `shouldBe` 30
        len <- length v
        len `shouldBe` 2

      it "multiple popBack operations work correctly" $ do
        v <- new 0 :: IO (IOVector Int)
        forM_ [1..5] $ \i -> pushBack v i
        val1 <- popBack v
        val2 <- popBack v
        val3 <- popBack v
        [val1, val2, val3] `shouldBe` [5, 4, 3]
        len <- length v
        len `shouldBe` 2

    describe "Extend" $ do
      it "extend concatenates two vectors" $ do
        v1 <- new 0 :: IO (IOVector Int)
        v2 <- new 0 :: IO (IOVector Int)
        forM_ [1..3] $ \i -> pushBack v1 i
        forM_ [4..6] $ \i -> pushBack v2 i
        extend v1 v2
        len <- length v1
        len `shouldBe` 6
        vals <- mapM (read v1) [0..5]
        vals `shouldBe` [1, 2, 3, 4, 5, 6]

      it "extend works when exceeding capacity" $ do
        v1 <- replicate 2 10 :: IO (IOVector Int)
        v2 <- replicate 5 20 :: IO (IOVector Int)
        extend v1 v2
        len <- length v1
        len `shouldBe` 7

    describe "Freeze and Thaw" $ do
      it "freeze creates immutable copy" $ do
        v <- new 0 :: IO (IOVector Int)
        forM_ [1..5] $ \i -> pushBack v i
        imm <- freeze v
        V.toList imm `shouldBe` [1, 2, 3, 4, 5]

      it "thaw creates mutable copy from immutable" $ do
        let imm = V.fromList [10, 20, 30]
        v <- thaw imm :: IO (IOVector Int)
        len <- length v
        len `shouldBe` 3
        vals <- mapM (read v) [0..2]
        vals `shouldBe` [10, 20, 30]

    describe "Clone and Copy" $ do
      it "clone creates independent copy" $ do
        v1 <- new 0 :: IO (IOVector Int)
        forM_ [1..3] $ \i -> pushBack v1 i
        v2 <- clone v1
        write v1 0 100
        val1 <- read v1 0
        val2 <- read v2 0
        val1 `shouldBe` 100
        val2 `shouldBe` 1

    describe "Property-based tests" $ do
      prop "pushBack and popBack are inverses" $ \(x :: Int) -> do
        v <- new 0 :: IO (IOVector Int)
        pushBack v x
        result <- popBack v
        len <- length v
        len `shouldBe` 0
        result `shouldBe` x

      prop "length after n pushBacks is n" $ \(xs :: [Int]) -> do
        let n = P.length xs
        v <- new 0 :: IO (IOVector Int)
        forM_ xs $ \x -> pushBack v x
        len <- length v
        len `shouldBe` n

      prop "replicate creates correct values" $ \(n' :: Int) (val :: Int) -> do
        let n = abs n' `mod` 100  -- Limit size for performance
        v <- replicate n val :: IO (IOVector Int)
        if n > 0 then do
          vals <- mapM (read v) [0..n-1]
          all (== val) vals `shouldBe` True
        else
          return ()

    describe "Comprehensive capacity tests" $ do
      it "maintains capacity invariants across operations" $ do
        forM_ [1..28] $ \desiredCap -> do
          v <- new desiredCap :: IO (IOVector Int)

          -- Check initial state
          len <- length v
          len `shouldBe` desiredCap
          actualCap <- capacity v
          actualCap `shouldBe` (desiredCap + 5)  -- newReserve = 5

          -- Clear reallocates with newReserve capacity
          clear v
          lenAfterClear <- length v
          lenAfterClear `shouldBe` 0
          capAfterClear <- capacity v
          capAfterClear `shouldBe` 5  -- clear creates new vector with newReserve = 5

          -- Push within the new capacity (5 elements)
          let pushCount = min desiredCap 5
          forM_ [0..pred pushCount] $ \val -> do
            pushBack v val
            curCap <- capacity v
            curCap `shouldBe` 5

          -- Verify values
          forM_ [0..pred pushCount] $ \idx -> do
            val <- read v idx
            val `shouldBe` idx
