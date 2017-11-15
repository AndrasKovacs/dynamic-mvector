{-# LANGUAGE DeriveDataTypeable #-}

-- | A wrapper around MVector that enables pushing, popping and extending.

module Data.Vector.Mutable.Dynamic
    {-# DEPRECATED "Use the more generic Data.Vector.Generic.Mutable.Dynamic instead" #-}
    (
      MVector, STVector, IOVector,
      -- * Initialization
      new, replicate, unsafeNew, unsafeReplicate,

      -- * Accessing
      read, write, readFront, readBack,
      unsafeRead, unsafeWrite, unsafeReadFront, unsafeReadBack, set,

      -- * Conversion
      freeze, thaw, frozen, unsafeFreeze, unsafeThaw, unsafeFrozen,

      -- * Length information
      length, null, capacity,

      -- * Copying
      clone, copy, move, unsafeCopy, unsafeMove,

      -- * Modification
      clear, reserve, unsafeReserve, trim, pushBack, popBack, unsafePopBack, extend
      ) where


import Prelude hiding (read, length, replicate, null)
import Data.Data (Typeable)

import Control.Monad
import Control.Monad.ST

import Control.Monad.Primitive
import Data.Primitive.MutVar

import qualified Data.Vector.Generic.Mutable.Dynamic as GDMV
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V

type MVector s a = GDMV.MVector V.Vector s a

type IOVector = GDMV.IOVector V.Vector
type STVector = GDMV.STVector V.Vector

freeze :: PrimMonad m => MVector (PrimState m) a -> m (V.Vector a)
freeze = GDMV.freeze
{-# INLINABLE freeze #-}

unsafeFreeze :: PrimMonad m => MVector (PrimState m) a -> m (V.Vector a)
unsafeFreeze = GDMV.unsafeFreeze
{-# INLINABLE unsafeFreeze #-}

thaw :: PrimMonad m => V.Vector a -> m (MVector (PrimState m) a)
thaw = GDMV.thaw
{-# INLINABLE thaw #-}

unsafeThaw :: PrimMonad m => V.Vector a -> m (MVector (PrimState m) a)
unsafeThaw = GDMV.unsafeThaw
{-# INLINABLE unsafeThaw #-}

length :: PrimMonad m => MVector (PrimState m) a -> m Int
length = GDMV.length
{-# INLINABLE length #-}

capacity :: PrimMonad m => MVector (PrimState m) a -> m Int
capacity = GDMV.capacity
{-# INLINABLE capacity #-}

null :: PrimMonad m => MVector (PrimState m) a -> m Bool
null = GDMV.null
{-# INLINABLE null #-}

new :: PrimMonad m => Int -> m (MVector (PrimState m) a)
new = GDMV.new
{-# INLINABLE new #-}

unsafeNew :: PrimMonad m => Int -> m (MVector (PrimState m) a)
unsafeNew = GDMV.unsafeNew
{-# INLINABLE unsafeNew #-}

replicate :: PrimMonad m => Int -> a -> m (MVector (PrimState m) a)
replicate = GDMV.replicate
{-# INLINABLE replicate #-}

unsafeReplicate :: PrimMonad m => Int -> a -> m (MVector (PrimState m) a)
unsafeReplicate = GDMV.unsafeReplicate
{-# INLINABLE unsafeReplicate #-}

read :: PrimMonad m => MVector (PrimState m) a -> Int -> m a
read = GDMV.read
{-# INLINABLE read #-}

unsafeRead :: PrimMonad m => MVector (PrimState m) a -> Int -> m a
unsafeRead = GDMV.unsafeRead
{-# INLINABLE unsafeRead #-}

write :: PrimMonad m => MVector (PrimState m) a -> Int -> a -> m ()
write = GDMV.write
{-# INLINABLE write #-}

unsafeWrite :: PrimMonad m => MVector (PrimState m) a -> Int -> a -> m ()
unsafeWrite = GDMV.unsafeWrite
{-# INLINABLE unsafeWrite #-}

clear :: PrimMonad m => MVector (PrimState m) a -> m ()
clear = GDMV.clear
{-# INLINABLE clear #-}

set :: PrimMonad m => MVector (PrimState m) a -> a -> m ()
set = GDMV.set
{-# INLINABLE set #-}

copy :: PrimMonad m => MVector (PrimState m) a -> MVector (PrimState m) a -> m ()
copy = GDMV.copy
{-# INLINABLE copy #-}

unsafeCopy :: PrimMonad m => MVector (PrimState m) a -> MVector (PrimState m) a -> m ()
unsafeCopy = GDMV.unsafeCopy
{-# INLINABLE unsafeCopy #-}

move :: PrimMonad m => MVector (PrimState m) a -> MVector (PrimState m) a -> m ()
move = GDMV.move
{-# INLINABLE move #-}

unsafeMove :: PrimMonad m => MVector (PrimState m) a -> MVector (PrimState m) a -> m ()
unsafeMove = GDMV.unsafeMove
{-# INLINABLE unsafeMove #-}

clone :: PrimMonad m => MVector (PrimState m) a -> m (MVector (PrimState m) a)
clone = GDMV.clone
{-# INLINABLE clone #-}

reserve :: PrimMonad m => MVector (PrimState m) a -> Int -> m ()
reserve = GDMV.reserve
{-# INLINABLE reserve #-}

unsafeReserve :: PrimMonad m => MVector (PrimState m) a -> Int -> m ()
unsafeReserve = GDMV.unsafeReserve
{-# INLINABLE unsafeReserve #-}

trim :: PrimMonad m => MVector (PrimState m) a -> m ()
trim = GDMV.trim
{-# INLINABLE trim #-}

pushBack :: PrimMonad m => MVector (PrimState m) a -> a -> m ()
pushBack = GDMV.pushBack
{-# INLINABLE pushBack #-}


popBack :: PrimMonad m => MVector (PrimState m) a -> m a
popBack = GDMV.popBack
{-# INLINABLE popBack #-}

unsafePopBack :: PrimMonad m => MVector (PrimState m) a -> m a
unsafePopBack = GDMV.unsafePopBack
{-# INLINABLE unsafePopBack #-}

readBack :: PrimMonad m => MVector (PrimState m) a -> m a
readBack = GDMV.readBack
{-# INLINABLE readBack #-}

unsafeReadBack :: PrimMonad m => MVector (PrimState m) a -> m a
unsafeReadBack = GDMV.unsafeReadBack
{-# INLINABLE unsafeReadBack #-}

readFront :: PrimMonad m => MVector (PrimState m) a -> m a
readFront = GDMV.readFront
{-# INLINABLE readFront #-}

unsafeReadFront :: PrimMonad m => MVector (PrimState m) a -> m a
unsafeReadFront = GDMV.unsafeReadFront
{-# INLINABLE unsafeReadFront #-}

extend :: PrimMonad m => MVector (PrimState m) a -> MVector (PrimState m) a -> m ()
extend = GDMV.extend
{-# INLINABLE extend #-}

frozen :: PrimMonad m => MVector (PrimState m) a -> (V.Vector a -> b) -> m b
frozen = GDMV.frozen
{-# INLINABLE frozen #-}

unsafeFrozen :: PrimMonad m => MVector (PrimState m) a -> (V.Vector a -> b) -> m b
unsafeFrozen = GDMV.unsafeFrozen
{-# INLINABLE unsafeFrozen #-}
