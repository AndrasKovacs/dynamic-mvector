
module Data.Vector.Mutable.Dynamic(
      DynVector
    , unsafeFreeze
    , unsafeThaw
    , freeze
    , thaw
    , length
    , unsafeNew
    , new
    , unsafeReplicate
    , replicate
    , unsafeRead
    , read
    , unsafeWrite
    , write
    , clear
    , set
    , unsafeCopy
    , copy
    , unsafeMove
    , move
    , clone
    , unsafeReserve
    , reserve
    , trim
    , pushBack
    , unsafePopBack
    , popBack
    , unsafeReadBack
    , readBack
    , extend ) where


import Prelude hiding (read, length, replicate)
import Control.Monad
import Control.Monad.ST

import Control.Monad.Primitive
import Data.Primitive.MutVar

import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V



newtype DynVector s a = DynVector (MutVar s (DynVectorData s a))

data DynVectorData s a = DynVectorData {
    _size     ::  {-# UNPACK #-} !Int,
    _data     ::  {-# UNPACK #-} !(MV.MVector s a)}


-- amount of extra reserved space when creating a new vector
newReserve :: Int
newReserve = 5


freeze :: PrimMonad m => DynVector (PrimState m) a -> m (V.Vector a)
freeze (DynVector v) = do
    DynVectorData s v <- readMutVar v
    V.freeze (MV.unsafeSlice 0 s v)
{-# INLINE freeze #-}

unsafeFreeze :: PrimMonad m => DynVector (PrimState m) a -> m (V.Vector a)
unsafeFreeze (DynVector v) = do
    DynVectorData s v <- readMutVar v
    V.unsafeFreeze (MV.unsafeSlice 0 s v)
{-# INLINE unsafeFreeze #-}

thaw :: PrimMonad m => V.Vector a -> m (DynVector (PrimState m) a)
thaw v = do
    vdat <- V.thaw v
    v <- newMutVar (DynVectorData (V.length v) vdat)
    return (DynVector v)
{-# INLINE thaw #-}

unsafeThaw :: PrimMonad m => V.Vector a -> m (DynVector (PrimState m) a)
unsafeThaw v = do
    vdat <- V.unsafeThaw v
    v <- newMutVar (DynVectorData (V.length v) vdat)
    return (DynVector v)
{-# INLINE unsafeThaw #-}


length :: PrimMonad m => DynVector (PrimState m) a -> m Int
length (DynVector v) = liftM (MV.length . _data) (readMutVar v)
{-# INLINE length #-}


new :: PrimMonad m => Int -> m (DynVector (PrimState m) a)
new i = do
    v  <- MV.new (i + newReserve)
    liftM DynVector $ newMutVar (DynVectorData i v)
{-# INLINE new #-}

unsafeNew :: PrimMonad m => Int -> m (DynVector (PrimState m) a)
unsafeNew i = do
    v  <- MV.unsafeNew (i + newReserve)
    liftM DynVector $ newMutVar (DynVectorData i v)
{-# INLINE unsafeNew #-}

replicate :: PrimMonad m => Int -> a -> m (DynVector (PrimState m) a)
replicate i a = do
    v <- MV.new i
    MV.set v a  
    liftM DynVector $ newMutVar (DynVectorData i v)
{-# INLINE replicate #-}

unsafeReplicate :: PrimMonad m => Int -> a -> m (DynVector (PrimState m) a)
unsafeReplicate i a = do
    v <- MV.unsafeNew i
    MV.set v a  
    liftM DynVector $ newMutVar (DynVectorData i v)
{-# INLINE unsafeReplicate #-}

read :: PrimMonad m => DynVector (PrimState m) a -> Int -> m a
read (DynVector v) i = do
    DynVectorData s v <- readMutVar v
    if (i >= s || i < 0) then
        error "Data.Vector.Mutable.Dynamic: read: index out of bounds"
    else 
        MV.unsafeRead v i
{-# INLINE read #-}

unsafeRead :: PrimMonad m => DynVector (PrimState m) a -> Int -> m a
unsafeRead (DynVector v) i = (`MV.unsafeRead` i) . _data =<< readMutVar v
{-# INLINE unsafeRead #-}

write :: PrimMonad m => DynVector (PrimState m) a -> Int -> a -> m ()
write (DynVector v) i a = do
    DynVectorData s v <- readMutVar v
    if (i >= s || i < 0) then
        error "Data.Vector.Mutable.Dynamic: write: index out of bounds"
    else 
        MV.unsafeWrite v i a
{-# INLINE write #-}

unsafeWrite :: PrimMonad m => DynVector (PrimState m) a -> Int -> a -> m ()
unsafeWrite (DynVector v) i a = do
    v <- readMutVar v
    MV.unsafeWrite (_data v) i a
{-# INLINE unsafeWrite #-}

-- | Clear the vector of all contents, setting its length to 0.
clear :: PrimMonad m => DynVector (PrimState m) a -> m ()
clear (DynVector var) = do
    v <- MV.unsafeNew newReserve
    writeMutVar var (DynVectorData 0 v)
{-# INLINE clear #-}

-- | Set all the elements to a value. 
set :: PrimMonad m => DynVector (PrimState m) a -> a -> m ()
set (DynVector v) a = do
    DynVectorData s v <- readMutVar v
    MV.set (MV.unsafeSlice 0 s v) a
{-# INLINE set #-}

-- | Move the contents of the right vector to the left one. Inputs must have the same length and must not overlap.
copy :: PrimMonad m => DynVector (PrimState m) a -> DynVector (PrimState m) a -> m ()
copy (DynVector v1) (DynVector v2) = do
    v1 <- readMutVar v1
    v2 <- readMutVar v2
    MV.copy (_data v1) (_data v2)
{-# INLINE copy #-}

-- | Copy the contents of the right vector to the left one without checking length and overlapping. 
unsafeCopy :: PrimMonad m => DynVector (PrimState m) a -> DynVector (PrimState m) a -> m ()
unsafeCopy (DynVector v1) (DynVector v2) = do
    v1 <- readMutVar v1
    v2 <- readMutVar v2
    MV.unsafeCopy (_data v1) (_data v2)
{-# INLINE unsafeCopy #-}

-- | Move the contents of the right vector to the left one. The vectors must be the same length but may overlap.
move :: PrimMonad m => DynVector (PrimState m) a -> DynVector (PrimState m) a -> m ()
move (DynVector v1) (DynVector v2) = do
    v1 <- readMutVar v1
    v2 <- readMutVar v2
    MV.move (_data v1) (_data v2)
{-# INLINE move#-}

-- | Move the contents of the right vector to the left one. The vectors must have the same length and may overlap.
-- Input lengths are unchecked.  
unsafeMove :: PrimMonad m => DynVector (PrimState m) a -> DynVector (PrimState m) a -> m ()
unsafeMove (DynVector v1) (DynVector v2) = do
    v1 <- readMutVar v1
    v2 <- readMutVar v2
    MV.unsafeMove (_data v1) (_data v2)
{-# INLINE unsafeMove #-}

-- | Create a copy from a mutable vector. 
clone :: PrimMonad m => DynVector (PrimState m) a -> m (DynVector (PrimState m) a)
clone (DynVector v) = do
    DynVectorData s v <- readMutVar v
    v' <- MV.clone v
    var <- newMutVar (DynVectorData s v')
    return (DynVector var)
{-# INLINE clone #-}

-- | Ensure that an amount of capacity is reserved in the vector. A no-op if there is already enough capacity.
-- Throws an error if the argument is negative.
reserve :: PrimMonad m => DynVector (PrimState m) a -> Int -> m ()
reserve (DynVector v) i = do
    DynVectorData s v' <- readMutVar v
    if (i < 0) then
        error "Data.Vector.Mutable.Dynamic: reserve: negative argument"
    else if (s + i <= MV.length v') then
        return ()
    else do
        v'' <- MV.unsafeGrow v' i
        writeMutVar v (DynVectorData s v'')
{-# INLINE reserve #-}

-- | Ensure that an amount of capacity is reserved in the vector. A no-op if there is already enough capacity.
-- The argument is unchecked. 
unsafeReserve :: PrimMonad m => DynVector (PrimState m) a -> Int -> m ()
unsafeReserve (DynVector v) i = do
    DynVectorData s v' <- readMutVar v
    if (s + i <= MV.length v') then
        return ()
    else do
        v'' <- MV.unsafeGrow v' i
        writeMutVar v (DynVectorData s v'')
{-# INLINE unsafeReserve #-}

-- | Set reserved capacity to 0. 
trim :: PrimMonad m => DynVector (PrimState m) a -> m ()
trim v = unsafeReserve v 0
{-# INLINE trim #-}

-- | Increment the size of the vector and write a value to the back.
-- Pushing to a slice will potentially overwrite the original vector's elements.
pushBack :: PrimMonad m => DynVector (PrimState m) a -> a -> m ()
pushBack (DynVector v) a = do
    DynVectorData s v' <- readMutVar v
    if (s == MV.length v') then do
        v'' <- MV.unsafeGrow v' (s * 2 + 1)
        MV.unsafeWrite v'' s a
        writeMutVar v (DynVectorData (s + 1) v'')
    else do
        MV.unsafeWrite v' s a
        writeMutVar v (DynVectorData (s + 1) v')
{-# INLINE pushBack #-}
      
-- | Read the back value and remove it from the vector. Throws an error if the vector is empty. 
popBack :: PrimMonad m => DynVector (PrimState m) a -> m a 
popBack (DynVector v) = do
    DynVectorData s v' <- readMutVar v
    if (s <= 0) then
        error "Data.Vector.Mutable.Dynamic: popBack: empty vector"
    else do 
        a <- MV.unsafeRead v' (s - 1)
        when (s < quot (MV.length v') 2) $ do 
            v'' <- MV.unsafeGrow v' (s - 1)
            writeMutVar v (DynVectorData (s - 1) v'')
        return a 
{-# INLINE popBack #-}

-- | Read the back value and remove it from the vector, without checking.
unsafePopBack :: PrimMonad m => DynVector (PrimState m) a -> m a 
unsafePopBack (DynVector v) = do
    DynVectorData s v' <- readMutVar v
    a <- MV.unsafeRead v' (s - 1)
    when (s < quot (MV.length v') 2) $ do 
        v'' <- MV.unsafeGrow v' (s - 1)
        writeMutVar v (DynVectorData (s - 1) v'')
    return a 
{-# INLINE unsafePopBack #-}

-- | Read the back value.  Throws an error if the vector is empty.
readBack :: PrimMonad m => DynVector (PrimState m) a -> m a
readBack (DynVector v) = do
    DynVectorData s v <- readMutVar v
    if (s <= 0) then
        error "Data.Vector.Mutable.Dynamic: reading the back of an empty vector"
    else
        MV.unsafeRead v (MV.length v - 1)
{-# INLINE readBack #-}

-- | Read the back value without checking.
unsafeReadBack :: PrimMonad m => DynVector (PrimState m) a -> m a
unsafeReadBack (DynVector v) = do
    DynVectorData s v <- readMutVar v
    MV.unsafeRead v (MV.length v - 1)
{-# INLINE unsafeReadBack #-}

-- | Read the front value. Throws an error if the vector is empty.
readFront :: PrimMonad m => DynVector (PrimState m) a -> m a
readFront (DynVector v) = do
    DynVectorData s v <- readMutVar v
    if (s <= 0) then
        error "Data.Vector.Mutable.Dynamic: reading the front of an empty vector"
    else
        MV.unsafeRead v 0
{-# INLINE readFront #-}

-- | Read the front value without checking. 
unsafeReadFront :: PrimMonad m => DynVector (PrimState m) a -> m a
unsafeReadFront (DynVector v) = do
    DynVectorData s v <- readMutVar v
    MV.unsafeRead v 0
{-# INLINE unsafeReadFront #-}

-- | Extend the vector on the left with the elements of the vector on right.
-- | Extending a slice will potentially overwrite the original vector's elements.
extend :: PrimMonad m => DynVector (PrimState m) a -> DynVector (PrimState m) a -> m ()
extend (DynVector a) (DynVector b) = do
    DynVectorData sa va <- readMutVar a
    DynVectorData sb vb <- readMutVar b
    if (sa + sb > MV.length va) then do
        va' <- MV.unsafeGrow va (sa + sb)
        MV.unsafeCopy (MV.unsafeSlice sa sb va') (MV.unsafeSlice 0 sb vb)
        writeMutVar a (DynVectorData (sa + sb) va') 
    else do
        MV.unsafeCopy (MV.unsafeSlice sa sb va) (MV.unsafeSlice 0 sb vb)
        writeMutVar a (DynVectorData (sa + sb) va) 
{-# INLINE extend #-}

-- | Apply a function to an immutable copy of the vector.
frozen :: PrimMonad m => DynVector (PrimState m) a -> (V.Vector a -> b) -> m b
frozen v f = liftM f (freeze v)
{-# INLINE frozen #-}

-- | Apply a function to the vector recast as immutable. 
-- This is usually unsafe if we later modify the vector. 
unsafeFrozen :: PrimMonad m => DynVector (PrimState m) a -> (V.Vector a -> b) -> m b
unsafeFrozen v f = liftM f (unsafeFreeze v)
{-# INLINE unsafeFrozen #-}

