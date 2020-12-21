#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Stream.StreamD.Common
-- Copyright   : (c) 2018 Composewell Technologies
--               (c) Roman Leshchinskiy 2008-2010
--               (c) The University of Glasgow, 2009
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.StreamD.Common
     (
       FromSVarState (..)
     , fromProducer
     , fromPrimIORef
     , takeWhileM
     , takeWhile
     )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (writeIORef)
import Streamly.Internal.Data.IORef.Prim (Prim)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)

import qualified Streamly.Internal.Data.IORef.Prim as Prim
import qualified Prelude

import Prelude hiding
       ( map, mapM, mapM_, repeat, foldr, last, take, filter
       , takeWhile, drop, dropWhile, all, any, maximum, minimum, elem
       , notElem, null, head, tail, zipWith, lookup, foldr1, sequence
       , (!!), scanl, scanl1, concatMap, replicate, enumFromTo, concat
       , reverse, iterate, splitAt)
import Streamly.Internal.Data.Stream.StreamD.Type
import Streamly.Internal.Data.SVar

data FromSVarState t m a =
      FromSVarInit
    | FromSVarRead (SVar t m a)
    | FromSVarLoop (SVar t m a) [ChildEvent a]
    | FromSVarDone (SVar t m a)

{-# INLINE_NORMAL fromPrimIORef #-}
fromPrimIORef :: (MonadIO m, Prim a) => Prim.IORef a -> Stream m a
fromPrimIORef var = Stream step ()
  where
    {-# INLINE_LATE step #-}
    step _ () = liftIO (Prim.readIORef var) >>= \x -> return $ Yield x ()

-------------------------------------------------------------------------------
-- Process events received by a fold consumer from a stream producer
-------------------------------------------------------------------------------

{-# INLINE_NORMAL fromProducer #-}
fromProducer :: (MonadAsync m) => SVar t m a -> Stream m a
fromProducer svar = Stream step (FromSVarRead svar)
    where

    {-# INLINE_LATE step #-}
    step _ (FromSVarRead sv) = do
        list <- readOutputQ sv
        -- Reversing the output is important to guarantee that we process the
        -- outputs in the same order as they were generated by the constituent
        -- streams.
        return $ Skip $ FromSVarLoop sv (Prelude.reverse list)

    step _ (FromSVarLoop sv []) = return $ Skip $ FromSVarRead sv
    step _ (FromSVarLoop sv (ev : es)) = do
        case ev of
            ChildYield a -> return $ Yield a (FromSVarLoop sv es)
            ChildStop tid e -> do
                accountThread sv tid
                case e of
                    Nothing -> do
                        sendStopToProducer sv
                        return $ Skip (FromSVarDone sv)
                    Just _ -> error "Bug: fromProducer: received exception"

    step _ (FromSVarDone sv) = do
        when (svarInspectMode sv) $ do
            t <- liftIO $ getTime Monotonic
            liftIO $ writeIORef (svarStopTime (svarStats sv)) (Just t)
            liftIO $ printSVar sv "SVar Done"
        return Stop

    step _ FromSVarInit = undefined

-------------------------------------------------------------------------------
-- Filtering
-------------------------------------------------------------------------------

{-# INLINE_NORMAL takeWhileM #-}
takeWhileM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
takeWhileM f (Stream step state) = Stream step' state
  where
    {-# INLINE_LATE step' #-}
    step' gst st = do
        r <- step gst st
        case r of
            Yield x s -> do
                b <- f x
                return $ if b then Yield x s else Stop
            Skip s -> return $ Skip s
            Stop   -> return Stop

{-# INLINE takeWhile #-}
takeWhile :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
takeWhile f = takeWhileM (return . f)
