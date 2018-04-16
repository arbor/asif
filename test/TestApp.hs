{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TestApp
where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           System.IO.Unsafe

newtype TestApp a = TestApp
  { unTestApp :: IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)


runTestApp :: TestApp a -> IO a
runTestApp = unTestApp

unsafeRunTest :: TestApp a -> a
unsafeRunTest = unsafePerformIO . runTestApp
{-# NOINLINE unsafeRunTest #-}
