{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_streamly (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "streamly"
version :: Version
version = Version [0,10,1] []

synopsis :: String
synopsis = "Streaming, dataflow programming and declarative concurrency"
copyright :: String
copyright = "2017 Composewell Technologies"
homepage :: String
homepage = "https://streamly.composewell.com"
