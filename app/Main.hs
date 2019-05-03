module Main where

import           App                      (mkApplication)
import           Network.Wai.Handler.Warp (run)

main :: IO ()
main = mkApplication >>= run 8080
