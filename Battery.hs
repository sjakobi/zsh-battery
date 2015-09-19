{-
 -  Copyright (c) 2010 Mats Rauhala <mats.rauhala@gmail.com>
 -
 -  Permission is hereby granted, free of charge, to any person
 -  obtaining a copy of this software and associated documentation
 -  files (the "Software"), to deal in the Software without
 -  restriction, including without limitation the rights to use,
 -  copy, modify, merge, publish, distribute, sublicense, and/or sell
 -  copies of the Software, and to permit persons to whom the
 -  Software is furnished to do so, subject to the following
 -  conditions:
 -
 -  The above copyright notice and this permission notice shall be
 -  included in all copies or substantial portions of the Software.
 -
 -  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 -  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 -  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 -  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 -  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 -  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 -  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 -  OTHER DEALINGS IN THE SOFTWARE.
 -}
module Main (main) where

import Control.Monad.Error
import System.Console.ANSI
import System.IO
import Symbols
import Files

setColor c = setSGR [SetColor Foreground Vivid c]

barstotal = 10

charging' =
  do setColor Blue
     putChar up
discharging' =
  do setColor Red
     putChar down
full' =
  do setColor Green
     putChar koppa

charging :: String -> IO ()
charging "Charging\n" = charging'
charging "Full\n" = full'
charging _	    = discharging'

warning = 0.1 -- 10%

percent :: Double -> Double -> Double
percent o n = n / o

warn :: Double -> Bool
warn = (>) warning

bar :: Double -> IO ()
bar p =
  do setColor Green
     putStr (replicate greens barsymbol)
     setColor yellow
     putStr (replicate yellows barsymbol)
  where greens = truncate (p * fromIntegral barstotal) :: Int
        yellows = barstotal - greens :: Int
        yellow = if warn p then Red else Yellow

printBar =
  do f <- fmap read $ readFileM =<< full
     c <- fmap read $ readFileM =<< charge
     return $ bar (percent f c)

main :: IO ()
main = do
  fillLevel <- runErrorT $ readFileM =<< full
  case fillLevel of
    Right a ->
      charging a
    Left x ->
      hPutStrLn stderr x
  hFlush stdout
  bar <- runErrorT printBar
  case bar of
    Right x ->
      do x
         putStrLn ""
    Left x -> hPutStrLn stderr x
