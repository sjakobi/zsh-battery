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
module Main where

import Data.List (isPrefixOf)
import qualified System.Console.ANSI as ANSI
import System.Console.ANSI (Color(..))
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))

colorize :: Color -> String -> String
colorize c = (ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid c] ++)

rightTriangle, up, down, energySymbol :: Char
rightTriangle = '▶'
up = '↑'
down = '↓'
energySymbol = 'ϟ'

powerDir ::  FilePath
powerDir = "/sys/class/power_supply/"

batteryDir :: IO FilePath
batteryDir =
  do entries <- getDirectoryContents powerDir
     let (d:_) = filter ("BAT" `isPrefixOf`) entries
     return (powerDir </> d)

barsTotal :: Int
barsTotal = 10

chargingProcessSymbol :: IO String
chargingProcessSymbol =
  do d <- batteryDir
     process <- readFile (d </> "status")
     case process of
       "Full\n" ->
         return (colorize Green [energySymbol])
       "Discharging\n" ->
         return (colorize Red [down])
       _ ->
         return (colorize Blue [up])

warningChargeLevel :: Fractional a => a
warningChargeLevel = 0.1

batteryBar :: Double -> String
batteryBar p = greenPart ++ otherPart
  where greenPart = colorize Green (replicate greens rightTriangle)
        greens = truncate (p * fromIntegral barsTotal)
        otherPart = colorize otherColor (replicate rest rightTriangle)
        rest = barsTotal - greens
        otherColor = if p < warningChargeLevel then Red else Yellow

main :: IO ()
main =
  do dir <- batteryDir
     process <- chargingProcessSymbol
     currentCharge <- fmap read (readFile (dir </> "charge_now"))
     fullCharge <- fmap read (readFile (dir </> "charge_full"))
     let bar = batteryBar (currentCharge / fullCharge)
     putStrLn (process ++ " " ++ bar)
     ANSI.setSGR [ANSI.Reset]
