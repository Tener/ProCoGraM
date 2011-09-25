module Main where

import Song
import Grammar
import Util

import System.Random.MWC.Monad
import System.IO

import Data.Time
import System.Locale

main :: IO ()
main = do
  -- instrTest -- playSong Nothing (composeSong baseSongs)
  -- hPrint stderr baseSongs 
  go mutStep emptyGrammar

mutStep :: Integer
mutStep = 250

go :: Integer -> Grammar -> IO ()
go 0 gr = do
  gr' <- runWithSystemRandom (mut gr)
--  num <- runWithSystemRandom ((uniformR (0,1000000)) :: RandIO Int)

  now <- getCurrentTime
  graphToDot (formatTimeExt now "dot") gr'
  piece <- generateMusicPiece' gr' songClean
  
  hPrint stdout ("Piece",piece)
  hPrint stdout ("Graph",gr')
  hPrint stderr (length baseSongs, length instr)
  playSong now (Just gr) (Just $ show gr') piece
  go mutStep gr'

go n gr = do
  gr' <- runWithSystemRandom (mut gr)
  print gr'
  go (n-1) gr'
