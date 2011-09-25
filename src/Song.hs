{-# LANGUAGE ViewPatterns #-}

-- all hairy MIDI stuff goes here

module Song where

import System.Random.MWC.Monad -- Control.Monad.Mersenne.Random

import Haskore.Melody
import Haskore.Music.GeneralMIDI as MIDI hiding (drum)
import Haskore.Music as M
import Haskore.Basic.Duration as D
import Haskore.Composition.Drum  as Drum
import Haskore.Interface.MIDI.Render as R

import Data.List.Split(splitEvery)
import Data.Time
import System.Locale
import System.Process

import Util


data TerminalData = TD { song :: MIDI.T, channel :: Int } deriving (Show,Eq,Ord)
type MusicPiece = MIDI.T


baseSongs :: [MIDI.T]
-- baseSongs = map (MIDI.fromMelodyNullAttr MIDI.AcousticGrandPiano) cMaj
baseSongs = concat [ map (MIDI.fromMelodyNullAttr ins) (cMaj++notes) | ins <- instr ]

-- cMaj' :: [T ()]
--baseConc = line baseSongs

-- produces short, totally random note
randomNote :: RandIO TerminalData
randomNote = do
  song' <- equiprobable' baseSongs
  drum' <- equiprobable' notesDrum
  liftR (print (song',drum'))
  choices' [ (4,TD song' 1)
           , (3,TD song' 2)
           , (2,TD song' 3) 
           , (2,TD song' 4) 
           , (5,TD drum' 5)]
  
-- staccato = articulation . Staccato $ qn
-- legato   = articulation . Legato   $ qn

composeSong :: [TerminalData] -> MusicPiece
composeSong tds = let chan1 = line . map (chord . f1). splitEvery 2 . map song . Prelude.filter ((==1) . channel) $ tds
                      chan2 = line . map (chord . f2). splitEvery 2 . map song . Prelude.filter ((==2) . channel) $ tds
                      chan3 = line . map (chord . f3). splitEvery 2 . map song . Prelude.filter ((==3) . channel) $ tds
                      chan4 = line . map (chord . f4). splitEvery 2 . map song . Prelude.filter ((==4) . channel) $ tds
                      chan5 = line . map (chord . f5). splitEvery 2 . map song . Prelude.filter ((==5) . channel) $ tds

                      f1 = id

                      f2 (x:xs) = ((staccato qn x):xs)
                      f2 [] = []

                      f3 (x:y:xs) = (x:(legato qn y):xs)
                      f3 xs = xs

                      f4 (x:y:xs) = ((staccato qn y):(legato qn x):xs)
                      f4 xs = xs

                      f5 xs = f4 (Prelude.reverse xs)

                  in chord [chan1,chan2,chan3,chan4,chan5]

-- playSong :: Maybe a -> (Maybe String) -> MusicPiece -> IO ()
playSong time _graph info (changeTempo 4 -> t) = do
  let fmt = formatTimeExt time "mid"
      fmt2 = formatTimeExt time "txt"
      fmt3 = formatTimeExt time "svg"
      fmt4 = formatTimeExt time "flac"

  case info of
    Nothing -> return ()
    Just txt -> writeFile fmt2 txt

--  case graph of
--    Nothing -> return ()
--    Just gr -> viewFile fmt3 gr 

  R.fileFromGeneralMIDIMusic fmt t
-- with timidity
--  R.playTimidity t >> return ()
-- with fluidsynth
  let soundfont = "soundfont/Unison.SF2"
  rawSystem "fluidsynth" [soundfont, fmt, ("-F"++fmt4)]
  rawSystem "mplayer" [fmt4]

-- mutate note slightly

-- instruments

instrTest :: IO ()
instrTest = do
  now <- getCurrentTime
  mapM_ (\ i -> do
           print i
           let set = line $ map (MIDI.fromMelodyNullAttr i) (Prelude.take 10 notes)
           playSong now Nothing Nothing set
        ) instr

instr = [ MIDI.AcousticBass
        , MIDI.AcousticGuitarSteel
        , MIDI.ElectricGuitarJazz
        , MIDI.ElectricGuitarClean
        , MIDI.ElectricGuitarMuted
        , MIDI.MutedTrumpet
        , MIDI.MutedTrumpet
        , MIDI.MutedTrumpet
        , MIDI.MutedTrumpet
        ]

instrSet1 :: [Instrument]
instrSet1 = [ MIDI.ElectricPiano1
            , MIDI.ElectricPiano2
            , MIDI.DrawbarOrgan
            , MIDI.AcousticBass
            , MIDI.ElectricBassFinger
            , MIDI.Flute
            , MIDI.PanFlute
            , MIDI.ChoirAahs
            , MIDI.SynthVoice]

instrSet2 = [ MIDI.AcousticGuitarNylon
            , MIDI.AcousticGuitarSteel
            , MIDI.ElectricGuitarJazz
            , MIDI.ElectricGuitarClean
            , MIDI.ElectricGuitarMuted
            , MIDI.OverdrivenGuitar
            , MIDI.DistortionGuitar
            , MIDI.GuitarHarmonics
            ]

instrSet3 = [ MIDI.DistortionGuitar
            , MIDI.PanFlute
            , MIDI.OverdrivenGuitar
            , MIDI.BlownBottle
            ]


instrFull :: [Instrument]
instrFull = [ MIDI.AcousticGrandPiano
        , MIDI.BrightAcousticPiano
        , MIDI.ElectricGrandPiano
        , MIDI.HonkyTonk
        , MIDI.ElectricPiano1
        , MIDI.ElectricPiano2
        , MIDI.Harpsichord
      , MIDI.Clavinet
        , MIDI.Celesta
        , MIDI.Glockenspiel
        , MIDI.MusicBox
      , MIDI.Vibraphone
      , MIDI.Marimba
      , MIDI.Xylophone
        , MIDI.TubularBells
        , MIDI.Dulcimer
        , MIDI.DrawbarOrgan
        , MIDI.PercussiveOrgan
        , MIDI.RockOrgan
        , MIDI.ChurchOrgan
        , MIDI.ReedOrgan
        , MIDI.Accordion
        , MIDI.Harmonica
        , MIDI.TangoAccordian
        , MIDI.AcousticGuitarNylon
        , MIDI.AcousticGuitarSteel
        , MIDI.ElectricGuitarJazz
        , MIDI.ElectricGuitarClean
        , MIDI.ElectricGuitarMuted
        , MIDI.OverdrivenGuitar
        , MIDI.DistortionGuitar
        , MIDI.GuitarHarmonics
      , MIDI.AcousticBass
        , MIDI.ElectricBassFinger
        , MIDI.ElectricBassPick
        , MIDI.FretlessBass
        , MIDI.SlapBass1
        , MIDI.SlapBass2
        , MIDI.SynthBass1
        , MIDI.SynthBass2
      , MIDI.Violin
        , MIDI.Viola
        , MIDI.Cello
      , MIDI.Contrabass
        , MIDI.TremoloStrings
        , MIDI.PizzicatoStrings
        , MIDI.OrchestralHarp
        , MIDI.Timpani
        , MIDI.StringEnsemble1
        , MIDI.StringEnsemble2
        , MIDI.SynthStrings1
        , MIDI.SynthStrings2
        , MIDI.ChoirAahs
        , MIDI.VoiceOohs
        , MIDI.SynthVoice
        , MIDI.OrchestraHit
        , MIDI.Trumpet
        , MIDI.Trombone
        , MIDI.Tuba
        , MIDI.MutedTrumpet
        , MIDI.FrenchHorn
        , MIDI.BrassSection
        , MIDI.SynthBrass1
        , MIDI.SynthBrass2
        , MIDI.SopranoSax
        , MIDI.AltoSax
        , MIDI.TenorSax
        , MIDI.BaritoneSax
        , MIDI.Oboe
        , MIDI.EnglishHorn
        , MIDI.Bassoon
        , MIDI.Clarinet
      , MIDI.Piccolo
      , MIDI.Flute
        , MIDI.Recorder
        , MIDI.PanFlute
      , MIDI.BlownBottle
      , MIDI.Skakuhachi
        , MIDI.Whistle
        , MIDI.Ocarina
        ]

-- allNotes :: [Octave -> T -> attr -> T attr]
allNotes = [
  cf, c, cs,
  res rest,
  df, d, ds,
  res rest,
  ef, e, es,
  res rest,
  ff, f, fs,
  res rest,
  gf, g, gs,
  res rest,
  af, a, as,
  res rest,
  bf, b, bs,
  res rest]

res f _ dur _ = f dur

allOct = [0..2]
allDur = [wn,wn,wn,hn,hn,hn,hn,qn,qn,qn,en,en,en]

notes = [ n oct dur () | n <- allNotes, oct <- allOct, dur <- allDur]


notesDrum = [ Drum.toMusicDefaultAttr dr dur | dur <- allDur, dr <- myDrumSet ] 

myDrumSet = [ BassDrum1, AcousticBassDrum, OpenHiConga, LowMidTom ]

myDrums :: [Drum]
myDrums = [       AcousticBassDrum  -- Midi Key 35
      , BassDrum1         -- Midi Key 36
      , SideStick         -- ...
      , AcousticSnare , HandClap      , ElectricSnare , LowFloorTom
      , ClosedHiHat   , HighFloorTom  , PedalHiHat    , LowTom
      , OpenHiHat     , LowMidTom     , HiMidTom      , CrashCymbal1 ]