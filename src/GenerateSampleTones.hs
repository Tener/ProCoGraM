module Main where

import Song
import Grammar
import Util

import System.Random.MWC.Monad
import System.IO

import Data.List.Split(splitEvery)
import Data.Time
import System.Locale
import System.Process
import System.Directory
import System.FilePath
import Control.Monad

import Haskore.Interface.MIDI.Render as R
import Haskore.Music.GeneralMIDI as MIDI hiding (drum)
import Haskore.Music as M
import Haskore.Basic.Duration as D
import Haskore.Melody

midiFile = "input.mid"
soundfont = "/usr/share/soundfonts/FluidR3_GM2-2.sf2"

fileFormat = "wav"

instruments = [Piccolo, Flute, AcousticGuitarNylon, SynthVoice, DistortionGuitar, ElectricPiano1,
               BlownBottle, Celesta, ChurchOrgan, Clarinet, Glockenspiel, Ocarina, OverdrivenGuitar, PizzicatoStrings]
-- instrument = 
samplePiece allNotes allOct allDur instrument = line $ map (MIDI.fromMelodyNullAttr instrument) notes -- (Prelude.take 10 notes)
  where notes = [ n oct dur () | oct <- allOct, n <- allNotes, dur <- allDur]
        -- allNotes =
        --   [c,
        --    d,
        --    e,
        --    f,
        --    g,
        --    a,
        --    b]
        -- allNotes = [
        --   cf, c, cs,
        --   df, d, ds,
        --   ef, e, es,
        --   ff, f, fs,
        --   gf, g, gs,
        --   af, a, as,
        --   bf, b, bs]
         
        res f _ dur _ = f dur
         
        -- allOct = [0..4]
        -- allDur = [qn] -- wn,wn,wn,hn,hn,hn,hn,qn,qn,qn,en,en,en]

main :: IO ()
main = do
  forM_ instruments $ \ instrument -> do
    let allNotes =
           [c,
            d,
            e,
            f,
            g,
            a,
            b]
        dur = qn/6
    forM_ [0..4] $ \ octave -> do
      forM_ (zip allNotes [0..]) $ \ (note,noteName) -> do
        let outputFile = outputDirectory </>
                         (show instrument) ++ "_oct_" ++ (show octave) ++ "_note_" ++ (show noteName) ++ "_dur_" ++ (show dur) <.> fileFormat
            outputDirectory = "output" </> (show instrument)
            piece :: MusicPiece
            piece = samplePiece [note] [octave] [dur] instrument
        R.fileFromGeneralMIDIMusic midiFile piece
        createDirectoryIfMissing True outputDirectory
        rawSystem "fluidsynth" ["-g", "5", soundfont, midiFile, ("-F"++outputFile)]
        return ()
   
    do
      let outputFile = "output" </> (show instrument) ++ "_complete" <.> fileFormat
      R.fileFromGeneralMIDIMusic midiFile (samplePiece allNotes [0..4] [dur] instrument)
      rawSystem "fluidsynth" ["-g", "0.5", soundfont, midiFile, ("-F"++outputFile)]
   
    return ()

