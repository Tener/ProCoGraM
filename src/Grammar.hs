{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}

module Grammar where

import Song
import Util

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Map(Map)
import System.Random.MWC.Monad
import Data.Maybe(catMaybes)
import Text.Printf
import System.FilePath

type NonTermIdent = Int

data Symbol = NonTerm NonTermIdent | Term TerminalData deriving (Show,Eq,Ord)
type Grammar = Map Int [(Double,Song)]
type Song = [Symbol]

start :: Symbol
start = NonTerm 0
songClean :: Song
songClean = cycle [start]

emptyGrammar :: Grammar
emptyGrammar = Map.singleton 0 [(1,[])]

produce :: Grammar -> NonTermIdent -> RandIO Song
produce gr nterm = case Map.lookup nterm gr of
                     Nothing -> return []
                     Just [] -> error "WTF? []"
                     Just xs -> choices' xs

-- choose :: (Show a) => Double -> [(Double,a)] -> a
-- choose thr xs = let aux acc [] = error ("something is wrong. erronous data:" ++ show (thr,xs,acc,sum (map fst xs)))
--                     aux acc ((p,v):rest) = let p' = acc+p in if thr < p' then v else aux p' rest
--                 in aux 0 xs

nextTerminal :: Grammar -> Song -> RandIO (Maybe (TerminalData,Song))
nextTerminal gr song = let go 0 _ = return Nothing
                           go _ [] = error "nextTerminal: empty song."
                           go n (s:ss) = 
                               case s of
                                 Term td -> return $ Just (td,ss)
                                 NonTerm tid -> do
                                        ss' <- produce gr tid
                                        go (n-1) (ss' ++ ss)
                         in go 100000 song

generateMusicPiece :: Int -> Grammar -> Song -> RandIO MusicPiece
generateMusicPiece count gr song0 = let go 0 acc _song = return $ composeSong (reverse acc)
                                        go n acc song = do
                                         nte <- nextTerminal gr song
                                         case nte of
                                           Just (td,song') -> go (n-1) (td:acc) song'
                                           Nothing -> go 0 acc song
                                    in
                                      go count [] song0

generateMusicPiece' :: Grammar -> Song -> IO MusicPiece
generateMusicPiece' gr s = runWithSystemRandom $ generateMusicPiece 900 gr s

-- mutate the grammar definition. options:
-- 1. adjust probabilities 
-- 2. choose random production, modify results (right hand side)
-- 3. create new production. Possible results: [], [note], [existing non-terminal], mix of above
-- 4. create new non-terminal
mut1, mut2, mut3, mut4 :: Grammar -> RandIO Grammar

cleanupGrammar :: Grammar -> Grammar
cleanupGrammar = cleanupBranches . pruneGrammar

mut :: Grammar -> Rand IO Grammar
mut gr = choices [(50,mut1 gr)
                 ,(20,mut2 gr)
                 ,(10,mut3 gr)
                 ,(30,mut4 gr)
                 ,(30,mut =<< mut gr)
                 ,(1,return (cleanupGrammar gr))]

randomProduction :: Grammar -> RandIO Song
randomProduction gr = do
  choices [(5,(return []))
          ,(2,(do
                n <- randomNote
                return ([Term n])))
          ,(5,(do
                n <- equiprobable' (Map.keys gr)
                return ([NonTerm n])))
          ,(1,(do
                e1 <- randomProduction gr
                e2 <- randomProduction gr
                return (e1++e2)
              ))]

-- 1. adjust probabilities 
mut1 gr = do
  (k,v) <- equiprobable' (Map.assocs gr)
  let l = length v
  if l < 2 then return gr else do
   i1 <- uniformR (0,l-1)
   i2 <- uniformR (0,l-2)
   let (v0,(e0:v1)) = splitAt i1 v
       (v2,(e1:v3)) = splitAt i2 (v0++v1)
       p0'Old = fst e0
       p1'Old = fst e1
       p0'Val = snd e0
       p1'Val = snd e1
       pTotal = p0'Old + p1'Old

   pNew' <- uniformR (0, pTotal)
   let pNew = if pNew' == 0 then pTotal / 2 else pNew'
       p0'New = pNew
       p1'New = pTotal - pNew
       e0' = (p0'New,p0'Val)
       e1' = (p1'New,p1'Val)

   return $ Map.adjust (const ([e0',e1'] ++ v2 ++ v3)) k gr

-- 2. choose random production, modify results (right hand side)
mut2 gr = do
  let deleteRandomElement [] = return []
      deleteRandomElement xs = do
            let l = length xs
            del <- uniformR (0,l-1)
            let (pre,(_:post)) = splitAt del xs
            return (pre++post)
 
      insertRandomProduction [] = randomProduction gr
      insertRandomProduction xs = do
            prod <- randomProduction gr
            let l = length xs
            ins <- uniformR (0,l-1)
            let (pre,post) = splitAt ins xs
            return (pre++prod++post)

      appendRandomProduction xs = do
            prod <- randomProduction gr
            return (xs++prod)

      prependRandomProduction xs = do
            prod <- randomProduction gr
            return (prod++xs)

      reverseAppend xs = do
            return (reverse xs++xs)

      reverseL xs = do
            return (reverse xs)

      shiftChannel xs = do
            d <- uniformR (-1,1)
            let f = (\ t@(channel -> c) -> if c == 5 then t else t { channel = (c+d) `mod` 4 })
                g h (Term td) = (Term (h td))
                g _ nt = nt
            return $! map (g f) xs

      modif (p,v) = do
            v' <- choices [ (4, deleteRandomElement v)
                          , (4, insertRandomProduction v)
                          , (4, appendRandomProduction v)
                          , (4, prependRandomProduction v)
                          , (4, shiftChannel v)
                          , (1, reverseL v)
                          , (1, reverseAppend v)]
            return $ Just (p,v')

  modifyRandomKeyElement gr modif

--  (k,v) <- equiprobable' (Map.assocs gr)
--  v' <- 
--  return $ Map.adjust (const v) k gr


-- 3. create new production for existing non-terminal. Possible results: [], [note], [existing non-terminal], mix of above
-- mut3 :: Grammar -> RandIO Grammar
mut3 gr = do
 -- modifyRandomKey gr ($ \ (v :: [(Double,Song)]) -> do
 --           (prod :: Song) <- randomProduction gr
 --           (v' :: [(Double,Song)]) <- modifyRandomElementList v (\ (p :: Double,e :: Song) -> do
 --                                                                   p0 <- uniformR (0,p)
 --                                                                   let p1 = p - p0
 --                                                                   return [(p0,e),(p1,prod)])
 --           return $ Just v')

 
  (k,v) <- equiprobable' (Map.assocs gr)
  val1 <- randomProduction gr
  let l = length v
  i <- uniformR (0,l-1)
  let (v0,((p0,val0):v1)) = splitAt i v
  p1' <- uniformR (0,p0)
  let p1 = if p1' == 0 then p0 / 2 else p1'
  return $ Map.adjust (const ([(p1,val1),((p0-p1),val0)] ++ v0 ++ v1)) k gr
 

-- 4. create new non-terminal
mut4 gr = do
  pr1 <- randomProduction gr
  pr2 <- randomProduction gr
  p1 <- uniformR (0.5,0.9)
  
  let p2 = 1 - p1
      val = [(p1,pr1),(p2,pr2)]
      
  let k = length (Map.assocs gr) -- keys are 0-based

  return $ Map.insert k val gr

-- merge identical productions, summing their probabilities
cleanupBranches :: Grammar -> Grammar
cleanupBranches gr = let swap (x,y) = (y,x) in
                     Map.map (\ xs -> [ let p = sum (map fst ys) in (p,snd (head ys)) 
                                            | ys <- List.groupBy (\ (snd -> a) (snd -> b) -> a == b) . map swap . List.sort . map swap $ xs] ) gr

-- delete branches unless they can be reached
pruneGrammar :: Grammar -> Grammar
pruneGrammar gr = let walk accSet toV | Set.null toV = accSet
                                      | otherwise = let (x,xs) = Set.deleteFindMin toV
                                                        nonterminals = catMaybes . map f . concat . map snd $ Map.findWithDefault [] x gr
                                                        f (NonTerm nt) = Just nt
                                                        f _ = Nothing
                                                        unvisited = Set.difference (Set.fromList nonterminals) accSet
                                                    in walk (Set.insert x accSet) (Set.union unvisited xs)
                      visited = walk Set.empty (Set.singleton 0)
                      allKeys = Map.keysSet gr
                      toDelete = Set.difference allKeys visited
                  in List.foldl' (\ gr' e -> Map.delete e gr') gr (Set.toList toDelete)

graphToDot :: String -> Grammar -> IO ()
graphToDot fn ({- cleanupGrammar -> -} gr) = do
{-

digraph %s {
  ratio = "auto" ;

  "NT#%d" [ shape=box ] ;
  "prod#%d#%d" [ shape=circle, weight=%f ] ;
  "prod#%d#%d" -> "NT#%d" ;
  "prod#%d#%d" -> "NT#%d" ;
  "prod#%d#%d" -> "NT#%d" ;

  "T#%d: %s" [ shape=diamond ] ;

-}
  let terminalsAll = concatMap (catMaybes . concatMap (map f . snd)) $ Map.elems gr
      f (NonTerm _) = Nothing
      f (Term td) = Just td
  
      terminals :: [(Int,TerminalData)]
      terminals = zip [0..] . Set.toList . Set.fromList $ terminalsAll

      terminals' :: [TerminalData]
      terminals' = map snd terminals

      enumTerminal :: TerminalData -> Int
      enumTerminal td = head . List.elemIndices td $ terminals'

  print terminals
  
  let prefix = ["digraph " ++ (takeBaseName fn)++ "{",
                "ratio = \"auto\" ;",
                ""]

               ++
               
               ["## " ++ show nt | nt <- Map.keys gr] 
               
  
      prodName :: Symbol -> String
      prodName (NonTerm nt) = printf "\"NT#%d\"" nt
      prodName (Term td) = printf "\"T#%d\"" (enumTerminal td)

      nonTerminalsDot = concat [ ["",printf "\"NT#%d\" [ shape=box,style=filled,fillcolor=yellow ] ;" nonTerm] 
                              ++ [ printf "\"NT#%d\" -> \"prod#%d#%d\" ;" nonTerm nonTerm pix | pix <- [0 .. length prods-1]]
                              ++ concat [ ["", printf "\"prod#%d#%d\" [ shape=circle,weight=%f,style=filled,fillcolor=blue ] ;" nonTerm (pix :: Int) w]
                                        ++[printf "\"prod#%d#%d\" -> %s ;" nonTerm pix (prodName pr) | pr <- (products :: Song) ]      
                                              | (pix,(w,products)) <- zip ([0..] :: [Int]) prods]       
                                     | (nonTerm, prods) <- (Map.assocs (gr :: Grammar)) ]
                                  
      terminalsDot = [ printf "\"T#%d\" [ shape=diamond,style=filled,fillcolor=green ] ;" i {- (show t) -} | (i,_t) <- zip (map enumTerminal terminals') terminals' ]
      suffix = ["", "}"]
      
      allDot = unlines $ prefix ++ nonTerminalsDot ++ terminalsDot ++ suffix
--   let fmt = printf "/tmp/foo_%s.dot" fn
  writeFile fn allDot
