{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A tool for examining shared disk usage across trees sharing
-- hardlinked files

module Main where

import           Control.Monad              (liftM, when)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Bits
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import qualified Filesystem                 as FS
import           Filesystem.Path.CurrentOS  (FilePath)
import qualified Filesystem.Path.CurrentOS  as FSP
import           Prelude                    hiding (FilePath)
import           System.Environment         (getArgs)
import           System.Posix.Files         (FileStatus)
import qualified System.Posix.Files         as FMM
import           System.Posix.Types         (DeviceID, FileID)

-- | Bit vector describing the set of roots holding hard links to a
-- particular file
type RootSet = Integer

-- | Parametric type for totals of various flavors, providing both
-- size and count fields
type Total k = Map k (Integer, Int)

-- | 'Analyzer' 'Monad' with no restrictions
type Analyzer a = StateT TreeState IO a

-- | 'Analyzer' 'Monad' constrained to only read-only access to the 'TreeState'
-- with no side effects
type AnalyzerRO a = Reader TreeState a

-- | 'Analyzer' 'Monad' constrained to read-only access to the 'TreeState' but
-- allowing IO
type AnalyzerROIO a = ReaderT TreeState IO a

-- | Tuple uniquely identifying a file accessible on the machine; device and inode
data FileUnique = FileUnique DeviceID FileID
                deriving (Eq, Ord, Show)

-- | Data about a file while hardlinks to it are being collected
data LinkFile = LinkFile { lfFirstName  :: FilePath
                           -- ^ First hardlink name found
                         , lfNames      :: Set FilePath
                           -- ^ Other names, with root removed
                         , lfID         :: FileUnique
                           -- ^ Device and Inode, for quick lookup
                         , lfLinksTotal :: Int
                         , lfLinksFound :: Int
                         , lfRoots      :: RootSet
                         , lfSize       :: Integer
                         } deriving (Eq, Show)

-- | State of the analyzer as it crawls through the roots
data TreeState = TreeState { tsIDRoot        :: Map FilePath Int
                             -- ^ Bit-index mapping of known roots
                           , tsFiles         :: Map FileUnique LinkFile
                             -- ^ Files with partially-discovered hardlinks
                           , tsSharedFileT   :: Total RootSet
                             -- ^ Totals for shared file data
                           , tsUnsharedDirT  :: Total FilePath
                             -- ^ Totals for unshared directories
                           , tsUnsharedFileT :: Total FilePath
                             -- ^ Totals for unshared file data
                           } deriving (Show)

-- | Combine two 'LinkFile' records, for use with 'Map.insertWith'.
combineLF :: LinkFile -> LinkFile -> LinkFile
combineLF a b =
  a { lfNames      = Set.union (lfNames a) (lfNames b)
    , lfLinksFound = (lfLinksFound a) + (lfLinksFound b)
    , lfRoots      = (lfRoots a) .|. (lfRoots b) }

-- | Generate the initial 'TreeState', consisting of empty 'Map's.
nullState :: TreeState
nullState = TreeState Map.empty Map.empty Map.empty Map.empty Map.empty

-- | Run a read-only analyzer function from a full-power one.
liftRO :: AnalyzerRO a -> Analyzer a
liftRO roOp = do
  ts <- get
  return $ runReader roOp ts

-- | Run a read-only analyzer function allowed IO from a full-power one.
liftROIO :: AnalyzerROIO a -> Analyzer a
liftROIO roIoOp = do
  ts <- get
  liftIO $ runReaderT roIoOp ts

-- | Run a read-only analyzer function from a read-only analyzer
-- function allowed IO.
liftROIORO :: AnalyzerRO a -> AnalyzerROIO a
liftROIORO roOp = do
  ts <- ask
  return $ runReader roOp ts

-- | Concisely abbreviate numbers using SI scales, using information
-- (1ki = 1024) sizing if requested.
showScale :: (Integral t, Show t) => t -> Bool -> String
showScale size bytes = findScale revScale $ fromIntegral size
  where
    scalePower :: Integer
    scalePower = if bytes then 1024 else 1000
    revScale :: [(Char, Integer)]
    revScale = reverse $ zip scales [scalePower^n | n <- [0::Integer ..]]
    findScale :: [(Char, Integer)] -> Integer -> String
    findScale [] _ = "no scale err for " ++ (show size)
    findScale ((c,s):_) n | n >= s = simpleRound n s c
    findScale (_:ss) n = findScale ss n
    scales = " kMGTPEZY"
    simpleRound :: Integer -> Integer -> Char -> String
    simpleRound n s c = fmt $ c:(reverse $ show $ n * 100 `div` s)
    fmt :: String -> String
    fmt (c:h:d:ds) = (replicate (4-length ds) ' ' ++ reverse ds)
                     ++ case (c,bytes) of
                         (' ', True)  -> "   Bytes"
                         (' ', False) -> ""
                         (_, True)    -> '.':d:h:' ':c:"iBytes"
                         (_, False)   -> '.':d:h:' ':c:[]
    fmt _ = "error in fmt for " ++ (show size)

-- | Emit a LinkFile for identifying unresolved hardlinks.
printLF :: LinkFile -> AnalyzerROIO ()
printLF lf = do
  rs <- liftROIORO $ fromRootSet $ lfRoots lf
  liftIO $ do
    putStrLn $ "ID: " ++ (show $ lfFirstName lf)
    putStrLn $ "    Roots: " ++ (show $ Set.toList rs)
    putStrLn $ "    Names: " ++ (show $ Set.toList $ lfNames lf)
    putStrLn $ "    Found " ++ (show $ lfLinksFound lf)
      ++ " of " ++ (show $ lfLinksTotal lf) ++ " links"

-- | Describe a 'Total' using a label.
showTotal :: String -> (Integer, Int) -> String
showTotal label (s, c) =
  "  " ++ label ++ " count: " ++ (showScale c False)
  ++ "\n  "++ label ++ "  size: " ++ (showScale s True)

-- | Emit the statistics for a single root.
printRoot :: (FilePath, Int) -> AnalyzerROIO ()
printRoot (rootP, rootID) = do
  ts <- ask
  let rootMask = (bit rootID) :: Integer in liftIO $ do
    putStrLn $ "Root: " ++ (show rootP)
    putStrLn $ showTotal "unshared file" $ lookupFP $ tsUnsharedFileT ts
    putStrLn $ showTotal "unshared  dir" $ lookupFP $ tsUnsharedDirT ts
    putStrLn $ showTotal "  shared file" $ lookupRS rootMask $ tsSharedFileT ts
  where
    lookupFP m = Map.findWithDefault (0,0) rootP m
    lookupRS :: Integer -> (Total RootSet) -> (Integer, Int)
    lookupRS mask m = Map.foldr addTotal (0,0)
                 $ Map.filterWithKey (\ k _ -> (k .&. mask) /= 0) m

-- | Emit the total statistics.
grandTotal :: AnalyzerROIO ()
grandTotal = do
  ts <- ask
  liftIO $ do
    putStrLn "Grand total:"
    putStrLn $ showTotal "unshared file" $ gTot $ tsUnsharedFileT ts
    putStrLn $ showTotal "unshared  dir" $ gTot $ tsUnsharedDirT ts
    putStrLn $ showTotal "  shared file" $ gTot $ tsSharedFileT ts
  where
    gTot = (foldr addTotal (0,0)) . Map.elems

-- | Convert a 'Set' of 'FilePath's into the more concise 'RootSet'
-- form; an Integer used as a bit-vector.
toRootSet :: Set FilePath -> AnalyzerRO RootSet
toRootSet sFP = do
  rootMap <- asks tsIDRoot
  return $ foldr (.|.) 0 $ map (bit . (look rootMap)) $ Set.toList sFP
  where
    look m k = Map.findWithDefault 0 k m

-- | Convert a 'RootSet' back into a more readable 'Set' of 'FilePath's.
fromRootSet :: RootSet -> AnalyzerRO (Set FilePath)
fromRootSet rs = do
  rootMap <- asks tsIDRoot
  return
    $ Set.fromList
    $ map snd
    $ filter (\ (mask, _) -> rs .&. mask /= 0)
    $ map (\ (k, v) -> (bit v, k))
    $ Map.toList rootMap

-- | Round the size of a file or directory up to the nearest full
-- block size.  This makes an unfortunate assumption that the
-- filesystem block size is 4 kiB, as 'FileStatus' does not
-- currently export the block size.
blockedSize :: FileStatus -> Integer
blockedSize s = (fsize + blockMask) .&. (complement blockMask)
  where
    blockBits = 12 -- assume 4096 byte blocks, ugh
    blockS = 1 `shift` blockBits
    blockMask = blockS - 1
    fsize = fromIntegral $ FMM.fileSize s

-- | Add two 'Total' records.
addTotal :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addTotal (aS, aC) (bS, bC) = (aS+bS, aC+bC)

-- | Increment a 'Total' record, for use with 'Map.alter'.
incTotal :: (Num a, Num b) => (a, b) -> Maybe (a, b) -> Maybe (a, b)
incTotal n Nothing = Just n
incTotal n (Just o) = Just (n `addTotal` o)

-- | Increment the unshared totals for a file or directory.  This does
-- not check that the file has no external hardlinks.
incrementTotals :: FilePath -> FileStatus -> Analyzer ()
incrementTotals useRoot fs = do
  when (FMM.isDirectory fs) $
    modify $ \ s -> s { tsUnsharedDirT = updateT (tsUnsharedDirT s) }
  when (not $ FMM.isDirectory fs) $ do
    modify $ \ s -> s { tsUnsharedFileT = updateT (tsUnsharedFileT s) }
  where
    bs = blockedSize fs
    updateT = Map.alter (incTotal (bs, 1)) useRoot

-- | Increment the shared totals for a file.  This will send
-- hardlinked files where all links are contained within a single root
-- to the unshared 'incrementTotals' with the appropriate root
-- 'FilePath'.
incrementSharedTotal :: LinkFile -> Analyzer ()
incrementSharedTotal lf =
  if popCount (lfRoots lf) == 1
  then do
    lfS <- liftIO $ FMM.getSymbolicLinkStatus $ FSP.encodeString $ lfFirstName lf
    rs <- liftRO $ fromRootSet $ lfRoots lf
    case Set.toList rs of
     [r] -> incrementTotals r lfS
     _ -> error "error in incrementSharedTotal rootset decode"
  else
    modify $ \ s -> s { tsSharedFileT = updateT (tsSharedFileT s)
                     , tsFiles = Map.delete (lfID lf) (tsFiles s) }
    where
      updateT = Map.alter (incTotal ((lfSize lf), 1)) (lfRoots lf)

-- | Remove the root from a path, creating a relative path.
stripRoot :: FilePath -> FilePath -> FilePath
stripRoot r p =
  case FSP.stripPrefix r p of
   Just newP -> newP
   Nothing -> FSP.decodeString $ (FSP.encodeString r)++":"++(FSP.encodeString p)

-- | Generate a new 'LinkFile' record for a newly discovered file or
-- link.
newLinkFile :: FilePath -> FilePath -> FileStatus -> AnalyzerRO LinkFile
newLinkFile rootP newP newS = do
  rs <- toRootSet $ Set.singleton rootP
  return LinkFile { lfFirstName  = newP
                  , lfNames      = Set.singleton $ stripRoot rootP newP
                  , lfID         = FileUnique (FMM.deviceID newS) (FMM.fileID newS)
                  , lfLinksTotal = fromIntegral $ FMM.linkCount newS
                  , lfLinksFound = 1
                  , lfRoots      = rs
                  , lfSize       = blockedSize newS }

-- | Recursively add a tree of files.
addTree :: FilePath -> FilePath -> Analyzer ()
addTree rootP newP = do
  newS <- liftIO $ FMM.getSymbolicLinkStatus $ FSP.encodeString newP
  case ((FMM.fileMode newS) .&. FMM.fileTypeModes, FMM.linkCount newS) of
   (m, _) | m == FMM.directoryMode -> do
     incrementTotals rootP newS
     fps <- liftIO $ FS.listDirectory newP
     mapM_ (addTree rootP) fps
   (_, 1) -> incrementTotals rootP newS
   _ -> do
     lf <- liftRO $ newLinkFile rootP newP newS
     modify $ \ s -> s { tsFiles = Map.insertWith combineLF (lfID lf) lf
                                  $ tsFiles s }
     tsF <- gets tsFiles
     let lf' = Map.findWithDefault lf (lfID lf) tsF in
      when (lfLinksFound lf' == lfLinksTotal lf') $ incrementSharedTotal lf'

-- | Add a new root, allocating a new root id for it and recursively
-- analyze its contents.
addRoot :: FilePath -> Analyzer ()
addRoot newRoot = do
  canRoot <- liftIO $ FS.canonicalizePath $ newRoot FSP.</> "."
  newID <- liftM (Map.size) $ gets tsIDRoot
  modify $ \ s -> s { tsIDRoot = Map.insert canRoot newID $ tsIDRoot s }
  addTree canRoot canRoot

-- | Check the 'TreeState' for hardlinked files with unmatched links.
finalizeTree :: Analyzer ()
finalizeTree = do
  files <- gets tsFiles
  when (not $ null files) $ do
    liftIO $ putStr $ (show $ length files) ++ " files with unmatched links:\n"
    liftROIO $ mapM_ printLF files
    mapM_ incrementSharedTotal $ Map.elems files
    modify $ \ s -> s { tsFiles = Map.empty }

-- | Emit a terse summary of file sharing statistics.
summarize :: AnalyzerROIO ()
summarize = do
  ts <- ask
  mapM_ printRoot $ Map.toList $ tsIDRoot ts
  grandTotal

-- | Analyze a series of roots, check for unmatched hardlinks, and
-- display statistics.
doRoots :: [FilePath] -> Analyzer ()
doRoots rs = do
  mapM_ addRoot rs
  finalizeTree
  liftROIO $ summarize

-- | Invoke the 'Analyzer' 'Monad' over a series of roots.
calculateShared :: [FilePath] -> IO ()
calculateShared rs = evalStateT (doRoots rs) nullState

-- | Treat arguments as roots to analyze.
main :: IO ()
main = do
  args <- getArgs
  calculateShared $ map FSP.decodeString args
