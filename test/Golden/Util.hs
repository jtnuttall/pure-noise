{-# LANGUAGE OverloadedStrings #-}

-- | Utilities for golden testing of noise functions
module Golden.Util (
  -- * Test configuration
  defaultSeeds,
  cellularSeeds,
  sliceOffsets3D,

  -- * Grid generation
  generateGrid2D,
  generateGrid3D,

  -- * Sparse sampling
  SparseTest (..),
  generateSparse2D,
  generateSparse3D,

  -- * High-level test builders
  goldenImageTest2D,
  goldenImageTest3D,
  goldenSparseTest2D,
  goldenSparseTest3D,

  -- * Convenience batch functions
  golden2DImageTests,
  golden2DSparseTests,
  golden3DImageTests,
  golden3DSparseTests,
) where

import Codec.Picture
import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LB
import Data.Massiv.Array (Array, B (..), Comp (..), Ix2 (..), Ix3, IxN (..), Sz (..))
import Data.Massiv.Array qualified as M
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Foreign.C (eNOENT)
import Foreign.C.Error (errnoToIOError)
import GHC.Generics (Generic)
import Numeric.Noise (Noise2, Noise3, Seed, noise2At, noise3At, sliceZ)
import System.Directory
import System.Exit
import System.FilePath
import System.Process.Typed qualified as PT
import Test.Tasty (TestName, TestTree, askOption)
import Test.Tasty.Golden
import Test.Tasty.Golden.Advanced (goldenTest2)

defaultSeeds :: [Seed]
defaultSeeds = [0, 42, 12345]

cellularSeeds :: [Seed]
cellularSeeds = [0, 42]

width2D, height2D :: Int
width2D = 1024
height2D = 1024

width3D, height3D :: Int
width3D = 256
height3D = 256

-- | Z-offsets for 3D slicing (fractional positions avoiding edges)
sliceOffsets3D :: [Double]
sliceOffsets3D = [0.125, 0.375, 0.625, 0.875]

data SparseTest a = SparseTest
  { coordinates :: [a]
  , expected :: a
  }
  deriving (Eq, Show, Generic)

instance (FromJSON a) => FromJSON (SparseTest a)
instance (ToJSON a) => ToJSON (SparseTest a)

-- | Generate a 2D noise grid using massiv
generateGrid2D
  :: Noise2 Double
  -> Seed
  -> Int
  -- ^ Width
  -> Int
  -- ^ Height
  -> Array B Ix2 Double
generateGrid2D noise seed width height =
  M.makeArray Par (Sz2 height width) $ \(row :. col) ->
    let x = fromIntegral col / fromIntegral width
        y = fromIntegral row / fromIntegral height
     in noise2At noise seed x y
{-# INLINE generateGrid2D #-}

-- | Generate a 3D noise grid using massiv
generateGrid3D
  :: Noise3 Double
  -> Seed
  -> Int
  -- ^ Width
  -> Int
  -- ^ Height
  -> Int
  -- ^ Depth
  -> Array B Ix3 Double
generateGrid3D noise seed width height depth =
  M.makeArray Par (Sz3 depth height width) $ \(d :> row :. col) ->
    let x = fromIntegral col / fromIntegral width
        y = fromIntegral row / fromIntegral height
        z = fromIntegral d / fromIntegral depth
     in noise3At noise seed x y z
{-# INLINE generateGrid3D #-}

-- | Convert a 2D noise grid to a grayscale PNG image
-- Maps noise values from [-1, 1] to pixel values [0, 255]
noiseGridToImage :: Array B Ix2 Double -> Image Pixel8
noiseGridToImage arr =
  let Sz2 height width = M.size arr
      getPixel x y =
        let val = arr M.! (y :. x)
            -- Map [-1, 1] to [0, 255]
            normalized = (val + 1.0) / 2.0
            clamped = max 0.0 (min 1.0 normalized)
         in round (clamped * 255.0)
   in generateImage getPixel width height
{-# INLINE noiseGridToImage #-}

-- | Strategic test points for 2D noise
-- Includes corners, edges, zero, unit values, negatives, and fractional coordinates
sparseTestPoints2D :: [(Double, Double)]
sparseTestPoints2D =
  [ -- Corners and edges
    (0.0, 0.0)
  , (1.0, 1.0)
  , (0.0, 1.0)
  , (1.0, 0.0)
  , -- Negative values
    (-1.0, -1.0)
  , (-1.0, 0.0)
  , (0.0, -1.0)
  , (-1.0, 1.0)
  , (1.0, -1.0)
  , -- Fractional coordinates
    (0.5, 0.5)
  , (0.25, 0.75)
  , (0.75, 0.25)
  , (0.1, 0.9)
  , (0.9, 0.1)
  , -- Larger values
    (10.0, 10.0)
  , (100.0, 100.0)
  , (-10.0, -10.0)
  , (5.5, 7.3)
  , (-3.2, 4.7)
  , -- Edge cases
    (1.0e-10, 1.0e-10)
  , (1.0e10, 1.0e10)
  , -- More varied points
    (2.5, 3.7)
  , (-5.2, 8.9)
  , (12.34, -56.78)
  , (0.123, 0.456)
  , (0.789, 0.321)
  , (-0.5, -0.5)
  , (0.5, -0.5)
  , (-0.5, 0.5)
  , -- Prime-like coordinates for good distribution
    (2.0, 3.0)
  , (5.0, 7.0)
  , (11.0, 13.0)
  , (17.0, 19.0)
  , (23.0, 29.0)
  , (31.0, 37.0)
  , (41.0, 43.0)
  , (47.0, 53.0)
  , -- Diagonal patterns
    (2.0, 2.0)
  , (3.0, 3.0)
  , (-2.0, -2.0)
  , -- Anti-diagonal
    (2.0, -2.0)
  , (-2.0, 2.0)
  , -- Discontinuity
    (0.999999, 0.999999)
  , (0.000001, 0.000001)
  , (0.999999999, 0.999999999)
  , (1.000000001, 1.000000001)
  , -- Powers of two
    (256.0, 256.0)
  , (1024.0, 1024.1)
  , (65536.5, 65536.5)
  , -- Large inputs
    (10000.0, 10000.0)
  , (10000.1, 10000.1)
  , -- Floating-point tomfoolery
    (1 / 0, 1.0) -- infinity
  , (-1 / 0, 1.0) -- negative infinity
  , (0 / 0, 1.0) -- NaN
  ]

-- | Strategic test points for 3D noise
sparseTestPoints3D :: [(Double, Double, Double)]
sparseTestPoints3D =
  [ -- Corners
    (0.0, 0.0, 0.0)
  , (1.0, 1.0, 1.0)
  , (0.0, 0.0, 1.0)
  , (0.0, 1.0, 0.0)
  , (1.0, 0.0, 0.0)
  , (0.0, 1.0, 1.0)
  , (1.0, 0.0, 1.0)
  , (1.0, 1.0, 0.0)
  , -- Negative corners
    (-1.0, -1.0, -1.0)
  , (-1.0, -1.0, 1.0)
  , (-1.0, 1.0, -1.0)
  , (1.0, -1.0, -1.0)
  , -- Fractional center and edges
    (0.5, 0.5, 0.5)
  , (0.25, 0.5, 0.75)
  , (0.75, 0.25, 0.5)
  , -- Larger values
    (10.0, 10.0, 10.0)
  , (100.0, 100.0, 100.0)
  , (-10.0, -10.0, -10.0)
  , -- Varied points
    (2.5, 3.7, 4.2)
  , (-5.2, 8.9, -3.1)
  , (1.23, -4.56, 7.89)
  , -- Edge cases
    (1.0e-10, 1.0e-10, 1.0e-10)
  , (1.0e10, 1.0e10, 1.0e10)
  , -- Mixed signs
    (1.0, -1.0, 1.0)
  , (-1.0, 1.0, -1.0)
  , (1.0, 1.0, -1.0)
  , (-1.0, -1.0, 1.0)
  , -- Prime-like distribution
    (2.0, 3.0, 5.0)
  , (7.0, 11.0, 13.0)
  , (17.0, 19.0, 23.0)
  ]

-- | Generate sparse test samples for 2D noise
generateSparse2D :: Noise2 Double -> Seed -> [SparseTest Double]
generateSparse2D noise seed =
  map (\(x, y) -> SparseTest [x, y] (noise2At noise seed x y)) sparseTestPoints2D

-- | Generate sparse test samples for 3D noise
generateSparse3D :: Noise3 Double -> Seed -> [SparseTest Double]
generateSparse3D noise seed =
  map (\(x, y, z) -> SparseTest [x, y, z] (noise3At noise seed x y z)) sparseTestPoints3D

-- -----------------------------------------------------------------------------
-- High-level test builders
-- -----------------------------------------------------------------------------

goldenDir :: FilePath
goldenDir = "test-data" </> "golden"

goldenImageTest :: String -> String -> (String -> IO ()) -> TestTree
goldenImageTest noiseName variant act =
  let testName = noiseName <> " " <> variant
      imageRoot = goldenDir </> "images" </> noiseName
      goldenPath = imageRoot </> variant <.> "png"
      actualPath = imageRoot </> variant <.> "actual" <.> "png"
   in goldenVsImage
        testName
        goldenPath
        actualPath
        (createDirectoryIfMissing True imageRoot >> act actualPath)

-- This is more-or-less the same as goldenVsFileDiff, but it doesn't share stdio with
-- the child process, which is pretty important because odiff doesn't have a quiet option
goldenVsImage :: TestName -> FilePath -> FilePath -> IO () -> TestTree
goldenVsImage name ref new act = askOption $ \sizeCutoff ->
  goldenTest2
    name
    throwIfDoesNotExist
    act
    (\_ _ -> runDiff sizeCutoff)
    update
    delete
 where
  throwIfDoesNotExist = do
    exists <- doesFileExist ref
    unless exists $
      ioError $
        errnoToIOError "goldenVsFileDiff" eNOENT Nothing Nothing
  runDiff
    :: SizeCutoff
    -> IO (Maybe String)
  runDiff sizeCutoff = do
    let (refName, refExt) = splitExtension ref
        proc =
          PT.proc
            "odiff"
            [ "-t"
            , "0.004" -- Tolerate roughly +/- 1 error for 8-bit grayscale.
            , "--diff-overlay"
            , "--fail-on-layout"
            , "--output-diff-lines"
            , ref
            , new
            , refName <.> "diff" <.> refExt
            ]
        procConf = PT.setStdin PT.closed proc

    (exitCode, out) <- PT.readProcessInterleaved procConf
    return $ case exitCode of
      ExitSuccess -> Nothing
      _ -> Just . LT.unpack . LT.decodeUtf8 . truncateLargeOutput sizeCutoff $ out
  truncateLargeOutput (SizeCutoff n) str =
    if LB.length str <= n
      then str
      else
        LB.take n str
          <> "<truncated>"
          <> "\nUse --accept or increase --size-cutoff to see full output."
  update _ = do
    f <- BS.readFile new
    createDirectoriesAndWriteFile ref (LB.fromStrict f)
  delete = removeFile new

goldenImageTest2D :: String -> String -> Noise2 Double -> Seed -> TestTree
goldenImageTest2D noiseName variant noise seed = goldenImageTest noiseName variant $ \path -> do
  let grid = generateGrid2D noise seed width2D height2D
      img = noiseGridToImage grid
  savePngImage path (ImageY8 img)

goldenImageTest3D :: String -> String -> Noise3 Double -> Seed -> Double -> TestTree
goldenImageTest3D noiseName variant noise seed zOffset = goldenImageTest noiseName variant $ \path -> do
  let grid2D = generateGrid2D (sliceZ zOffset noise) seed width3D height3D
      img = noiseGridToImage grid2D
  savePngImage path (ImageY8 img)

goldenSparseTest :: String -> String -> (String -> IO ()) -> TestTree
goldenSparseTest noiseName variant act =
  let testName = noiseName <> " " <> variant <> " (sparse)"
      jsonRoot = goldenDir </> "sparse" </> noiseName
      goldenPath = jsonRoot </> variant <.> "json"
      actualPath = jsonRoot </> variant <.> "actual" <.> "json"
   in goldenVsFileDiff
        testName
        (\ref new -> ["diff", "-u", ref, new])
        goldenPath
        actualPath
        (createDirectoryIfMissing True jsonRoot >> act actualPath)

goldenSparseTest2D :: String -> String -> Noise2 Double -> Seed -> TestTree
goldenSparseTest2D noiseName variant noise seed = goldenSparseTest noiseName variant $ \path -> do
  let samples = generateSparse2D noise seed
      json = encodePretty samples
  LB.writeFile path json

goldenSparseTest3D :: String -> String -> Noise3 Double -> Seed -> TestTree
goldenSparseTest3D noiseName variant noise seed = goldenSparseTest noiseName variant $ \path -> do
  let samples = generateSparse3D noise seed
      json = encodePretty samples
  LB.writeFile path json

labelBatch :: (Show a) => String -> a -> String
labelBatch dim seed = dim <> "-seed_" <> show seed

labelBatch3D :: Seed -> Int -> String
labelBatch3D seed sliceIdx = "3d-seed_" <> show seed <> "-slice_" <> show sliceIdx

golden2DImageTests :: String -> [Seed] -> Noise2 Double -> [TestTree]
golden2DImageTests noiseName seeds noise =
  [ goldenImageTest2D noiseName (labelBatch "2d" seed) noise seed
  | seed <- seeds
  ]

golden2DSparseTests :: String -> [Seed] -> Noise2 Double -> [TestTree]
golden2DSparseTests noiseName seeds noise =
  [ goldenSparseTest2D noiseName (labelBatch "2d" seed) noise seed
  | seed <- seeds
  ]

golden3DImageTests :: String -> [Seed] -> Noise3 Double -> [TestTree]
golden3DImageTests noiseName seeds noise =
  [ goldenImageTest3D noiseName (labelBatch3D seed idx) noise seed zOffset
  | seed <- seeds
  , (idx, zOffset) <- zip [0 ..] sliceOffsets3D
  ]

golden3DSparseTests :: String -> [Seed] -> Noise3 Double -> [TestTree]
golden3DSparseTests noiseName seeds noise =
  [ goldenSparseTest3D noiseName (labelBatch "3d" seed) noise seed
  | seed <- seeds
  ]
