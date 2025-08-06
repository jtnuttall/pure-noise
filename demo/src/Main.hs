{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main (main) where

import Control.Lens hiding (universe)
import Control.Monad.Trans.Resource
import Data.Massiv.Array qualified as MA
import Data.Massiv.Array.Unsafe qualified as MAU
import Data.StateVar qualified as StateVar
import Data.Text qualified as T
import Data.Vector qualified as V
import DearImGui qualified as ImGui
import DearImGui.OpenGL3 qualified as ImGui
import DearImGui.Raw qualified as ImGuiRaw
import DearImGui.SDL qualified as ImGui
import DearImGui.SDL.OpenGL qualified as ImGui
import GHC.Clock (getMonotonicTimeNSec)
import Graphics.GL
import Linear (V2 (V2), _x, _y)
import Numeric.Noise qualified as Noise
import Relude
import SDL qualified
import Text.RawString.QQ
import Text.Show qualified
import UnliftIO (withRunInIO)
import UnliftIO.Exception (throwString)
import UnliftIO.Foreign

type NoiseImg = MA.Array MA.S MA.Ix2 Float

data AppState = AppState
  { _windowSize :: V2 Int
  , _deltaTime :: DeltaTime
  , _fullscreenShaderProgram :: GLuint
  , _fullscreenTexture :: Maybe GLuint
  , _noiseTextureLocation :: GLint
  , asInputState :: UiState
  , asNoiseConfig :: NoiseConfig
  }

newtype UiState = UiState
  { _leftMBDown :: Bool
  }

initInputState :: UiState
initInputState =
  UiState
    { _leftMBDown = False
    }

data NoiseConfig = NoiseConfig
  { _dirty :: Bool
  , _clamped :: Bool
  , _seed :: Noise.Seed
  , _noiseType :: NoiseType
  , _noiseOffset :: V2 Float
  , _frequency :: Float
  , _cellularConfig :: Noise.CellularConfig Float
  , ncFractalConfig :: UiFractalConfig
  }

defaultNoiseConfig :: NoiseConfig
defaultNoiseConfig =
  NoiseConfig
    { _dirty = True
    , _clamped = True
    , _seed = 1337
    , _noiseType = Perlin
    , _noiseOffset = V2 0.7 0.7
    , _frequency = 0.01
    , _cellularConfig = Noise.defaultCellularConfig
    , ncFractalConfig = defaultFractalConfig
    }

data UiFractalConfig = UiFractalConfig
  { _fractalEnabled :: Bool
  , _fractalType :: FractalType
  , _pingPongStrength :: Noise.PingPongStrength Float
  , _fractalConfig :: Noise.FractalConfig Float
  }

defaultFractalConfig :: UiFractalConfig
defaultFractalConfig =
  UiFractalConfig
    { _fractalEnabled = False
    , _fractalType = FBM
    , _pingPongStrength = Noise.defaultPingPongStrength
    , _fractalConfig = Noise.defaultFractalConfig
    }

data FractalType
  = FBM
  | Ridged
  | Billow
  | PingPong
  deriving (Eq, Ord, Enum, Bounded)

instance Show FractalType where
  show = \case
    FBM -> "fBm"
    Ridged -> "Ridged"
    Billow -> "Billow"
    PingPong -> "Ping Pong"

data NoiseType
  = Perlin
  | OpenSimplex2
  | SuperSimplex
  | Cellular
  | Value
  | ValueCubic
  | PerlinPlusSuperSimplex
  | ValuePlusOpenSimplex2
  deriving (Eq, Ord, Enum, Bounded)

instance Show NoiseType where
  show = \case
    Perlin -> "Perlin"
    OpenSimplex2 -> "OpenSimplex2"
    SuperSimplex -> "SuperSimplex (OpenSimplex2S)"
    Cellular -> "Cellular (Worley)"
    Value -> "Value"
    ValueCubic -> "Value Cubic"
    PerlinPlusSuperSimplex -> "Perlin + SuperSimplex (see README)"
    ValuePlusOpenSimplex2 -> "Value + OpenSimplex2"

data DeltaTime = DeltaTime
  { previousMono :: Double
  , currentMono :: Double
  }
  deriving (Show)

makeClassy ''AppState

makeClassy ''UiState

instance HasUiState AppState where
  uiState = lens asInputState (\x y -> x{asInputState = y})
  {-# INLINE uiState #-}

makeClassy ''NoiseConfig

instance HasNoiseConfig AppState where
  noiseConfig = lens asNoiseConfig (\x y -> x{asNoiseConfig = y})
  {-# INLINE noiseConfig #-}

makeClassy ''UiFractalConfig

instance HasUiFractalConfig NoiseConfig where
  uiFractalConfig = lens ncFractalConfig (\x y -> x{ncFractalConfig = y})
  {-# INLINE uiFractalConfig #-}

instance HasUiFractalConfig AppState where
  uiFractalConfig = noiseConfig . uiFractalConfig
  {-# INLINE uiFractalConfig #-}

fractalOctaves :: (HasUiFractalConfig c, Functor f) => (Int -> f Int) -> c -> f c
fractalOctaves = fractalConfig . lens Noise.octaves (\x y -> x{Noise.octaves = y})

fractalLacunarity :: (HasUiFractalConfig c, Functor f) => (Float -> f Float) -> c -> f c
fractalLacunarity = fractalConfig . lens Noise.lacunarity (\x y -> x{Noise.lacunarity = y})

fractalGain :: (HasUiFractalConfig c, Functor f) => (Float -> f Float) -> c -> f c
fractalGain = fractalConfig . lens Noise.gain (\x y -> x{Noise.gain = y})

fractalWeightedStrength :: (HasUiFractalConfig c, Functor f) => (Float -> f Float) -> c -> f c
fractalWeightedStrength = fractalConfig . lens Noise.weightedStrength (\x y -> x{Noise.weightedStrength = y})

cellularDistanceFn
  :: (HasNoiseConfig c, Functor f) => (Noise.CellularDistanceFn -> f Noise.CellularDistanceFn) -> c -> f c
cellularDistanceFn = cellularConfig . lens Noise.cellularDistanceFn (\x y -> x{Noise.cellularDistanceFn = y})

cellularResult :: (HasNoiseConfig c, Functor f) => (Noise.CellularResult -> f Noise.CellularResult) -> c -> f c
cellularResult = cellularConfig . lens Noise.cellularResult (\x y -> x{Noise.cellularResult = y})

cellularJitter :: (HasNoiseConfig c, Functor f) => (Float -> f Float) -> c -> f c
cellularJitter = cellularConfig . lens Noise.cellularJitter (\x y -> x{Noise.cellularJitter = y})

getMonotonicTimeSec :: (MonadIO m) => m Double
getMonotonicTimeSec = (/ 1e+9) . fromIntegral <$> liftIO getMonotonicTimeNSec

initDT :: (MonadIO f) => f DeltaTime
initDT = DeltaTime <$> getMonotonicTimeSec <*> getMonotonicTimeSec

getDT :: (HasAppState s, MonadState s m) => m Double
getDT = do
  DeltaTime{..} <- use deltaTime
  pure (currentMono - previousMono)

getFPS :: (HasAppState s, MonadState s m) => m Double
getFPS = do
  dt <- getDT
  pure (1 / dt)

tick :: (MonadIO m, HasAppState s, MonadState s m) => m ()
tick = do
  currentDt <- getMonotonicTimeSec
  deltaTime
    %= ( \DeltaTime{..} ->
          DeltaTime
            { previousMono = currentMono
            , currentMono = currentDt
            }
       )

newtype AppT m a = AppT (ReaderT (IORef AppState) m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadTrans, MonadResource)

instance (MonadIO m) => MonadState AppState (AppT m) where
  get = AppT $ ask >>= readIORef
  put conf = AppT do
    ref <- ask
    writeIORef ref conf

runAppT :: (MonadIO m) => AppT m a -> AppState -> m a
runAppT (AppT act) conf = do
  ref <- newIORef conf
  runReaderT act ref

scale :: (HasAppState p, HasNoiseConfig p) => p -> V2 Float
scale config =
  let freq = config ^. frequency
      V2 w h = config ^. windowSize
      x = fromIntegral w / freq / fromIntegral h
      y = fromIntegral w / freq / fromIntegral h
   in V2 (1 / x) (1 / y)

createNoiseImage :: (HasAppState c, HasNoiseConfig c, HasUiFractalConfig c) => c -> NoiseImg
createNoiseImage config =
  let noiseF = noiseFrom config
      s = config ^. seed
      V2 w h = config ^. windowSize
      offX = config ^. noiseOffset . _y
      offY = config ^. noiseOffset . _x
      V2 xScale yScale = scale config
   in MA.makeArray MA.Par (MA.Sz2 h w) \(i MA.:. j) ->
        let x = fromIntegral i - fromIntegral h / 2
            y = fromIntegral j - fromIntegral w / 2
            noise = Noise.noise2At noiseF s (x * xScale + offX) (y * yScale + offY)
         in (noise + 1) / 2

noiseFrom :: (HasNoiseConfig c, HasUiFractalConfig c) => c -> Noise.Noise2 Float
noiseFrom config = clamp (fractal noise)
 where
  clamp
    | config ^. clamped = Noise.clamp2 (-1) 1
    | otherwise = id

  noise = case config ^. noiseType of
    Perlin -> Noise.perlin2
    OpenSimplex2 -> Noise.openSimplex2
    SuperSimplex -> Noise.superSimplex2
    Cellular -> Noise.cellular2 (config ^. cellularConfig)
    Value -> Noise.value2
    ValueCubic -> Noise.valueCubic2
    PerlinPlusSuperSimplex -> (Noise.superSimplex2 + Noise.perlin2) / 2
    ValuePlusOpenSimplex2 -> (Noise.value2 + Noise.openSimplex2) / 2

  fractal
    | config ^. fractalEnabled =
        let conf = config ^. fractalConfig
         in case config ^. fractalType of
              FBM -> Noise.fractal2 conf
              Ridged -> Noise.ridged2 conf
              Billow -> Noise.billow2 conf
              PingPong -> Noise.pingPong2 conf (config ^. pingPongStrength)
    | otherwise = id

renderNoise :: (MonadUnliftIO m) => AppT m ()
renderNoise = do
  glUseProgram =<< use fullscreenShaderProgram
  writeNoise
  emptyVAO <- liftIO $ alloca \emptyVAOPtr -> do
    glGenVertexArrays 1 emptyVAOPtr
    peek emptyVAOPtr
  glBindVertexArray emptyVAO
  glDrawArrays GL_TRIANGLES 0 3
  glBindTexture GL_TEXTURE_2D 0
 where
  writeNoise = do
    V2 w h <- fmap fromIntegral <$> use windowSize
    mNoiseTex <- use fullscreenTexture
    case mNoiseTex of
      Just noiseTex -> do
        glBindTexture GL_TEXTURE_2D noiseTex
      Nothing -> do
        noiseTex <- alloca \ptr -> do
          glGenTextures 1 ptr
          liftIO $ peek ptr
        glBindTexture GL_TEXTURE_2D noiseTex
        glTexParameterf GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_LINEAR)
        glDisable GL_DEPTH
        fullscreenTexture .= Just noiseTex

    mImage <- bool (pure Nothing) (Just . createNoiseImage <$> get) =<< use dirty
    case mImage of
      Just image -> do
        MAU.unsafeWithPtr image $
          glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_LUMINANCE) w h 0 GL_LUMINANCE GL_FLOAT
        glActiveTexture GL_TEXTURE0
        noiseLoc <- use noiseTextureLocation
        glUniform1i noiseLoc 0
        dirty .= False
      Nothing ->
        pure ()

noiseDash
  :: (MonadUnliftIO m) => AppT m ()
noiseDash = dashWin do
  fps <- getFPS
  ImGui.labelText "FPS" (show $ round @Double @Int fps)

  noiseConfigTab
 where
  dashWin act = do
    V2 w h <- fmap fromIntegral <$> use windowSize
    ImGui.setNextWindowSize (pure @IO $ ImGui.ImVec2 (max (w * 0.20) 200) h) ImGui.ImGuiCond_Appearing
    ImGui.setNextWindowPos (pure @IO (ImGui.ImVec2 0 0)) ImGui.ImGuiCond_Appearing Nothing
    ImGui.withWindowOpen "Noise Config" act

  noiseConfigTab = do
    shouldReset <- ImGui.button "Reset"
    when shouldReset do
      noiseConfig .= defaultNoiseConfig
      dirty .= True

    clampedSV <- mkSVFor clamped
    _ <- ImGui.checkbox "clamp" clampedSV

    seedSV <- mkSVFor seed
    _ <-
      ImGui.dragScalar
        "Seed"
        ImGui.ImGuiDataType_U64
        seedSV
        0.05
        (StateVar.makeGettableStateVar (pure minBound))
        (StateVar.makeGettableStateVar (pure maxBound))
        "%u"
        ImGui.ImGuiSliderFlags_WrapAround

    _ <- dragFloat "frequency" frequency 0.001 (-0.5) 0.5 "%.5f"

    _ <- combo "noise function" noiseType

    offsetSV <- mkSVFor (noiseOffset . v2tuple)
    _ <- ImGui.dragFloat2 "offset" offsetSV 0.1 (-10) 10

    noiseFn <- use noiseType
    case noiseFn of
      Cellular -> do
        _ <- dragFloat "cellular jitter" cellularJitter 0.005 (-1) 1 "%.4f"
        _ <- combo "distance fn" cellularDistanceFn
        _ <- combo "cellular result" cellularResult
        pure ()
      _ -> pure ()

    fractalEnabledSV <- mkSVFor fractalEnabled
    _ <- ImGui.checkbox "fractal" fractalEnabledSV
    withEnabled fractalEnabledSV do
      _ <- combo "fractal type" fractalType

      octavesSV <- mkSVFor fractalOctaves
      _ <- ImGui.dragInt "octaves" octavesSV 0.05 1 24
      _ <- dragFloat "weighted strength" fractalWeightedStrength 0.001 (-5) 5 "%.4f"
      _ <- dragFloat "lacunarity" fractalLacunarity 0.001 0 7 "%.4f"
      _ <- dragFloat "gain" fractalGain 0.0001 (-1.5) 1.5 "%.5f"

      ty <- use fractalType
      withDisabledPure (ty /= PingPong) do
        _ <- dragFloat "ping pong strength" (pingPongStrength . coerced) 0.001 (-5) 5 "%.5f"
        pure ()

v2tuple :: Iso' (V2 a) (a, a)
v2tuple = iso (\(V2 x y) -> (x, y)) (uncurry V2)

withEnabled :: forall ref a m. (StateVar.HasGetter ref Bool, MonadUnliftIO m) => ref -> m a -> m a
withEnabled sv = ImGui.withDisabled (not <$> StateVar.get @ref @Bool @IO sv)

withDisabledPure :: (MonadUnliftIO m) => Bool -> m a -> m a
withDisabledPure p = ImGui.withDisabled (pure @IO p)

-- | So we can change the format precision
dragFloat
  :: (MonadState s m, HasNoiseConfig s, MonadUnliftIO m)
  => Text
  -> Lens' s Float
  -> Float
  -> Float
  -> Float
  -> Text
  -> m Bool
dragFloat d l speed minV maxV fmt = do
  v <- CFloat <$> use l
  with v \vPtr -> do
    changed <- withCString (T.unpack d) \dPtr ->
      withCString (T.unpack fmt) $
        ImGuiRaw.dragFloat dPtr vPtr (CFloat speed) (CFloat minV) (CFloat maxV)
    when changed do
      (CFloat v') <- liftIO (peek vPtr)
      l .= v'
      dirty .= True
    pure changed

mkSVFor :: (MonadUnliftIO m, HasNoiseConfig s, MonadState s m) => Lens' s b -> m (StateVar.StateVar b)
mkSVFor l = withRunInIO \run ->
  let g = run (use l)
      s x = run do
        l .= x
        dirty .= True
   in pure (StateVar.makeStateVar g s)

combo
  :: forall a s m
   . (MonadUnliftIO m, HasNoiseConfig s, MonadState s m, Show a, Eq a, Enum a, Bounded a)
  => Text
  -> Lens' s a
  -> m Bool
combo lbl l = do
  let vs = V.fromList (universe @a)
      asEnum = iso (\v -> fromMaybe 0 . V.findIndex (== v) $ vs) (\i -> fromMaybe (vs V.! 0) (vs V.!? i))
  sv <- mkSVFor (l . asEnum)
  ImGui.combo lbl sv (show <$> toList vs)

mainLoop :: (MonadResource m, MonadUnliftIO m) => SDL.Window -> AppT m ()
mainLoop window = processEventsUntilQuit do
  tick
  ImGui.openGL3NewFrame
  ImGui.sdl2NewFrame
  ImGui.newFrame

  noiseDash

  glClear GL_COLOR_BUFFER_BIT
  renderNoise
  ImGui.render
  ImGui.openGL3RenderDrawData =<< ImGui.getDrawData
  SDL.glSwapWindow window
  mainLoop window
 where
  processEventsUntilQuit act = do
    continue <- processEvents
    when continue act

  processEvents = ImGui.pollEventsWithImGui >>= go . map SDL.eventPayload
   where
    go [] = pure True
    go (SDL.QuitEvent : _) = pure False
    go (e : es) = do
      case e of
        SDL.WindowResizedEvent (SDL.WindowResizedEventData{..}) -> do
          windowSize .= (fromIntegral <$> windowResizedEventSize)
          let V2 w h = windowResizedEventSize
          glViewport 0 0 w h
          glLoadIdentity
          dirty .= True
        evt -> handleInput evt
      go es

    handleInput evt = unlessCaptured case evt of
      SDL.MouseButtonEvent (SDL.MouseButtonEventData{..}) -> whenCurrentWindow mouseButtonEventWindow do
        case mouseButtonEventButton of
          SDL.ButtonLeft ->
            leftMBDown .= (mouseButtonEventMotion == SDL.Pressed)
          _ -> pure ()
      SDL.MouseMotionEvent (SDL.MouseMotionEventData{..}) -> do
        active <- use leftMBDown
        when active do
          let V2 rx ry = fromIntegral <$> mouseMotionEventRelMotion
          moveImage (V2 (-rx) ry)
      SDL.MouseWheelEvent (SDL.MouseWheelEventData{..}) -> do
        freq <- use frequency
        let frequencyScale = 0.05 * freq
            V2 _ zoomY = fromIntegral <$> mouseWheelEventPos
        frequency -= (frequencyScale * zoomY)
        dirty .= True
      _ -> pure ()

  moveImage by = do
    config <- use appState
    noiseOffset += by * scale config
    dirty .= True

  unlessCaptured act = do
    capM <- ImGui.wantCaptureMouse
    capKB <- ImGui.wantCaptureKeyboard
    unless (capM || capKB) act

  whenCurrentWindow Nothing _ = pure ()
  whenCurrentWindow (Just evtWindow) act = when (evtWindow == window) act

initShaderProgram :: (MonadUnliftIO m) => m GLuint
initShaderProgram = do
  vertShader <- glCreateShader GL_VERTEX_SHADER
  compShader "vertex" vertShader vertShaderSrc
  fragShader <- glCreateShader GL_FRAGMENT_SHADER
  compShader "fragment" fragShader fragShaderSrc

  shaderProgramId <- glCreateProgram
  glAttachShader shaderProgramId vertShader
  glAttachShader shaderProgramId fragShader

  let linkMsg = "Failed to link shader program"
  throwOnGlFalse (glGetShaderInfoLog shaderProgramId) linkMsg do
    glLinkProgram shaderProgramId
    alloca \ptr -> do
      glGetProgramiv shaderProgramId GL_LINK_STATUS ptr
      fromIntegral <$> liftIO (peek ptr)

  glDeleteShader vertShader
  glDeleteShader fragShader
  pure shaderProgramId
 where
  compShader st sid srcS =
    withCStringLen srcS \(src, len) ->
      with src \linesPtr ->
        with (fromIntegral len) \lengthsPtr -> do
          let msg = "Failed to compile " <> st <> " shader"
          throwOnGlFalse (glGetShaderInfoLog sid) msg do
            glShaderSource sid 1 linesPtr lengthsPtr
            glCompileShader sid
            alloca \ptr -> do
              glGetShaderiv sid GL_COMPILE_STATUS ptr
              fromIntegral <$> liftIO (peek ptr)

  vertShaderSrc =
    [r|
      #version 330
      out vec2 texcoords;
      void main() {
          vec2 vertices[3]=vec2[3](vec2(-1,-1), vec2(3,-1), vec2(-1, 3));
          gl_Position = vec4(vertices[gl_VertexID],0,1);
          texcoords = 0.5 * gl_Position.xy + vec2(0.5);
      }
    |]
  fragShaderSrc =
    [r|
      #version 330
      in vec2 texcoords;
      out vec4 color;
      
      uniform sampler2D noiseTex;
      void main() {
        color = texture(noiseTex, texcoords);
      }
    |]

throwOnGlFalse
  :: (MonadUnliftIO m)
  => (GLsizei -> Ptr GLsizei -> Ptr GLchar -> m ())
  -> String
  -> m GLboolean
  -> m ()
throwOnGlFalse getInfoLog msg act = do
  success <- act
  when (success == GL_FALSE) do
    let len = 512
    log <-
      alloca \resultPtr ->
        allocaArray len \logPtr -> do
          getInfoLog len resultPtr logPtr
          resLen <- fromIntegral <$> liftIO (peek resultPtr)
          map (toEnum @Char . fromEnum) <$> peekArray resLen logPtr
    throwString $ msg <> ": " <> log

main :: IO ()
main = runResourceT do
  SDL.initializeAll
  let wWidth = 1920
      wHeight = 1080
      initialSize = V2 wWidth wHeight
  (_, window) <- do
    let title = "Noise Demo"
        config =
          SDL.defaultWindow
            { SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
            , SDL.windowInitialSize = initialSize
            , SDL.windowPosition = SDL.Centered
            , SDL.windowResizable = True
            }
    allocate (SDL.createWindow title config) SDL.destroyWindow
  (_, glContext) <- allocate (SDL.glCreateContext window) SDL.glDeleteContext
  _ <- allocate ImGui.createContext ImGui.destroyContext
  _ <- allocate_ (ImGui.sdl2InitForOpenGL window glContext) ImGui.sdl2Shutdown
  _ <- allocate_ ImGui.openGL3Init ImGui.openGL3Shutdown

  _fullscreenShaderProgram <- initShaderProgram
  _noiseTextureLocation <- withCString "noiseText" (glGetUniformLocation _fullscreenShaderProgram)
  _deltaTime <- initDT

  runAppT
    (mainLoop window)
    AppState
      { _windowSize = initialSize
      , _deltaTime
      , _fullscreenShaderProgram
      , _fullscreenTexture = Nothing
      , _noiseTextureLocation
      , asInputState = initInputState
      , asNoiseConfig = defaultNoiseConfig
      }