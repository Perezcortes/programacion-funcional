module Main where

import System.IO
import Text.Printf (printf)
import Text.Read (readMaybe)
import Control.Monad (when)

-- ====================================
-- TIPOS Y CONFIGURACIÓN
-- ====================================

data PlaneState = PlaneState 
    { posX :: !Float
    , posY :: !Float
    , angle :: !Float
    , speed :: !Float
    } deriving (Show)

data Input = Input 
    { rotInput :: !Float
    , thrustInput :: !Float
    } deriving (Show)

data PhysicsConfig = PhysicsConfig
    { maxSpeed :: Float
    , minSpeed :: Float
    , acceleration :: Float
    , deceleration :: Float
    , brakeForce :: Float
    , turnSpeed :: Float
    , friction :: Float
    , worldWidth :: Float
    , worldHeight :: Float
    } deriving (Show)

-- Configuración por defecto
defaultConfig :: PhysicsConfig
defaultConfig = PhysicsConfig
    { maxSpeed = 8.0
    , minSpeed = 0.0
    , acceleration = 0.25
    , deceleration = 0.08
    , brakeForce = 0.15
    , turnSpeed = 4.0
    , friction = 0.02
    , worldWidth = 800.0
    , worldHeight = 600.0
    }

-- ====================================
-- FÍSICA MEJORADA
-- ====================================

updatePhysics :: PhysicsConfig -> PlaneState -> Input -> PlaneState
updatePhysics config state input = PlaneState newX newY newAngle newSpeed
  where
    -- 1. ROTACIÓN (con inercia angular)
    targetRotation = rotInput input * turnSpeed config
    -- La rotación es más efectiva a mayor velocidad
    speedFactor = min 1.0 (speed state / maxSpeed config + 0.3)
    effectiveRotation = targetRotation * speedFactor
    newAngle = normalizeAngle (angle state + effectiveRotation)

    -- 2. ACELERACIÓN/FRENADO
    thrust = thrustInput input
    rawSpeed = speed state
    
    -- Acelerar
    accelSpeed = if thrust > 0.1
                 then min (maxSpeed config) (rawSpeed + acceleration config * thrust)
                 else rawSpeed
    
    -- Frenar activamente
    brakeSpeed = if thrust < -0.1
                then max (minSpeed config) (accelSpeed - brakeForce config)
                else accelSpeed
    
    -- Fricción natural (siempre presente)
    newSpeed = max (minSpeed config) (brakeSpeed - friction config)

    -- 3. MOVIMIENTO (Trigonometría con wrapping)
    rad = newAngle * pi / 180.0
    deltaX = cos rad * newSpeed
    deltaY = sin rad * newSpeed
    
    newX = wrapCoordinate (posX state + deltaX) (worldWidth config)
    newY = wrapCoordinate (posY state + deltaY) (worldHeight config)

-- Normaliza ángulos entre 0-360
normalizeAngle :: Float -> Float
normalizeAngle a 
    | a < 0 = normalizeAngle (a + 360)
    | a >= 360 = normalizeAngle (a - 360)
    | otherwise = a

-- Wrap coordenadas (teletransporte en bordes)
wrapCoordinate :: Float -> Float -> Float --Si sales por la derecha, apareces por la izquierda.
wrapCoordinate val limit
    | val > limit = 0
    | val < 0 = limit
    | otherwise = val

-- ====================================
-- PREDICCIÓN DE COLISIONES (NUEVO)
-- ====================================

-- Predice la posición futura del avión después de N pasos
predictPosition :: PhysicsConfig -> PlaneState -> Input -> Int -> PlaneState
predictPosition config state input steps = iterate' (updatePhysics config ?? input) state !! steps
  where
    (??) f x y = f y x
    iterate' f x = x : iterate' f (f x)

-- Verifica si habrá colisión con los límites
willCollideWithBounds :: PhysicsConfig -> PlaneState -> Int -> Bool
willCollideWithBounds config state steps =
    let future = predictPosition config state (Input 0 1) steps
        margin = 50.0  -- Margen de seguridad
    in posX future < margin || posX future > worldWidth config - margin ||
       posY future < margin || posY future > worldHeight config - margin

-- ====================================
-- PARSEO SEGURO CON MANEJO DE ERRORES
-- ====================================

parseInput :: String -> Maybe (PlaneState, Input) --Si Python le envía basura (ej: una letra en vez de un número), el programa podría romperse.
parseInput line = do
    let tokens = words line
    when (length tokens /= 6) Nothing
    
    [x, y, a, s, ir, it] <- mapM readMaybe tokens
    
    return (PlaneState x y a s, Input ir it)

-- ====================================
-- OUTPUT JSON MEJORADO
-- ====================================

formatOutput :: PlaneState -> String
formatOutput (PlaneState x y a s) = 
    printf "{\"x\": %.2f, \"y\": %.2f, \"angle\": %.2f, \"speed\": %.2f}" 
           x y (normalizeAngle a) s

-- Formato de error
formatError :: String -> String
formatError msg = printf "{\"error\": \"%s\", \"x\": 0, \"y\": 0, \"angle\": 0, \"speed\": 0}" msg

-- ====================================
-- BUCLE PRINCIPAL
-- ====================================

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr NoBuffering
    
    -- Mensaje de inicio para debugging
    hPutStrLn stderr "Haskell Physics Engine Started OK"
    
    loop defaultConfig

loop :: PhysicsConfig -> IO ()
loop config = do
    eof <- isEOF --Verifica si Python cerró la conexión
    if eof 
        then hPutStrLn stderr "Physics Engine Terminated"
        else do
            line <- getLine --Lee la cadena "200.0 300.0 90.0 ..." desde Python.
            case parseInput line of
                Just (state, input) -> do
                    let newState = updatePhysics config state input
                    putStrLn (formatOutput newState)
                    
                    -- Debug: advertir si va a colisionar
                    when (willCollideWithBounds config newState 10) $
                        hPutStrLn stderr "[WARN] Warning: Collision predicted in 10 frames"
                    
                Nothing -> do
                    hPutStrLn stderr $ "[ERR] Parse error: " ++ line
                    putStrLn (formatError "Invalid input format")
            
            loop config

-- ====================================
-- UTILIDADES ADICIONALES (FUTURO)
-- ====================================

-- Calcula distancia entre dos estados
distance :: PlaneState -> PlaneState -> Float
distance s1 s2 = sqrt (dx*dx + dy*dy)
  where
    dx = posX s1 - posX s2
    dy = posY s1 - posY s2

-- Calcula ángulo de intercepción para disparar
calculateLeadAngle :: PlaneState -> PlaneState -> Float -> Maybe Float
calculateLeadAngle shooter target bulletSpeed =
    let dx = posX target - posX shooter
        dy = posY target - posY shooter
        dist = sqrt (dx*dx + dy*dy)
        
        -- Tiempo de vuelo de la bala
        timeToTarget = dist / bulletSpeed
        
        -- Predicción simplificada (asume velocidad constante del objetivo)
        futureX = posX target + cos (angle target * pi / 180) * speed target * timeToTarget
        futureY = posY target + sin (angle target * pi / 180) * speed target * timeToTarget
        
        leadAngle = atan2 (futureY - posY shooter) (futureX - posX shooter) * 180 / pi
    in if dist > 0 then Just (normalizeAngle leadAngle) else Nothing