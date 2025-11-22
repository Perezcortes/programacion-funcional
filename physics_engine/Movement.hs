module Main where

import System.IO
import Text.Printf (printf)

-- Estado y Entradas
data PlaneState = PlaneState { posX :: Float, posY :: Float, angle :: Float, speed :: Float }
data Input = Input { rotInput :: Float, thrustInput :: Float }

-- Configuración
maxSpeed = 8.0
turnSpeed = 4.0

-- Lógica Pura
updatePhysics :: PlaneState -> Input -> PlaneState
updatePhysics (PlaneState x y a s) (Input dRot dThrust) = PlaneState newX newY newAngle newSpeed
  where
    newAngle = a + (dRot * turnSpeed)
    
    -- Aceleración / Fricción
    rawSpeed = if dThrust > 0.1 then min maxSpeed (s + 0.2) else max 0.0 (s - 0.05)
    newSpeed = if dThrust < -0.1 then max 0.0 (rawSpeed - 0.1) else rawSpeed

    -- Trigonometría (Grados a Radianes)
    rad = newAngle * (pi / 180.0)
    newX = wrap (x + cos rad * newSpeed) 800
    newY = wrap (y + sin rad * newSpeed) 600

    wrap val limit = if val > limit then 0 else if val < 0 then limit else val

-- Parseo manual: "x y ang spd rot thr"
parseInput :: String -> (PlaneState, Input)
parseInput line = 
    let [x, y, a, s, ir, it] = map read (words line)
    in (PlaneState x y a s, Input ir it)

-- Output JSON manual
formatOutput :: PlaneState -> String
formatOutput (PlaneState x y a s) = 
    printf "{\"x\": %.2f, \"y\": %.2f, \"angle\": %.2f, \"speed\": %.2f}" x y a s

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    loop

loop :: IO ()
loop = do
    eof <- isEOF
    if eof then return () else do
        line <- getLine
        let (st, inp) = parseInput line
        putStrLn (formatOutput (updatePhysics st inp))
        loop