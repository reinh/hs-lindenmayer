{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}

-- | A Turtle interpretation of L-system strings.
--
-- See: <http://algorithmicbotany.org/papers/abop/abop-ch1.pdfL>
module Main where

import Data.Foldable
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import Diagrams.TwoD.Path.Turtle
import Lindenmayer.D0L

main :: IO ()
main = mainWith (vsep 0.1 rows :: Diagram B)
  where rows = map hcat
          [take 3 islands
          ,take 3 lakes
          ,take 3 triangles
          ,take 3 . drop 3 $ triangles
          ,take 3 . drop 6 $ triangles
          ]

type LTurtle = D0L [] Char

-- | Dragon curve
dragon :: LTurtle
dragon = D0L rls "FX"
  where rls 'F' = "Z"
        rls 'X' = "FX+FY+"
        rls 'Y' = "-FX-FY"
        rls c   = [c]

-- | Sierpinski Triangle
sierpinski :: LTurtle
sierpinski = D0L rls "FX"
  where rls 'F' = "Z"
        rls 'X' = "+FY-FX-FY+"
        rls 'Y' = "-FX+FY+FX-"
        rls c   = [c]

-- | The quadratic Koch island (from Mandlebrot's book).
--
--   * \V : {F, +, -}\
--   * \ω : F − F − F − F\
--   * \P : F → F − F + F + F F − F − F + F\
kochIsland :: LTurtle
kochIsland = D0L rls "F-F-F-F"
  where rls 'F' = "F-F+F+FF-F-F+F"
        rls c = [c]

-- | Koch lake
kochLake :: LTurtle
kochLake = D0L rls "F+F+F+F"
  where rls 'F' = "F+f-FF+F+FF+Ff+FF-f+FF-F-FF-Ff-FFF"
        rls 'f' = "ffffff"
        rls c   = [c]

islands :: [QDiagram B V2 Double Any]
islands = row (90 @@ deg) kochIsland

dragons :: [QDiagram B V2 Double Any]
dragons = row (90 @@ deg) dragon

triangles :: [QDiagram B V2 Double Any]
triangles = row (60 @@ deg) sierpinski

lakes :: [QDiagram B V2 Double Any]
lakes = row (90 @@ deg) kochLake

-- Rendering helpers

runLTurtle :: Angle Double -> LTurtle -> Turtle Double ()
runLTurtle d = traverse_ toCmd . axiom
  where toCmd :: Char -> Turtle Double ()
        toCmd c =
          case c of
            'F' -> penDown >> forward 1
            'f' -> penUp   >> forward 1
            '+' -> penDown >> right (d ^. deg)
            '-' -> penDown >> left (d ^. deg)
            _   -> return ()

row :: Angle Double ->  LTurtle -> [QDiagram B V2 Double Any]
row d = map  (sketch . runLTurtle d) . generate

sketch :: Turtle Double a -> QDiagram B V2 Double Any
sketch = frame 0.1 . sized (dims2D 1 1) . lwL 0.2 . stroke . sketchTurtle
