-- This module is a starting point for implementing the Graph Drawing
-- Calculator as described in Part II of the Standard Lab. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.

import ThreepennyPages
import Expr
import Data.Maybe
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI

canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300


main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input   <- mkInput 20 "x"                -- The formula input
     draw    <- mkButton "Draw graph"
     diff    <- mkButton "Differentiate"
     zoomIn  <- mkButton "Zoom In"
     zoomOut <- mkButton "Zoom Out"
     
     constZoomStr   <- mkLabel "Current zoom factor: "
     initZoomfactor <- mkLabel "0.04"
     area           <- mkLabel "Width * Height = "
     mul            <- mkLabel " x "
     initWidth      <- mkLabel "12"
     initHeight     <- mkLabel "12"
     
    
     formula  <- row [pure fx,pure input]
     zooms    <- row [pure zoomIn, pure zoomOut]
     zoomText <- row [pure constZoomStr, pure initZoomfactor]
     areaText <- row [pure area, pure initWidth, pure mul, pure initHeight]
     getBody window #+ [column [pure canvas,pure formula,pure draw,pure diff, pure zooms, pure zoomText, pure areaText]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]

     -- On events
     on UI.click     draw  $ \ _ -> do
        -- Set zoom and area values to defaul
        element initZoomfactor # set value ("0.04")
        element initWidth # set value ("12")
        element initHeight # set value ("12") 
        element initWidth # set UI.html ("12")
        element initHeight # set UI.html ("12")
        element initZoomfactor # set UI.html ("0.04")
        
        readAndDraw input initZoomfactor initWidth initHeight canvas
     
     on UI.click     diff  $ \ _ -> do 
        
        inp <- get value input
        let expVal = readExpr $ inp
        element input # set value (showExpr $ differentiate $ fromJust $ expVal)
        readAndDraw input initZoomfactor initWidth initHeight canvas
     
     on UI.click     zoomIn  $ \ _ -> do
        
        -- change zoomfactor
        currentZfac <- get value initZoomfactor
        let newZfactor = read currentZfac :: Double
        element initZoomfactor # set value (show  (newZfactor * 0.5))
        element initZoomfactor # set UI.html (show  (newZfactor * 0.5))
        
        -- change Width
        currWidth <- get value initWidth
        let newWidth = read currWidth :: Int
        element initWidth # set value (show (div newWidth 2))
        element initWidth # set UI.html (show (div newWidth 2))

        -- change Height
        currHeight <- get value initHeight
        let newHeight = read currHeight :: Int
        element initHeight # set value (show (div newHeight 2))
        element initHeight # set UI.html (show (div newWidth 2))


        readAndDraw input initZoomfactor initWidth initHeight canvas

     on UI.click     zoomOut  $ \ _ -> do
        
        -- change zoomfactor
        currentZfac <- get value initZoomfactor
        let newZfactor = read currentZfac :: Double
        element initZoomfactor # set value (show  (newZfactor * 2.0))
        element initZoomfactor # set UI.html (show  (newZfactor * 2.0))

        -- change Width
        currWidth <- get value initWidth
        let newWidth = read currWidth :: Int
        element initWidth # set value (show (newWidth * 2))
        element initWidth # set UI.html (show (newWidth * 2))

        -- change Height
        currHeight <- get value initHeight
        let newHeight = read currHeight :: Int
        element initHeight # set value (show (newHeight * 2))
        element initHeight # set UI.html (show (newWidth * 2))
        
        readAndDraw input initZoomfactor initWidth initHeight canvas
     
     on valueChange' input $ \ _ -> readAndDraw input initZoomfactor initWidth initHeight canvas


readAndDraw :: Element -> Element ->  Element -> Element -> Canvas -> UI ()
readAndDraw input z w h canvas =
  do -- Get the values for input, zoomfactor, withd, height (Strings)
     formula <- get value input
     let ex = readExpr formula

     zFacStr <- get value z
     let zFac = read zFacStr :: Double

     widthStr <- get value w
     let width = read widthStr :: Int 

     heightStr <- get value h
     let height = read heightStr :: Int

     -- Clear the canvas
     clearCanvas canvas

     -- The following code draws the formula text in the canvas and a blue line.
     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
     UI.fillText ("Simplified: " ++ (showExpr (simplify (fromJust ex)))) (10,canHeight-10) canvas
     path "blue" (points ((fromJust ex)) zFac (width,height)) canvas


     -- converts a pixel x-coordinate to a real x-coordinate
pixToReal_x :: Double -> Double
pixToReal_x x = ((2* x) - (canWidth)) / 2 

-- converts a pixel y-coordinate to a real y-coordinate
pixToReal_y :: Double -> Double
pixToReal_y y = (canHeight - (2*y)) / 2  

  -- converts a real x-coordinate to a pixel x-coordinate
realToPix_x :: Double -> Double
realToPix_x x = ((2* x) + (canWidth)) / 2 

-- converts a real x-coordinate to a pixel x-coordinate
realToPix_y :: Double -> Double
realToPix_y y = ( canHeight - (2*y)) / 2 

-- H
points:: Expr -> Double -> (Int, Int) -> [Point]
points expr scale (width,hight) = [realPoints (x,y) scale | x <- [(-offset),(-offset + scale)..(offset)],
  y <- [(eval expr x)]]
    where offset = fromIntegral width/2

realPoints:: Point -> Double -> Point
realPoints (x,y) scale = (realToPix_x (x/scale) ,realToPix_y (y/scale) )

-- This function made everything slower. Why? 
changeAxis :: Element -> UI Int
changeAxis e = do 
    valStr <- get value e
    let v = (read valStr :: Int) 
    return v

-- mkLabel can be seen as a html
mkLabel :: String -> UI Element
mkLabel html = UI.span # set UI.html (html)
                       # set value (html)
