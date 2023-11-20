import Graphics.Gloss

-- | Display a red circle with a radius of 50 at the center of the window.
main :: IO ()
main = display window bgColor drawing
  where
    window = InWindow "Simple Circle" (400, 400) (10, 10)
    bgColor = white
    drawing = color red (circleSolid 50)
